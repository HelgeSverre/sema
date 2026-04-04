//! MCP (Model Context Protocol) server for Sema.
//!
//! Exposes all `deftool` definitions from a Sema script as MCP tools,
//! communicating via JSON-RPC 2.0 over stdio.

use sema_core::{call_callback, json_to_value, value_to_json_lossy, EvalContext, Value};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

/// A discovered tool from the Sema environment.
pub struct McpTool {
    pub name: String,
    pub description: String,
    pub parameters: Value,
    pub handler: Value,
}

/// Scan an environment for all `ToolDef` values and return them.
pub fn discover_tools(env: &sema_core::Env) -> Vec<McpTool> {
    let mut tools = Vec::new();
    let names = env.all_names();
    for spur in names {
        if let Some(val) = env.get(spur) {
            if let Some(td) = val.as_tool_def_rc() {
                tools.push(McpTool {
                    name: td.name.clone(),
                    description: td.description.clone(),
                    parameters: td.parameters.clone(),
                    handler: td.handler.clone(),
                });
            }
        }
    }
    tools
}

/// Convert a Sema parameter schema to JSON Schema (same logic as sema-llm).
fn sema_params_to_json_schema(val: &Value) -> serde_json::Value {
    if let Some(map) = val.as_map_rc() {
        let mut properties = serde_json::Map::new();
        let mut required = Vec::new();
        for (k, v) in map.iter() {
            let key = k
                .as_keyword()
                .or_else(|| k.as_str().map(|s| s.to_string()))
                .unwrap_or_else(|| k.to_string());
            let prop = if let Some(inner) = v.as_map_rc() {
                let mut prop_obj = serde_json::Map::new();
                if let Some(t) = inner.get(&Value::keyword("type")) {
                    let type_str = t
                        .as_keyword()
                        .or_else(|| t.as_str().map(|s| s.to_string()))
                        .unwrap_or_else(|| "string".to_string());
                    prop_obj.insert("type".to_string(), serde_json::Value::String(type_str));
                }
                if let Some(d) = inner.get(&Value::keyword("description")) {
                    let desc = d
                        .as_str()
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| d.to_string());
                    prop_obj.insert("description".to_string(), serde_json::Value::String(desc));
                }
                if let Some(e) = inner.get(&Value::keyword("enum")) {
                    if let Some(items) = e.as_seq() {
                        let vals: Vec<serde_json::Value> = items
                            .iter()
                            .map(|v| {
                                serde_json::Value::String(
                                    v.as_str()
                                        .map(|s| s.to_string())
                                        .or_else(|| v.as_keyword())
                                        .unwrap_or_else(|| v.to_string()),
                                )
                            })
                            .collect();
                        prop_obj.insert("enum".to_string(), serde_json::Value::Array(vals));
                    }
                }
                let optional = inner
                    .get(&Value::keyword("optional"))
                    .map(|v| v.is_truthy())
                    .unwrap_or(false);
                if !optional {
                    required.push(serde_json::Value::String(key.clone()));
                }
                serde_json::Value::Object(prop_obj)
            } else {
                required.push(serde_json::Value::String(key.clone()));
                serde_json::json!({"type": "string"})
            };
            properties.insert(key, prop);
        }
        serde_json::json!({
            "type": "object",
            "properties": properties,
            "required": required
        })
    } else {
        serde_json::json!({"type": "object", "properties": {}})
    }
}

// ── JSON-RPC types ──────────────────────────────────────────────────

#[derive(Deserialize)]
struct JsonRpcRequest {
    #[allow(dead_code)]
    jsonrpc: String,
    id: Option<serde_json::Value>,
    method: String,
    #[serde(default)]
    params: serde_json::Value,
}

#[derive(Serialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

#[derive(Serialize)]
struct JsonRpcError {
    code: i64,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<serde_json::Value>,
}

impl JsonRpcResponse {
    fn success(id: serde_json::Value, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }

    fn error(id: serde_json::Value, code: i64, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(JsonRpcError {
                code,
                message: message.into(),
                data: None,
            }),
        }
    }
}

// ── MCP Server ──────────────────────────────────────────────────────

/// Run the MCP server over stdio with the given discovered tools and eval context.
pub fn run_server(
    tools: Vec<McpTool>,
    ctx: &EvalContext,
    env: &Rc<sema_core::Env>,
) -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let reader = stdin.lock();
    let mut writer = stdout.lock();

    // Build tool list response once
    let tools_list: Vec<serde_json::Value> = tools
        .iter()
        .map(|t| {
            serde_json::json!({
                "name": t.name,
                "description": t.description,
                "inputSchema": sema_params_to_json_schema(&t.parameters),
            })
        })
        .collect();

    // Build a lookup map for tool handlers
    let tool_map: BTreeMap<String, &McpTool> = tools.iter().map(|t| (t.name.clone(), t)).collect();

    for line in reader.lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let req: JsonRpcRequest = match serde_json::from_str(line) {
            Ok(r) => r,
            Err(e) => {
                let resp = JsonRpcResponse::error(
                    serde_json::Value::Null,
                    -32700,
                    format!("Parse error: {e}"),
                );
                write_response(&mut writer, &resp)?;
                continue;
            }
        };

        // Notifications (no id) don't get responses
        if req.id.is_none() {
            // Handle notifications/initialized silently
            continue;
        }

        let id = req.id.unwrap_or(serde_json::Value::Null);

        let resp = match req.method.as_str() {
            "initialize" => JsonRpcResponse::success(
                id,
                serde_json::json!({
                    "protocolVersion": "2024-11-05",
                    "capabilities": {
                        "tools": {}
                    },
                    "serverInfo": {
                        "name": "sema-mcp",
                        "version": env!("CARGO_PKG_VERSION")
                    }
                }),
            ),
            "ping" => JsonRpcResponse::success(id, serde_json::json!({})),
            "tools/list" => JsonRpcResponse::success(
                id,
                serde_json::json!({
                    "tools": tools_list
                }),
            ),
            "tools/call" => handle_tool_call(id, &req.params, &tool_map, ctx, env),
            _ => JsonRpcResponse::error(id, -32601, format!("Method not found: {}", req.method)),
        };

        write_response(&mut writer, &resp)?;
    }

    Ok(())
}

fn handle_tool_call(
    id: serde_json::Value,
    params: &serde_json::Value,
    tool_map: &BTreeMap<String, &McpTool>,
    ctx: &EvalContext,
    _env: &Rc<sema_core::Env>,
) -> JsonRpcResponse {
    let tool_name = params.get("name").and_then(|v| v.as_str()).unwrap_or("");

    let tool = match tool_map.get(tool_name) {
        Some(t) => t,
        None => {
            return JsonRpcResponse::error(id, -32602, format!("Unknown tool: {tool_name}"));
        }
    };

    // Extract arguments from params.arguments
    let arguments = params
        .get("arguments")
        .cloned()
        .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));

    // Build argument list in parameter order
    let args = build_args_from_params(&tool.parameters, &arguments);

    // Call the handler
    match call_callback(ctx, &tool.handler, &args) {
        Ok(result) => {
            let result_str = match result.as_str() {
                Some(s) => s.to_string(),
                None => {
                    // Convert to JSON and then to string
                    let json = value_to_json_lossy(&result);
                    match json {
                        serde_json::Value::String(s) => s,
                        other => other.to_string(),
                    }
                }
            };
            JsonRpcResponse::success(
                id,
                serde_json::json!({
                    "content": [{
                        "type": "text",
                        "text": result_str
                    }]
                }),
            )
        }
        Err(e) => JsonRpcResponse::success(
            id,
            serde_json::json!({
                "content": [{
                    "type": "text",
                    "text": format!("Error: {e}")
                }],
                "isError": true
            }),
        ),
    }
}

/// Build positional args from JSON arguments object, ordered by parameter definition.
fn build_args_from_params(params: &Value, arguments: &serde_json::Value) -> Vec<Value> {
    let args_obj = arguments.as_object();
    if let Some(map) = params.as_map_rc() {
        map.keys()
            .map(|k| {
                let key = k
                    .as_keyword()
                    .or_else(|| k.as_str().map(|s| s.to_string()))
                    .unwrap_or_else(|| k.to_string());
                match args_obj.and_then(|obj| obj.get(&key)) {
                    Some(v) => json_to_value(v),
                    None => Value::nil(),
                }
            })
            .collect()
    } else if let Some(obj) = args_obj {
        // No schema — pass all values as a map
        let mut map = BTreeMap::new();
        for (k, v) in obj {
            map.insert(Value::keyword(k), json_to_value(v));
        }
        vec![Value::map(map)]
    } else {
        vec![]
    }
}

fn write_response(
    writer: &mut impl Write,
    resp: &JsonRpcResponse,
) -> Result<(), Box<dyn std::error::Error>> {
    let json = serde_json::to_string(resp)?;
    writeln!(writer, "{json}")?;
    writer.flush()?;
    Ok(())
}
