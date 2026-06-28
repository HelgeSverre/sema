use std::collections::HashMap;
use std::path::PathBuf;

use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};

use crate::protocol::{CallToolResult, JsonRpcRequest, JsonRpcResponse, Tool};

#[derive(Debug, Clone, Default)]
pub struct McpClientConfig {
    pub command: String,
    pub args: Vec<String>,
    pub env: Option<HashMap<String, String>>,
    pub cwd: Option<PathBuf>,
    pub auth: Option<McpAuthConfig>,
}

impl McpClientConfig {
    pub fn new(command: impl Into<String>) -> Self {
        Self {
            command: command.into(),
            args: Vec::new(),
            env: None,
            cwd: None,
            auth: None,
        }
    }

    pub fn with_auth(mut self, auth: McpAuthConfig) -> Self {
        self.auth = Some(auth);
        self
    }

    pub fn with_auth_token(mut self, token: impl Into<String>) -> Self {
        self.auth = Some(McpAuthConfig::token(token));
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct McpAuthConfig {
    pub token: Option<String>,
    pub authorization: Option<String>,
}

impl McpAuthConfig {
    pub fn token(token: impl Into<String>) -> Self {
        Self {
            token: Some(token.into()),
            authorization: None,
        }
    }

    pub fn oauth(token: impl Into<String>) -> Self {
        Self::token(token)
    }

    pub fn with_authorization(authorization: impl Into<String>) -> Self {
        Self {
            token: None,
            authorization: Some(authorization.into()),
        }
    }

    fn bearer_header(token: &str) -> String {
        let mut header = String::from("Bearer ");
        header.push_str(token);
        header
    }

    pub fn from_config_value(config: &serde_json::Value) -> Option<Self> {
        if let Some(auth_value) = config.get("auth") {
            if let Some(auth) = Self::from_json_value(auth_value) {
                return Some(auth);
            }
        }

        let mut auth = Self::default();
        auth.token = config
            .get("token")
            .and_then(|value| value.as_str())
            .map(str::to_string)
            .or_else(|| {
                config
                    .get("auth-token")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
            })
            .or_else(|| {
                config
                    .get("auth_token")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
            })
            .or_else(|| {
                config
                    .get("access_token")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
            })
            .or_else(|| {
                config
                    .get("accessToken")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
            });
        auth.authorization = config
            .get("authorization")
            .and_then(|value| value.as_str())
            .map(str::to_string)
            .or_else(|| {
                config
                    .get("auth-authorization")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
            })
            .or_else(|| {
                config
                    .get("auth_authorization")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
            })
            .or_else(|| {
                config
                    .get("bearer")
                    .and_then(|value| value.as_str())
                    .map(Self::bearer_header)
            });

        if auth.authorization.is_none() {
            if let Some(token) = auth.token.as_deref() {
                auth.authorization = Some(Self::bearer_header(token));
            }
        }

        if auth.token.is_some() || auth.authorization.is_some() {
            Some(auth)
        } else {
            None
        }
    }

    pub fn from_json_value(value: &serde_json::Value) -> Option<Self> {
        match value {
            serde_json::Value::String(token) => Some(Self::token(token)),
            serde_json::Value::Object(map) => {
                let mut auth = Self::default();
                auth.token = map
                    .get("token")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
                    .or_else(|| {
                        map.get("access_token")
                            .and_then(|value| value.as_str())
                            .map(str::to_string)
                    })
                    .or_else(|| {
                        map.get("accessToken")
                            .and_then(|value| value.as_str())
                            .map(str::to_string)
                    })
                    .or_else(|| {
                        map.get("auth-token")
                            .and_then(|value| value.as_str())
                            .map(str::to_string)
                    })
                    .or_else(|| {
                        map.get("auth_token")
                            .and_then(|value| value.as_str())
                            .map(str::to_string)
                    });
                auth.authorization = map
                    .get("authorization")
                    .and_then(|value| value.as_str())
                    .map(str::to_string)
                    .or_else(|| {
                        map.get("auth-authorization")
                            .and_then(|value| value.as_str())
                            .map(str::to_string)
                    })
                    .or_else(|| {
                        map.get("auth_authorization")
                            .and_then(|value| value.as_str())
                            .map(str::to_string)
                    })
                    .or_else(|| {
                        map.get("bearer")
                            .and_then(|value| value.as_str())
                            .map(Self::bearer_header)
                    });

                if auth.authorization.is_none() {
                    if let Some(token) = auth.token.as_deref() {
                        auth.authorization = Some(Self::bearer_header(token));
                    }
                }

                if auth.token.is_some() || auth.authorization.is_some() {
                    Some(auth)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn apply_to_env(&self, env: &mut HashMap<String, String>) {
        if let Some(token) = self.token.as_deref() {
            env.insert("MCP_AUTH_TOKEN".to_string(), token.to_string());
        }
        if let Some(authorization) = self.authorization.as_deref() {
            env.insert("MCP_AUTHORIZATION".to_string(), authorization.to_string());
        } else if let Some(token) = self.token.as_deref() {
            env.insert("MCP_AUTHORIZATION".to_string(), Self::bearer_header(token));
        }
    }
}

pub struct McpClient {
    child: Child,
    stdin: ChildStdin,
    reader: BufReader<ChildStdout>,
    next_id: i64,
}

impl McpClient {
    pub async fn connect(config: McpClientConfig) -> Result<Self, String> {
        let mut command = Command::new(&config.command);
        command.args(&config.args);
        let mut env_map = config.env.unwrap_or_default();
        if let Some(auth) = config.auth.as_ref() {
            auth.apply_to_env(&mut env_map);
        }
        if !env_map.is_empty() {
            for (key, value) in env_map {
                command.env(key, value);
            }
        }
        if let Some(cwd) = config.cwd {
            command.current_dir(cwd);
        }
        command.stdin(std::process::Stdio::piped());
        command.stdout(std::process::Stdio::piped());
        command.stderr(std::process::Stdio::inherit());

        let mut child = command
            .spawn()
            .map_err(|err| format!("failed to spawn MCP server process: {err}"))?;

        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| "failed to open MCP server stdin".to_string())?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| "failed to open MCP server stdout".to_string())?;

        Ok(Self {
            child,
            stdin,
            reader: BufReader::new(stdout),
            next_id: 1,
        })
    }

    pub async fn initialize(&mut self) -> Result<serde_json::Value, String> {
        self.request(
            "initialize",
            Some(serde_json::json!({
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {
                    "name": "sema-mcp-client",
                    "version": env!("CARGO_PKG_VERSION")
                }
            })),
        )
        .await
    }

    pub async fn list_tools(&mut self) -> Result<Vec<Tool>, String> {
        let result = self.request("tools/list", None).await?;
        let tools = result
            .get("tools")
            .ok_or_else(|| "tools/list result did not include a tools array".to_string())?;
        serde_json::from_value(tools.clone())
            .map_err(|err| format!("failed to decode tools/list response: {err}"))
    }

    pub async fn call_tool(
        &mut self,
        name: &str,
        arguments: serde_json::Value,
    ) -> Result<CallToolResult, String> {
        let result = self
            .request(
                "tools/call",
                Some(serde_json::json!({
                    "name": name,
                    "arguments": arguments
                })),
            )
            .await?;
        serde_json::from_value(result)
            .map_err(|err| format!("failed to decode tools/call response: {err}"))
    }

    pub async fn close(&mut self) -> Result<(), String> {
        self.child
            .start_kill()
            .map_err(|err| format!("failed to terminate MCP server process: {err}"))?;
        self.child
            .wait()
            .await
            .map_err(|err| format!("failed to wait for MCP server process: {err}"))?;
        Ok(())
    }

    async fn request(
        &mut self,
        method: &str,
        params: Option<serde_json::Value>,
    ) -> Result<serde_json::Value, String> {
        let id = self.next_id;
        self.next_id = self
            .next_id
            .checked_add(1)
            .ok_or_else(|| "MCP request ID overflow".to_string())?;

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params,
            id: Some(serde_json::Value::Number(serde_json::Number::from(id))),
        };

        self.send_request(&request).await
    }

    async fn send_request(
        &mut self,
        request: &JsonRpcRequest,
    ) -> Result<serde_json::Value, String> {
        let line = serde_json::to_string(request)
            .map_err(|err| format!("failed to encode MCP request: {err}"))?;
        self.stdin
            .write_all(format!("{line}\n").as_bytes())
            .await
            .map_err(|err| format!("failed to write MCP request: {err}"))?;
        self.stdin
            .flush()
            .await
            .map_err(|err| format!("failed to flush MCP request: {err}"))?;

        let mut response_line = String::new();
        let bytes_read = self
            .reader
            .read_line(&mut response_line)
            .await
            .map_err(|err| format!("failed to read MCP response: {err}"))?;

        if bytes_read == 0 {
            return Err("MCP server closed the connection before responding".to_string());
        }

        if response_line.trim().is_empty() {
            return Err("MCP server returned an empty response line".to_string());
        }

        let response: JsonRpcResponse = serde_json::from_str(response_line.trim())
            .map_err(|err| format!("failed to decode MCP response: {err}"))?;

        if let Some(error) = response.error {
            return Err(format!("MCP RPC error {}: {}", error.code, error.message));
        }

        response
            .result
            .ok_or_else(|| "MCP response did not include a result".to_string())
    }
}

impl Drop for McpClient {
    fn drop(&mut self) {
        let _ = self.child.start_kill();
    }
}
