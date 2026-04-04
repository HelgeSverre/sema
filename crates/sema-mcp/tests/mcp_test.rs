use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

fn sema_bin() -> String {
    let mut path = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    path.push("sema");
    path.to_string_lossy().to_string()
}

fn send_and_recv(
    stdin: &mut impl Write,
    stdout: &mut impl BufRead,
    request: &serde_json::Value,
) -> serde_json::Value {
    let req_str = serde_json::to_string(request).unwrap();
    writeln!(stdin, "{req_str}").unwrap();
    stdin.flush().unwrap();

    let mut line = String::new();
    stdout.read_line(&mut line).unwrap();
    serde_json::from_str(line.trim()).unwrap()
}

#[test]
fn test_mcp_server_initialize_and_list_tools() {
    // Write a temp sema file with a deftool
    let dir = std::env::temp_dir().join("sema-mcp-test");
    std::fs::create_dir_all(&dir).unwrap();
    let tool_file = dir.join("tools.sema");
    std::fs::write(
        &tool_file,
        r#"
(deftool greet
  "Greet someone by name"
  {:name {:type :string :description "The name to greet"}}
  (lambda (name)
    (string-append "Hello, " name "!")))
"#,
    )
    .unwrap();

    let mut child = Command::new(sema_bin())
        .args(["serve", "--no-llm", tool_file.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to start sema serve");

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());

    // 1. Initialize
    let resp = send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "test", "version": "0.1.0"}
            }
        }),
    );
    assert_eq!(resp["result"]["protocolVersion"], "2024-11-05");
    assert!(resp["result"]["capabilities"]["tools"].is_object());

    // 2. Send initialized notification (no response expected)
    let notif = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    });
    writeln!(stdin, "{}", serde_json::to_string(&notif).unwrap()).unwrap();
    stdin.flush().unwrap();

    // 3. List tools
    let resp = send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list",
            "params": {}
        }),
    );
    let tools = resp["result"]["tools"].as_array().unwrap();
    assert_eq!(tools.len(), 1);
    assert_eq!(tools[0]["name"], "greet");
    assert_eq!(tools[0]["description"], "Greet someone by name");
    assert!(tools[0]["inputSchema"]["properties"]["name"].is_object());

    // 4. Call a tool
    let resp = send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "greet",
                "arguments": {"name": "World"}
            }
        }),
    );
    let content = resp["result"]["content"].as_array().unwrap();
    assert_eq!(content[0]["type"], "text");
    assert_eq!(content[0]["text"], "Hello, World!");

    // Clean up
    drop(stdin);
    child.kill().ok();
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn test_mcp_server_with_compiled_bytecode() {
    // Compile a .sema file to .semac, then serve it
    let dir = std::env::temp_dir().join("sema-mcp-test-semac");
    std::fs::create_dir_all(&dir).unwrap();
    let source_file = dir.join("tools.sema");
    let bytecode_file = dir.join("tools.semac");
    std::fs::write(
        &source_file,
        r#"
(deftool reverse-text
  "Reverse a string"
  {:text {:type :string :description "Text to reverse"}}
  (lambda (text)
    (list->string (reverse (string->list text)))))
"#,
    )
    .unwrap();

    // Compile to bytecode
    let compile_output = Command::new(sema_bin())
        .args([
            "compile",
            source_file.to_str().unwrap(),
            "-o",
            bytecode_file.to_str().unwrap(),
        ])
        .output()
        .expect("failed to compile");
    assert!(
        compile_output.status.success(),
        "compile failed: {}",
        String::from_utf8_lossy(&compile_output.stderr)
    );

    // Serve the compiled bytecode
    let mut child = Command::new(sema_bin())
        .args(["serve", "--no-llm", bytecode_file.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to start sema serve with .semac");

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());

    // Initialize
    send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {}
        }),
    );

    // List tools — should find the compiled tool
    let resp = send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list",
            "params": {}
        }),
    );
    let tools = resp["result"]["tools"].as_array().unwrap();
    assert_eq!(tools.len(), 1);
    assert_eq!(tools[0]["name"], "reverse-text");

    // Call the tool
    let resp = send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "reverse-text",
                "arguments": {"text": "hello"}
            }
        }),
    );
    let content = resp["result"]["content"].as_array().unwrap();
    assert_eq!(content[0]["text"], "olleh");

    drop(stdin);
    child.kill().ok();
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn test_mcp_server_unknown_tool() {
    let dir = std::env::temp_dir().join("sema-mcp-test-2");
    std::fs::create_dir_all(&dir).unwrap();
    let tool_file = dir.join("empty.sema");
    std::fs::write(&tool_file, "(define x 42)\n").unwrap();

    let mut child = Command::new(sema_bin())
        .args(["serve", "--no-llm", tool_file.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to start sema serve");

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());

    // Initialize first
    send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {}
        }),
    );

    // Call a non-existent tool
    let resp = send_and_recv(
        &mut stdin,
        &mut stdout,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {"name": "nonexistent", "arguments": {}}
        }),
    );
    assert!(resp["error"].is_object());
    assert_eq!(resp["error"]["code"], -32602);

    drop(stdin);
    child.kill().ok();
    std::fs::remove_dir_all(&dir).ok();
}
