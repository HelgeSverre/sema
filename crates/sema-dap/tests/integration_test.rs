use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Command, Stdio};
use std::time::Duration;

fn sema_binary() -> String {
    // Find the sema binary in the target directory
    let mut path = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    path.push("sema");
    if cfg!(windows) {
        path.set_extension("exe");
    }
    path.to_string_lossy().to_string()
}

fn unique_temp_dir(prefix: &str) -> std::path::PathBuf {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let dir =
        std::env::temp_dir().join(format!("sema-dap-{prefix}-{}-{nanos}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");
    dir
}

fn send_dap(stdin: &mut impl Write, seq: u64, command: &str, args: Option<serde_json::Value>) {
    let mut msg = serde_json::json!({
        "seq": seq,
        "type": "request",
        "command": command,
    });
    if let Some(a) = args {
        msg.as_object_mut()
            .unwrap()
            .insert("arguments".to_string(), a);
    }
    let body = serde_json::to_string(&msg).unwrap();
    let header = format!("Content-Length: {}\r\n\r\n", body.len());
    stdin.write_all(header.as_bytes()).unwrap();
    stdin.write_all(body.as_bytes()).unwrap();
    stdin.flush().unwrap();
}

/// Read a DAP message from the child process stdout with a timeout.
fn read_dap_timeout(
    reader: &mut BufReader<impl Read>,
    timeout: Duration,
) -> Option<serde_json::Value> {
    let deadline = std::time::Instant::now() + timeout;

    let mut header = String::new();
    let mut content_length: Option<usize> = None;
    loop {
        if std::time::Instant::now() > deadline {
            return None;
        }
        header.clear();
        let n = reader.read_line(&mut header).ok()?;
        if n == 0 {
            return None;
        }
        let trimmed = header.trim();
        if trimmed.is_empty() {
            break;
        }
        if let Some(len_str) = trimmed.strip_prefix("Content-Length: ") {
            content_length = len_str.parse().ok();
        }
    }
    let len = content_length?;
    let mut body = vec![0u8; len];
    reader.read_exact(&mut body).ok()?;
    serde_json::from_slice(&body).ok()
}

/// Default read timeout for DAP messages (10 seconds).
const DAP_TIMEOUT: Duration = Duration::from_secs(10);

fn read_dap(reader: &mut BufReader<impl Read>) -> Option<serde_json::Value> {
    read_dap_timeout(reader, DAP_TIMEOUT)
}

/// Wait for a specific DAP event, skipping unrelated messages.
fn wait_for_event(
    reader: &mut BufReader<impl Read>,
    event_name: &str,
    max_messages: usize,
) -> bool {
    for _ in 0..max_messages {
        if let Some(msg) = read_dap(reader) {
            if msg["type"] == "event" && msg["event"] == event_name {
                return true;
            }
        } else {
            return false;
        }
    }
    false
}

#[test]
fn test_dap_initialize_and_disconnect() {
    let binary = sema_binary();

    let mut child = Command::new(&binary)
        .arg("dap")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {binary}: {e}"));

    let mut stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout);

    // Send initialize
    send_dap(&mut stdin, 1, "initialize", Some(serde_json::json!({})));

    // Read initialize response
    let resp = read_dap(&mut reader).expect("should get initialize response");
    assert_eq!(resp["type"], "response");
    assert_eq!(resp["command"], "initialize");
    assert_eq!(resp["success"], true);
    assert_eq!(
        resp["body"]["supportsConfigurationDoneRequest"], true,
        "should support configurationDone"
    );

    // Read initialized event
    let event = read_dap(&mut reader).expect("should get initialized event");
    assert_eq!(event["type"], "event");
    assert_eq!(event["event"], "initialized");

    // Send disconnect
    send_dap(&mut stdin, 2, "disconnect", None);

    // Read disconnect response
    let resp = read_dap(&mut reader).expect("should get disconnect response");
    assert_eq!(resp["type"], "response");
    assert_eq!(resp["command"], "disconnect");
    assert_eq!(resp["success"], true);

    // Process should exit
    let status = child.wait().expect("failed to wait for child");
    assert!(
        status.success(),
        "sema dap should exit cleanly after disconnect"
    );
}

#[test]
fn test_dap_launch_and_run() {
    let binary = sema_binary();

    // Create a simple test program in a unique temp dir
    let dir = unique_temp_dir("launch");
    let program_path = dir.join("test.sema");
    std::fs::write(&program_path, "(+ 1 2)\n").unwrap();

    let mut child = Command::new(&binary)
        .arg("dap")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {binary}: {e}"));

    let mut stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout);

    // Initialize
    send_dap(&mut stdin, 1, "initialize", Some(serde_json::json!({})));
    let _resp = read_dap(&mut reader).unwrap(); // response
    let _event = read_dap(&mut reader).unwrap(); // initialized event

    // Launch
    send_dap(
        &mut stdin,
        2,
        "launch",
        Some(serde_json::json!({
            "program": program_path.to_string_lossy(),
        })),
    );
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "launch");
    assert_eq!(resp["success"], true);

    // ConfigurationDone — this triggers execution
    send_dap(&mut stdin, 3, "configurationDone", None);
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "configurationDone");
    assert_eq!(resp["success"], true);

    // Should get terminated event (program runs to completion)
    // May get output events first — allow up to 50 messages
    assert!(
        wait_for_event(&mut reader, "terminated", 50),
        "should receive terminated event"
    );

    // Disconnect
    send_dap(&mut stdin, 4, "disconnect", None);
    let _ = read_dap(&mut reader); // disconnect response

    let status = child.wait().expect("failed to wait for child");
    assert!(status.success());

    // Cleanup
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_dap_breakpoint_and_continue() {
    let binary = sema_binary();

    let dir = unique_temp_dir("bp");
    let program_path = dir.join("test_bp.sema");
    // Multi-line program — set breakpoint on line 2
    std::fs::write(&program_path, "(define x 1)\n(define y 2)\n(+ x y)\n").unwrap();

    let mut child = Command::new(&binary)
        .arg("dap")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {binary}: {e}"));

    let mut stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout);

    // Initialize
    send_dap(&mut stdin, 1, "initialize", Some(serde_json::json!({})));
    let _resp = read_dap(&mut reader).unwrap();
    let _event = read_dap(&mut reader).unwrap();

    // Set breakpoint on line 2 before launch
    send_dap(
        &mut stdin,
        2,
        "setBreakpoints",
        Some(serde_json::json!({
            "source": { "path": program_path.to_string_lossy() },
            "breakpoints": [{ "line": 2 }],
        })),
    );
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "setBreakpoints");
    assert_eq!(resp["success"], true);

    // Launch with stopOnEntry = false
    send_dap(
        &mut stdin,
        3,
        "launch",
        Some(serde_json::json!({
            "program": program_path.to_string_lossy(),
        })),
    );
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "launch");
    assert_eq!(resp["success"], true);

    // ConfigurationDone
    send_dap(&mut stdin, 4, "configurationDone", None);
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "configurationDone");
    assert_eq!(resp["success"], true);

    // Should get a stopped event (breakpoint or step) — allow up to 50 messages
    assert!(
        wait_for_event(&mut reader, "stopped", 50),
        "should receive stopped event at breakpoint"
    );

    // Request stack trace while stopped
    send_dap(&mut stdin, 5, "stackTrace", Some(serde_json::json!({})));
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "stackTrace");
    assert_eq!(resp["success"], true);
    let frames = resp["body"]["stackFrames"].as_array().unwrap();
    assert!(!frames.is_empty(), "should have at least one stack frame");

    // Continue execution
    send_dap(&mut stdin, 6, "continue", Some(serde_json::json!({})));
    let resp = read_dap(&mut reader).unwrap();
    assert_eq!(resp["command"], "continue");
    assert_eq!(resp["success"], true);

    // Should get terminated event — allow up to 50 messages
    assert!(
        wait_for_event(&mut reader, "terminated", 50),
        "should receive terminated event"
    );

    // Disconnect
    send_dap(&mut stdin, 7, "disconnect", None);
    let _ = read_dap(&mut reader);

    let status = child.wait().expect("failed to wait for child");
    assert!(status.success());

    let _ = std::fs::remove_dir_all(&dir);
}
