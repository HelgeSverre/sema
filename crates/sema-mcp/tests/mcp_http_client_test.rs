//! Streamable-HTTP MCP client tests. A scripted Python server exercises the
//! wire contract the transport must satisfy: it assigns an `Mcp-Session-Id` on
//! the `initialize` response and **rejects** any later request that fails to
//! echo it or the negotiated `MCP-Protocol-Version` (so a client that drops
//! either header fails here); it returns `tools/list` as a single JSON object
//! but `tools/call` as an SSE stream with an interleaved notification ahead of
//! the real response (so a client that mishandles SSE or fails to skip
//! unrelated messages fails here); and it echoes the caller's `Authorization`
//! header back through the tool result (so bring-your-own-token is verified).

use std::io::{BufRead, BufReader};
use std::process::{Child, ChildStdout, Command, Stdio};

use sema_mcp::{McpClient, McpHttpConfig};
use serde_json::json;

const HTTP_SERVER: &str = r#"
import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

SESSION = "sema-test-session"
PROTOCOL = "2025-11-25"
state = {"auth": None}

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass  # keep the test output clean

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length) if length else b""
        return json.loads(raw) if raw else {}

    def _json(self, payload, extra_headers=None):
        data = json.dumps(payload).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        for k, v in (extra_headers or {}).items():
            self.send_header(k, v)
        self.end_headers()
        self.wfile.write(data)

    def _bad(self, code):
        self.send_response(code)
        self.end_headers()

    def do_DELETE(self):
        # Client-driven session termination.
        self.send_response(200)
        self.end_headers()

    def do_POST(self):
        msg = self._read_body()
        method = msg.get("method")
        rid = msg.get("id")

        # Notifications carry no id and are accepted with 202; they must still
        # carry the session id once one has been assigned.
        if rid is None:
            if self.headers.get("Mcp-Session-Id") != SESSION:
                return self._bad(400)
            self.send_response(202)
            self.end_headers()
            return

        if method == "initialize":
            state["auth"] = self.headers.get("Authorization")
            return self._json(
                {"jsonrpc": "2.0", "id": rid, "result": {
                    "protocolVersion": PROTOCOL,
                    "capabilities": {},
                    "serverInfo": {"name": "http-test-server", "version": "1.0"}}},
                extra_headers={"Mcp-Session-Id": SESSION})

        # Every post-init request MUST echo the session id + protocol version.
        if self.headers.get("Mcp-Session-Id") != SESSION:
            return self._bad(400)
        if self.headers.get("MCP-Protocol-Version") != PROTOCOL:
            return self._bad(400)

        if method == "tools/list":
            return self._json({"jsonrpc": "2.0", "id": rid, "result": {"tools": [
                {"name": "echo", "description": "Echo a string",
                 "inputSchema": {"type": "object",
                                 "properties": {"text": {"type": "string"}},
                                 "required": ["text"]}}]}})

        if method == "tools/call":
            args = msg.get("params", {}).get("arguments", {})
            text = args.get("text", "")
            payload = {"jsonrpc": "2.0", "id": rid, "result": {
                "content": [{"type": "text", "text": text + "|auth=" + str(state["auth"])}],
                "isError": False}}
            note = {"jsonrpc": "2.0", "method": "notifications/progress", "params": {}}
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.end_headers()
            self.wfile.write(b": priming\n\n")
            self.wfile.write(("data: " + json.dumps(note) + "\n\n").encode())
            self.wfile.write(("data: " + json.dumps(payload) + "\n\n").encode())
            self.wfile.flush()
            return

        return self._json({"jsonrpc": "2.0", "id": rid,
                           "error": {"code": -32601, "message": "Method not found"}})

server = HTTPServer(("127.0.0.1", 0), Handler)
print(server.server_address[1], flush=True)
server.serve_forever()
"#;

/// Owns the spawned server; killing it on drop keeps the BufReader (and thus the
/// stdout pipe) alive so the server never takes SIGPIPE mid-test.
struct ServerGuard {
    child: Child,
    _stdout: BufReader<ChildStdout>,
}

impl Drop for ServerGuard {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn start_server() -> (ServerGuard, u16) {
    let mut child = Command::new("python3")
        .args(["-c", HTTP_SERVER])
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn python3 HTTP MCP server");
    let stdout = child.stdout.take().expect("server stdout");
    let mut reader = BufReader::new(stdout);
    let mut line = String::new();
    reader
        .read_line(&mut line)
        .expect("failed to read server port line");
    let port: u16 = line.trim().parse().expect("server should print its port");
    (
        ServerGuard {
            child,
            _stdout: reader,
        },
        port,
    )
}

#[tokio::test]
async fn test_http_client_streamable_transport() {
    let (_server, port) = start_server();
    let url = format!("http://127.0.0.1:{port}/mcp");

    let mut config = McpHttpConfig::new(url);
    config
        .headers
        .insert("Authorization".to_string(), "Bearer test-token".to_string());

    let mut client = McpClient::connect_http(config)
        .await
        .expect("failed to prepare HTTP MCP client");

    let init = client
        .initialize()
        .await
        .expect("initialize over HTTP should succeed");
    assert_eq!(init["serverInfo"]["name"], "http-test-server");
    assert_eq!(init["protocolVersion"], "2025-11-25");

    // Succeeds only if the client echoed the assigned Mcp-Session-Id and the
    // negotiated MCP-Protocol-Version header (the server 400s otherwise).
    let tools = client
        .list_tools()
        .await
        .expect("tools/list over HTTP should succeed");
    assert_eq!(tools.len(), 1);
    assert_eq!(tools[0].name, "echo");

    // tools/call arrives as SSE with an interleaved notification the client must
    // skip; the Authorization header must have round-tripped to the server.
    let result = client
        .call_tool("echo", json!({ "text": "hi" }))
        .await
        .expect("tools/call over SSE should succeed");
    assert_eq!(result["content"][0]["text"], "hi|auth=Bearer test-token");
    assert_eq!(result["isError"], false);

    client.close().await.expect("closing should succeed");
}
