//! End-to-end OAuth login test (the M4 acceptance gate), fully offline.
//!
//! A scripted Python server plays BOTH roles: the OAuth authorization server
//! (RFC 9728/8414 metadata, RFC 7591 DCR, /authorize, /token) and the MCP
//! resource server. The Rust side runs the real `oauth::login` against it,
//! driving the browser leg with a `LoopbackDriver` whose "opener" is a blocking
//! GET that follows the authorization server's 302 to our real loopback
//! listener — so the loopback capture, `state`/CSRF check, and token exchange
//! all execute for real. The server recomputes `S256(code_verifier)` and rejects
//! a PKCE mismatch, so a green test proves the PKCE round-trip end-to-end. It
//! also asserts the RFC 8707 `resource` parameter is present on both legs.

use std::io::{BufRead, BufReader};
use std::process::{Child, ChildStdout, Command, Stdio};
use std::time::Duration;

use sema_mcp::oauth::login::{login, LoginConfig};
use sema_mcp::oauth::loopback::{BrowserOpener, LoopbackDriver};

const OAUTH_SERVER: &str = r#"
import json, hashlib, base64
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse, parse_qs, urlencode

PORT = None
codes = {}
ACCESS = "access-token-xyz"
REFRESH = "refresh-token-xyz"

def b64url_nopad(b):
    return base64.urlsafe_b64encode(b).rstrip(b"=").decode()

class H(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def base(self):
        return "http://127.0.0.1:%d" % PORT

    def _json(self, obj, code=200):
        data = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        p = urlparse(self.path)
        if p.path == "/.well-known/oauth-protected-resource":
            return self._json({
                "resource": self.base() + "/mcp",
                "authorization_servers": [self.base()],
                "scopes_supported": ["mcp:tools"]})
        if p.path == "/.well-known/oauth-authorization-server":
            return self._json({
                "issuer": self.base(),
                "authorization_endpoint": self.base() + "/authorize",
                "token_endpoint": self.base() + "/token",
                "registration_endpoint": self.base() + "/register",
                "code_challenge_methods_supported": ["S256"],
                "grant_types_supported": ["authorization_code", "refresh_token"],
                "scopes_supported": ["mcp:tools"]})
        if p.path == "/authorize":
            q = parse_qs(p.query)
            assert q.get("code_challenge_method", [""])[0] == "S256", "missing S256"
            assert q.get("resource", [""])[0] == self.base() + "/mcp", "missing resource"
            redirect_uri = q.get("redirect_uri", [""])[0]
            assert redirect_uri.startswith("http://127.0.0.1:"), "non-loopback redirect"
            state = q.get("state", [""])[0]
            code = "authcode-123"
            codes[code] = q.get("code_challenge", [""])[0]
            loc = redirect_uri + "?" + urlencode({"code": code, "state": state})
            self.send_response(302)
            self.send_header("Location", loc)
            self.end_headers()
            return
        self.send_response(404)
        self.end_headers()

    def do_POST(self):
        p = urlparse(self.path)
        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length) if length else b""
        if p.path == "/register":
            return self._json({"client_id": "dcr-client-1"}, code=201)
        if p.path == "/token":
            body = parse_qs(raw.decode())
            assert body.get("resource", [""])[0] == self.base() + "/mcp", "token missing resource"
            grant = body.get("grant_type", [""])[0]
            if grant == "authorization_code":
                code = body.get("code", [""])[0]
                verifier = body.get("code_verifier", [""])[0]
                challenge = codes.get(code)
                calc = b64url_nopad(hashlib.sha256(verifier.encode()).digest())
                if challenge is None or calc != challenge:
                    return self._json({"error": "invalid_grant",
                                       "error_description": "PKCE verification failed"}, code=400)
                return self._json({"access_token": ACCESS, "refresh_token": REFRESH,
                                   "token_type": "Bearer", "expires_in": 3600, "scope": "mcp:tools"})
            if grant == "refresh_token":
                assert body.get("refresh_token", [""])[0] == REFRESH
                return self._json({"access_token": "access-token-2", "refresh_token": "refresh-token-2",
                                   "token_type": "Bearer", "expires_in": 3600, "scope": "mcp:tools"})
            return self._json({"error": "unsupported_grant_type"}, code=400)
        self.send_response(404)
        self.end_headers()

srv = HTTPServer(("127.0.0.1", 0), H)
PORT = srv.server_address[1]
print(PORT, flush=True)
srv.serve_forever()
"#;

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
        .args(["-c", OAUTH_SERVER])
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn python3 OAuth server");
    let stdout = child.stdout.take().expect("server stdout");
    let mut reader = BufReader::new(stdout);
    let mut line = String::new();
    reader.read_line(&mut line).expect("read server port");
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
async fn test_oauth_login_end_to_end() {
    let (_server, port) = start_server();
    let base = format!("http://127.0.0.1:{port}");
    let mcp_url = format!("{base}/mcp");

    let http = reqwest::Client::new();

    // The "browser": a blocking GET that follows the 302 from /authorize to our
    // loopback listener. Runs on the driver's own thread, so it never nests a
    // runtime inside the test's tokio runtime.
    let opener: BrowserOpener = Box::new(|url: &str| {
        reqwest::blocking::Client::new()
            .get(url)
            .send()
            .map(|_| ())
            .map_err(|e| e.to_string())
    });
    let driver = LoopbackDriver::with_opener(Duration::from_secs(10), opener)
        .expect("failed to bind loopback listener");

    let config = LoginConfig {
        mcp_url: &mcp_url,
        resource_metadata_url: None,
        requested_scope: None,
        preconfigured_client_id: None,
    };

    let creds = login(&http, &config, None, &driver)
        .await
        .expect("OAuth login should complete end-to-end");

    assert_eq!(creds.server_url, mcp_url);
    assert_eq!(creds.tokens.access_token, "access-token-xyz");
    assert_eq!(
        creds.tokens.refresh_token.as_deref(),
        Some("refresh-token-xyz")
    );
    assert_eq!(creds.tokens.scope.as_deref(), Some("mcp:tools"));
    assert!(
        creds.tokens.expires_at.is_some(),
        "expiry computed from expires_in"
    );
    assert_eq!(
        creds.client_info.expect("DCR client registered").client_id,
        "dcr-client-1"
    );
}
