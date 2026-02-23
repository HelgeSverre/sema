use sema_core::{SemaError, Value};
use sema_eval::Interpreter;

fn eval(input: &str) -> Value {
    let interp = Interpreter::new();
    interp
        .eval_str(input)
        .expect(&format!("failed to eval: {input}"))
}

fn eval_err(input: &str) -> SemaError {
    let interp = Interpreter::new();
    interp.eval_str(input).unwrap_err()
}

// ---------------------------------------------------------------------------
// Web server integration tests (server-based, require network)
// ---------------------------------------------------------------------------

#[test]
#[ignore] // requires network
fn test_http_serve_json_body_parsing() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(r#"(http/serve (fn (req) (http/ok (or (:json req) "no json"))) {:port 19880})"#)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();

    // POST with JSON body
    let resp = client
        .post("http://127.0.0.1:19880/test")
        .header("content-type", "application/json")
        .body(r#"{"name":"Ada","age":36}"#)
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to POST");
    assert_eq!(resp.status(), 200);
    let body: serde_json::Value = resp.json().expect("Failed to parse JSON response");
    assert_eq!(body["name"], "Ada");
    assert_eq!(body["age"], 36);

    // GET without JSON body should get "no json"
    let resp = client
        .get("http://127.0.0.1:19880/test")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET");
    assert_eq!(resp.status(), 200);
    let body: serde_json::Value = resp.json().expect("Failed to parse JSON response");
    assert_eq!(body, "no json");

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_query_string() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(r#"(http/serve (fn (req) (http/ok (:query req))) {:port 19881})"#)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();
    let resp = client
        .get("http://127.0.0.1:19881/search?q=hello&page=2")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET");
    assert_eq!(resp.status(), 200);
    let body: serde_json::Value = resp.json().expect("Failed to parse JSON response");
    assert_eq!(body["page"], "2");
    assert_eq!(body["q"], "hello");

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_handler_error() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(r#"(http/serve (fn (req) (error "something broke")) {:port 19882})"#)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();

    // First request: handler errors, should get 500
    let resp = client
        .get("http://127.0.0.1:19882/test")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET");
    assert_eq!(resp.status(), 500);

    // Second request: server should still be running
    let resp = client
        .get("http://127.0.0.1:19882/test2")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Server should still be running after handler error");
    assert_eq!(resp.status(), 500);

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_method_dispatch() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(
            r#"
            (http/serve
              (http/router
                [[:get "/data" (fn (req) (http/ok "got"))]
                 [:post "/data" (fn (req) (http/ok "posted"))]])
              {:port 19883})
        "#,
        )
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();

    // GET /data
    let resp = client
        .get("http://127.0.0.1:19883/data")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET /data");
    assert_eq!(resp.status(), 200);
    let body: serde_json::Value = resp.json().unwrap();
    assert_eq!(body, "got");

    // POST /data
    let resp = client
        .post("http://127.0.0.1:19883/data")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to POST /data");
    assert_eq!(resp.status(), 200);
    let body: serde_json::Value = resp.json().unwrap();
    assert_eq!(body, "posted");

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_custom_headers() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(
            r#"
            (http/serve
              (fn (req)
                {:status 200
                 :headers {"x-custom" "hello" "content-type" "text/plain"}
                 :body "ok"})
              {:port 19884})
        "#,
        )
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();
    let resp = client
        .get("http://127.0.0.1:19884/test")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET");
    assert_eq!(resp.status(), 200);
    assert_eq!(
        resp.headers().get("x-custom").map(|v| v.to_str().unwrap()),
        Some("hello")
    );
    assert_eq!(
        resp.headers()
            .get("content-type")
            .map(|v| v.to_str().unwrap()),
        Some("text/plain")
    );
    let body = resp.text().unwrap();
    assert_eq!(body, "ok");

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_wildcard_route() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(
            r#"
            (http/serve
              (http/router
                [[:get "/files/*" (fn (req) (http/ok (:* (:params req))))]])
              {:port 19885})
        "#,
        )
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();
    let resp = client
        .get("http://127.0.0.1:19885/files/a/b/c.txt")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET /files/a/b/c.txt");
    assert_eq!(resp.status(), 200);
    let body: serde_json::Value = resp.json().unwrap();
    assert_eq!(body, "a/b/c.txt");

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_concurrent_requests() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(r#"(http/serve (fn (req) (http/ok {:path (:path req)})) {:port 19886})"#)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    // Spawn 5 threads, each making a request
    let handles: Vec<_> = (0..5)
        .map(|i| {
            std::thread::spawn(move || {
                let client = reqwest::blocking::Client::new();
                let resp = client
                    .get(format!("http://127.0.0.1:19886/item/{i}"))
                    .timeout(Duration::from_secs(10))
                    .send()
                    .expect("Failed to GET");
                assert_eq!(resp.status(), 200);
                let body: serde_json::Value = resp.json().unwrap();
                assert_eq!(body["path"], format!("/item/{i}"));
            })
        })
        .collect();

    for h in handles {
        h.join().expect("Thread panicked");
    }

    child.kill().ok();
    child.wait().ok();
}

#[test]
#[ignore] // requires network
fn test_http_serve_middleware() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(
            r#"
            (begin
              (define (add-header handler)
                (fn (req)
                  (let ((resp (handler req)))
                    (let ((headers (or (:headers resp) {})))
                      (assoc resp :headers (assoc headers "x-middleware" "applied"))))))
              (http/serve
                (add-header (fn (req) (http/ok "hello")))
                {:port 19887}))
        "#,
        )
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();
    let resp = client
        .get("http://127.0.0.1:19887/test")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET");
    assert_eq!(resp.status(), 200);
    assert_eq!(
        resp.headers()
            .get("x-middleware")
            .map(|v| v.to_str().unwrap()),
        Some("applied")
    );

    child.kill().ok();
    child.wait().ok();
}

// ---------------------------------------------------------------------------
// Router unit tests (no server needed)
// ---------------------------------------------------------------------------

#[test]
fn test_http_router_multiple_methods_same_path() {
    let get_result = eval(
        r#"
        (let ((router (http/router
                       [[:get "/data" (fn (req) (http/ok "got"))]
                        [:post "/data" (fn (req) (http/ok "posted"))]])))
          (router {:method :get :path "/data" :headers {} :query {} :params {} :body "" :remote "127.0.0.1"}))
    "#,
    );
    let map = get_result.as_map_rc().unwrap();
    let body = map.get(&Value::keyword("body")).unwrap();
    assert!(
        body.as_str().unwrap().contains("got"),
        "GET should return 'got', got: {}",
        body
    );

    let post_result = eval(
        r#"
        (let ((router (http/router
                       [[:get "/data" (fn (req) (http/ok "got"))]
                        [:post "/data" (fn (req) (http/ok "posted"))]])))
          (router {:method :post :path "/data" :headers {} :query {} :params {} :body "" :remote "127.0.0.1"}))
    "#,
    );
    let map = post_result.as_map_rc().unwrap();
    let body = map.get(&Value::keyword("body")).unwrap();
    assert!(
        body.as_str().unwrap().contains("posted"),
        "POST should return 'posted', got: {}",
        body
    );
}

#[test]
fn test_http_router_wildcard() {
    let result = eval(
        r#"
        (let ((router (http/router
                       [[:get "/files/*" (fn (req) (http/ok (:params req)))]])))
          (router {:method :get :path "/files/a/b/c.txt" :headers {} :query {} :params {} :body "" :remote "127.0.0.1"}))
    "#,
    );
    let map = result.as_map_rc().unwrap();
    let body = map.get(&Value::keyword("body")).unwrap();
    assert!(
        body.as_str().unwrap().contains("a/b/c.txt"),
        "wildcard should capture 'a/b/c.txt', got: {}",
        body
    );
}

#[test]
fn test_http_response_helpers_arity() {
    // http/ok requires exactly 1 arg
    let _err = eval_err(r#"(http/ok)"#);
    let _err = eval_err(r#"(http/ok 1 2)"#);
    // http/created requires exactly 1 arg
    let _err = eval_err(r#"(http/created)"#);
    // http/not-found requires exactly 1 arg
    let _err = eval_err(r#"(http/not-found)"#);
    // http/redirect requires exactly 1 string arg
    let _err = eval_err(r#"(http/redirect)"#);
    let _err = eval_err(r#"(http/redirect 123)"#);
    // http/error requires 2 args: integer status + body
    let _err = eval_err(r#"(http/error 422)"#);
    let _err = eval_err(r#"(http/error "not-a-number" "body")"#);
    // http/html requires 1 string arg
    let _err = eval_err(r#"(http/html 123)"#);
    // http/text requires 1 string arg
    let _err = eval_err(r#"(http/text 123)"#);
    // http/no-content requires 0 args
    let _err = eval_err(r#"(http/no-content "extra")"#);
    // http/file requires 1-2 args
    let _err = eval_err(r#"(http/file)"#);
    let _err = eval_err(r#"(http/file "a" "b" "c")"#);
    let _err = eval_err(r#"(http/file 123)"#);
}

#[test]
fn test_http_file_returns_marker() {
    // http/file on an existing file returns a map with __file marker
    let result = eval(r#"(http/file "Cargo.toml")"#);
    let map = result.as_map_rc().unwrap();
    assert_eq!(
        map.get(&Value::keyword("__file")).and_then(|v| v.as_bool()),
        Some(true)
    );
    assert!(map
        .get(&Value::keyword("__file_path"))
        .and_then(|v| v.as_str())
        .unwrap()
        .contains("Cargo.toml"));
    assert_eq!(
        map.get(&Value::keyword("__file_content_type"))
            .and_then(|v| v.as_str()),
        Some("text/x-toml")
    );
}

#[test]
fn test_http_file_custom_content_type() {
    let result = eval(r#"(http/file "Cargo.toml" "application/json")"#);
    let map = result.as_map_rc().unwrap();
    assert_eq!(
        map.get(&Value::keyword("__file_content_type"))
            .and_then(|v| v.as_str()),
        Some("application/json")
    );
}

#[test]
fn test_http_file_nonexistent() {
    let err = eval_err(r#"(http/file "nonexistent-file-12345.txt")"#);
    let msg = err.to_string();
    assert!(
        msg.contains("http/file"),
        "error should mention http/file: {msg}"
    );
}

#[test]
fn test_http_router_static_route() {
    // Create a temp directory with a test file
    let tmp = std::env::temp_dir().join("sema-static-route-test");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();
    std::fs::write(tmp.join("hello.txt"), "hello world").unwrap();
    let dir = tmp.to_string_lossy().replace('\\', "/");

    let result = eval(&format!(
        r#"
        (let ((router (http/router
                       [[:static "/assets" "{dir}"]])))
          (router {{:method :get :path "/assets/hello.txt" :headers {{}} :query {{}} :params {{}} :body "" :remote "127.0.0.1"}}))
    "#
    ));
    let map = result.as_map_rc().unwrap();
    assert_eq!(
        map.get(&Value::keyword("__file")).and_then(|v| v.as_bool()),
        Some(true),
        "should return a file marker for existing file"
    );
    assert!(map
        .get(&Value::keyword("__file_path"))
        .and_then(|v| v.as_str())
        .unwrap()
        .contains("hello.txt"));
    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn test_http_router_static_fallthrough() {
    let tmp = std::env::temp_dir().join("sema-static-fallthrough-test");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();
    std::fs::write(tmp.join("exists.txt"), "yes").unwrap();
    let dir = tmp.to_string_lossy().replace('\\', "/");

    let result = eval(&format!(
        r#"
        (let ((router (http/router
                       [[:static "/assets" "{dir}"]
                        [:get "/*" (fn (req) (http/html "<h1>SPA</h1>"))]])))
          (router {{:method :get :path "/assets/nonexistent.xyz" :headers {{}} :query {{}} :params {{}} :body "" :remote "127.0.0.1"}}))
    "#
    ));
    let map = result.as_map_rc().unwrap();
    let body = map
        .get(&Value::keyword("body"))
        .and_then(|v| v.as_str())
        .unwrap();
    assert!(
        body.contains("SPA"),
        "non-existent static file should fall through to SPA route, got: {body}"
    );
    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn test_http_router_static_path_traversal() {
    let tmp = std::env::temp_dir().join("sema-static-traversal-test");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();
    std::fs::write(tmp.join("safe.txt"), "safe").unwrap();
    let dir = tmp.to_string_lossy().replace('\\', "/");

    let result = eval(&format!(
        r#"
        (let ((router (http/router
                       [[:static "/assets" "{dir}"]])))
          (router {{:method :get :path "/assets/../etc/passwd" :headers {{}} :query {{}} :params {{}} :body "" :remote "127.0.0.1"}}))
    "#
    ));
    let map = result.as_map_rc().unwrap();
    let status = map
        .get(&Value::keyword("status"))
        .and_then(|v| v.as_int())
        .unwrap();
    assert_eq!(status, 400, "path traversal should return 400");
    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
fn test_http_router_static_post_rejected() {
    let tmp = std::env::temp_dir().join("sema-static-post-test");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();
    std::fs::write(tmp.join("file.txt"), "content").unwrap();
    let dir = tmp.to_string_lossy().replace('\\', "/");

    let result = eval(&format!(
        r#"
        (let ((router (http/router
                       [[:static "/assets" "{dir}"]])))
          (router {{:method :post :path "/assets/file.txt" :headers {{}} :query {{}} :params {{}} :body "" :remote "127.0.0.1"}}))
    "#
    ));
    let map = result.as_map_rc().unwrap();
    let status = map
        .get(&Value::keyword("status"))
        .and_then(|v| v.as_int())
        .unwrap();
    assert_eq!(status, 404, "POST to static should 404");
    let _ = std::fs::remove_dir_all(&tmp);
}

#[test]
#[ignore] // requires network
fn test_http_serve_static_files() {
    use std::process::{Command, Stdio};
    use std::time::Duration;

    // Create a temp directory with test files
    let tmp = std::env::temp_dir().join("sema-static-test");
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).unwrap();
    std::fs::write(tmp.join("index.html"), "<h1>Hello</h1>").unwrap();
    std::fs::write(tmp.join("style.css"), "body { color: red; }").unwrap();
    std::fs::write(tmp.join("app.js"), "console.log('hi');").unwrap();

    let sema_code = format!(
        r#"(http/serve
             (http/router
               [[:static "/static" "{dir}"]
                [:get "/*" (fn (_) (http/file "{dir}/index.html"))]])
             {{:port 19895}})"#,
        dir = tmp.to_string_lossy().replace('\\', "/")
    );

    let mut child = Command::new(env!("CARGO_BIN_EXE_sema"))
        .arg("-e")
        .arg(&sema_code)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn sema");

    std::thread::sleep(Duration::from_millis(1500));

    let client = reqwest::blocking::Client::new();

    // Serve HTML
    let resp = client
        .get("http://127.0.0.1:19895/static/index.html")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET html");
    assert_eq!(resp.status(), 200);
    assert!(resp
        .headers()
        .get("content-type")
        .unwrap()
        .to_str()
        .unwrap()
        .contains("text/html"));
    assert_eq!(resp.text().unwrap(), "<h1>Hello</h1>");

    // Serve CSS with correct MIME type
    let resp = client
        .get("http://127.0.0.1:19895/static/style.css")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET css");
    assert_eq!(resp.status(), 200);
    assert!(resp
        .headers()
        .get("content-type")
        .unwrap()
        .to_str()
        .unwrap()
        .contains("text/css"));

    // Serve JS with correct MIME type
    let resp = client
        .get("http://127.0.0.1:19895/static/app.js")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET js");
    assert_eq!(resp.status(), 200);
    assert!(resp
        .headers()
        .get("content-type")
        .unwrap()
        .to_str()
        .unwrap()
        .contains("javascript"));

    // Non-existent static file falls through to SPA
    let resp = client
        .get("http://127.0.0.1:19895/about")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET spa");
    assert_eq!(resp.status(), 200);
    assert_eq!(resp.text().unwrap(), "<h1>Hello</h1>");

    // 404 for non-existent static file (no SPA match for /static/ prefix)
    let resp = client
        .get("http://127.0.0.1:19895/static/nonexistent.txt")
        .timeout(Duration::from_secs(5))
        .send()
        .expect("Failed to GET nonexistent");
    // Falls through static, then matches SPA catch-all
    assert_eq!(resp.status(), 200);

    child.kill().ok();
    child.wait().ok();
    let _ = std::fs::remove_dir_all(&tmp);
}
