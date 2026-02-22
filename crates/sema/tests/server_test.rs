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
}
