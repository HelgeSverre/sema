use sema_core::Value;
use sema_eval::Interpreter;

fn eval(input: &str) -> Value {
    let interp = Interpreter::new();
    interp
        .eval_str(input)
        .expect(&format!("failed to eval: {input}"))
}

fn eval_err(input: &str) -> sema_core::SemaError {
    let interp = Interpreter::new();
    interp.eval_str(input).unwrap_err()
}

#[test]
#[ignore]
fn test_http_get() {
    let result = eval(r#"(http/get "https://httpbin.org/get")"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("status")), Some(&Value::int(200)));
    } else {
        panic!("expected map");
    }
}

#[test]
#[ignore]
fn test_http_post() {
    let result = eval(r#"(http/post "https://httpbin.org/post" {:name "sema"})"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("status")), Some(&Value::int(200)));
    } else {
        panic!("expected map");
    }
}

#[test]
#[ignore]
fn test_http_request_generic() {
    let result = eval(r#"(http/request "PATCH" "https://httpbin.org/patch" {} "data")"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("status")), Some(&Value::int(200)));
    } else {
        panic!("expected map");
    }
}

#[test]
#[ignore]
fn test_http_response_has_body() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (string? (:body resp)))
    "#,
    );
    assert_eq!(result, Value::bool(true));
    // Also verify body is non-empty
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (> (string-length (:body resp)) 0))
    "#,
    );
    assert_eq!(result, Value::bool(true));
}

#[test]
#[ignore]
fn test_http_response_has_headers() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (map? (:headers resp)))
    "#,
    );
    assert_eq!(result, Value::bool(true));
    // Verify content-type header exists
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (string? (get (:headers resp) :content-type)))
    "#,
    );
    assert_eq!(result, Value::bool(true));
}

#[test]
#[ignore]
fn test_http_response_body_json_decode() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (let ((data (json/decode (:body resp))))
            (get data :url)))
    "#,
    );
    assert_eq!(result, Value::string("https://httpbin.org/get"));
}

#[test]
#[ignore]
fn test_http_put() {
    let result = eval(
        r#"
        (let ((resp (http/put "https://httpbin.org/put" {:name "sema"})))
          (let ((data (json/decode (:body resp))))
            (list (:status resp) (get (get data :json) :name))))
    "#,
    );
    assert_eq!(
        result,
        Value::list(vec![Value::int(200), Value::string("sema")])
    );
}

#[test]
#[ignore]
fn test_http_delete() {
    let result = eval(
        r#"
        (let ((resp (http/delete "https://httpbin.org/delete")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

#[test]
#[ignore]
fn test_http_head() {
    let result = eval(
        r#"
        (let ((resp (http/request "HEAD" "https://httpbin.org/get")))
          (list (:status resp) (:body resp)))
    "#,
    );
    // HEAD returns 200 with empty body
    assert_eq!(
        result,
        Value::list(vec![Value::int(200), Value::string("")])
    );
}

#[test]
#[ignore]
fn test_http_patch_with_json_body() {
    let result = eval(
        r#"
        (let ((resp (http/request "PATCH" "https://httpbin.org/patch" {} "{\"key\":\"val\"}")))
          (let ((data (json/decode (:body resp))))
            (list (:status resp) (get data :data))))
    "#,
    );
    assert_eq!(
        result,
        Value::list(vec![Value::int(200), Value::string("{\"key\":\"val\"}")])
    );
}

#[test]
#[ignore]
fn test_http_custom_headers() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/headers"
                              {:headers {:x-custom-header "sema-test"}})))
          (let ((data (json/decode (:body resp))))
            (get (get data :headers) :X-Custom-Header)))
    "#,
    );
    assert_eq!(result, Value::string("sema-test"));
}

#[test]
#[ignore]
fn test_http_multiple_headers() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/headers"
                              {:headers {:x-first "one" :x-second "two"}})))
          (let ((hdrs (get (json/decode (:body resp)) :headers)))
            (list (get hdrs :X-First) (get hdrs :X-Second))))
    "#,
    );
    assert_eq!(
        result,
        Value::list(vec![Value::string("one"), Value::string("two")])
    );
}

#[test]
#[ignore]
fn test_http_post_map_body_echoed() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:name "sema" :version 1})))
          (let ((data (json/decode (:body resp))))
            (get (get data :json) :name)))
    "#,
    );
    assert_eq!(result, Value::string("sema"));
}

#[test]
#[ignore]
fn test_http_post_string_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" "raw-body-data"
                               {:headers {:content-type "text/plain"}})))
          (let ((data (json/decode (:body resp))))
            (get data :data)))
    "#,
    );
    assert_eq!(result, Value::string("raw-body-data"));
}

#[test]
#[ignore]
fn test_http_post_nested_map() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:user {:name "test"}})))
          (let ((data (json/decode (:body resp))))
            (get (get (get data :json) :user) :name)))
    "#,
    );
    assert_eq!(result, Value::string("test"));
}

#[test]
#[ignore]
fn test_http_post_empty_string_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" "")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

#[test]
#[ignore]
fn test_http_status_404() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/status/404")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(404));
}

#[test]
#[ignore]
fn test_http_status_500() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/status/500")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(500));
}

#[test]
#[ignore]
fn test_http_get_with_query_params() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get?foo=bar&baz=42")))
          (let ((data (json/decode (:body resp))))
            (get (get data :args) :foo)))
    "#,
    );
    assert_eq!(result, Value::string("bar"));
}

#[test]
#[ignore]
fn test_http_timeout() {
    let _err = eval_err(r#"(http/get "https://httpbin.org/delay/10" {:timeout 1000})"#);
    // Just verifying it errors (timeout after 1s, but server delays 10s)
}

#[test]
#[ignore]
fn test_http_invalid_url() {
    let _err = eval_err(r#"(http/get "http://invalid.invalid.invalid")"#);
}

#[test]
#[ignore]
fn test_http_unicode_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:text "Hello 世界"})))
          (let ((data (json/decode (:body resp))))
            (get (get data :json) :text)))
    "#,
    );
    assert_eq!(result, Value::string("Hello 世界"));
}

#[test]
#[ignore]
fn test_http_redirect() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/redirect/1")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// http/request minimal 2-arg path (opts and body both None)
#[test]
#[ignore]
fn test_http_request_minimal_args() {
    let result = eval(
        r#"
        (let ((resp (http/request "GET" "https://httpbin.org/get")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// Non-string/non-map body → to_string() fallback (http.rs line 71-73)
#[test]
#[ignore]
fn test_http_post_integer_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" 42)))
          (let ((data (json/decode (:body resp))))
            (get data :data)))
    "#,
    );
    assert_eq!(result, Value::string("42"));
}

// Map body auto-sets Content-Type: application/json (http.rs line 67-69)
#[test]
#[ignore]
fn test_http_post_map_sets_content_type_json() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:a 1})))
          (let ((data (json/decode (:body resp))))
            (string/starts-with? (get (get data :headers) :Content-Type) "application/json")))
    "#,
    );
    assert_eq!(result, Value::bool(true));
}

// String header keys (http.rs line 39 - Value::String branch)
#[test]
#[ignore]
fn test_http_headers_with_string_keys() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/headers"
                              {:headers {"X-String-Key" "string-val"}})))
          (let ((data (json/decode (:body resp))))
            (get (get data :headers) :X-String-Key)))
    "#,
    );
    assert_eq!(result, Value::string("string-val"));
}

// Opts as non-map → silently ignored (http.rs line 35)
#[test]
#[ignore]
fn test_http_get_opts_non_map_ignored() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get" "not-a-map")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// http/delete with opts (exercises the opts path for delete)
#[test]
#[ignore]
fn test_http_delete_with_opts() {
    let result = eval(
        r#"
        (let ((resp (http/delete "https://httpbin.org/delete"
                                 {:headers {:x-delete-test "yes"}})))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}
