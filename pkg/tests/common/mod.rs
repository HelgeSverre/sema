use axum::body::Body;
use axum::http::{Request, Response, StatusCode};
use axum::Router;
use http_body_util::BodyExt;
use serde_json::Value;
use std::sync::Arc;
use tempfile::TempDir;
use tower::ServiceExt;

use sema_pkg::{build_router, AppState};

pub async fn test_app() -> (Router, TempDir) {
    let dir = tempfile::tempdir().unwrap();
    let db_path = dir.path().join("test.db");
    let blob_dir = dir.path().join("blobs");
    std::fs::create_dir_all(&blob_dir).unwrap();

    let db_url = format!("sqlite://{}?mode=rwc", db_path.display());
    let db = sema_pkg::db::connect(&db_url).await;

    let config = sema_pkg::config::Config {
        host: "127.0.0.1".into(),
        port: 0,
        database_url: db_url,
        blob_dir: blob_dir.to_str().unwrap().into(),
        base_url: "http://localhost:3000".into(),
        github_client_id: None,
        github_client_secret: None,
        session_secret: "test-secret".into(),
        max_tarball_bytes: 10 * 1024 * 1024,
    };

    let state = Arc::new(AppState { db, config });
    let app = build_router(state);
    (app, dir)
}

pub async fn body_json(res: Response<Body>) -> Value {
    let bytes = res.into_body().collect().await.unwrap().to_bytes();
    serde_json::from_slice(&bytes).unwrap()
}

pub async fn body_string(res: Response<Body>) -> String {
    let bytes = res.into_body().collect().await.unwrap().to_bytes();
    String::from_utf8(bytes.to_vec()).unwrap()
}

pub async fn post_json(app: Router, uri: &str, body: Value) -> Response<Body> {
    app.oneshot(
        Request::builder()
            .method("POST")
            .uri(uri)
            .header("content-type", "application/json")
            .body(Body::from(serde_json::to_string(&body).unwrap()))
            .unwrap(),
    )
    .await
    .unwrap()
}

pub async fn post_json_with_session(
    app: Router,
    uri: &str,
    body: Value,
    session: &str,
) -> Response<Body> {
    app.oneshot(
        Request::builder()
            .method("POST")
            .uri(uri)
            .header("content-type", "application/json")
            .header("cookie", format!("session={session}"))
            .body(Body::from(serde_json::to_string(&body).unwrap()))
            .unwrap(),
    )
    .await
    .unwrap()
}

pub async fn get_with_session(app: Router, uri: &str, session: &str) -> Response<Body> {
    app.oneshot(
        Request::builder()
            .uri(uri)
            .header("cookie", format!("session={session}"))
            .body(Body::empty())
            .unwrap(),
    )
    .await
    .unwrap()
}

/// Register a user and return the session ID
pub async fn register_user(app: Router, username: &str, email: &str) -> String {
    let res = post_json(
        app,
        "/api/v1/auth/register",
        serde_json::json!({
            "username": username,
            "email": email,
            "password": "password123"
        }),
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);
    extract_session(res)
}

/// Extract session ID from Set-Cookie header
fn extract_session(res: Response<Body>) -> String {
    let cookie = res.headers().get("set-cookie").unwrap().to_str().unwrap();
    cookie
        .split(';')
        .next()
        .unwrap()
        .strip_prefix("session=")
        .unwrap()
        .to_string()
}

/// Create an API token and return the raw token string
pub async fn create_api_token(app: Router, session: &str, name: &str) -> String {
    let res = post_json_with_session(
        app,
        "/api/v1/tokens",
        serde_json::json!({"name": name}),
        session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);
    let body = body_json(res).await;
    body["token"].as_str().unwrap().to_string()
}

/// Publish a package with a fake tarball via multipart
pub async fn publish_package(
    app: Router,
    token: &str,
    name: &str,
    version: &str,
    data: &[u8],
) -> Response<Body> {
    let boundary = "----testboundary";
    let mut body_bytes = Vec::new();

    // Metadata field
    let meta = serde_json::json!({"description": format!("A test package: {name}")});
    body_bytes.extend_from_slice(format!("--{boundary}\r\n").as_bytes());
    body_bytes.extend_from_slice(
        b"Content-Disposition: form-data; name=\"metadata\"\r\n\r\n",
    );
    body_bytes.extend_from_slice(serde_json::to_string(&meta).unwrap().as_bytes());
    body_bytes.extend_from_slice(b"\r\n");

    // Tarball field
    body_bytes.extend_from_slice(format!("--{boundary}\r\n").as_bytes());
    body_bytes.extend_from_slice(
        b"Content-Disposition: form-data; name=\"tarball\"; filename=\"pkg.tar.gz\"\r\n",
    );
    body_bytes.extend_from_slice(b"Content-Type: application/gzip\r\n\r\n");
    body_bytes.extend_from_slice(data);
    body_bytes.extend_from_slice(b"\r\n");
    body_bytes.extend_from_slice(format!("--{boundary}--\r\n").as_bytes());

    app.oneshot(
        Request::builder()
            .method("PUT")
            .uri(format!("/api/v1/packages/{name}/{version}"))
            .header("authorization", format!("Bearer {token}"))
            .header(
                "content-type",
                format!("multipart/form-data; boundary={boundary}"),
            )
            .body(Body::from(body_bytes))
            .unwrap(),
    )
    .await
    .unwrap()
}
