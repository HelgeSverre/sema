use axum::body::Body;
use axum::http::{Request, StatusCode};
use http_body_util::BodyExt;
use tower::ServiceExt;

mod common;
use common::*;

// ── Auth Tests ──

#[tokio::test]
async fn test_register_and_login() {
    let (app, _dir) = test_app().await;

    // Register
    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "alice", "email": "alice@example.com", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);
    assert!(res.headers().get("set-cookie").is_some());

    let body = body_json(res).await;
    assert_eq!(body["ok"], true);
    assert_eq!(body["username"], "alice");

    // Login
    let res = post_json(
        app.clone(),
        "/api/v1/auth/login",
        serde_json::json!({"username": "alice", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);
}

#[tokio::test]
async fn test_register_normalizes_case() {
    let (app, _dir) = test_app().await;

    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "Alice", "email": "Alice@Example.COM", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);
    let body = body_json(res).await;
    assert_eq!(body["username"], "alice");

    // Login with different case should work
    let res = post_json(
        app.clone(),
        "/api/v1/auth/login",
        serde_json::json!({"username": "ALICE", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_register_duplicate_username() {
    let (app, _dir) = test_app().await;

    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "bob", "email": "bob@example.com", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);

    // Same username
    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "bob", "email": "bob2@example.com", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::CONFLICT);
    let body = body_json(res).await;
    // Should NOT leak whether username or email was the conflict
    assert_eq!(body["error"], "Registration failed");
}

#[tokio::test]
async fn test_login_wrong_password() {
    let (app, _dir) = test_app().await;

    register_user(app.clone(), "carol", "carol@example.com").await;

    let res = post_json(
        app.clone(),
        "/api/v1/auth/login",
        serde_json::json!({"username": "carol", "password": "wrongpassword"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_login_nonexistent_user() {
    let (app, _dir) = test_app().await;

    let res = post_json(
        app.clone(),
        "/api/v1/auth/login",
        serde_json::json!({"username": "nobody", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_register_validation() {
    let (app, _dir) = test_app().await;

    // Username too short
    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "a", "email": "a@b.com", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);

    // Password too short
    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "validuser", "email": "v@b.com", "password": "short"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);

    // Invalid email
    let res = post_json(
        app.clone(),
        "/api/v1/auth/register",
        serde_json::json!({"username": "validuser", "email": "notanemail", "password": "password123"}),
    )
    .await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);
}

// ── Token Tests ──

#[tokio::test]
async fn test_create_and_list_tokens() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "tokenuser", "token@example.com").await;

    // Create token
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/tokens",
        serde_json::json!({"name": "ci-token"}),
        &session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);
    let body = body_json(res).await;
    assert!(body["token"].as_str().unwrap().starts_with("sema_pat_"));
    assert_eq!(body["name"], "ci-token");

    // List tokens
    let res = get_with_session(app.clone(), "/api/v1/tokens", &session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["tokens"].as_array().unwrap().len(), 1);
}

#[tokio::test]
async fn test_revoke_token() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "revokeuser", "revoke@example.com").await;

    let res = post_json_with_session(
        app.clone(),
        "/api/v1/tokens",
        serde_json::json!({"name": "temp-token"}),
        &session,
    )
    .await;
    let body = body_json(res).await;
    let token_id = body["id"].as_i64().unwrap();

    // Revoke
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri(format!("/api/v1/tokens/{token_id}"))
                .header("cookie", format!("session={session}"))
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);

    // List should be empty
    let res = get_with_session(app.clone(), "/api/v1/tokens", &session).await;
    let body = body_json(res).await;
    assert_eq!(body["tokens"].as_array().unwrap().len(), 0);
}

// ── Package Tests ──

#[tokio::test]
async fn test_publish_and_get_package() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "publisher", "pub@example.com").await;
    let token = create_api_token(app.clone(), &session, "pub-token").await;

    // Publish
    let res = publish_package(app.clone(), &token, "my-pkg", "1.0.0", b"fake tarball data").await;
    assert_eq!(res.status(), StatusCode::CREATED);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);
    assert_eq!(body["package"], "my-pkg");
    assert_eq!(body["version"], "1.0.0");

    // Get package
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/my-pkg")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["package"]["name"], "my-pkg");
    assert_eq!(body["versions"].as_array().unwrap().len(), 1);
    assert_eq!(body["owners"][0], "publisher");
}

#[tokio::test]
async fn test_publish_duplicate_version() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "dup-pub", "dup@example.com").await;
    let token = create_api_token(app.clone(), &session, "dup-token").await;

    publish_package(app.clone(), &token, "dup-pkg", "1.0.0", b"data").await;

    let res = publish_package(app.clone(), &token, "dup-pkg", "1.0.0", b"data2").await;
    assert_eq!(res.status(), StatusCode::CONFLICT);
}

#[tokio::test]
async fn test_publish_invalid_semver() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "semver-pub", "sv@example.com").await;
    let token = create_api_token(app.clone(), &session, "sv-token").await;

    let res = publish_package(app.clone(), &token, "sv-pkg", "not-a-version", b"data").await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn test_publish_not_owner() {
    let (app, _dir) = test_app().await;
    let session1 = register_user(app.clone(), "owner1", "o1@example.com").await;
    let token1 = create_api_token(app.clone(), &session1, "t1").await;

    let session2 = register_user(app.clone(), "intruder", "o2@example.com").await;
    let token2 = create_api_token(app.clone(), &session2, "t2").await;

    // Owner1 publishes
    publish_package(app.clone(), &token1, "owned-pkg", "1.0.0", b"data").await;

    // Intruder tries to publish a new version
    let res = publish_package(app.clone(), &token2, "owned-pkg", "2.0.0", b"data").await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN);
}

#[tokio::test]
async fn test_download_package() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "dluser", "dl@example.com").await;
    let token = create_api_token(app.clone(), &session, "dl-token").await;

    let data = b"hello tarball content";
    publish_package(app.clone(), &token, "dl-pkg", "1.0.0", data).await;

    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/dl-pkg/1.0.0/download")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
    assert_eq!(
        res.headers().get("content-type").unwrap(),
        "application/gzip"
    );
    let body = res.into_body().collect().await.unwrap().to_bytes();
    assert_eq!(&body[..], data);
}

#[tokio::test]
async fn test_yank_package() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "yankuser", "yank@example.com").await;
    let token = create_api_token(app.clone(), &session, "yank-token").await;

    publish_package(app.clone(), &token, "yank-pkg", "1.0.0", b"data").await;

    // Yank
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/api/v1/packages/yank-pkg/1.0.0/yank")
                .header("authorization", format!("Bearer {token}"))
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);

    // Download should fail for yanked
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/yank-pkg/1.0.0/download")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::NOT_FOUND);
}

// ── Search Tests ──

#[tokio::test]
async fn test_search_api() {
    let (app, _dir) = test_app().await;
    let session = register_user(app.clone(), "searchuser", "search@example.com").await;
    let token = create_api_token(app.clone(), &session, "s-token").await;

    publish_package(app.clone(), &token, "http-client", "0.1.0", b"data").await;
    publish_package(app.clone(), &token, "json-parser", "0.2.0", b"data").await;

    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/search?q=http")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["total"], 1);
    assert_eq!(body["packages"][0]["name"], "http-client");
}

// ── Ownership Tests ──

#[tokio::test]
async fn test_add_and_remove_owner() {
    let (app, _dir) = test_app().await;
    let session1 = register_user(app.clone(), "origowner", "oo@example.com").await;
    let token1 = create_api_token(app.clone(), &session1, "oo-token").await;
    register_user(app.clone(), "newowner", "no@example.com").await;

    publish_package(app.clone(), &token1, "shared-pkg", "1.0.0", b"data").await;

    // Add owner
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/api/v1/packages/shared-pkg/owners")
                .header("authorization", format!("Bearer {token1}"))
                .header("content-type", "application/json")
                .body(Body::from(
                    serde_json::to_string(&serde_json::json!({"username": "newowner"})).unwrap(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);

    // List owners
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/shared-pkg/owners")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    let body = body_json(res).await;
    assert_eq!(body["owners"].as_array().unwrap().len(), 2);

    // Remove new owner
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri("/api/v1/packages/shared-pkg/owners")
                .header("authorization", format!("Bearer {token1}"))
                .header("content-type", "application/json")
                .body(Body::from(
                    serde_json::to_string(&serde_json::json!({"username": "newowner"})).unwrap(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);

    // Cannot remove last owner
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri("/api/v1/packages/shared-pkg/owners")
                .header("authorization", format!("Bearer {token1}"))
                .header("content-type", "application/json")
                .body(Body::from(
                    serde_json::to_string(&serde_json::json!({"username": "origowner"})).unwrap(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);
}

// ── Web Page Tests ──

#[tokio::test]
async fn test_web_index() {
    let (app, _dir) = test_app().await;

    let res = app
        .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
    let html = body_string(res).await;
    assert!(html.contains("Sema Packages"));
    assert!(html.contains("0 packages published"));
}

#[tokio::test]
async fn test_web_login_page() {
    let (app, _dir) = test_app().await;

    let res = app
        .oneshot(
            Request::builder()
                .uri("/login")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
    let html = body_string(res).await;
    assert!(html.contains("Sign In"));
}

#[tokio::test]
async fn test_web_account_redirects_when_logged_out() {
    let (app, _dir) = test_app().await;

    let res = app
        .oneshot(
            Request::builder()
                .uri("/account")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    // Should redirect to /login
    assert_eq!(res.status(), StatusCode::SEE_OTHER);
    assert_eq!(res.headers().get("location").unwrap(), "/login");
}

#[tokio::test]
async fn test_web_search_page() {
    let (app, _dir) = test_app().await;

    let res = app
        .oneshot(
            Request::builder()
                .uri("/search?q=test")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
    let html = body_string(res).await;
    assert!(html.contains("Results for"));
    assert!(html.contains("test"));
}

#[tokio::test]
async fn test_web_package_not_found() {
    let (app, _dir) = test_app().await;

    let res = app
        .oneshot(
            Request::builder()
                .uri("/packages/nonexistent")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_healthz() {
    let (app, _dir) = test_app().await;

    let res = app
        .oneshot(
            Request::builder()
                .uri("/healthz")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::OK);
}

// ── Auth Helpers (unit-level) ──

#[test]
fn test_password_hash_and_verify() {
    let hash = sema_pkg::auth::hash_password("mypassword");
    assert!(sema_pkg::auth::verify_password("mypassword", &hash));
    assert!(!sema_pkg::auth::verify_password("wrongpassword", &hash));
}

#[test]
fn test_token_generation() {
    let token = sema_pkg::auth::generate_token();
    assert!(token.starts_with("sema_pat_"));
    assert!(token.len() > 20);
}

#[test]
fn test_username_validation() {
    assert!(sema_pkg::auth::validate_username("alice").is_ok());
    assert!(sema_pkg::auth::validate_username("alice-bob").is_ok());
    assert!(sema_pkg::auth::validate_username("a").is_err()); // too short
    assert!(sema_pkg::auth::validate_username("-alice").is_err()); // starts with hyphen
    assert!(sema_pkg::auth::validate_username("alice-").is_err()); // ends with hyphen
    assert!(sema_pkg::auth::validate_username("alice bob").is_err()); // space
}

#[test]
fn test_email_validation() {
    assert!(sema_pkg::auth::validate_email("a@b.com").is_ok());
    assert!(sema_pkg::auth::validate_email("notanemail").is_err());
    assert!(sema_pkg::auth::validate_email("ab").is_err());
}

#[test]
fn test_password_validation() {
    assert!(sema_pkg::auth::validate_password("longpassword").is_ok());
    assert!(sema_pkg::auth::validate_password("short").is_err());
}

#[test]
fn test_blob_path() {
    let path = sema_pkg::blob::blob_path("/data/blobs", "abcdef.tar.gz");
    assert_eq!(path.to_str().unwrap(), "/data/blobs/ab/abcdef.tar.gz");
}
