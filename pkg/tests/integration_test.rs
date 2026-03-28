use axum::body::Body;
use axum::http::{Request, StatusCode};
use http_body_util::BodyExt;
use sqlx::Row;
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

// ── Meta Registry (GitHub redirect) Tests ──

#[tokio::test]
async fn test_download_github_package_redirects() {
    let (app, state, _dir) = test_app_with_state().await;

    // Insert a GitHub-linked package with tarball_url directly in the DB
    sqlx::query("INSERT INTO packages (name, description, source, github_repo) VALUES ('gh-pkg', 'A GitHub package', 'github', 'testowner/testrepo')")
        .execute(&state.db)
        .await
        .unwrap();

    let pkg_id: i64 = sqlx::query("SELECT id FROM packages WHERE name = 'gh-pkg'")
        .fetch_one(&state.db)
        .await
        .unwrap()
        .get("id");

    sqlx::query(
        "INSERT INTO package_versions (package_id, version, checksum_sha256, blob_key, size_bytes, tarball_url) VALUES (?, '1.0.0', '', '', 0, 'https://api.github.com/repos/testowner/testrepo/tarball/v1.0.0')"
    )
    .bind(pkg_id)
    .execute(&state.db)
    .await
    .unwrap();

    // Download should return a 302 redirect
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/gh-pkg/1.0.0/download")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(res.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        res.headers().get("location").unwrap(),
        "https://api.github.com/repos/testowner/testrepo/tarball/v1.0.0"
    );
}

#[tokio::test]
async fn test_get_package_includes_tarball_url() {
    let (app, state, _dir) = test_app_with_state().await;

    sqlx::query("INSERT INTO packages (name, description, source, github_repo) VALUES ('gh-pkg2', 'Another GH package', 'github', 'owner/repo')")
        .execute(&state.db)
        .await
        .unwrap();

    let pkg_id: i64 = sqlx::query("SELECT id FROM packages WHERE name = 'gh-pkg2'")
        .fetch_one(&state.db)
        .await
        .unwrap()
        .get("id");

    sqlx::query(
        "INSERT INTO package_versions (package_id, version, checksum_sha256, blob_key, size_bytes, tarball_url) VALUES (?, '2.0.0', '', '', 0, 'https://api.github.com/repos/owner/repo/tarball/v2.0.0')"
    )
    .bind(pkg_id)
    .execute(&state.db)
    .await
    .unwrap();

    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/gh-pkg2")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(
        body["versions"][0]["tarball_url"],
        "https://api.github.com/repos/owner/repo/tarball/v2.0.0"
    );
}

#[test]
fn test_blob_path() {
    let path = sema_pkg::blob::blob_path("/data/blobs", "abcdef.tar.gz");
    assert_eq!(path.to_str().unwrap(), "/data/blobs/ab/abcdef.tar.gz");
}

// ── Security Tests ──

#[tokio::test]
async fn test_security_non_admin_cannot_access_any_admin_endpoint() {
    let (app, _state, _dir) = test_app_with_state().await;
    let session = register_user(app.clone(), "user1", "user1@example.com").await;

    // GET endpoints
    for uri in [
        "/api/v1/admin/stats",
        "/api/v1/admin/users",
        "/api/v1/admin/users/1",
        "/api/v1/admin/packages",
        "/api/v1/admin/packages/x",
        "/api/v1/admin/audit",
        "/api/v1/admin/reports",
    ] {
        let res = get_with_session(app.clone(), uri, &session).await;
        assert_eq!(res.status(), StatusCode::FORBIDDEN, "GET {uri} should be 403 for non-admin");
    }

    // POST endpoints with empty body
    for uri in [
        "/api/v1/admin/users/1/ban",
        "/api/v1/admin/users/1/unban",
        "/api/v1/admin/users/1/revoke-tokens",
        "/api/v1/admin/packages/x/yank-all",
        "/api/v1/admin/reports/1/action",
        "/api/v1/admin/reports/1/dismiss",
    ] {
        let res = post_empty_with_session(app.clone(), uri, &session).await;
        assert_eq!(res.status(), StatusCode::FORBIDDEN, "POST {uri} should be 403 for non-admin");
    }

    // PUT with JSON body
    let res = put_json_with_session(
        app.clone(),
        "/api/v1/admin/users/1/role",
        serde_json::json!({"is_admin": true}),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN, "PUT /api/v1/admin/users/1/role should be 403");

    // DELETE endpoint
    let res = delete_with_session(app.clone(), "/api/v1/admin/packages/x", &session).await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN, "DELETE /api/v1/admin/packages/x should be 403");

    // POST with JSON body
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/admin/packages/x/transfer",
        serde_json::json!({"to_username": "x"}),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN, "POST /api/v1/admin/packages/x/transfer should be 403");
}

#[tokio::test]
async fn test_security_unauthenticated_cannot_access_admin_endpoints() {
    let (app, _dir) = test_app().await;

    // GET endpoints
    for uri in [
        "/api/v1/admin/stats",
        "/api/v1/admin/users",
        "/api/v1/admin/users/1",
        "/api/v1/admin/packages",
        "/api/v1/admin/packages/x",
        "/api/v1/admin/audit",
        "/api/v1/admin/reports",
    ] {
        let res = app
            .clone()
            .oneshot(Request::builder().uri(uri).body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "GET {uri} should be 401 unauthenticated");
    }

    // POST endpoints
    for uri in [
        "/api/v1/admin/users/1/ban",
        "/api/v1/admin/users/1/unban",
        "/api/v1/admin/users/1/revoke-tokens",
        "/api/v1/admin/packages/x/yank-all",
        "/api/v1/admin/reports/1/action",
        "/api/v1/admin/reports/1/dismiss",
    ] {
        let res = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri(uri)
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "POST {uri} should be 401 unauthenticated");
    }

    // PUT with JSON body
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/api/v1/admin/users/1/role")
                .header("content-type", "application/json")
                .body(Body::from(
                    serde_json::to_string(&serde_json::json!({"is_admin": true})).unwrap(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "PUT /api/v1/admin/users/1/role should be 401");

    // DELETE endpoint
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri("/api/v1/admin/packages/x")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "DELETE /api/v1/admin/packages/x should be 401");

    // POST with JSON body
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/api/v1/admin/packages/x/transfer")
                .header("content-type", "application/json")
                .body(Body::from(
                    serde_json::to_string(&serde_json::json!({"to_username": "x"})).unwrap(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "POST /api/v1/admin/packages/x/transfer should be 401");
}

#[tokio::test]
async fn test_security_banned_user_cannot_access_endpoints() {
    let (app, state, _dir) = test_app_with_state().await;

    // Register admin and victim
    let admin_session = register_admin(app.clone(), &state, "admin1", "admin1@example.com").await;
    let victim_session = register_user(app.clone(), "victim", "victim@example.com").await;

    // Create an API token for victim before ban
    let victim_token = create_api_token(app.clone(), &victim_session, "my-token").await;

    // Get victim's user ID and ban them
    let victim_id = get_user_id(&state, "victim").await;
    let res = post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/ban"),
        &admin_session,
    ).await;
    assert_eq!(res.status(), StatusCode::OK);

    // Banned user's session should be rejected
    let res = get_with_session(app.clone(), "/api/v1/tokens", &victim_session).await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "banned user session: GET /api/v1/tokens should be 401");

    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "some-pkg",
            "report_type": "malware",
            "reason": "suspicious"
        }),
        &victim_session,
    ).await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "banned user session: POST /api/v1/reports should be 401");

    // Banned user's API token should be rejected for publish
    let res = publish_package(app.clone(), &victim_token, "x", "1.0.0", b"fake-tarball").await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED, "banned user token: PUT publish should be 401");
}

#[tokio::test]
async fn test_admin_cannot_ban_self() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session = register_admin(app.clone(), &state, "admin1", "admin1@example.com").await;
    let admin_id = get_user_id(&state, "admin1").await;

    let res = post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{admin_id}/ban"),
        &admin_session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);
    let body = body_json(res).await;
    assert_eq!(body["error"], "Cannot ban yourself");
}

#[tokio::test]
async fn test_admin_cannot_change_own_role() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session = register_admin(app.clone(), &state, "admin1", "admin1@example.com").await;
    let admin_id = get_user_id(&state, "admin1").await;

    let res = put_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{admin_id}/role"),
        serde_json::json!({"is_admin": false}),
        &admin_session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST);
    let body = body_json(res).await;
    assert_eq!(body["error"], "Cannot change your own admin role");
}

#[tokio::test]
async fn test_submit_report_validates_input() {
    let (app, _state, _dir) = test_app_with_state().await;
    let session = register_user(app.clone(), "reporter", "reporter@example.com").await;

    // Invalid target_type
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "invalid",
            "target_name": "some-pkg",
            "report_type": "malware",
            "reason": "suspicious"
        }),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST, "invalid target_type should be 400");

    // Invalid report_type
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "some-pkg",
            "report_type": "unknown",
            "reason": "suspicious"
        }),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST, "invalid report_type should be 400");

    // Empty reason
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "some-pkg",
            "report_type": "malware",
            "reason": ""
        }),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST, "empty reason should be 400");

    // Empty target_name
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "",
            "report_type": "malware",
            "reason": "suspicious"
        }),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST, "empty target_name should be 400");

    // Very long reason (3000 chars)
    let long_reason = "a".repeat(3000);
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "some-pkg",
            "report_type": "malware",
            "reason": long_reason
        }),
        &session,
    ).await;
    assert_eq!(res.status(), StatusCode::BAD_REQUEST, "very long reason should be 400");
}

#[tokio::test]
async fn test_submit_report_unauthenticated() {
    let (app, _dir) = test_app().await;

    let res = post_json(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "some-pkg",
            "report_type": "malware",
            "reason": "suspicious"
        }),
    ).await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_web_admin_page_requires_admin() {
    let (app, _state, _dir) = test_app_with_state().await;
    let session = register_user(app.clone(), "user1", "user1@example.com").await;

    let res = get_with_session(app.clone(), "/admin", &session).await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN);
}

#[tokio::test]
async fn test_web_admin_page_redirects_unauthenticated() {
    let (app, _dir) = test_app().await;

    let res = app
        .clone()
        .oneshot(Request::builder().uri("/admin").body(Body::empty()).unwrap())
        .await
        .unwrap();
    // Should redirect to /login
    assert!(
        res.status() == StatusCode::SEE_OTHER || res.status() == StatusCode::FOUND
            || res.status() == StatusCode::TEMPORARY_REDIRECT,
        "GET /admin unauthenticated should redirect, got {}",
        res.status()
    );
    let location = res.headers().get("location").expect("should have location header");
    assert!(
        location.to_str().unwrap().contains("/login"),
        "redirect should point to /login"
    );
}

#[tokio::test]
async fn test_web_admin_page_loads_for_admin() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session = register_admin(app.clone(), &state, "admin1", "admin1@example.com").await;

    let res = get_with_session(app.clone(), "/admin", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let html = body_string(res).await;
    assert!(html.contains("Admin"), "admin page should contain 'Admin'");
}

// ── Admin Happy-Path Tests ──

#[tokio::test]
async fn test_admin_stats() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "stats-admin", "stats-admin@test.com").await;
    let user_session = register_user(app.clone(), "stats-user", "stats-user@test.com").await;
    let token = create_api_token(app.clone(), &user_session, "stats-token").await;
    publish_package(app.clone(), &token, "stats-pkg", "1.0.0", b"data").await;

    let res = get_with_session(app.clone(), "/api/v1/admin/stats", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["total_users"], 2);
    assert_eq!(body["total_packages"], 1);
    assert_eq!(body["banned_users"], 0);
    assert_eq!(body["open_reports"], 0);
}

#[tokio::test]
async fn test_admin_list_users() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "lu-admin", "lu-admin@test.com").await;
    register_user(app.clone(), "lu-alice", "lu-alice@test.com").await;
    register_user(app.clone(), "lu-bob", "lu-bob@test.com").await;

    let res = get_with_session(app.clone(), "/api/v1/admin/users", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let users = body["users"].as_array().unwrap();
    assert_eq!(users.len(), 3);
    for user in users {
        assert!(user["username"].as_str().is_some());
        assert!(user["email"].as_str().is_some());
        assert!(user.get("is_admin").is_some());
        assert!(user.get("package_count").is_some());
        assert!(user.get("banned").is_some());
        assert!(user["created_at"].as_str().is_some());
    }
}

#[tokio::test]
async fn test_admin_list_users_search() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "lus-admin", "lus-admin@test.com").await;
    register_user(app.clone(), "lus-alice", "lus-alice@test.com").await;
    register_user(app.clone(), "lus-bob", "lus-bob@test.com").await;

    let res =
        get_with_session(app.clone(), "/api/v1/admin/users?q=alice", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let users = body["users"].as_array().unwrap();
    assert_eq!(users.len(), 1);
    assert_eq!(users[0]["username"], "lus-alice");
}

#[tokio::test]
async fn test_admin_get_user() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "gu-admin", "gu-admin@test.com").await;
    let alice_session = register_user(app.clone(), "gu-alice", "gu-alice@test.com").await;
    let alice_token = create_api_token(app.clone(), &alice_session, "gu-token").await;
    publish_package(app.clone(), &alice_token, "gu-pkg", "1.0.0", b"data").await;

    let alice_id = get_user_id(&state, "gu-alice").await;
    let res = get_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{alice_id}"),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["user"]["username"], "gu-alice");
    assert_eq!(body["user"]["email"], "gu-alice@test.com");
    assert!(body["packages"]
        .as_array()
        .unwrap()
        .contains(&serde_json::json!("gu-pkg")));
    assert_eq!(body["active_token_count"], 1);
}

#[tokio::test]
async fn test_admin_get_user_not_found() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "gunf-admin", "gunf-admin@test.com").await;

    let res =
        get_with_session(app.clone(), "/api/v1/admin/users/99999", &admin_session).await;
    assert_eq!(res.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_admin_ban_user() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "ban-admin", "ban-admin@test.com").await;
    let victim_session =
        register_user(app.clone(), "ban-victim", "ban-victim@test.com").await;
    let victim_token = create_api_token(app.clone(), &victim_session, "ban-token").await;

    let victim_id = get_user_id(&state, "ban-victim").await;

    // Ban the user
    let res = post_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/ban"),
        serde_json::json!({"reason": "spam"}),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);

    // Victim's session should be invalidated (401)
    let res = get_with_session(app.clone(), "/api/v1/tokens", &victim_session).await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);

    // Victim's token should be revoked (401 on publish)
    let res =
        publish_package(app.clone(), &victim_token, "ban-pkg", "1.0.0", b"data").await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_admin_unban_user() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "unban-admin", "unban-admin@test.com").await;
    register_user(app.clone(), "unban-victim", "unban-victim@test.com").await;

    let victim_id = get_user_id(&state, "unban-victim").await;

    // Ban
    post_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/ban"),
        serde_json::json!({"reason": "test"}),
        &admin_session,
    )
    .await;

    // Unban
    let res = post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/unban"),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);

    // Victim can log in again and access authenticated endpoints
    let login_res = post_json(
        app.clone(),
        "/api/v1/auth/login",
        serde_json::json!({"username": "unban-victim", "password": "password123"}),
    )
    .await;
    assert_eq!(login_res.status(), StatusCode::OK);
    let new_session = login_res
        .headers()
        .get("set-cookie")
        .unwrap()
        .to_str()
        .unwrap()
        .split(';')
        .next()
        .unwrap()
        .strip_prefix("session=")
        .unwrap()
        .to_string();

    let res = get_with_session(app.clone(), "/api/v1/tokens", &new_session).await;
    assert_eq!(res.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_admin_revoke_tokens() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "rt-admin", "rt-admin@test.com").await;
    let victim_session =
        register_user(app.clone(), "rt-victim", "rt-victim@test.com").await;
    let token1 = create_api_token(app.clone(), &victim_session, "rt-tok1").await;
    let token2 = create_api_token(app.clone(), &victim_session, "rt-tok2").await;

    let victim_id = get_user_id(&state, "rt-victim").await;

    let res = post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/revoke-tokens"),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);
    assert_eq!(body["revoked"], 2);

    // Both tokens should be unusable
    let res =
        publish_package(app.clone(), &token1, "rt-pkg1", "1.0.0", b"data").await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);
    let res =
        publish_package(app.clone(), &token2, "rt-pkg2", "1.0.0", b"data").await;
    assert_eq!(res.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_admin_set_role() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "sr-admin", "sr-admin@test.com").await;
    let user_session =
        register_user(app.clone(), "sr-user1", "sr-user1@test.com").await;

    let user_id = get_user_id(&state, "sr-user1").await;

    // Promote to admin
    let res = put_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{user_id}/role"),
        serde_json::json!({"is_admin": true}),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);

    // User1 can now access admin stats
    let res =
        get_with_session(app.clone(), "/api/v1/admin/stats", &user_session).await;
    assert_eq!(res.status(), StatusCode::OK);

    // Demote back to regular user
    let res = put_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{user_id}/role"),
        serde_json::json!({"is_admin": false}),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);

    // User1 should get 403 on admin endpoints
    let res =
        get_with_session(app.clone(), "/api/v1/admin/stats", &user_session).await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN);
}

#[tokio::test]
async fn test_admin_list_packages() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "lp-admin", "lp-admin@test.com").await;
    let user_session =
        register_user(app.clone(), "lp-user", "lp-user@test.com").await;
    let token = create_api_token(app.clone(), &user_session, "lp-token").await;
    publish_package(app.clone(), &token, "lp-alpha", "1.0.0", b"data").await;
    publish_package(app.clone(), &token, "lp-beta", "0.1.0", b"data").await;

    let res =
        get_with_session(app.clone(), "/api/v1/admin/packages", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let packages = body["packages"].as_array().unwrap();
    assert_eq!(packages.len(), 2);
    for pkg in packages {
        assert!(pkg["name"].as_str().is_some());
        assert!(pkg["description"].as_str().is_some());
        assert!(pkg.get("latest_version").is_some());
        assert!(pkg.get("version_count").is_some());
        assert!(pkg["created_at"].as_str().is_some());
    }
}

#[tokio::test]
async fn test_admin_list_packages_search() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "lps-admin", "lps-admin@test.com").await;
    let user_session =
        register_user(app.clone(), "lps-user", "lps-user@test.com").await;
    let token = create_api_token(app.clone(), &user_session, "lps-token").await;
    publish_package(app.clone(), &token, "lps-foo-bar", "1.0.0", b"data").await;
    publish_package(app.clone(), &token, "lps-baz-qux", "1.0.0", b"data").await;

    let res = get_with_session(
        app.clone(),
        "/api/v1/admin/packages?q=foo",
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let packages = body["packages"].as_array().unwrap();
    assert_eq!(packages.len(), 1);
    assert_eq!(packages[0]["name"], "lps-foo-bar");
}

#[tokio::test]
async fn test_admin_yank_all() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "ya-admin", "ya-admin@test.com").await;
    let user_session =
        register_user(app.clone(), "ya-user", "ya-user@test.com").await;
    let token = create_api_token(app.clone(), &user_session, "ya-token").await;
    publish_package(app.clone(), &token, "ya-pkg", "1.0.0", b"data1").await;
    publish_package(app.clone(), &token, "ya-pkg", "2.0.0", b"data2").await;
    publish_package(app.clone(), &token, "ya-pkg", "3.0.0", b"data3").await;

    let res = post_empty_with_session(
        app.clone(),
        "/api/v1/admin/packages/ya-pkg/yank-all",
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);
    assert_eq!(body["yanked"], 3);

    // Downloading any version should return 404
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/ya-pkg/1.0.0/download")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_admin_remove_package() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "rp-admin", "rp-admin@test.com").await;
    let user_session =
        register_user(app.clone(), "rp-user", "rp-user@test.com").await;
    let token = create_api_token(app.clone(), &user_session, "rp-token").await;
    publish_package(app.clone(), &token, "rp-pkg", "1.0.0", b"data").await;

    let res = delete_with_session(
        app.clone(),
        "/api/v1/admin/packages/rp-pkg",
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);

    // Package should no longer exist
    let res = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/api/v1/packages/rp-pkg")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(res.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_admin_transfer_ownership() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "to-admin", "to-admin@test.com").await;
    let owner_session =
        register_user(app.clone(), "to-owner1", "to-owner1@test.com").await;
    let owner_token = create_api_token(app.clone(), &owner_session, "to-tok1").await;
    let new_owner_session =
        register_user(app.clone(), "to-newowner", "to-newowner@test.com").await;
    let new_owner_token =
        create_api_token(app.clone(), &new_owner_session, "to-tok2").await;

    publish_package(app.clone(), &owner_token, "to-pkg", "1.0.0", b"data").await;

    // Transfer ownership
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/admin/packages/to-pkg/transfer",
        serde_json::json!({"to_username": "to-newowner"}),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    assert_eq!(body["ok"], true);

    // New owner can publish a new version
    let res = publish_package(
        app.clone(),
        &new_owner_token,
        "to-pkg",
        "2.0.0",
        b"data2",
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);

    // Original owner cannot publish
    let res =
        publish_package(app.clone(), &owner_token, "to-pkg", "3.0.0", b"data3").await;
    assert_eq!(res.status(), StatusCode::FORBIDDEN);
}

#[tokio::test]
async fn test_admin_audit_log() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "al-admin", "al-admin@test.com").await;
    let victim_session =
        register_user(app.clone(), "al-victim", "al-victim@test.com").await;
    let token = create_api_token(app.clone(), &victim_session, "al-token").await;
    publish_package(app.clone(), &token, "al-pkg", "1.0.0", b"data").await;

    let victim_id = get_user_id(&state, "al-victim").await;

    // Ban the user (generates audit entry)
    post_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/ban"),
        serde_json::json!({"reason": "audit test"}),
        &admin_session,
    )
    .await;

    // Check audit log
    let res =
        get_with_session(app.clone(), "/api/v1/admin/audit", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let entries = body["entries"].as_array().unwrap();
    assert!(!entries.is_empty());
    let actions: Vec<&str> = entries
        .iter()
        .map(|e| e["action"].as_str().unwrap())
        .collect();
    assert!(actions.contains(&"ban_user"));
}

#[tokio::test]
async fn test_admin_audit_filter() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "af-admin", "af-admin@test.com").await;
    register_user(app.clone(), "af-victim", "af-victim@test.com").await;

    let victim_id = get_user_id(&state, "af-victim").await;

    // Ban (generates ban_user audit entry)
    post_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/ban"),
        serde_json::json!({"reason": "filter test"}),
        &admin_session,
    )
    .await;

    // Unban (generates unban_user audit entry)
    post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{victim_id}/unban"),
        &admin_session,
    )
    .await;

    // Filter by ban_user action
    let res = get_with_session(
        app.clone(),
        "/api/v1/admin/audit?action=ban_user",
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let entries = body["entries"].as_array().unwrap();
    assert!(!entries.is_empty());
    for entry in entries {
        assert_eq!(entry["action"], "ban_user");
    }
}

#[tokio::test]
async fn test_admin_reports_lifecycle() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "rl-admin", "rl-admin@test.com").await;
    let user_session =
        register_user(app.clone(), "rl-reporter", "rl-reporter@test.com").await;

    // User submits a report
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "package",
            "target_name": "suspicious-pkg",
            "report_type": "spam",
            "reason": "This package is spam"
        }),
        &user_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);

    // Admin lists reports and sees it
    let res =
        get_with_session(app.clone(), "/api/v1/admin/reports", &admin_session).await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let reports = body["reports"].as_array().unwrap();
    assert_eq!(reports.len(), 1);
    let report_id = reports[0]["id"].as_i64().unwrap();
    assert_eq!(reports[0]["status"], "open");

    // Admin dismisses the report
    let res = post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/reports/{report_id}/dismiss"),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);

    // Open reports list should be empty now
    let res =
        get_with_session(app.clone(), "/api/v1/admin/reports", &admin_session).await;
    let body = body_json(res).await;
    assert_eq!(body["reports"].as_array().unwrap().len(), 0);

    // Submit another report
    let res = post_json_with_session(
        app.clone(),
        "/api/v1/reports",
        serde_json::json!({
            "target_type": "user",
            "target_name": "bad-actor",
            "report_type": "abuse",
            "reason": "Abusive behavior"
        }),
        &user_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::CREATED);

    // Admin actions the second report
    let res =
        get_with_session(app.clone(), "/api/v1/admin/reports", &admin_session).await;
    let body = body_json(res).await;
    let reports = body["reports"].as_array().unwrap();
    assert_eq!(reports.len(), 1);
    let report_id2 = reports[0]["id"].as_i64().unwrap();

    let res = post_empty_with_session(
        app.clone(),
        &format!("/api/v1/admin/reports/{report_id2}/action"),
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);

    // No more open reports
    let res =
        get_with_session(app.clone(), "/api/v1/admin/reports", &admin_session).await;
    let body = body_json(res).await;
    assert_eq!(body["reports"].as_array().unwrap().len(), 0);
}

#[tokio::test]
async fn test_admin_list_users_filter_banned() {
    let (app, state, _dir) = test_app_with_state().await;
    let admin_session =
        register_admin(app.clone(), &state, "fb-admin", "fb-admin@test.com").await;
    register_user(app.clone(), "fb-good", "fb-good@test.com").await;
    register_user(app.clone(), "fb-bad", "fb-bad@test.com").await;

    let bad_id = get_user_id(&state, "fb-bad").await;

    // Ban one user
    post_json_with_session(
        app.clone(),
        &format!("/api/v1/admin/users/{bad_id}/ban"),
        serde_json::json!({"reason": "test"}),
        &admin_session,
    )
    .await;

    // Filter banned
    let res = get_with_session(
        app.clone(),
        "/api/v1/admin/users?status=banned",
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let users = body["users"].as_array().unwrap();
    assert_eq!(users.len(), 1);
    assert_eq!(users[0]["username"], "fb-bad");

    // Filter active -- should exclude banned user
    let res = get_with_session(
        app.clone(),
        "/api/v1/admin/users?status=active",
        &admin_session,
    )
    .await;
    assert_eq!(res.status(), StatusCode::OK);
    let body = body_json(res).await;
    let users = body["users"].as_array().unwrap();
    assert_eq!(users.len(), 2);
    let usernames: Vec<&str> = users
        .iter()
        .map(|u| u["username"].as_str().unwrap())
        .collect();
    assert!(!usernames.contains(&"fb-bad"));
}
