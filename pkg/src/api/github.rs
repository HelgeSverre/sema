use axum::{
    body::Bytes,
    extract::State,
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use hmac::{Hmac, Mac};
use sea_orm::*;
use sha2::Sha256;
use serde::Deserialize;
use std::sync::Arc;

use crate::{
    auth::AuthUser,
    entity::{github_sync_log, owner, package},
    github_sync,
    AppState,
};

#[derive(Deserialize)]
pub struct LinkRequest {
    pub repository_url: String,
}

pub async fn link(
    State(state): State<Arc<AppState>>,
    AuthUser(user): AuthUser,
    Json(body): Json<LinkRequest>,
) -> impl IntoResponse {
    // Parse the GitHub URL
    let (owner_name, repo) = match github_sync::parse_github_url(&body.repository_url) {
        Some(pair) => pair,
        None => return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "Invalid GitHub URL. Expected format: github.com/owner/repo"})),
        ).into_response(),
    };

    // Get the user's GitHub token
    let token = match github_sync::get_github_token(&state.db, user.id, &state.config.oauth_token_key).await {
        Some(t) => t,
        None => return (
            StatusCode::FORBIDDEN,
            Json(serde_json::json!({
                "error": "GitHub not connected",
                "connect_url": "/auth/github?mode=connect&return_to=/account"
            })),
        ).into_response(),
    };

    let client = reqwest::Client::new();

    // Validate repo exists and has sema.toml
    let manifest = match github_sync::validate_repo(&client, &token, &owner_name, &repo).await {
        Ok(m) => m,
        Err(e) => {
            if e.contains("invalid or revoked") {
                github_sync::mark_token_revoked(&state.db, user.id).await;
            }
            return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": e}))).into_response();
        }
    };

    // Check if package name is already taken
    let existing = package::Entity::find()
        .filter(package::Column::Name.eq(&manifest.name))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    if let Some(pkg) = existing {
        return (
            StatusCode::CONFLICT,
            Json(serde_json::json!({
                "error": format!("Package '{}' already exists (source: {})", manifest.name, pkg.source)
            })),
        ).into_response();
    }

    // Generate webhook secret
    let webhook_secret = github_sync::generate_webhook_secret();
    let github_repo = format!("{owner_name}/{repo}");

    // Create package with source=github
    let pkg_model = package::ActiveModel {
        name: Set(manifest.name.clone()),
        description: Set(manifest.description.clone()),
        repository_url: Set(Some(format!("https://github.com/{github_repo}"))),
        source: Set("github".into()),
        github_repo: Set(Some(github_repo.clone())),
        webhook_secret: Set(Some(webhook_secret.clone())),
        ..Default::default()
    };

    let pkg_result = pkg_model.insert(&state.db).await;

    let package_id = match pkg_result {
        Ok(p) => p.id,
        Err(e) => return (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({"error": format!("Failed to create package: {e}")})),
        ).into_response(),
    };

    // Add user as owner
    let owner_model = owner::ActiveModel {
        package_id: Set(package_id),
        user_id: Set(user.id),
    };
    let _ = owner_model.insert(&state.db).await;

    // Register webhook
    let webhook_url = format!("{}/api/v1/webhooks/github", state.config.base_url);
    if let Err(e) = github_sync::register_webhook(&client, &token, &owner_name, &repo, &webhook_url, &webhook_secret).await {
        tracing::warn!("Failed to register webhook for {github_repo}: {e}");
    }

    // Import existing semver tags
    let tags = github_sync::list_semver_tags(&client, &token, &owner_name, &repo).await.unwrap_or_default();
    let mut imported = 0u32;
    let mut errors = Vec::new();

    for (tag_name, version) in &tags {
        match github_sync::sync_tag(
            &state.db, &owner_name, &repo,
            tag_name, version, package_id,
            manifest.sema_version_req.as_deref(),
        ).await {
            Ok(true) => imported += 1,
            Ok(false) => {}
            Err(e) => {
                let log_model = github_sync_log::ActiveModel {
                    package_id: Set(package_id),
                    tag: Set(tag_name.clone()),
                    status: Set("error".into()),
                    error: Set(Some(e.clone())),
                    ..Default::default()
                };
                let _ = log_model.insert(&state.db).await;
                errors.push(format!("{tag_name}: {e}"));
            }
        }
    }

    crate::audit::log(&state.db, &user.username, "link_repo", Some("package"), Some(&manifest.name), Some(&github_repo)).await;

    (StatusCode::CREATED, Json(serde_json::json!({
        "ok": true,
        "package": manifest.name,
        "source": "github",
        "github_repo": github_repo,
        "tags_found": tags.len(),
        "versions_imported": imported,
        "errors": errors,
    }))).into_response()
}

pub async fn sync(
    State(state): State<Arc<AppState>>,
    AuthUser(user): AuthUser,
    axum::extract::Path(name): axum::extract::Path<String>,
) -> impl IntoResponse {
    let pkg_row = state.db.query_one(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        "SELECT p.id, p.source, p.github_repo FROM packages p JOIN owners o ON o.package_id = p.id WHERE p.name = ? AND o.user_id = ?",
        [name.clone().into(), user.id.into()],
    ))
    .await
    .ok()
    .flatten();

    let pkg_row = match pkg_row {
        Some(r) => r,
        None => return (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Package not found or you are not an owner"})),
        ).into_response(),
    };

    let source: String = pkg_row.try_get("", "source").unwrap_or_default();
    if source != "github" {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "Package is not GitHub-linked"})),
        ).into_response();
    }

    let package_id: i64 = pkg_row.try_get("", "id").unwrap_or_default();
    let github_repo: String = pkg_row.try_get("", "github_repo").unwrap_or_default();

    let (owner_name, repo) = match github_sync::parse_github_url(&github_repo) {
        Some(pair) => pair,
        None => return (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({"error": "Invalid github_repo in database"})),
        ).into_response(),
    };

    let token = match github_sync::get_github_token(&state.db, user.id, &state.config.oauth_token_key).await {
        Some(t) => t,
        None => return (
            StatusCode::FORBIDDEN,
            Json(serde_json::json!({"error": "GitHub not connected. Reconnect at /auth/github?mode=connect"})),
        ).into_response(),
    };

    let client = reqwest::Client::new();
    let tags = match github_sync::list_semver_tags(&client, &token, &owner_name, &repo).await {
        Ok(t) => t,
        Err(e) => {
            if e.contains("invalid or revoked") || e.contains("401") {
                github_sync::mark_token_revoked(&state.db, user.id).await;
            }
            return (StatusCode::BAD_GATEWAY, Json(serde_json::json!({"error": e}))).into_response();
        }
    };

    let mut imported = 0u32;
    for (tag_name, version) in &tags {
        match github_sync::sync_tag(
            &state.db, &owner_name, &repo,
            tag_name, version, package_id, None,
        ).await {
            Ok(true) => imported += 1,
            Ok(false) => {}
            Err(e) => {
                let log_model = github_sync_log::ActiveModel {
                    package_id: Set(package_id),
                    tag: Set(tag_name.clone()),
                    status: Set("error".into()),
                    error: Set(Some(e)),
                    ..Default::default()
                };
                let _ = log_model.insert(&state.db).await;
            }
        }
    }

    Json(serde_json::json!({
        "ok": true,
        "tags_found": tags.len(),
        "versions_imported": imported,
    })).into_response()
}

type HmacSha256 = Hmac<Sha256>;

pub async fn webhook(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
    body: Bytes,
) -> impl IntoResponse {
    // Get the signature header
    let signature = match headers.get("x-hub-signature-256").and_then(|v| v.to_str().ok()) {
        Some(s) => s.to_string(),
        None => return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": "Missing signature"}))).into_response(),
    };

    // Parse the event type
    let event = headers.get("x-github-event").and_then(|v| v.to_str().ok()).unwrap_or("");
    if event == "ping" {
        return Json(serde_json::json!({"ok": true, "event": "ping"})).into_response();
    }
    if event != "push" {
        return Json(serde_json::json!({"ok": true, "event": event, "skipped": true})).into_response();
    }

    // Parse the push payload to get the ref and repo
    let payload: serde_json::Value = match serde_json::from_slice(&body) {
        Ok(v) => v,
        Err(_) => return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": "Invalid JSON"}))).into_response(),
    };

    let git_ref = payload.get("ref").and_then(|r| r.as_str()).unwrap_or("");
    // Only process tag pushes
    let tag_name = match git_ref.strip_prefix("refs/tags/") {
        Some(t) => t,
        None => return Json(serde_json::json!({"ok": true, "skipped": "not a tag push"})).into_response(),
    };

    // Parse as semver (strip v prefix)
    let version_str = tag_name.strip_prefix('v').unwrap_or(tag_name);
    let version = match semver::Version::parse(version_str) {
        Ok(v) => v,
        Err(_) => return Json(serde_json::json!({"ok": true, "skipped": "not a semver tag"})).into_response(),
    };

    // Get the repo full_name from the payload
    let repo_full_name = payload.get("repository")
        .and_then(|r| r.get("full_name"))
        .and_then(|n| n.as_str())
        .unwrap_or("");

    if repo_full_name.is_empty() {
        return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": "Missing repository info"}))).into_response();
    }

    // Find the package by github_repo
    let pkg = package::Entity::find()
        .filter(package::Column::GithubRepo.eq(repo_full_name))
        .filter(package::Column::Source.eq("github"))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let pkg = match pkg {
        Some(p) => p,
        None => return (StatusCode::NOT_FOUND, Json(serde_json::json!({"error": "No linked package for this repo"}))).into_response(),
    };

    let package_id = pkg.id;
    let webhook_secret = pkg.webhook_secret.unwrap_or_default();

    // Verify HMAC signature
    let expected_sig = format!("sha256={}", compute_hmac(&webhook_secret, &body));
    if !constant_time_eq(signature.as_bytes(), expected_sig.as_bytes()) {
        return (StatusCode::FORBIDDEN, Json(serde_json::json!({"error": "Invalid signature"}))).into_response();
    }

    let (owner_name, repo) = match github_sync::parse_github_url(repo_full_name) {
        Some(pair) => pair,
        None => return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": "Invalid repo name"}))).into_response(),
    };

    match github_sync::sync_tag(
        &state.db, &owner_name, &repo,
        tag_name, &version, package_id, None,
    ).await {
        Ok(true) => {
            tracing::info!("Webhook: synced {repo_full_name} tag {tag_name} as {version}");
            crate::audit::log(&state.db, "system", "webhook_sync", Some("package"), Some(repo_full_name), Some(tag_name)).await;
            Json(serde_json::json!({"ok": true, "version": version.to_string(), "imported": true})).into_response()
        }
        Ok(false) => {
            Json(serde_json::json!({"ok": true, "version": version.to_string(), "imported": false, "reason": "already exists"})).into_response()
        }
        Err(e) => {
            let log_model = github_sync_log::ActiveModel {
                package_id: Set(package_id),
                tag: Set(tag_name.to_string()),
                status: Set("error".into()),
                error: Set(Some(e.clone())),
                ..Default::default()
            };
            let _ = log_model.insert(&state.db).await;
            (StatusCode::INTERNAL_SERVER_ERROR, Json(serde_json::json!({"error": e}))).into_response()
        }
    }
}

fn compute_hmac(secret: &str, data: &[u8]) -> String {
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).expect("HMAC key size");
    mac.update(data);
    hex::encode(mac.finalize().into_bytes())
}

fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().zip(b.iter()).fold(0u8, |acc, (x, y)| acc | (x ^ y)) == 0
}
