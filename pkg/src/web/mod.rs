use askama::Template;
use axum::{
    extract::{Query, State},
    http::{header, StatusCode},
    response::{Html, IntoResponse, Redirect},
};
use serde::Deserialize;
use sqlx::Row;
use std::sync::Arc;

use crate::{auth::get_session_user, AppState};

fn render<T: Template>(tmpl: T) -> impl IntoResponse {
    match tmpl.render() {
        Ok(html) => Html(html).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, format!("Template error: {e}")).into_response(),
    }
}

// Helper to extract optional username from session cookie
async fn get_username(state: &AppState, headers: &axum::http::HeaderMap) -> Option<String> {
    let cookie = headers.get(header::COOKIE)?.to_str().ok()?;
    let session_id = cookie.split(';')
        .filter_map(|c| c.trim().strip_prefix("session="))
        .next()?;
    let user = get_session_user(&state.db, session_id).await?;
    Some(user.username)
}

// ── Data types for templates ──

pub struct PackageSummary {
    pub name: String,
    pub description: String,
    pub latest_version: String,
    pub published_at: String,
}

pub struct VersionInfo {
    pub version: String,
    pub published_at: String,
    pub yanked: bool,
    pub size_bytes: i64,
    pub checksum_sha256: String,
    pub sema_version_req: Option<String>,
}

pub struct DepInfo {
    pub dependency_name: String,
    pub version_req: String,
}

pub struct TokenInfo {
    pub id: i64,
    pub name: String,
    pub created_at: String,
    pub last_used_at: Option<String>,
}

// ── Templates ──

#[derive(Template)]
#[template(path = "index.html")]
pub struct IndexTemplate {
    pub username: Option<String>,
    pub total_packages: i64,
    pub recent: Vec<PackageSummary>,
}

#[derive(Template)]
#[template(path = "search.html")]
pub struct SearchTemplate {
    pub username: Option<String>,
    pub query: String,
    pub packages: Vec<PackageSummary>,
    pub total: i64,
    pub page: i64,
    pub per_page: i64,
}

#[derive(Template)]
#[template(path = "package.html")]
pub struct PackageTemplate {
    pub username: Option<String>,
    pub name: String,
    pub description: String,
    pub repository_url: Option<String>,
    pub source: String,
    pub github_repo: Option<String>,
    pub owners: Vec<String>,
    pub versions: Vec<VersionInfo>,
    pub deps: Vec<DepInfo>,
}

#[derive(Template)]
#[template(path = "login.html")]
pub struct LoginTemplate {
    pub username: Option<String>,
    pub github_enabled: bool,
}

#[derive(Template)]
#[template(path = "link.html")]
pub struct LinkTemplate {
    pub username: Option<String>,
    pub github_connected: bool,
    pub github_login: Option<String>,
}

#[derive(Template)]
#[template(path = "account.html")]
pub struct AccountTemplate {
    pub username: Option<String>,
    pub user_email: String,
    pub user_homepage: Option<String>,
    pub github_connected: bool,
    pub github_login: Option<String>,
    pub packages: Vec<PackageSummary>,
    pub tokens: Vec<TokenInfo>,
}

// ── Handlers ──

pub async fn index(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let username = get_username(&state, &headers).await;

    let total_row = sqlx::query("SELECT COUNT(*) as cnt FROM packages")
        .fetch_one(&state.db)
        .await
        .ok();
    let total_packages: i64 = total_row.map(|r| r.get("cnt")).unwrap_or(0);

    let rows = sqlx::query(
        r#"SELECT p.name, p.description, pv.version, pv.published_at
           FROM packages p
           JOIN package_versions pv ON pv.package_id = p.id
           WHERE pv.id = (SELECT MAX(pv2.id) FROM package_versions pv2 WHERE pv2.package_id = p.id)
           ORDER BY pv.published_at DESC
           LIMIT 10"#,
    )
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let recent = rows.iter().map(|r| PackageSummary {
        name: r.get("name"),
        description: r.get("description"),
        latest_version: r.get("version"),
        published_at: r.get("published_at"),
    }).collect();

    render(IndexTemplate { username, total_packages, recent })
}

#[derive(Deserialize)]
pub struct SearchParams {
    pub q: Option<String>,
    pub page: Option<i64>,
}

pub async fn search(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
    Query(params): Query<SearchParams>,
) -> impl IntoResponse {
    let username = get_username(&state, &headers).await;
    let query = params.q.unwrap_or_default();
    let page = params.page.unwrap_or(1).max(1);
    let per_page: i64 = 20;
    let offset = (page - 1) * per_page;
    let pattern = format!("%{}%", query);

    let rows = sqlx::query(
        r#"SELECT p.name, p.description,
           COALESCE((SELECT pv.version FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), '') as latest_version,
           COALESCE((SELECT pv.published_at FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), p.created_at) as published_at
           FROM packages p
           WHERE p.name LIKE ? OR p.description LIKE ?
           ORDER BY p.name
           LIMIT ? OFFSET ?"#,
    )
    .bind(&pattern)
    .bind(&pattern)
    .bind(per_page)
    .bind(offset)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let packages = rows.iter().map(|r| PackageSummary {
        name: r.get("name"),
        description: r.get("description"),
        latest_version: r.get("latest_version"),
        published_at: r.get("published_at"),
    }).collect();

    let total_row = sqlx::query(
        "SELECT COUNT(*) as cnt FROM packages WHERE name LIKE ? OR description LIKE ?",
    )
    .bind(&pattern)
    .bind(&pattern)
    .fetch_one(&state.db)
    .await
    .ok();
    let total: i64 = total_row.map(|r| r.get("cnt")).unwrap_or(0);

    render(SearchTemplate { username, query, packages, total, page, per_page })
}

pub async fn package_detail(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
    axum::extract::Path(name): axum::extract::Path<String>,
) -> impl IntoResponse {
    let username = get_username(&state, &headers).await;

    let pkg = sqlx::query("SELECT id, name, description, repository_url, source, github_repo FROM packages WHERE name = ?")
        .bind(&name)
        .fetch_optional(&state.db)
        .await
        .ok()
        .flatten();

    let pkg = match pkg {
        Some(p) => p,
        None => return (StatusCode::NOT_FOUND, Html("Package not found".to_string())).into_response(),
    };

    let pkg_id: i64 = pkg.get("id");
    let description: String = pkg.get("description");
    let repository_url: Option<String> = pkg.get("repository_url");
    let source: String = pkg.get("source");
    let github_repo: Option<String> = pkg.get("github_repo");

    let version_rows = sqlx::query(
        "SELECT version, published_at, yanked, size_bytes, checksum_sha256, sema_version_req FROM package_versions WHERE package_id = ? ORDER BY id DESC",
    )
    .bind(pkg_id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let versions: Vec<VersionInfo> = version_rows.iter().map(|r| VersionInfo {
        version: r.get("version"),
        published_at: r.get("published_at"),
        yanked: r.get::<i32, _>("yanked") != 0,
        size_bytes: r.get("size_bytes"),
        checksum_sha256: r.get("checksum_sha256"),
        sema_version_req: r.get("sema_version_req"),
    }).collect();

    // Get deps for latest version
    let deps = if let Some(v) = version_rows.first() {
        // We need the version id - get it
        let latest_ver: String = v.get("version");
        let vid_row = sqlx::query("SELECT id FROM package_versions WHERE package_id = ? AND version = ?")
            .bind(pkg_id)
            .bind(&latest_ver)
            .fetch_optional(&state.db)
            .await
            .ok()
            .flatten();
        if let Some(vid_row) = vid_row {
            let vid: i64 = vid_row.get("id");
            let dep_rows = sqlx::query("SELECT dependency_name, version_req FROM dependencies WHERE version_id = ?")
                .bind(vid)
                .fetch_all(&state.db)
                .await
                .unwrap_or_default();
            dep_rows.iter().map(|r| DepInfo {
                dependency_name: r.get("dependency_name"),
                version_req: r.get("version_req"),
            }).collect()
        } else {
            vec![]
        }
    } else {
        vec![]
    };

    let owner_rows = sqlx::query(
        "SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = ?",
    )
    .bind(pkg_id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();
    let owners: Vec<String> = owner_rows.iter().map(|r| r.get("username")).collect();

    render(PackageTemplate { username, name, description, repository_url, source, github_repo, owners, versions, deps }).into_response()
}

pub async fn login(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let username = get_username(&state, &headers).await;
    if username.is_some() {
        return Redirect::to("/account").into_response();
    }
    let github_enabled = state.config.github_client_id.is_some();
    render(LoginTemplate { username: None, github_enabled }).into_response()
}

pub async fn link_page(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let cookie = headers.get(header::COOKIE).and_then(|v| v.to_str().ok()).unwrap_or("");
    let session_id = cookie.split(';')
        .filter_map(|c| c.trim().strip_prefix("session="))
        .next();

    let session_id = match session_id {
        Some(s) => s,
        None => return Redirect::to("/login").into_response(),
    };

    let user = match get_session_user(&state.db, session_id).await {
        Some(u) => u,
        None => return Redirect::to("/login").into_response(),
    };

    let github_row = sqlx::query(
        "SELECT provider_login FROM oauth_connections WHERE user_id = ? AND provider = 'github' AND revoked_at IS NULL"
    )
    .bind(user.id)
    .fetch_optional(&state.db)
    .await
    .ok()
    .flatten();

    let github_connected = github_row.is_some();
    let github_login = github_row.map(|r| r.get::<String, _>("provider_login"));

    render(LinkTemplate {
        username: Some(user.username),
        github_connected,
        github_login,
    }).into_response()
}

pub async fn account(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let cookie = headers.get(header::COOKIE).and_then(|v| v.to_str().ok()).unwrap_or("");
    let session_id = cookie.split(';')
        .filter_map(|c| c.trim().strip_prefix("session="))
        .next();

    let session_id = match session_id {
        Some(s) => s,
        None => return Redirect::to("/login").into_response(),
    };

    let user = match get_session_user(&state.db, session_id).await {
        Some(u) => u,
        None => return Redirect::to("/login").into_response(),
    };

    // Get user details
    let user_row = sqlx::query("SELECT email, homepage FROM users WHERE id = ?")
        .bind(user.id)
        .fetch_optional(&state.db)
        .await
        .ok()
        .flatten();

    let (user_email, user_homepage) = match user_row {
        Some(r) => (r.get::<String, _>("email"), r.get::<Option<String>, _>("homepage")),
        None => (String::new(), None),
    };

    // Get user's packages
    let pkg_rows = sqlx::query(
        r#"SELECT p.name, p.description,
           COALESCE((SELECT pv.version FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), '') as latest_version,
           COALESCE((SELECT pv.published_at FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), p.created_at) as published_at
           FROM packages p
           JOIN owners o ON o.package_id = p.id
           WHERE o.user_id = ?
           ORDER BY p.name"#,
    )
    .bind(user.id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let packages = pkg_rows.iter().map(|r| PackageSummary {
        name: r.get("name"),
        description: r.get("description"),
        latest_version: r.get("latest_version"),
        published_at: r.get("published_at"),
    }).collect();

    // Get tokens
    let token_rows = sqlx::query(
        "SELECT id, name, created_at, last_used_at FROM api_tokens WHERE user_id = ? AND revoked_at IS NULL ORDER BY created_at DESC",
    )
    .bind(user.id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let tokens = token_rows.iter().map(|r| TokenInfo {
        id: r.get("id"),
        name: r.get("name"),
        created_at: r.get("created_at"),
        last_used_at: r.get("last_used_at"),
    }).collect();

    // Get GitHub connection status
    let github_row = sqlx::query(
        "SELECT provider_login FROM oauth_connections WHERE user_id = ? AND provider = 'github' AND revoked_at IS NULL"
    )
    .bind(user.id)
    .fetch_optional(&state.db)
    .await
    .ok()
    .flatten();

    let github_connected = github_row.is_some();
    let github_login = github_row.map(|r| r.get::<String, _>("provider_login"));

    render(AccountTemplate {
        username: Some(user.username),
        user_email,
        user_homepage,
        github_connected,
        github_login,
        packages,
        tokens,
    }).into_response()
}
