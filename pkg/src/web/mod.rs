use askama::Template;
use axum::{
    extract::{Query, State},
    http::{header, StatusCode},
    response::{Html, IntoResponse, Redirect},
};
use sea_orm::*;
use serde::Deserialize;
use std::sync::Arc;

use crate::{
    auth::get_session_user,
    entity::{api_token, dependency, oauth_connection, package, package_version, user},
    AppState,
};

fn render<T: Template>(tmpl: T) -> impl IntoResponse {
    match tmpl.render() {
        Ok(html) => Html(html).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, format!("Template error: {e}")).into_response(),
    }
}

struct SessionInfo {
    username: Option<String>,
    is_admin: bool,
}

async fn get_session_info(state: &AppState, headers: &axum::http::HeaderMap) -> SessionInfo {
    let user = (|| async {
        let cookie = headers.get(header::COOKIE)?.to_str().ok()?;
        let session_id = cookie.split(';')
            .filter_map(|c| c.trim().strip_prefix("session="))
            .next()?;
        get_session_user(&state.db, session_id).await
    })().await;

    match user {
        Some(u) => SessionInfo { username: Some(u.username), is_admin: u.is_admin },
        None => SessionInfo { username: None, is_admin: false },
    }
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
    pub is_admin: bool,
    pub total_packages: i64,
    pub recent: Vec<PackageSummary>,
}

#[derive(Template)]
#[template(path = "search.html")]
pub struct SearchTemplate {
    pub username: Option<String>,
    pub is_admin: bool,
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
    pub is_admin: bool,
    pub name: String,
    pub description: String,
    pub repository_url: Option<String>,
    pub source: String,
    pub github_repo: Option<String>,
    pub owners: Vec<String>,
    pub versions: Vec<VersionInfo>,
    pub deps: Vec<DepInfo>,
    pub total_downloads: i64,
}

#[derive(Template)]
#[template(path = "login.html")]
pub struct LoginTemplate {
    pub username: Option<String>,
    pub is_admin: bool,
    pub github_enabled: bool,
}

#[derive(Template)]
#[template(path = "link.html")]
pub struct LinkTemplate {
    pub username: Option<String>,
    pub is_admin: bool,
    pub github_connected: bool,
    pub github_login: Option<String>,
}

#[derive(Template)]
#[template(path = "account.html")]
pub struct AccountTemplate {
    pub username: Option<String>,
    pub is_admin: bool,
    pub user_email: String,
    pub user_homepage: Option<String>,
    pub github_connected: bool,
    pub github_login: Option<String>,
    pub packages: Vec<PackageSummary>,
    pub tokens: Vec<TokenInfo>,
}

#[derive(Template)]
#[template(path = "admin.html")]
pub struct AdminTemplate {
    pub username: Option<String>,
    pub is_admin: bool,
}

// ── Handlers ──

pub async fn admin_page(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let si = get_session_info(&state, &headers).await;
    match si.username {
        None => Redirect::to("/login").into_response(),
        Some(ref _u) if !si.is_admin => {
            (StatusCode::FORBIDDEN, "Admin access required").into_response()
        }
        Some(_) => render(AdminTemplate {
            username: si.username,
            is_admin: si.is_admin,
        }).into_response(),
    }
}

pub async fn index(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let si = get_session_info(&state, &headers).await;

    let total_packages = package::Entity::find()
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let rows = state.db.query_all(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        r#"SELECT p.name, p.description, pv.version, pv.published_at
           FROM packages p
           JOIN package_versions pv ON pv.package_id = p.id
           WHERE pv.id = (SELECT MAX(pv2.id) FROM package_versions pv2 WHERE pv2.package_id = p.id)
           ORDER BY pv.published_at DESC
           LIMIT ?"#,
        [10i64.into()],
    ))
    .await
    .unwrap_or_default();

    let recent = rows.iter().map(|r| PackageSummary {
        name: r.try_get("", "name").unwrap_or_default(),
        description: r.try_get("", "description").unwrap_or_default(),
        latest_version: r.try_get("", "version").unwrap_or_default(),
        published_at: r.try_get("", "published_at").unwrap_or_default(),
    }).collect();

    render(IndexTemplate { username: si.username, is_admin: si.is_admin, total_packages, recent })
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
    let si = get_session_info(&state, &headers).await;
    let query = params.q.unwrap_or_default();
    let page = params.page.unwrap_or(1).max(1);
    let per_page: i64 = 20;
    let offset = (page - 1) * per_page;
    let pattern = format!("%{}%", query);

    let rows = state.db.query_all(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        r#"SELECT p.name, p.description,
           COALESCE((SELECT pv.version FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), '') as latest_version,
           COALESCE((SELECT pv.published_at FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), p.created_at) as published_at
           FROM packages p
           WHERE p.name LIKE ? OR p.description LIKE ?
           ORDER BY p.name
           LIMIT ? OFFSET ?"#,
        [pattern.clone().into(), pattern.clone().into(), per_page.into(), offset.into()],
    ))
    .await
    .unwrap_or_default();

    let packages = rows.iter().map(|r| PackageSummary {
        name: r.try_get("", "name").unwrap_or_default(),
        description: r.try_get("", "description").unwrap_or_default(),
        latest_version: r.try_get("", "latest_version").unwrap_or_default(),
        published_at: r.try_get("", "published_at").unwrap_or_default(),
    }).collect();

    let total_row = state.db.query_one(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        "SELECT COUNT(*) as cnt FROM packages WHERE name LIKE ? OR description LIKE ?",
        [pattern.clone().into(), pattern.into()],
    ))
    .await
    .ok()
    .flatten();
    let total: i64 = total_row.and_then(|r| r.try_get("", "cnt").ok()).unwrap_or(0);

    render(SearchTemplate { username: si.username, is_admin: si.is_admin, query, packages, total, page, per_page })
}

pub async fn package_detail(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
    axum::extract::Path(name): axum::extract::Path<String>,
) -> impl IntoResponse {
    let si = get_session_info(&state, &headers).await;

    let pkg = package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let pkg = match pkg {
        Some(p) => p,
        None => return (StatusCode::NOT_FOUND, Html("Package not found".to_string())).into_response(),
    };

    let pkg_id = pkg.id;
    let description = pkg.description.clone();
    let repository_url = pkg.repository_url.clone();
    let source = pkg.source.clone();
    let github_repo = pkg.github_repo.clone();

    let version_models = package_version::Entity::find()
        .filter(package_version::Column::PackageId.eq(pkg_id))
        .order_by_desc(package_version::Column::Id)
        .all(&state.db)
        .await
        .unwrap_or_default();

    let versions: Vec<VersionInfo> = version_models.iter().map(|v| VersionInfo {
        version: v.version.clone(),
        published_at: v.published_at.clone(),
        yanked: v.yanked != 0,
        size_bytes: v.size_bytes,
        checksum_sha256: v.checksum_sha256.clone(),
        sema_version_req: v.sema_version_req.clone(),
    }).collect();

    // Get deps for latest version
    let deps = if let Some(latest) = version_models.first() {
        let dep_models = dependency::Entity::find()
            .filter(dependency::Column::VersionId.eq(latest.id))
            .all(&state.db)
            .await
            .unwrap_or_default();
        dep_models.iter().map(|d| DepInfo {
            dependency_name: d.dependency_name.clone(),
            version_req: d.version_req.clone(),
        }).collect()
    } else {
        vec![]
    };

    let owner_rows = state.db.query_all(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        "SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = ?",
        [pkg_id.into()],
    ))
    .await
    .unwrap_or_default();
    let owners: Vec<String> = owner_rows.iter().map(|r| r.try_get("", "username").unwrap_or_default()).collect();

    let total_row = state.db.query_one(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        "SELECT COALESCE(SUM(count), 0) as cnt FROM download_daily WHERE package_name = ?",
        [name.clone().into()],
    ))
    .await
    .ok()
    .flatten();
    let total_downloads: i64 = total_row.and_then(|r| r.try_get("", "cnt").ok()).unwrap_or(0);

    render(PackageTemplate { username: si.username, is_admin: si.is_admin, name, description, repository_url, source, github_repo, owners, versions, deps, total_downloads }).into_response()
}

pub async fn login(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> impl IntoResponse {
    let si = get_session_info(&state, &headers).await;
    if si.username.is_some() {
        return Redirect::to("/account").into_response();
    }
    let github_enabled = state.config.github_client_id.is_some();
    render(LoginTemplate { username: None, is_admin: false, github_enabled }).into_response()
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

    let github_row = oauth_connection::Entity::find()
        .filter(oauth_connection::Column::UserId.eq(user.id))
        .filter(oauth_connection::Column::Provider.eq("github"))
        .filter(oauth_connection::Column::RevokedAt.is_null())
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let github_connected = github_row.is_some();
    let github_login = github_row.and_then(|r| r.provider_login);

    render(LinkTemplate {
        username: Some(user.username),
        is_admin: user.is_admin,
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
    let user_model = user::Entity::find_by_id(user.id)
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let (user_email, user_homepage) = match user_model {
        Some(u) => (u.email, u.homepage),
        None => (String::new(), None),
    };

    // Get user's packages (JOIN query)
    let pkg_rows = state.db.query_all(Statement::from_sql_and_values(
        state.db.get_database_backend(),
        r#"SELECT p.name, p.description,
           COALESCE((SELECT pv.version FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), '') as latest_version,
           COALESCE((SELECT pv.published_at FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.id DESC LIMIT 1), p.created_at) as published_at
           FROM packages p
           JOIN owners o ON o.package_id = p.id
           WHERE o.user_id = ?
           ORDER BY p.name"#,
        [user.id.into()],
    ))
    .await
    .unwrap_or_default();

    let packages = pkg_rows.iter().map(|r| PackageSummary {
        name: r.try_get("", "name").unwrap_or_default(),
        description: r.try_get("", "description").unwrap_or_default(),
        latest_version: r.try_get("", "latest_version").unwrap_or_default(),
        published_at: r.try_get("", "published_at").unwrap_or_default(),
    }).collect();

    // Get tokens
    let token_models = api_token::Entity::find()
        .filter(api_token::Column::UserId.eq(user.id))
        .filter(api_token::Column::RevokedAt.is_null())
        .order_by_desc(api_token::Column::CreatedAt)
        .all(&state.db)
        .await
        .unwrap_or_default();

    let tokens = token_models.iter().map(|t| TokenInfo {
        id: t.id,
        name: t.name.clone(),
        created_at: t.created_at.clone(),
        last_used_at: t.last_used_at.clone(),
    }).collect();

    // Get GitHub connection status
    let github_row = oauth_connection::Entity::find()
        .filter(oauth_connection::Column::UserId.eq(user.id))
        .filter(oauth_connection::Column::Provider.eq("github"))
        .filter(oauth_connection::Column::RevokedAt.is_null())
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let github_connected = github_row.is_some();
    let github_login = github_row.and_then(|r| r.provider_login);

    render(AccountTemplate {
        username: Some(user.username.clone()),
        is_admin: user.is_admin,
        user_email,
        user_homepage,
        github_connected,
        github_login,
        packages,
        tokens,
    }).into_response()
}
