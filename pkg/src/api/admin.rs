use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use sqlx::Row;
use std::sync::Arc;

use crate::{audit, auth::AdminUser, AppState};

// ── Dashboard ──

pub async fn stats(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
) -> impl IntoResponse {
    let total_users: i64 = sqlx::query("SELECT COUNT(*) as cnt FROM users")
        .fetch_one(&state.db)
        .await
        .map(|r| r.get("cnt"))
        .unwrap_or(0);

    let total_packages: i64 = sqlx::query("SELECT COUNT(*) as cnt FROM packages")
        .fetch_one(&state.db)
        .await
        .map(|r| r.get("cnt"))
        .unwrap_or(0);

    let banned_users: i64 =
        sqlx::query("SELECT COUNT(*) as cnt FROM users WHERE banned_at IS NOT NULL")
            .fetch_one(&state.db)
            .await
            .map(|r| r.get("cnt"))
            .unwrap_or(0);

    let open_reports: i64 =
        sqlx::query("SELECT COUNT(*) as cnt FROM reports WHERE status = 'open'")
            .fetch_one(&state.db)
            .await
            .map(|r| r.get("cnt"))
            .unwrap_or(0);

    Json(serde_json::json!({
        "total_users": total_users,
        "total_packages": total_packages,
        "banned_users": banned_users,
        "open_reports": open_reports,
        "total_downloads": 0,
    }))
}

// ── Users ──

#[derive(Deserialize)]
pub struct UserListParams {
    pub q: Option<String>,
    pub status: Option<String>,
}

pub async fn list_users(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<UserListParams>,
) -> impl IntoResponse {
    let mut where_clauses: Vec<String> = vec!["1=1".to_string()];
    let mut binds: Vec<String> = Vec::new();

    if let Some(ref q) = params.q {
        let pattern = format!("%{q}%");
        where_clauses.push("(u.username LIKE ? OR u.email LIKE ?)".to_string());
        binds.push(pattern.clone());
        binds.push(pattern);
    }

    match params.status.as_deref() {
        Some("banned") => where_clauses.push("u.banned_at IS NOT NULL".to_string()),
        Some("active") => where_clauses.push("u.banned_at IS NULL".to_string()),
        Some("github") => where_clauses.push("u.github_id IS NOT NULL".to_string()),
        _ => {}
    }

    let where_sql = where_clauses.join(" AND ");
    let sql = format!(
        r#"SELECT u.id, u.username, u.email, u.is_admin, u.github_id,
              (SELECT COUNT(*) FROM owners WHERE owners.user_id = u.id) as package_count,
              (SELECT COUNT(*) FROM api_tokens WHERE api_tokens.user_id = u.id AND api_tokens.revoked_at IS NULL) as token_count,
              u.banned_at, u.created_at
           FROM users u
           WHERE {where_sql}
           ORDER BY u.created_at DESC"#
    );

    let mut query = sqlx::query(&sql);
    for b in &binds {
        query = query.bind(b);
    }

    let rows = query.fetch_all(&state.db).await.unwrap_or_default();

    let users: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            let banned_at: Option<String> = r.get("banned_at");
            let github_id: Option<i64> = r.get("github_id");
            serde_json::json!({
                "id": r.get::<i64, _>("id"),
                "username": r.get::<String, _>("username"),
                "email": r.get::<String, _>("email"),
                "is_admin": r.get::<i32, _>("is_admin") != 0,
                "github_id": github_id,
                "package_count": r.get::<i64, _>("package_count"),
                "token_count": r.get::<i64, _>("token_count"),
                "banned": banned_at.is_some(),
                "created_at": r.get::<String, _>("created_at"),
            })
        })
        .collect();

    Json(serde_json::json!({ "users": users }))
}

pub async fn get_user(
    State(state): State<Arc<AppState>>,
    AdminUser(_admin): AdminUser,
    Path(user_id): Path<i64>,
) -> impl IntoResponse {
    let row = sqlx::query(
        r#"SELECT id, username, email, is_admin, github_id, banned_at, created_at
           FROM users WHERE id = ?"#,
    )
    .bind(user_id)
    .fetch_optional(&state.db)
    .await;

    let row = match row {
        Ok(Some(r)) => r,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let banned_at: Option<String> = row.get("banned_at");
    let github_id: Option<i64> = row.get("github_id");

    let packages = sqlx::query(
        r#"SELECT p.name FROM packages p
           JOIN owners o ON o.package_id = p.id
           WHERE o.user_id = ?
           ORDER BY p.name"#,
    )
    .bind(user_id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let package_names: Vec<String> = packages.iter().map(|r| r.get("name")).collect();

    let token_count: i64 = sqlx::query(
        "SELECT COUNT(*) as cnt FROM api_tokens WHERE user_id = ? AND revoked_at IS NULL",
    )
    .bind(user_id)
    .fetch_one(&state.db)
    .await
    .map(|r| r.get("cnt"))
    .unwrap_or(0);

    Json(serde_json::json!({
        "user": {
            "id": row.get::<i64, _>("id"),
            "username": row.get::<String, _>("username"),
            "email": row.get::<String, _>("email"),
            "is_admin": row.get::<i32, _>("is_admin") != 0,
            "github_id": github_id,
            "banned": banned_at.is_some(),
            "created_at": row.get::<String, _>("created_at"),
        },
        "packages": package_names,
        "active_token_count": token_count,
    }))
    .into_response()
}

#[derive(Deserialize)]
pub struct BanRequest {
    pub reason: Option<String>,
}

pub async fn ban_user(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(user_id): Path<i64>,
    body: Option<Json<BanRequest>>,
) -> impl IntoResponse {
    if user_id == admin.id {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "Cannot ban yourself"})),
        )
            .into_response();
    }

    let reason = body.and_then(|b| b.0.reason);

    // Verify user exists
    let user_row = sqlx::query("SELECT username FROM users WHERE id = ?")
        .bind(user_id)
        .fetch_optional(&state.db)
        .await;

    let username: String = match user_row {
        Ok(Some(r)) => r.get("username"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    // Ban the user
    let _ = sqlx::query("UPDATE users SET banned_at = datetime('now') WHERE id = ?")
        .bind(user_id)
        .execute(&state.db)
        .await;

    // Revoke all active tokens
    let _ = sqlx::query(
        "UPDATE api_tokens SET revoked_at = datetime('now') WHERE user_id = ? AND revoked_at IS NULL",
    )
    .bind(user_id)
    .execute(&state.db)
    .await;

    // Delete all sessions
    let _ = sqlx::query("DELETE FROM sessions WHERE user_id = ?")
        .bind(user_id)
        .execute(&state.db)
        .await;

    let detail = reason.as_deref().unwrap_or("no reason given");
    audit::log(
        &state.db,
        &admin.username,
        "ban_user",
        Some("user"),
        Some(&username),
        Some(detail),
    )
    .await;

    Json(serde_json::json!({"ok": true})).into_response()
}

pub async fn unban_user(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(user_id): Path<i64>,
) -> impl IntoResponse {
    let user_row = sqlx::query("SELECT username FROM users WHERE id = ?")
        .bind(user_id)
        .fetch_optional(&state.db)
        .await;

    let username: String = match user_row {
        Ok(Some(r)) => r.get("username"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let _ = sqlx::query("UPDATE users SET banned_at = NULL WHERE id = ?")
        .bind(user_id)
        .execute(&state.db)
        .await;

    audit::log(
        &state.db,
        &admin.username,
        "unban_user",
        Some("user"),
        Some(&username),
        None,
    )
    .await;

    Json(serde_json::json!({"ok": true})).into_response()
}

pub async fn revoke_user_tokens(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(user_id): Path<i64>,
) -> impl IntoResponse {
    let user_row = sqlx::query("SELECT username FROM users WHERE id = ?")
        .bind(user_id)
        .fetch_optional(&state.db)
        .await;

    let username: String = match user_row {
        Ok(Some(r)) => r.get("username"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let result = sqlx::query(
        "UPDATE api_tokens SET revoked_at = datetime('now') WHERE user_id = ? AND revoked_at IS NULL",
    )
    .bind(user_id)
    .execute(&state.db)
    .await;

    let count = result.map(|r| r.rows_affected()).unwrap_or(0);

    audit::log(
        &state.db,
        &admin.username,
        "revoke_tokens",
        Some("user"),
        Some(&username),
        Some(&format!("revoked {count} tokens")),
    )
    .await;

    Json(serde_json::json!({"ok": true, "revoked": count})).into_response()
}

#[derive(Deserialize)]
pub struct RoleRequest {
    pub is_admin: bool,
}

pub async fn set_user_role(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(user_id): Path<i64>,
    Json(body): Json<RoleRequest>,
) -> impl IntoResponse {
    if user_id == admin.id {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "Cannot change your own admin role"})),
        )
            .into_response();
    }

    let user_row = sqlx::query("SELECT username FROM users WHERE id = ?")
        .bind(user_id)
        .fetch_optional(&state.db)
        .await;

    let username: String = match user_row {
        Ok(Some(r)) => r.get("username"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let admin_val: i32 = if body.is_admin { 1 } else { 0 };
    let _ = sqlx::query("UPDATE users SET is_admin = ? WHERE id = ?")
        .bind(admin_val)
        .bind(user_id)
        .execute(&state.db)
        .await;

    let role_str = if body.is_admin { "admin" } else { "user" };
    audit::log(
        &state.db,
        &admin.username,
        "set_role",
        Some("user"),
        Some(&username),
        Some(&format!("set role to {role_str}")),
    )
    .await;

    Json(serde_json::json!({"ok": true})).into_response()
}

// ── Packages ──

#[derive(Deserialize)]
pub struct PkgListParams {
    pub q: Option<String>,
    pub source: Option<String>,
    pub reported: Option<bool>,
}

pub async fn list_packages(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<PkgListParams>,
) -> impl IntoResponse {
    let mut where_clauses: Vec<String> = vec!["1=1".to_string()];
    let mut binds: Vec<String> = Vec::new();

    if let Some(ref q) = params.q {
        let pattern = format!("%{q}%");
        where_clauses.push("p.name LIKE ?".to_string());
        binds.push(pattern);
    }

    if let Some(ref source) = params.source {
        where_clauses.push("p.source = ?".to_string());
        binds.push(source.clone());
    }

    if params.reported == Some(true) {
        where_clauses.push(
            "EXISTS (SELECT 1 FROM reports r WHERE r.target_type = 'package' AND r.target_name = p.name AND r.status = 'open')"
                .to_string(),
        );
    }

    let where_sql = where_clauses.join(" AND ");
    let sql = format!(
        r#"SELECT p.name, p.description, p.source, p.created_at,
              (SELECT pv.version FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.published_at DESC LIMIT 1) as latest_version,
              (SELECT COUNT(*) FROM package_versions pv WHERE pv.package_id = p.id) as version_count,
              (SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = p.id LIMIT 1) as owner,
              EXISTS (SELECT 1 FROM reports r WHERE r.target_type = 'package' AND r.target_name = p.name AND r.status = 'open') as reported
           FROM packages p
           WHERE {where_sql}
           ORDER BY p.created_at DESC"#
    );

    let mut query = sqlx::query(&sql);
    for b in &binds {
        query = query.bind(b);
    }

    let rows = query.fetch_all(&state.db).await.unwrap_or_default();

    let packages: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "name": r.get::<String, _>("name"),
                "description": r.get::<String, _>("description"),
                "latest_version": r.get::<Option<String>, _>("latest_version"),
                "version_count": r.get::<i64, _>("version_count"),
                "source": r.get::<String, _>("source"),
                "owner": r.get::<Option<String>, _>("owner"),
                "reported": r.get::<i32, _>("reported") != 0,
                "created_at": r.get::<String, _>("created_at"),
            })
        })
        .collect();

    Json(serde_json::json!({ "packages": packages }))
}

pub async fn get_package(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let pkg = sqlx::query(
        "SELECT id, name, description, repository_url, source, github_repo, created_at FROM packages WHERE name = ?",
    )
    .bind(&name)
    .fetch_optional(&state.db)
    .await;

    let pkg = match pkg {
        Ok(Some(p)) => p,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Package not found"})),
            )
                .into_response();
        }
    };

    let pkg_id: i64 = pkg.get("id");

    let versions = sqlx::query(
        r#"SELECT version, checksum_sha256, size_bytes, yanked, sema_version_req, published_at
           FROM package_versions WHERE package_id = ?
           ORDER BY published_at DESC"#,
    )
    .bind(pkg_id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let version_list: Vec<serde_json::Value> = versions
        .iter()
        .map(|r| {
            serde_json::json!({
                "version": r.get::<String, _>("version"),
                "checksum_sha256": r.get::<String, _>("checksum_sha256"),
                "size_bytes": r.get::<i64, _>("size_bytes"),
                "yanked": r.get::<i32, _>("yanked") != 0,
                "sema_version_req": r.get::<Option<String>, _>("sema_version_req"),
                "published_at": r.get::<String, _>("published_at"),
            })
        })
        .collect();

    let owner_rows = sqlx::query(
        "SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = ?",
    )
    .bind(pkg_id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let owners: Vec<String> = owner_rows.iter().map(|r| r.get("username")).collect();

    let open_reports: i64 = sqlx::query(
        "SELECT COUNT(*) as cnt FROM reports WHERE target_type = 'package' AND target_name = ? AND status = 'open'",
    )
    .bind(&name)
    .fetch_one(&state.db)
    .await
    .map(|r| r.get("cnt"))
    .unwrap_or(0);

    Json(serde_json::json!({
        "package": {
            "name": pkg.get::<String, _>("name"),
            "description": pkg.get::<String, _>("description"),
            "repository_url": pkg.get::<Option<String>, _>("repository_url"),
            "source": pkg.get::<String, _>("source"),
            "github_repo": pkg.get::<Option<String>, _>("github_repo"),
            "created_at": pkg.get::<String, _>("created_at"),
        },
        "versions": version_list,
        "owners": owners,
        "open_reports": open_reports,
    }))
    .into_response()
}

pub async fn yank_all_versions(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let result = sqlx::query(
        "UPDATE package_versions SET yanked = 1 WHERE package_id = (SELECT id FROM packages WHERE name = ?)",
    )
    .bind(&name)
    .execute(&state.db)
    .await;

    let count = result.map(|r| r.rows_affected()).unwrap_or(0);

    if count == 0 {
        return (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Package not found or no versions to yank"})),
        )
            .into_response();
    }

    audit::log(
        &state.db,
        &admin.username,
        "yank_all",
        Some("package"),
        Some(&name),
        Some(&format!("yanked {count} versions")),
    )
    .await;

    Json(serde_json::json!({"ok": true, "yanked": count})).into_response()
}

pub async fn remove_package(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let pkg = sqlx::query("SELECT id FROM packages WHERE name = ?")
        .bind(&name)
        .fetch_optional(&state.db)
        .await;

    let pkg_id: i64 = match pkg {
        Ok(Some(r)) => r.get("id"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Package not found"})),
            )
                .into_response();
        }
    };

    // Delete dependencies via version_id join
    let _ = sqlx::query(
        "DELETE FROM dependencies WHERE version_id IN (SELECT id FROM package_versions WHERE package_id = ?)",
    )
    .bind(pkg_id)
    .execute(&state.db)
    .await;

    // Delete versions
    let _ = sqlx::query("DELETE FROM package_versions WHERE package_id = ?")
        .bind(pkg_id)
        .execute(&state.db)
        .await;

    // Delete owners
    let _ = sqlx::query("DELETE FROM owners WHERE package_id = ?")
        .bind(pkg_id)
        .execute(&state.db)
        .await;

    // Delete the package
    let _ = sqlx::query("DELETE FROM packages WHERE id = ?")
        .bind(pkg_id)
        .execute(&state.db)
        .await;

    // Clean up any reports targeting this package
    let _ = sqlx::query("DELETE FROM reports WHERE target_type = 'package' AND target_name = ?")
        .bind(&name)
        .execute(&state.db)
        .await;

    audit::log(
        &state.db,
        &admin.username,
        "remove_package",
        Some("package"),
        Some(&name),
        None,
    )
    .await;

    Json(serde_json::json!({"ok": true})).into_response()
}

#[derive(Deserialize)]
pub struct TransferRequest {
    pub to_username: String,
}

pub async fn transfer_ownership(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(name): Path<String>,
    Json(body): Json<TransferRequest>,
) -> impl IntoResponse {
    let pkg = sqlx::query("SELECT id FROM packages WHERE name = ?")
        .bind(&name)
        .fetch_optional(&state.db)
        .await;

    let pkg_id: i64 = match pkg {
        Ok(Some(r)) => r.get("id"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Package not found"})),
            )
                .into_response();
        }
    };

    let target_user = sqlx::query("SELECT id FROM users WHERE username = ?")
        .bind(&body.to_username)
        .fetch_optional(&state.db)
        .await;

    let target_id: i64 = match target_user {
        Ok(Some(r)) => r.get("id"),
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Target user not found"})),
            )
                .into_response();
        }
    };

    // Remove existing owners
    let _ = sqlx::query("DELETE FROM owners WHERE package_id = ?")
        .bind(pkg_id)
        .execute(&state.db)
        .await;

    // Insert new owner
    let _ = sqlx::query("INSERT INTO owners (package_id, user_id) VALUES (?, ?)")
        .bind(pkg_id)
        .bind(target_id)
        .execute(&state.db)
        .await;

    audit::log(
        &state.db,
        &admin.username,
        "transfer_ownership",
        Some("package"),
        Some(&name),
        Some(&format!("transferred to {}", body.to_username)),
    )
    .await;

    Json(serde_json::json!({"ok": true})).into_response()
}

// ── Audit Log ──

#[derive(Deserialize)]
pub struct AuditListParams {
    pub q: Option<String>,
    pub action: Option<String>,
    pub page: Option<i64>,
    pub per_page: Option<i64>,
}

pub async fn list_audit(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<AuditListParams>,
) -> impl IntoResponse {
    let per_page = params.per_page.unwrap_or(50).min(200);
    let page = params.page.unwrap_or(1).max(1);
    let offset = (page - 1) * per_page;

    let mut where_clauses: Vec<String> = vec!["1=1".to_string()];
    let mut binds: Vec<String> = Vec::new();

    if let Some(ref action) = params.action {
        where_clauses.push("action = ?".to_string());
        binds.push(action.clone());
    }

    if let Some(ref q) = params.q {
        let pattern = format!("%{q}%");
        where_clauses.push("(actor LIKE ? OR target_name LIKE ? OR detail LIKE ?)".to_string());
        binds.push(pattern.clone());
        binds.push(pattern.clone());
        binds.push(pattern);
    }

    let where_sql = where_clauses.join(" AND ");

    // Get total count
    let count_sql = format!("SELECT COUNT(*) as cnt FROM audit_log WHERE {where_sql}");
    let mut count_query = sqlx::query(&count_sql);
    for b in &binds {
        count_query = count_query.bind(b);
    }
    let total: i64 = count_query
        .fetch_one(&state.db)
        .await
        .map(|r| r.get("cnt"))
        .unwrap_or(0);

    // Get entries
    let sql = format!(
        r#"SELECT id, actor, action, target_type, target_name, detail, created_at
           FROM audit_log
           WHERE {where_sql}
           ORDER BY created_at DESC
           LIMIT ? OFFSET ?"#
    );
    let mut query = sqlx::query(&sql);
    for b in &binds {
        query = query.bind(b);
    }
    query = query.bind(per_page).bind(offset);

    let rows = query.fetch_all(&state.db).await.unwrap_or_default();

    let entries: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "id": r.get::<i64, _>("id"),
                "actor": r.get::<String, _>("actor"),
                "action": r.get::<String, _>("action"),
                "target_type": r.get::<Option<String>, _>("target_type"),
                "target_name": r.get::<Option<String>, _>("target_name"),
                "detail": r.get::<Option<String>, _>("detail"),
                "created_at": r.get::<String, _>("created_at"),
            })
        })
        .collect();

    Json(serde_json::json!({
        "entries": entries,
        "total": total,
        "page": page,
        "per_page": per_page,
    }))
}

// ── Reports ──

#[derive(Deserialize)]
pub struct ReportListParams {
    pub status: Option<String>,
}

pub async fn list_reports(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<ReportListParams>,
) -> impl IntoResponse {
    let status = params.status.unwrap_or_else(|| "open".to_string());

    let rows = sqlx::query(
        r#"SELECT r.id, u.username as reporter, r.target_type, r.target_name,
              r.report_type, r.reason, r.status, r.created_at
           FROM reports r
           LEFT JOIN users u ON u.id = r.reporter_id
           WHERE r.status = ?
           ORDER BY r.created_at DESC"#,
    )
    .bind(&status)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let reports: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "id": r.get::<i64, _>("id"),
                "reporter": r.get::<Option<String>, _>("reporter").unwrap_or_else(|| "[deleted]".to_string()),
                "target_type": r.get::<String, _>("target_type"),
                "target_name": r.get::<String, _>("target_name"),
                "report_type": r.get::<String, _>("report_type"),
                "reason": r.get::<String, _>("reason"),
                "status": r.get::<String, _>("status"),
                "created_at": r.get::<String, _>("created_at"),
            })
        })
        .collect();

    Json(serde_json::json!({ "reports": reports }))
}

pub async fn action_report(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(report_id): Path<i64>,
) -> impl IntoResponse {
    let result = sqlx::query(
        "UPDATE reports SET status = 'actioned', resolved_by = ?, resolved_at = datetime('now') WHERE id = ? AND status = 'open'",
    )
    .bind(admin.id)
    .bind(report_id)
    .execute(&state.db)
    .await;

    match result {
        Ok(r) if r.rows_affected() > 0 => {
            audit::log(
                &state.db,
                &admin.username,
                "action_report",
                Some("report"),
                Some(&report_id.to_string()),
                None,
            )
            .await;

            Json(serde_json::json!({"ok": true})).into_response()
        }
        _ => (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Report not found or already resolved"})),
        )
            .into_response(),
    }
}

pub async fn dismiss_report(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(report_id): Path<i64>,
) -> impl IntoResponse {
    let result = sqlx::query(
        "UPDATE reports SET status = 'dismissed', resolved_by = ?, resolved_at = datetime('now') WHERE id = ? AND status = 'open'",
    )
    .bind(admin.id)
    .bind(report_id)
    .execute(&state.db)
    .await;

    match result {
        Ok(r) if r.rows_affected() > 0 => {
            audit::log(
                &state.db,
                &admin.username,
                "dismiss_report",
                Some("report"),
                Some(&report_id.to_string()),
                None,
            )
            .await;

            Json(serde_json::json!({"ok": true})).into_response()
        }
        _ => (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Report not found or already resolved"})),
        )
            .into_response(),
    }
}

// ── Report Submission (non-admin) ──

use crate::auth::AuthUser;

#[derive(Deserialize)]
pub struct SubmitReportRequest {
    pub target_type: String,
    pub target_name: String,
    pub report_type: String,
    pub reason: String,
}

pub async fn submit_report(
    State(state): State<Arc<AppState>>,
    AuthUser(user): AuthUser,
    Json(body): Json<SubmitReportRequest>,
) -> impl IntoResponse {
    // Validate target_type
    if !matches!(body.target_type.as_str(), "package" | "user") {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "target_type must be 'package' or 'user'"})),
        )
            .into_response();
    }

    // Validate report_type
    if !matches!(body.report_type.as_str(), "spam" | "malware" | "abuse" | "other") {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "report_type must be 'spam', 'malware', 'abuse', or 'other'"})),
        )
            .into_response();
    }

    // Validate lengths
    if body.target_name.is_empty() || body.target_name.len() > 200 {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "target_name must be 1-200 characters"})),
        )
            .into_response();
    }

    if body.reason.is_empty() || body.reason.len() > 2000 {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "reason must be 1-2000 characters"})),
        )
            .into_response();
    }

    let result = sqlx::query(
        "INSERT INTO reports (reporter_id, target_type, target_name, report_type, reason) VALUES (?, ?, ?, ?, ?)",
    )
    .bind(user.id)
    .bind(&body.target_type)
    .bind(&body.target_name)
    .bind(&body.report_type)
    .bind(&body.reason)
    .execute(&state.db)
    .await;

    match result {
        Ok(_) => (
            StatusCode::CREATED,
            Json(serde_json::json!({"ok": true})),
        )
            .into_response(),
        Err(_) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({"error": "Failed to submit report"})),
        )
            .into_response(),
    }
}
