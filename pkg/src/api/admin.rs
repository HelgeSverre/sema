use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use sea_orm::sea_query::Expr;
use sea_orm::*;
use serde::Deserialize;
use std::sync::Arc;

use crate::entity::{
    api_token, owner, package, package_version, report, session, user,
};
use crate::{audit, auth::AdminUser, AppState};

// ── Dashboard ──

pub async fn stats(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
) -> impl IntoResponse {
    let total_users = user::Entity::find()
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let total_packages = package::Entity::find()
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let banned_users = user::Entity::find()
        .filter(user::Column::BannedAt.is_not_null())
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let open_reports = report::Entity::find()
        .filter(report::Column::Status.eq("open"))
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let total_downloads: i64 = {
        let result = state
            .db
            .query_one(Statement::from_sql_and_values(
                state.db.get_database_backend(),
                r#"SELECT COALESCE(SUM(count), 0) as cnt FROM download_daily WHERE download_date >= date('now', '-30 days')"#,
                [],
            ))
            .await;
        match result {
            Ok(Some(row)) => row.try_get_by_index::<i64>(0).unwrap_or(0),
            _ => 0,
        }
    };

    Json(serde_json::json!({
        "total_users": total_users,
        "total_packages": total_packages,
        "banned_users": banned_users,
        "open_reports": open_reports,
        "total_downloads": total_downloads,
    }))
}

// ── Users ──

#[derive(Deserialize)]
pub struct UserListParams {
    pub q: Option<String>,
    pub status: Option<String>,
    pub page: Option<i64>,
    pub per_page: Option<i64>,
}

pub async fn list_users(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<UserListParams>,
) -> impl IntoResponse {
    let per_page = params.per_page.unwrap_or(50).min(200);
    let page = params.page.unwrap_or(1).max(1);
    let offset = (page - 1) * per_page;

    let mut where_clauses: Vec<String> = vec!["1=1".to_string()];
    let mut binds: Vec<Value> = Vec::new();

    if let Some(ref q) = params.q {
        let pattern = format!("%{q}%");
        where_clauses.push("(u.username LIKE ? OR u.email LIKE ?)".to_string());
        binds.push(pattern.clone().into());
        binds.push(pattern.into());
    }

    match params.status.as_deref() {
        Some("banned") => where_clauses.push("u.banned_at IS NOT NULL".to_string()),
        Some("active") => where_clauses.push("u.banned_at IS NULL".to_string()),
        Some("github") => where_clauses.push("u.github_id IS NOT NULL".to_string()),
        _ => {}
    }

    let where_sql = where_clauses.join(" AND ");

    // Get total count
    let count_sql = format!("SELECT COUNT(*) as cnt FROM users u WHERE {where_sql}");
    let count_result = state
        .db
        .query_one(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            &count_sql,
            binds.clone(),
        ))
        .await;
    let total: i64 = match count_result {
        Ok(Some(row)) => row.try_get_by_index::<i64>(0).unwrap_or(0),
        _ => 0,
    };

    let sql = format!(
        r#"SELECT u.id, u.username, u.email, u.is_admin, u.github_id,
              oc.provider_login,
              (SELECT COUNT(*) FROM owners WHERE owners.user_id = u.id) as package_count,
              (SELECT COUNT(*) FROM api_tokens WHERE api_tokens.user_id = u.id AND api_tokens.revoked_at IS NULL) as token_count,
              u.banned_at, u.created_at
           FROM users u
           LEFT JOIN oauth_connections oc ON oc.user_id = u.id AND oc.provider = 'github' AND oc.revoked_at IS NULL
           WHERE {where_sql}
           ORDER BY u.created_at DESC
           LIMIT ? OFFSET ?"#
    );

    let mut all_binds = binds;
    all_binds.push(per_page.into());
    all_binds.push(offset.into());

    let rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            &sql,
            all_binds,
        ))
        .await
        .unwrap_or_default();

    let users: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            let banned_at: Option<String> = r.try_get_by("banned_at").unwrap_or(None);
            let github_id: Option<i64> = r.try_get_by("github_id").unwrap_or(None);
            serde_json::json!({
                "id": r.try_get_by::<i64, _>("id").unwrap_or(0),
                "username": r.try_get_by::<String, _>("username").unwrap_or_default(),
                "email": r.try_get_by::<String, _>("email").unwrap_or_default(),
                "is_admin": r.try_get_by::<i32, _>("is_admin").unwrap_or(0) != 0,
                "github_id": github_id,
                "github_login": r.try_get_by::<Option<String>, _>("provider_login").unwrap_or(None),
                "package_count": r.try_get_by::<i64, _>("package_count").unwrap_or(0),
                "token_count": r.try_get_by::<i64, _>("token_count").unwrap_or(0),
                "banned": banned_at.is_some(),
                "created_at": r.try_get_by::<String, _>("created_at").unwrap_or_default(),
            })
        })
        .collect();

    Json(serde_json::json!({
        "users": users,
        "total": total,
        "page": page,
        "per_page": per_page,
    }))
}

pub async fn get_user(
    State(state): State<Arc<AppState>>,
    AdminUser(_admin): AdminUser,
    Path(user_id): Path<i64>,
) -> impl IntoResponse {
    let user_model = user::Entity::find_by_id(user_id)
        .one(&state.db)
        .await;

    let user_model = match user_model {
        Ok(Some(u)) => u,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    // Get package names via join query
    let packages = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            r#"SELECT p.name FROM packages p
               JOIN owners o ON o.package_id = p.id
               WHERE o.user_id = ?
               ORDER BY p.name"#,
            [user_id.into()],
        ))
        .await
        .unwrap_or_default();

    let package_names: Vec<String> = packages
        .iter()
        .map(|r| r.try_get_by::<String, _>("name").unwrap_or_default())
        .collect();

    let token_count = api_token::Entity::find()
        .filter(api_token::Column::UserId.eq(user_id))
        .filter(api_token::Column::RevokedAt.is_null())
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let pkg_count = owner::Entity::find()
        .filter(owner::Column::UserId.eq(user_id))
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    Json(serde_json::json!({
        "user": {
            "id": user_model.id,
            "username": user_model.username,
            "email": user_model.email,
            "is_admin": user_model.is_admin != 0,
            "github_id": user_model.github_id,
            "banned": user_model.banned_at.is_some(),
            "created_at": user_model.created_at,
        },
        "packages": package_names,
        "package_count": pkg_count,
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
    let user_model = user::Entity::find_by_id(user_id)
        .one(&state.db)
        .await;

    let username = match user_model {
        Ok(Some(u)) => u.username,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    // Ban the user
    let _ = user::Entity::update_many()
        .col_expr(user::Column::BannedAt, Expr::cust("datetime('now')"))
        .filter(user::Column::Id.eq(user_id))
        .exec(&state.db)
        .await;

    // Revoke all active tokens
    let _ = api_token::Entity::update_many()
        .col_expr(
            api_token::Column::RevokedAt,
            Expr::cust("datetime('now')"),
        )
        .filter(api_token::Column::UserId.eq(user_id))
        .filter(api_token::Column::RevokedAt.is_null())
        .exec(&state.db)
        .await;

    // Delete all sessions
    let _ = session::Entity::delete_many()
        .filter(session::Column::UserId.eq(user_id))
        .exec(&state.db)
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
    let user_model = user::Entity::find_by_id(user_id)
        .one(&state.db)
        .await;

    let username = match user_model {
        Ok(Some(u)) => u.username,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let _ = user::Entity::update_many()
        .col_expr(user::Column::BannedAt, Expr::value(Value::String(None)))
        .filter(user::Column::Id.eq(user_id))
        .exec(&state.db)
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
    let user_model = user::Entity::find_by_id(user_id)
        .one(&state.db)
        .await;

    let username = match user_model {
        Ok(Some(u)) => u.username,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let result = api_token::Entity::update_many()
        .col_expr(
            api_token::Column::RevokedAt,
            Expr::cust("datetime('now')"),
        )
        .filter(api_token::Column::UserId.eq(user_id))
        .filter(api_token::Column::RevokedAt.is_null())
        .exec(&state.db)
        .await;

    let count = result.map(|r| r.rows_affected).unwrap_or(0);

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

    let user_model = user::Entity::find_by_id(user_id)
        .one(&state.db)
        .await;

    let username = match user_model {
        Ok(Some(u)) => u.username,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let admin_val: i32 = if body.is_admin { 1 } else { 0 };
    let _ = user::Entity::update_many()
        .col_expr(user::Column::IsAdmin, Expr::value(admin_val))
        .filter(user::Column::Id.eq(user_id))
        .exec(&state.db)
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
    pub page: Option<i64>,
    pub per_page: Option<i64>,
}

pub async fn list_packages(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<PkgListParams>,
) -> impl IntoResponse {
    let per_page = params.per_page.unwrap_or(50).min(200);
    let page = params.page.unwrap_or(1).max(1);
    let offset = (page - 1) * per_page;

    let mut where_clauses: Vec<String> = vec!["1=1".to_string()];
    let mut binds: Vec<Value> = Vec::new();

    if let Some(ref q) = params.q {
        let pattern = format!("%{q}%");
        where_clauses.push("p.name LIKE ?".to_string());
        binds.push(pattern.into());
    }

    if let Some(ref source) = params.source {
        where_clauses.push("p.source = ?".to_string());
        binds.push(source.clone().into());
    }

    if params.reported == Some(true) {
        where_clauses.push(
            "EXISTS (SELECT 1 FROM reports r WHERE r.target_type = 'package' AND r.target_name = p.name AND r.status = 'open')"
                .to_string(),
        );
    }

    let where_sql = where_clauses.join(" AND ");

    // Get total count
    let count_sql = format!("SELECT COUNT(*) as cnt FROM packages p WHERE {where_sql}");
    let count_result = state
        .db
        .query_one(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            &count_sql,
            binds.clone(),
        ))
        .await;
    let total: i64 = match count_result {
        Ok(Some(row)) => row.try_get_by_index::<i64>(0).unwrap_or(0),
        _ => 0,
    };

    let sql = format!(
        r#"SELECT p.name, p.description, p.source, p.created_at,
              (SELECT pv.version FROM package_versions pv WHERE pv.package_id = p.id ORDER BY pv.published_at DESC LIMIT 1) as latest_version,
              (SELECT COUNT(*) FROM package_versions pv WHERE pv.package_id = p.id) as version_count,
              (SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = p.id LIMIT 1) as owner,
              (SELECT COALESCE(SUM(count), 0) FROM download_daily dl WHERE dl.package_name = p.name) as downloads,
              EXISTS (SELECT 1 FROM reports r WHERE r.target_type = 'package' AND r.target_name = p.name AND r.status = 'open') as reported
           FROM packages p
           WHERE {where_sql}
           ORDER BY p.created_at DESC
           LIMIT ? OFFSET ?"#
    );

    let mut all_binds = binds;
    all_binds.push(per_page.into());
    all_binds.push(offset.into());

    let rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            &sql,
            all_binds,
        ))
        .await
        .unwrap_or_default();

    let packages: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "name": r.try_get_by::<String, _>("name").unwrap_or_default(),
                "description": r.try_get_by::<String, _>("description").unwrap_or_default(),
                "latest_version": r.try_get_by::<Option<String>, _>("latest_version").unwrap_or(None),
                "version_count": r.try_get_by::<i64, _>("version_count").unwrap_or(0),
                "source": r.try_get_by::<String, _>("source").unwrap_or_default(),
                "owner": r.try_get_by::<Option<String>, _>("owner").unwrap_or(None),
                "downloads": r.try_get_by::<i64, _>("downloads").unwrap_or(0),
                "reported": r.try_get_by::<i32, _>("reported").unwrap_or(0) != 0,
                "created_at": r.try_get_by::<String, _>("created_at").unwrap_or_default(),
            })
        })
        .collect();

    Json(serde_json::json!({
        "packages": packages,
        "total": total,
        "page": page,
        "per_page": per_page,
    }))
}

pub async fn get_package(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let pkg = package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
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

    let pkg_id = pkg.id;

    let versions = package_version::Entity::find()
        .filter(package_version::Column::PackageId.eq(pkg_id))
        .order_by_desc(package_version::Column::PublishedAt)
        .all(&state.db)
        .await
        .unwrap_or_default();

    let version_list: Vec<serde_json::Value> = versions
        .iter()
        .map(|v| {
            serde_json::json!({
                "version": v.version,
                "checksum_sha256": v.checksum_sha256,
                "size_bytes": v.size_bytes,
                "yanked": v.yanked != 0,
                "sema_version_req": v.sema_version_req,
                "published_at": v.published_at,
            })
        })
        .collect();

    // Get owners via join query
    let owner_rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = ?",
            [pkg_id.into()],
        ))
        .await
        .unwrap_or_default();

    let owners: Vec<String> = owner_rows
        .iter()
        .map(|r| r.try_get_by::<String, _>("username").unwrap_or_default())
        .collect();

    let open_reports = report::Entity::find()
        .filter(report::Column::TargetType.eq("package"))
        .filter(report::Column::TargetName.eq(&name))
        .filter(report::Column::Status.eq("open"))
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    let dl_count: i64 = {
        let result = state
            .db
            .query_one(Statement::from_sql_and_values(
                state.db.get_database_backend(),
                "SELECT COALESCE(SUM(count), 0) as cnt FROM download_daily WHERE package_name = ?",
                [name.clone().into()],
            ))
            .await;
        match result {
            Ok(Some(row)) => row.try_get_by_index::<i64>(0).unwrap_or(0),
            _ => 0,
        }
    };

    Json(serde_json::json!({
        "package": {
            "name": pkg.name,
            "description": pkg.description,
            "repository_url": pkg.repository_url,
            "source": pkg.source,
            "github_repo": pkg.github_repo,
            "created_at": pkg.created_at,
        },
        "versions": version_list,
        "owners": owners,
        "open_reports": open_reports,
        "total_downloads": dl_count,
    }))
    .into_response()
}

pub async fn yank_all_versions(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(name): Path<String>,
) -> impl IntoResponse {
    // Use raw SQL for the subquery-based update
    let result = state
        .db
        .execute(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "UPDATE package_versions SET yanked = 1 WHERE package_id = (SELECT id FROM packages WHERE name = ?)",
            [name.clone().into()],
        ))
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
    let pkg = package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
        .await;

    let pkg_id = match pkg {
        Ok(Some(p)) => p.id,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Package not found"})),
            )
                .into_response();
        }
    };

    // Delete dependencies via version_id join (raw SQL for subquery)
    let _ = state
        .db
        .execute(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "DELETE FROM dependencies WHERE version_id IN (SELECT id FROM package_versions WHERE package_id = ?)",
            [pkg_id.into()],
        ))
        .await;

    // Delete versions
    let _ = package_version::Entity::delete_many()
        .filter(package_version::Column::PackageId.eq(pkg_id))
        .exec(&state.db)
        .await;

    // Delete owners
    let _ = owner::Entity::delete_many()
        .filter(owner::Column::PackageId.eq(pkg_id))
        .exec(&state.db)
        .await;

    // Delete the package
    let _ = package::Entity::delete_by_id(pkg_id)
        .exec(&state.db)
        .await;

    // Clean up any reports targeting this package
    let _ = report::Entity::delete_many()
        .filter(report::Column::TargetType.eq("package"))
        .filter(report::Column::TargetName.eq(&name))
        .exec(&state.db)
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
    let pkg = package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
        .await;

    let pkg_id = match pkg {
        Ok(Some(p)) => p.id,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Package not found"})),
            )
                .into_response();
        }
    };

    let target_user = user::Entity::find()
        .filter(user::Column::Username.eq(&body.to_username))
        .one(&state.db)
        .await;

    let target_id = match target_user {
        Ok(Some(u)) => u.id,
        _ => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Target user not found"})),
            )
                .into_response();
        }
    };

    // Remove existing owners
    let _ = owner::Entity::delete_many()
        .filter(owner::Column::PackageId.eq(pkg_id))
        .exec(&state.db)
        .await;

    // Insert new owner
    let new_owner = owner::ActiveModel {
        package_id: Set(pkg_id),
        user_id: Set(target_id),
    };
    let _ = owner::Entity::insert(new_owner).exec(&state.db).await;

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
    let mut binds: Vec<Value> = Vec::new();

    if let Some(ref action) = params.action {
        where_clauses.push("action = ?".to_string());
        binds.push(action.clone().into());
    }

    if let Some(ref q) = params.q {
        let pattern = format!("%{q}%");
        where_clauses.push("(actor LIKE ? OR target_name LIKE ? OR detail LIKE ?)".to_string());
        binds.push(pattern.clone().into());
        binds.push(pattern.clone().into());
        binds.push(pattern.into());
    }

    let where_sql = where_clauses.join(" AND ");

    // Get total count
    let count_sql = format!("SELECT COUNT(*) as cnt FROM audit_log WHERE {where_sql}");
    let count_result = state
        .db
        .query_one(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            &count_sql,
            binds.clone(),
        ))
        .await;
    let total: i64 = match count_result {
        Ok(Some(row)) => row.try_get_by_index::<i64>(0).unwrap_or(0),
        _ => 0,
    };

    // Get entries
    let sql = format!(
        r#"SELECT id, actor, action, target_type, target_name, detail, created_at
           FROM audit_log
           WHERE {where_sql}
           ORDER BY created_at DESC
           LIMIT ? OFFSET ?"#
    );

    let mut all_binds = binds;
    all_binds.push(per_page.into());
    all_binds.push(offset.into());

    let rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            &sql,
            all_binds,
        ))
        .await
        .unwrap_or_default();

    let entries: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "id": r.try_get_by::<i64, _>("id").unwrap_or(0),
                "actor": r.try_get_by::<String, _>("actor").unwrap_or_default(),
                "action": r.try_get_by::<String, _>("action").unwrap_or_default(),
                "target_type": r.try_get_by::<Option<String>, _>("target_type").unwrap_or(None),
                "target_name": r.try_get_by::<Option<String>, _>("target_name").unwrap_or(None),
                "detail": r.try_get_by::<Option<String>, _>("detail").unwrap_or(None),
                "created_at": r.try_get_by::<String, _>("created_at").unwrap_or_default(),
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
    pub page: Option<i64>,
    pub per_page: Option<i64>,
}

pub async fn list_reports(
    State(state): State<Arc<AppState>>,
    AdminUser(_user): AdminUser,
    Query(params): Query<ReportListParams>,
) -> impl IntoResponse {
    let per_page = params.per_page.unwrap_or(50).min(200);
    let page = params.page.unwrap_or(1).max(1);
    let offset = (page - 1) * per_page;

    let status = params.status.unwrap_or_else(|| "open".to_string());

    // Get total count
    let total = report::Entity::find()
        .filter(report::Column::Status.eq(&status))
        .count(&state.db)
        .await
        .unwrap_or(0) as i64;

    // Use raw SQL for the LEFT JOIN with users
    let rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            r#"SELECT r.id, u.username as reporter, r.target_type, r.target_name,
                  r.report_type, r.reason, r.status, r.created_at
               FROM reports r
               LEFT JOIN users u ON u.id = r.reporter_id
               WHERE r.status = ?
               ORDER BY r.created_at DESC
               LIMIT ? OFFSET ?"#,
            [status.into(), per_page.into(), offset.into()],
        ))
        .await
        .unwrap_or_default();

    let reports: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "id": r.try_get_by::<i64, _>("id").unwrap_or(0),
                "reporter": r.try_get_by::<Option<String>, _>("reporter").unwrap_or(None).unwrap_or_else(|| "[deleted]".to_string()),
                "target_type": r.try_get_by::<String, _>("target_type").unwrap_or_default(),
                "target_name": r.try_get_by::<String, _>("target_name").unwrap_or_default(),
                "report_type": r.try_get_by::<String, _>("report_type").unwrap_or_default(),
                "reason": r.try_get_by::<String, _>("reason").unwrap_or_default(),
                "status": r.try_get_by::<String, _>("status").unwrap_or_default(),
                "created_at": r.try_get_by::<String, _>("created_at").unwrap_or_default(),
            })
        })
        .collect();

    Json(serde_json::json!({
        "reports": reports,
        "total": total,
        "page": page,
        "per_page": per_page,
    }))
}

pub async fn action_report(
    State(state): State<Arc<AppState>>,
    AdminUser(admin): AdminUser,
    Path(report_id): Path<i64>,
) -> impl IntoResponse {
    let result = state
        .db
        .execute(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "UPDATE reports SET status = 'actioned', resolved_by = ?, resolved_at = datetime('now') WHERE id = ? AND status = 'open'",
            [admin.id.into(), report_id.into()],
        ))
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
    let result = state
        .db
        .execute(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "UPDATE reports SET status = 'dismissed', resolved_by = ?, resolved_at = datetime('now') WHERE id = ? AND status = 'open'",
            [admin.id.into(), report_id.into()],
        ))
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

    let new_report = report::ActiveModel {
        id: NotSet,
        reporter_id: Set(user.id),
        target_type: Set(body.target_type),
        target_name: Set(body.target_name),
        report_type: Set(body.report_type),
        reason: Set(body.reason),
        status: Set("open".to_string()),
        resolved_by: Set(None),
        resolved_at: Set(None),
        created_at: NotSet,
    };

    let result = report::Entity::insert(new_report).exec(&state.db).await;

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
