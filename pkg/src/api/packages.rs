use axum::{
    body::Body,
    extract::{Multipart, Path, Query, State},
    http::{header, StatusCode},
    response::{IntoResponse, Redirect},
    Json,
};
use sea_orm::*;
use sea_orm::sea_query::{Expr, OnConflict};
use serde::Deserialize;
use std::sync::Arc;

use crate::{
    auth::TokenUser,
    blob,
    entity::{dependency, owner, package, package_version, user},
    AppState,
};

// ── Publish ──

#[derive(Deserialize, Default)]
struct PublishMetadata {
    #[serde(default)]
    description: String,
    #[serde(default)]
    repository_url: Option<String>,
    #[serde(default)]
    sema_version_req: Option<String>,
    #[serde(default)]
    dependencies: Vec<DepEntry>,
}

#[derive(Deserialize)]
struct DepEntry {
    name: String,
    version_req: String,
}

pub async fn publish(
    State(state): State<Arc<AppState>>,
    TokenUser { user, scopes }: TokenUser,
    Path((name, version)): Path<(String, String)>,
    mut multipart: Multipart,
) -> impl IntoResponse {
    if !scopes.contains("publish") {
        return (
            StatusCode::FORBIDDEN,
            Json(serde_json::json!({"error": "Token lacks publish scope"})),
        )
            .into_response();
    }

    let ver = match semver::Version::parse(&version) {
        Ok(v) => v,
        Err(_) => {
            return (
                StatusCode::BAD_REQUEST,
                Json(serde_json::json!({"error": "Invalid semver version"})),
            )
                .into_response();
        }
    };

    let mut tarball_data: Option<Vec<u8>> = None;
    let mut metadata = PublishMetadata::default();

    while let Ok(Some(field)) = multipart.next_field().await {
        let field_name = field.name().unwrap_or("").to_string();
        match field_name.as_str() {
            "tarball" => {
                let data = field.bytes().await.unwrap_or_default().to_vec();
                if data.len() > state.config.max_tarball_bytes {
                    return (
                        StatusCode::PAYLOAD_TOO_LARGE,
                        Json(serde_json::json!({"error": "Tarball too large"})),
                    )
                        .into_response();
                }
                tarball_data = Some(data);
            }
            "metadata" => {
                let text = field.text().await.unwrap_or_default();
                if let Ok(m) = serde_json::from_str::<PublishMetadata>(&text) {
                    metadata = m;
                }
            }
            _ => {}
        }
    }

    let tarball = match tarball_data {
        Some(d) if !d.is_empty() => d,
        _ => {
            return (
                StatusCode::BAD_REQUEST,
                Json(serde_json::json!({"error": "Missing tarball"})),
            )
                .into_response();
        }
    };

    // Find or create package
    let package_id: i64;
    let existing = package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    if let Some(pkg) = existing {
        package_id = pkg.id;

        if pkg.source == "github" {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({
                    "error": "This package is GitHub-linked and cannot be published via CLI. Push a new semver tag to the linked repository instead."
                })),
            )
                .into_response();
        }

        let is_owner = owner::Entity::find()
            .filter(owner::Column::PackageId.eq(package_id))
            .filter(owner::Column::UserId.eq(user.id))
            .count(&state.db)
            .await
            .unwrap_or(0);

        if is_owner == 0 {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "You are not an owner of this package"})),
            )
                .into_response();
        }
    } else {
        let txn = match state.db.begin().await {
            Ok(tx) => tx,
            Err(_) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(serde_json::json!({"error": "Failed to begin transaction"})),
                )
                    .into_response();
            }
        };

        let new_pkg = package::ActiveModel {
            name: Set(name.clone()),
            description: Set(metadata.description.clone()),
            repository_url: Set(metadata.repository_url.clone()),
            source: Set("upload".into()),
            ..Default::default()
        };

        match new_pkg.insert(&txn).await {
            Ok(pkg) => {
                package_id = pkg.id;
                let new_owner = owner::ActiveModel {
                    package_id: Set(package_id),
                    user_id: Set(user.id),
                };

                if new_owner.insert(&txn).await.is_err() {
                    return (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(serde_json::json!({"error": "Failed to create package owner"})),
                    )
                        .into_response();
                }
            }
            Err(_) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(serde_json::json!({"error": "Failed to create package"})),
                )
                    .into_response();
            }
        }

        if txn.commit().await.is_err() {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Failed to commit transaction"})),
            )
                .into_response();
        }
    }

    // Check version doesn't exist
    let version_str = ver.to_string();
    let exists = package_version::Entity::find()
        .filter(package_version::Column::PackageId.eq(package_id))
        .filter(package_version::Column::Version.eq(&version_str))
        .count(&state.db)
        .await
        .unwrap_or(0);

    if exists > 0 {
        return (
            StatusCode::CONFLICT,
            Json(serde_json::json!({"error": "Version already exists"})),
        )
            .into_response();
    }

    // Store blob
    let (blob_key, checksum, size) = blob::store(&state.config.blob_dir, &tarball).await;

    // Insert version
    let new_version = package_version::ActiveModel {
        package_id: Set(package_id),
        version: Set(version_str.clone()),
        checksum_sha256: Set(checksum.clone()),
        blob_key: Set(blob_key),
        size_bytes: Set(size as i64),
        sema_version_req: Set(metadata.sema_version_req.clone()),
        ..Default::default()
    };

    let version_model = match new_version.insert(&state.db).await {
        Ok(m) => m,
        Err(_) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Failed to insert version"})),
            )
                .into_response();
        }
    };

    let version_id = version_model.id;

    // Insert dependencies
    for dep in &metadata.dependencies {
        let new_dep = dependency::ActiveModel {
            version_id: Set(version_id),
            dependency_name: Set(dep.name.clone()),
            version_req: Set(dep.version_req.clone()),
            ..Default::default()
        };
        let _ = new_dep.insert(&state.db).await;
    }

    // Update package description if provided
    if !metadata.description.is_empty() {
        let _ = package::Entity::update_many()
            .col_expr(package::Column::Description, Expr::value(&metadata.description))
            .filter(package::Column::Id.eq(package_id))
            .exec(&state.db)
            .await;
    }

    crate::audit::log(&state.db, &user.username, "publish", Some("package"), Some(&name), Some(&version_str)).await;

    (
        StatusCode::CREATED,
        Json(serde_json::json!({
            "ok": true,
            "package": name,
            "version": version_str,
            "checksum": checksum,
            "size": size,
        })),
    )
        .into_response()
}

// ── Get Package ──

pub async fn get_package(
    State(state): State<Arc<AppState>>,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let pkg = match package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
        .await
    {
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
                "tarball_url": v.tarball_url,
                "published_at": v.published_at,
            })
        })
        .collect();

    // Owners via join query
    let owner_rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "SELECT u.username FROM users u JOIN owners o ON o.user_id = u.id WHERE o.package_id = $1",
            [pkg_id.into()],
        ))
        .await
        .unwrap_or_default();

    let owners: Vec<String> = owner_rows
        .iter()
        .filter_map(|r| r.try_get("", "username").ok())
        .collect();

    // Total downloads
    let dl_row = state
        .db
        .query_one(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "SELECT COALESCE(SUM(count), 0) as cnt FROM download_daily WHERE package_name = $1",
            [name.clone().into()],
        ))
        .await
        .ok()
        .flatten();

    let dl_count: i64 = dl_row
        .and_then(|r| r.try_get("", "cnt").ok())
        .unwrap_or(0);

    Json(serde_json::json!({
        "package": {
            "name": pkg.name,
            "description": pkg.description,
            "repository_url": pkg.repository_url,
            "created_at": pkg.created_at,
            "readme_html": pkg.readme_html,
        },
        "versions": version_list,
        "owners": owners,
        "total_downloads": dl_count,
    }))
    .into_response()
}

// ── Download ──

pub async fn download(
    State(state): State<Arc<AppState>>,
    Path((name, version)): Path<(String, String)>,
) -> impl IntoResponse {
    // Join query to find the version row
    let row = state
        .db
        .query_one(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            r#"SELECT pv.blob_key, pv.tarball_url FROM package_versions pv
               JOIN packages p ON p.id = pv.package_id
               WHERE p.name = $1 AND pv.version = $2 AND pv.yanked = 0"#,
            [name.clone().into(), version.clone().into()],
        ))
        .await
        .ok()
        .flatten();

    let row = match row {
        Some(r) => r,
        None => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Version not found"})),
            )
                .into_response();
        }
    };

    // Record download (UPSERT) — raw SQL needed for date('now') expression
    let _ = state
        .db
        .execute(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            "INSERT INTO download_daily (package_name, version, download_date, count) VALUES ($1, $2, date('now'), 1) ON CONFLICT(package_name, version, download_date) DO UPDATE SET count = count + 1",
            [name.clone().into(), version.clone().into()],
        ))
        .await;

    // GitHub-linked packages: redirect to upstream tarball
    let tarball_url: Option<String> = row.try_get("", "tarball_url").ok();
    if let Some(url) = tarball_url {
        if !url.is_empty() {
            return Redirect::temporary(&url).into_response();
        }
    }

    // Upload-sourced packages: serve blob from disk
    let blob_key: String = row.try_get("", "blob_key").unwrap_or_default();
    let data = match blob::read(&state.config.blob_dir, &blob_key).await {
        Some(d) => d,
        None => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Blob not found on disk"})),
            )
                .into_response();
        }
    };

    let filename = format!("{}-{}.tar.gz", name.replace('/', "-"), version);
    (
        [
            (header::CONTENT_TYPE, "application/gzip".to_string()),
            (
                header::CONTENT_DISPOSITION,
                format!("attachment; filename=\"{filename}\""),
            ),
        ],
        Body::from(data),
    )
        .into_response()
}

// ── Download Stats ──

pub async fn download_stats(
    State(state): State<Arc<AppState>>,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let backend = state.db.get_database_backend();

    // Total downloads
    let total_row = state
        .db
        .query_one(Statement::from_sql_and_values(
            backend,
            "SELECT COALESCE(SUM(count), 0) as cnt FROM download_daily WHERE package_name = $1",
            [name.clone().into()],
        ))
        .await
        .ok()
        .flatten();

    let total: i64 = total_row
        .and_then(|r| r.try_get("", "cnt").ok())
        .unwrap_or(0);

    // Daily counts (last 90 days)
    let daily_rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            backend,
            "SELECT download_date, SUM(count) as count FROM download_daily WHERE package_name = $1 AND download_date >= date('now', '-90 days') GROUP BY download_date ORDER BY download_date ASC",
            [name.clone().into()],
        ))
        .await
        .unwrap_or_default();

    let daily: Vec<serde_json::Value> = daily_rows
        .iter()
        .filter_map(|r| {
            let date: String = r.try_get("", "download_date").ok()?;
            let count: i64 = r.try_get("", "count").ok()?;
            Some(serde_json::json!({ "date": date, "count": count }))
        })
        .collect();

    // Per-version totals
    let version_rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            backend,
            "SELECT version, SUM(count) as total FROM download_daily WHERE package_name = $1 GROUP BY version ORDER BY total DESC",
            [name.clone().into()],
        ))
        .await
        .unwrap_or_default();

    let versions: serde_json::Map<String, serde_json::Value> = version_rows
        .iter()
        .filter_map(|r| {
            let version: String = r.try_get("", "version").ok()?;
            let total: i64 = r.try_get("", "total").ok()?;
            Some((version, serde_json::json!(total)))
        })
        .collect();

    Json(serde_json::json!({
        "package": name,
        "total": total,
        "daily": daily,
        "versions": versions,
    }))
}

// ── Search ──

#[derive(Deserialize)]
pub struct SearchQuery {
    pub q: Option<String>,
    pub page: Option<i64>,
    pub per_page: Option<i64>,
}

pub async fn search(
    State(state): State<Arc<AppState>>,
    Query(params): Query<SearchQuery>,
) -> impl IntoResponse {
    let q = params.q.unwrap_or_default();
    let per_page = params.per_page.unwrap_or(20).min(100);
    let page = params.page.unwrap_or(1).max(1);
    let pattern = format!("%{q}%");

    let backend = state.db.get_database_backend();

    // Search with LIKE on name and description
    let rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            backend,
            r#"SELECT name, description, created_at FROM packages
               WHERE name LIKE $1 OR description LIKE $2
               ORDER BY name
               LIMIT $3 OFFSET $4"#,
            [
                pattern.clone().into(),
                pattern.clone().into(),
                per_page.into(),
                ((page - 1) * per_page).into(),
            ],
        ))
        .await
        .unwrap_or_default();

    let packages: Vec<serde_json::Value> = rows
        .iter()
        .filter_map(|r| {
            let name: String = r.try_get("", "name").ok()?;
            let description: String = r.try_get("", "description").ok()?;
            let created_at: String = r.try_get("", "created_at").ok()?;
            Some(serde_json::json!({
                "name": name,
                "description": description,
                "created_at": created_at,
            }))
        })
        .collect();

    let total_row = state
        .db
        .query_one(Statement::from_sql_and_values(
            backend,
            "SELECT COUNT(*) as cnt FROM packages WHERE name LIKE $1 OR description LIKE $2",
            [pattern.clone().into(), pattern.into()],
        ))
        .await
        .ok()
        .flatten();

    let total: i64 = total_row
        .and_then(|r| r.try_get("", "cnt").ok())
        .unwrap_or(0);

    Json(serde_json::json!({
        "packages": packages,
        "total": total,
        "page": page,
        "per_page": per_page,
    }))
}

// ── Yank ──

pub async fn yank(
    State(state): State<Arc<AppState>>,
    TokenUser { user, .. }: TokenUser,
    Path((name, version)): Path<(String, String)>,
) -> impl IntoResponse {
    let backend = state.db.get_database_backend();

    // Check ownership via join
    let owner_row = state
        .db
        .query_one(Statement::from_sql_and_values(
            backend,
            r#"SELECT COUNT(*) as cnt FROM owners o
               JOIN packages p ON p.id = o.package_id
               WHERE p.name = $1 AND o.user_id = $2"#,
            [name.clone().into(), user.id.into()],
        ))
        .await
        .ok()
        .flatten();

    let is_owner: i64 = owner_row
        .and_then(|r| r.try_get("", "cnt").ok())
        .unwrap_or(0);

    if is_owner == 0 {
        return (
            StatusCode::FORBIDDEN,
            Json(serde_json::json!({"error": "Not an owner"})),
        )
            .into_response();
    }

    // Find the package to get its ID for the update
    let pkg = package::Entity::find()
        .filter(package::Column::Name.eq(&name))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let pkg = match pkg {
        Some(p) => p,
        None => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Version not found"})),
            )
                .into_response();
        }
    };

    let result = package_version::Entity::update_many()
        .col_expr(package_version::Column::Yanked, Expr::value(1))
        .filter(package_version::Column::PackageId.eq(pkg.id))
        .filter(package_version::Column::Version.eq(&version))
        .exec(&state.db)
        .await;

    match result {
        Ok(r) if r.rows_affected > 0 => {
            crate::audit::log(&state.db, &user.username, "yank", Some("version"), Some(&name), Some(&version)).await;
            Json(serde_json::json!({"ok": true})).into_response()
        }
        _ => (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Version not found"})),
        )
            .into_response(),
    }
}

// ── Ownership ──

pub async fn list_owners(
    State(state): State<Arc<AppState>>,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let rows = state
        .db
        .query_all(Statement::from_sql_and_values(
            state.db.get_database_backend(),
            r#"SELECT u.username FROM users u
               JOIN owners o ON o.user_id = u.id
               JOIN packages p ON p.id = o.package_id
               WHERE p.name = $1"#,
            [name.into()],
        ))
        .await
        .unwrap_or_default();

    let owners: Vec<String> = rows
        .iter()
        .filter_map(|r| r.try_get("", "username").ok())
        .collect();

    Json(serde_json::json!({"owners": owners}))
}

#[derive(Deserialize)]
pub struct OwnerRequest {
    pub username: String,
}

pub async fn add_owner(
    State(state): State<Arc<AppState>>,
    TokenUser { user, .. }: TokenUser,
    Path(name): Path<String>,
    Json(body): Json<OwnerRequest>,
) -> impl IntoResponse {
    let backend = state.db.get_database_backend();

    // Check caller is an owner
    let pkg_row = state
        .db
        .query_one(Statement::from_sql_and_values(
            backend,
            r#"SELECT p.id FROM packages p
               JOIN owners o ON o.package_id = p.id
               WHERE p.name = $1 AND o.user_id = $2"#,
            [name.clone().into(), user.id.into()],
        ))
        .await
        .ok()
        .flatten();

    let pkg_id: i64 = match pkg_row.and_then(|r| r.try_get("", "id").ok()) {
        Some(id) => id,
        None => {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "Not an owner or package not found"})),
            )
                .into_response();
        }
    };

    // Find the target user
    let new_owner = user::Entity::find()
        .filter(user::Column::Username.eq(&body.username))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    let new_owner_id = match new_owner {
        Some(u) => u.id,
        None => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    // INSERT OR IGNORE
    let new_owner_model = owner::ActiveModel {
        package_id: Set(pkg_id),
        user_id: Set(new_owner_id),
    };
    let _ = owner::Entity::insert(new_owner_model)
        .on_conflict(
            OnConflict::columns([owner::Column::PackageId, owner::Column::UserId])
                .do_nothing()
                .to_owned(),
        )
        .do_nothing()
        .exec(&state.db)
        .await;

    crate::audit::log(&state.db, &user.username, "add_owner", Some("package"), Some(&name), Some(&body.username)).await;

    Json(serde_json::json!({"ok": true})).into_response()
}

pub async fn remove_owner(
    State(state): State<Arc<AppState>>,
    TokenUser { user, .. }: TokenUser,
    Path(name): Path<String>,
    Json(body): Json<OwnerRequest>,
) -> impl IntoResponse {
    let backend = state.db.get_database_backend();

    // Check caller is an owner
    let pkg_row = state
        .db
        .query_one(Statement::from_sql_and_values(
            backend,
            r#"SELECT p.id FROM packages p
               JOIN owners o ON o.package_id = p.id
               WHERE p.name = $1 AND o.user_id = $2"#,
            [name.clone().into(), user.id.into()],
        ))
        .await
        .ok()
        .flatten();

    let pkg_id: i64 = match pkg_row.and_then(|r| r.try_get("", "id").ok()) {
        Some(id) => id,
        None => {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "Not an owner or package not found"})),
            )
                .into_response();
        }
    };

    // Check owner count
    let owner_count = owner::Entity::find()
        .filter(owner::Column::PackageId.eq(pkg_id))
        .count(&state.db)
        .await
        .unwrap_or(0);

    if owner_count <= 1 {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "Cannot remove the last owner"})),
        )
            .into_response();
    }

    // Find target user and delete ownership
    let target = user::Entity::find()
        .filter(user::Column::Username.eq(&body.username))
        .one(&state.db)
        .await
        .ok()
        .flatten();

    if let Some(target_user) = target {
        let _ = owner::Entity::delete_many()
            .filter(owner::Column::PackageId.eq(pkg_id))
            .filter(owner::Column::UserId.eq(target_user.id))
            .exec(&state.db)
            .await;
    }

    crate::audit::log(&state.db, &user.username, "remove_owner", Some("package"), Some(&name), Some(&body.username)).await;

    Json(serde_json::json!({"ok": true})).into_response()
}
