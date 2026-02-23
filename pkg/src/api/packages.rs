use axum::{
    body::Body,
    extract::{Multipart, Path, Query, State},
    http::{header, StatusCode},
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use sqlx::Row;
use std::sync::Arc;

use crate::{auth::TokenUser, blob, AppState};

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
    let existing = sqlx::query("SELECT id, source FROM packages WHERE name = ?")
        .bind(&name)
        .fetch_optional(&state.db)
        .await
        .ok()
        .flatten();

    if let Some(row) = existing {
        package_id = row.get("id");

        let source: String = row.get("source");
        if source == "github" {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({
                    "error": "This package is GitHub-linked and cannot be published via CLI. Push a new semver tag to the linked repository instead."
                })),
            )
                .into_response();
        }

        let owner_row = sqlx::query(
            "SELECT COUNT(*) as cnt FROM owners WHERE package_id = ? AND user_id = ?",
        )
        .bind(package_id)
        .bind(user.id)
        .fetch_one(&state.db)
        .await
        .ok();

        let is_owner: i32 = owner_row.map(|r| r.get("cnt")).unwrap_or(0);
        if is_owner == 0 {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "You are not an owner of this package"})),
            )
                .into_response();
        }
    } else {
        let mut tx = match state.db.begin().await {
            Ok(tx) => tx,
            Err(_) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(serde_json::json!({"error": "Failed to begin transaction"})),
                )
                    .into_response();
            }
        };

        let r = sqlx::query(
            "INSERT INTO packages (name, description, repository_url, source) VALUES (?, ?, ?, 'upload')",
        )
        .bind(&name)
        .bind(&metadata.description)
        .bind(&metadata.repository_url)
        .execute(&mut *tx)
        .await;

        match r {
            Ok(r) => {
                package_id = r.last_insert_rowid();
                let owner_result = sqlx::query(
                    "INSERT INTO owners (package_id, user_id) VALUES (?, ?)",
                )
                .bind(package_id)
                .bind(user.id)
                .execute(&mut *tx)
                .await;

                if owner_result.is_err() {
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

        if let Err(_) = tx.commit().await {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Failed to commit transaction"})),
            )
                .into_response();
        }
    }

    // Check version doesn't exist
    let version_str = ver.to_string();
    let exists_row = sqlx::query(
        "SELECT COUNT(*) as cnt FROM package_versions WHERE package_id = ? AND version = ?",
    )
    .bind(package_id)
    .bind(&version_str)
    .fetch_one(&state.db)
    .await
    .ok();

    let exists: i32 = exists_row.map(|r| r.get("cnt")).unwrap_or(0);
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
    let vr = sqlx::query(
        "INSERT INTO package_versions (package_id, version, checksum_sha256, blob_key, size_bytes, sema_version_req) VALUES (?, ?, ?, ?, ?, ?)",
    )
    .bind(package_id)
    .bind(&version_str)
    .bind(&checksum)
    .bind(&blob_key)
    .bind(size as i64)
    .bind(&metadata.sema_version_req)
    .execute(&state.db)
    .await;

    let version_id = match vr {
        Ok(r) => r.last_insert_rowid(),
        Err(_) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Failed to insert version"})),
            )
                .into_response();
        }
    };

    // Insert dependencies
    for dep in &metadata.dependencies {
        let _ = sqlx::query(
            "INSERT INTO dependencies (version_id, dependency_name, version_req) VALUES (?, ?, ?)",
        )
        .bind(version_id)
        .bind(&dep.name)
        .bind(&dep.version_req)
        .execute(&state.db)
        .await;
    }

    // Update package description if provided
    if !metadata.description.is_empty() {
        let _ = sqlx::query("UPDATE packages SET description = ? WHERE id = ?")
            .bind(&metadata.description)
            .bind(package_id)
            .execute(&state.db)
            .await;
    }

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
    let pkg = sqlx::query(
        "SELECT id, name, description, repository_url, created_at FROM packages WHERE name = ?",
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

    Json(serde_json::json!({
        "package": {
            "name": pkg.get::<String, _>("name"),
            "description": pkg.get::<String, _>("description"),
            "repository_url": pkg.get::<Option<String>, _>("repository_url"),
            "created_at": pkg.get::<String, _>("created_at"),
        },
        "versions": version_list,
        "owners": owners,
    }))
    .into_response()
}

// ── Download ──

pub async fn download(
    State(state): State<Arc<AppState>>,
    Path((name, version)): Path<(String, String)>,
) -> impl IntoResponse {
    let row = sqlx::query(
        r#"SELECT pv.blob_key FROM package_versions pv
           JOIN packages p ON p.id = pv.package_id
           WHERE p.name = ? AND pv.version = ? AND pv.yanked = 0"#,
    )
    .bind(&name)
    .bind(&version)
    .fetch_optional(&state.db)
    .await
    .ok()
    .flatten();

    let blob_key: String = match row {
        Some(r) => r.get("blob_key"),
        None => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "Version not found"})),
            )
                .into_response();
        }
    };

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
    let offset = (page - 1) * per_page;
    let pattern = format!("%{q}%");

    let rows = sqlx::query(
        r#"SELECT name, description, created_at FROM packages
           WHERE name LIKE ? OR description LIKE ?
           ORDER BY name
           LIMIT ? OFFSET ?"#,
    )
    .bind(&pattern)
    .bind(&pattern)
    .bind(per_page)
    .bind(offset)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let packages: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "name": r.get::<String, _>("name"),
                "description": r.get::<String, _>("description"),
                "created_at": r.get::<String, _>("created_at"),
            })
        })
        .collect();

    let total_row = sqlx::query(
        "SELECT COUNT(*) as cnt FROM packages WHERE name LIKE ? OR description LIKE ?",
    )
    .bind(&pattern)
    .bind(&pattern)
    .fetch_one(&state.db)
    .await
    .ok();

    let total: i64 = total_row.map(|r| r.get("cnt")).unwrap_or(0);

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
    let owner_row = sqlx::query(
        r#"SELECT COUNT(*) as cnt FROM owners o
           JOIN packages p ON p.id = o.package_id
           WHERE p.name = ? AND o.user_id = ?"#,
    )
    .bind(&name)
    .bind(user.id)
    .fetch_one(&state.db)
    .await
    .ok();

    let is_owner: i32 = owner_row.map(|r| r.get("cnt")).unwrap_or(0);
    if is_owner == 0 {
        return (
            StatusCode::FORBIDDEN,
            Json(serde_json::json!({"error": "Not an owner"})),
        )
            .into_response();
    }

    let result = sqlx::query(
        r#"UPDATE package_versions SET yanked = 1
           WHERE package_id = (SELECT id FROM packages WHERE name = ?)
           AND version = ?"#,
    )
    .bind(&name)
    .bind(&version)
    .execute(&state.db)
    .await;

    match result {
        Ok(r) if r.rows_affected() > 0 => {
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
    let rows = sqlx::query(
        r#"SELECT u.username FROM users u
           JOIN owners o ON o.user_id = u.id
           JOIN packages p ON p.id = o.package_id
           WHERE p.name = ?"#,
    )
    .bind(&name)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let owners: Vec<String> = rows.iter().map(|r| r.get("username")).collect();
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
    let pkg_row = sqlx::query(
        r#"SELECT p.id FROM packages p
           JOIN owners o ON o.package_id = p.id
           WHERE p.name = ? AND o.user_id = ?"#,
    )
    .bind(&name)
    .bind(user.id)
    .fetch_optional(&state.db)
    .await
    .ok()
    .flatten();

    let pkg_id: i64 = match pkg_row {
        Some(r) => r.get("id"),
        None => {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "Not an owner or package not found"})),
            )
                .into_response();
        }
    };

    let new_owner_row = sqlx::query("SELECT id FROM users WHERE username = ?")
        .bind(&body.username)
        .fetch_optional(&state.db)
        .await
        .ok()
        .flatten();

    let new_owner_id: i64 = match new_owner_row {
        Some(r) => r.get("id"),
        None => {
            return (
                StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "User not found"})),
            )
                .into_response();
        }
    };

    let _ = sqlx::query("INSERT OR IGNORE INTO owners (package_id, user_id) VALUES (?, ?)")
        .bind(pkg_id)
        .bind(new_owner_id)
        .execute(&state.db)
        .await;

    Json(serde_json::json!({"ok": true})).into_response()
}

pub async fn remove_owner(
    State(state): State<Arc<AppState>>,
    TokenUser { user, .. }: TokenUser,
    Path(name): Path<String>,
    Json(body): Json<OwnerRequest>,
) -> impl IntoResponse {
    let pkg_row = sqlx::query(
        r#"SELECT p.id FROM packages p
           JOIN owners o ON o.package_id = p.id
           WHERE p.name = ? AND o.user_id = ?"#,
    )
    .bind(&name)
    .bind(user.id)
    .fetch_optional(&state.db)
    .await
    .ok()
    .flatten();

    let pkg_id: i64 = match pkg_row {
        Some(r) => r.get("id"),
        None => {
            return (
                StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "Not an owner or package not found"})),
            )
                .into_response();
        }
    };

    let owner_count_row = sqlx::query("SELECT COUNT(*) as cnt FROM owners WHERE package_id = ?")
        .bind(pkg_id)
        .fetch_one(&state.db)
        .await
        .ok();

    let owner_count: i32 = owner_count_row.map(|r| r.get("cnt")).unwrap_or(0);
    if owner_count <= 1 {
        return (
            StatusCode::BAD_REQUEST,
            Json(serde_json::json!({"error": "Cannot remove the last owner"})),
        )
            .into_response();
    }

    let target_row = sqlx::query("SELECT id FROM users WHERE username = ?")
        .bind(&body.username)
        .fetch_optional(&state.db)
        .await
        .ok()
        .flatten();

    if let Some(r) = target_row {
        let tid: i64 = r.get("id");
        let _ = sqlx::query("DELETE FROM owners WHERE package_id = ? AND user_id = ?")
            .bind(pkg_id)
            .bind(tid)
            .execute(&state.db)
            .await;
    }

    Json(serde_json::json!({"ok": true})).into_response()
}
