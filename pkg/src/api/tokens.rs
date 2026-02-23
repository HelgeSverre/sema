use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use sqlx::Row;
use std::sync::Arc;

use crate::{
    auth::{generate_token, hash_token, AuthUser},
    AppState,
};

#[derive(Deserialize)]
pub struct CreateTokenRequest {
    pub name: String,
}

pub async fn create(
    State(state): State<Arc<AppState>>,
    AuthUser(user): AuthUser,
    Json(body): Json<CreateTokenRequest>,
) -> impl IntoResponse {
    let token = generate_token();
    let token_hash = hash_token(&token);

    let result = sqlx::query(
        "INSERT INTO api_tokens (user_id, name, token_hash) VALUES (?, ?, ?)",
    )
    .bind(user.id)
    .bind(&body.name)
    .bind(&token_hash)
    .execute(&state.db)
    .await;

    match result {
        Ok(r) => (
            StatusCode::CREATED,
            Json(serde_json::json!({
                "token": token,
                "id": r.last_insert_rowid(),
                "name": body.name,
            })),
        )
            .into_response(),
        Err(_) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({"error": "Failed to create token"})),
        )
            .into_response(),
    }
}

pub async fn list(
    State(state): State<Arc<AppState>>,
    AuthUser(user): AuthUser,
) -> impl IntoResponse {
    let rows = sqlx::query(
        r#"SELECT id, name, scopes, created_at, last_used_at
           FROM api_tokens
           WHERE user_id = ? AND revoked_at IS NULL
           ORDER BY created_at DESC"#,
    )
    .bind(user.id)
    .fetch_all(&state.db)
    .await
    .unwrap_or_default();

    let tokens: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "id": r.get::<i64, _>("id"),
                "name": r.get::<String, _>("name"),
                "scopes": r.get::<String, _>("scopes"),
                "created_at": r.get::<String, _>("created_at"),
                "last_used_at": r.get::<Option<String>, _>("last_used_at"),
            })
        })
        .collect();

    Json(serde_json::json!({"tokens": tokens}))
}

pub async fn revoke(
    State(state): State<Arc<AppState>>,
    AuthUser(user): AuthUser,
    Path(token_id): Path<i64>,
) -> impl IntoResponse {
    let result = sqlx::query(
        "UPDATE api_tokens SET revoked_at = datetime('now') WHERE id = ? AND user_id = ? AND revoked_at IS NULL",
    )
    .bind(token_id)
    .bind(user.id)
    .execute(&state.db)
    .await;

    match result {
        Ok(r) if r.rows_affected() > 0 => {
            (StatusCode::OK, Json(serde_json::json!({"ok": true}))).into_response()
        }
        _ => (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Token not found"})),
        )
            .into_response(),
    }
}
