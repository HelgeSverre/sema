use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use sea_orm::*;
use sea_orm::prelude::Expr;
use serde::Deserialize;
use std::sync::Arc;

use crate::{
    auth::{generate_token, hash_token, AuthUser},
    entity::api_token,
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

    let new_token = api_token::ActiveModel {
        user_id: Set(user.id),
        name: Set(body.name.clone()),
        token_hash: Set(token_hash),
        ..Default::default()
    };

    let result = new_token.insert(&state.db).await;

    match result {
        Ok(model) => (
            StatusCode::CREATED,
            Json(serde_json::json!({
                "token": token,
                "id": model.id,
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
    let rows = api_token::Entity::find()
        .filter(api_token::Column::UserId.eq(user.id))
        .filter(api_token::Column::RevokedAt.is_null())
        .order_by_desc(api_token::Column::CreatedAt)
        .all(&state.db)
        .await
        .unwrap_or_default();

    let tokens: Vec<serde_json::Value> = rows
        .iter()
        .map(|r| {
            serde_json::json!({
                "id": r.id,
                "name": r.name,
                "scopes": r.scopes,
                "created_at": r.created_at,
                "last_used_at": r.last_used_at,
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
    let result = api_token::Entity::update_many()
        .col_expr(api_token::Column::RevokedAt, Expr::cust("datetime('now')"))
        .filter(api_token::Column::Id.eq(token_id))
        .filter(api_token::Column::UserId.eq(user.id))
        .filter(api_token::Column::RevokedAt.is_null())
        .exec(&state.db)
        .await;

    match result {
        Ok(r) if r.rows_affected > 0 => {
            (StatusCode::OK, Json(serde_json::json!({"ok": true}))).into_response()
        }
        _ => (
            StatusCode::NOT_FOUND,
            Json(serde_json::json!({"error": "Token not found"})),
        )
            .into_response(),
    }
}
