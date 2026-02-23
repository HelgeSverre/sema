use axum::{
    extract::State,
    http::{header, StatusCode},
    response::IntoResponse,
    Json,
};
use serde::Deserialize;
use sqlx::Row;
use std::sync::Arc;

use crate::{
    auth::{create_session, hash_password, validate_email, validate_password, validate_username, verify_password},
    AppState,
};

#[derive(Deserialize)]
pub struct RegisterRequest {
    pub username: String,
    pub email: String,
    pub password: String,
}

#[derive(Deserialize)]
pub struct LoginRequest {
    pub username: String,
    pub password: String,
}

fn session_cookie(session_id: &str) -> String {
    format!("session={session_id}; Path=/; HttpOnly; SameSite=Lax; Max-Age=604800")
}

fn clear_cookie() -> String {
    "session=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0".to_string()
}

pub async fn register(
    State(state): State<Arc<AppState>>,
    Json(body): Json<RegisterRequest>,
) -> impl IntoResponse {
    if let Err(e) = validate_username(&body.username) {
        return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": e}))).into_response();
    }
    if let Err(e) = validate_email(&body.email) {
        return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": e}))).into_response();
    }
    if let Err(e) = validate_password(&body.password) {
        return (StatusCode::BAD_REQUEST, Json(serde_json::json!({"error": e}))).into_response();
    }

    let username = body.username.to_lowercase();
    let email = body.email.to_lowercase();

    let password_hash = hash_password(&body.password);

    let result = sqlx::query(
        "INSERT INTO users (username, email, password_hash) VALUES (?, ?, ?)",
    )
    .bind(&username)
    .bind(&email)
    .bind(&password_hash)
    .execute(&state.db)
    .await;

    match result {
        Ok(r) => {
            let user_id = r.last_insert_rowid();
            let session_id = create_session(&state.db, user_id).await;
            (
                StatusCode::CREATED,
                [(header::SET_COOKIE, session_cookie(&session_id))],
                Json(serde_json::json!({"ok": true, "username": username})),
            )
                .into_response()
        }
        Err(_) => {
            (StatusCode::CONFLICT, Json(serde_json::json!({"error": "Registration failed"}))).into_response()
        }
    }
}

pub async fn login(
    State(state): State<Arc<AppState>>,
    Json(body): Json<LoginRequest>,
) -> impl IntoResponse {
    let login_input = body.username.to_lowercase();
    let row = sqlx::query(
        "SELECT id, username, password_hash FROM users WHERE username = ? OR email = ?",
    )
    .bind(&login_input)
    .bind(&login_input)
    .fetch_optional(&state.db)
    .await;

    let row = match row {
        Ok(Some(r)) => r,
        _ => {
            return (
                StatusCode::UNAUTHORIZED,
                Json(serde_json::json!({"error": "Invalid credentials"})),
            )
                .into_response();
        }
    };

    let id: i64 = row.get("id");
    let username: String = row.get("username");
    let password_hash: Option<String> = row.get("password_hash");

    let hash = match &password_hash {
        Some(h) => h,
        None => {
            return (
                StatusCode::UNAUTHORIZED,
                Json(serde_json::json!({"error": "Account uses GitHub login only"})),
            )
                .into_response();
        }
    };

    if !verify_password(&body.password, hash) {
        return (
            StatusCode::UNAUTHORIZED,
            Json(serde_json::json!({"error": "Invalid credentials"})),
        )
            .into_response();
    }

    let session_id = create_session(&state.db, id).await;
    (
        StatusCode::OK,
        [(header::SET_COOKIE, session_cookie(&session_id))],
        Json(serde_json::json!({"ok": true, "username": username})),
    )
        .into_response()
}

pub async fn logout() -> impl IntoResponse {
    (
        StatusCode::OK,
        [(header::SET_COOKIE, clear_cookie())],
        Json(serde_json::json!({"ok": true})),
    )
}
