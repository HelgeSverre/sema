use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};
use axum::{
    extract::FromRequestParts,
    http::{header, request::Parts, StatusCode},
};
use rand::RngCore;
use sqlx::Row;
use std::sync::Arc;

use crate::{db::Db, AppState};

#[derive(Debug, Clone)]
pub struct User {
    pub id: i64,
    pub username: String,
    pub email: String,
}

pub fn hash_password(password: &str) -> String {
    let salt = SaltString::generate(&mut OsRng);
    Argon2::default()
        .hash_password(password.as_bytes(), &salt)
        .expect("Failed to hash password")
        .to_string()
}

pub fn verify_password(password: &str, hash: &str) -> bool {
    let parsed = match PasswordHash::new(hash) {
        Ok(h) => h,
        Err(_) => return false,
    };
    Argon2::default()
        .verify_password(password.as_bytes(), &parsed)
        .is_ok()
}

pub fn generate_session_id() -> String {
    let mut bytes = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    base64::Engine::encode(&base64::engine::general_purpose::URL_SAFE_NO_PAD, bytes)
}

pub fn generate_token() -> String {
    let mut bytes = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    let encoded = base64::Engine::encode(&base64::engine::general_purpose::URL_SAFE_NO_PAD, bytes);
    format!("sema_pat_{encoded}")
}

pub fn hash_token(token: &str) -> String {
    use sha2::{Digest, Sha256};
    let hash = Sha256::digest(token.as_bytes());
    hex::encode(hash)
}

pub async fn create_session(db: &Db, user_id: i64) -> String {
    let session_id = generate_session_id();
    // Note: 7 days must match session cookie Max-Age=604800 in api/auth.rs and github.rs
    sqlx::query(
        "INSERT INTO sessions (id, user_id, expires_at) VALUES (?, ?, datetime('now', '+7 days'))",
    )
    .bind(&session_id)
    .bind(user_id)
    .execute(db)
    .await
    .expect("Failed to create session");
    session_id
}

pub async fn get_session_user(db: &Db, session_id: &str) -> Option<User> {
    let row = sqlx::query(
        r#"SELECT u.id, u.username, u.email
           FROM users u
           JOIN sessions s ON s.user_id = u.id
           WHERE s.id = ? AND s.expires_at > datetime('now')"#,
    )
    .bind(session_id)
    .fetch_optional(db)
    .await
    .ok()??;

    Some(User {
        id: row.get("id"),
        username: row.get("username"),
        email: row.get("email"),
    })
}

/// Extractor: reads session cookie, resolves to User
pub struct AuthUser(pub User);

impl FromRequestParts<Arc<AppState>> for AuthUser {
    type Rejection = StatusCode;

    async fn from_request_parts(
        parts: &mut Parts,
        state: &Arc<AppState>,
    ) -> Result<Self, Self::Rejection> {
        let cookie_header = parts
            .headers
            .get(header::COOKIE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("");

        let session_id = cookie_header
            .split(';')
            .filter_map(|c| {
                let c = c.trim();
                c.strip_prefix("session=")
            })
            .next()
            .ok_or(StatusCode::UNAUTHORIZED)?;

        let user = get_session_user(&state.db, session_id)
            .await
            .ok_or(StatusCode::UNAUTHORIZED)?;

        Ok(AuthUser(user))
    }
}

/// Extractor: reads Bearer token, resolves to User + scopes
pub struct TokenUser {
    pub user: User,
    pub scopes: String,
}

impl FromRequestParts<Arc<AppState>> for TokenUser {
    type Rejection = StatusCode;

    async fn from_request_parts(
        parts: &mut Parts,
        state: &Arc<AppState>,
    ) -> Result<Self, Self::Rejection> {
        let auth_header = parts
            .headers
            .get(header::AUTHORIZATION)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("");

        let token = auth_header
            .strip_prefix("Bearer ")
            .ok_or(StatusCode::UNAUTHORIZED)?;

        let token_hash = hash_token(token);

        let row = sqlx::query(
            "SELECT id, user_id, scopes FROM api_tokens WHERE token_hash = ? AND revoked_at IS NULL",
        )
        .bind(&token_hash)
        .fetch_optional(&state.db)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
        .ok_or(StatusCode::UNAUTHORIZED)?;

        let token_id: i64 = row.get("id");
        let user_id: i64 = row.get("user_id");
        let scopes: String = row.get("scopes");

        // Update last_used_at
        let _ = sqlx::query("UPDATE api_tokens SET last_used_at = datetime('now') WHERE id = ?")
            .bind(token_id)
            .execute(&state.db)
            .await;

        let user_row = sqlx::query("SELECT id, username, email FROM users WHERE id = ?")
            .bind(user_id)
            .fetch_one(&state.db)
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

        Ok(TokenUser {
            user: User {
                id: user_row.get("id"),
                username: user_row.get("username"),
                email: user_row.get("email"),
            },
            scopes,
        })
    }
}

pub fn validate_username(username: &str) -> Result<(), &'static str> {
    if username.len() < 2 || username.len() > 39 {
        return Err("Username must be 2-39 characters");
    }
    if username.starts_with('-') || username.ends_with('-') {
        return Err("Username cannot start or end with a hyphen");
    }
    if !username
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-')
    {
        return Err("Username can only contain letters, numbers, and hyphens");
    }
    Ok(())
}

pub fn validate_password(password: &str) -> Result<(), &'static str> {
    if password.len() < 8 {
        return Err("Password must be at least 8 characters");
    }
    Ok(())
}

pub fn validate_email(email: &str) -> Result<(), &'static str> {
    if !email.contains('@') || email.len() < 3 {
        return Err("Invalid email address");
    }
    Ok(())
}
