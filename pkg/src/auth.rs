use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};
use axum::{
    extract::FromRequestParts,
    http::{header, request::Parts, StatusCode},
};
use rand::RngCore;
use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, Set};
use std::sync::Arc;
use time::{Duration, OffsetDateTime};

use crate::{
    db::Db,
    entity::{api_token, session, user},
    AppState,
};

#[derive(Debug, Clone)]
pub struct User {
    pub id: i64,
    pub username: String,
    pub email: String,
    pub is_admin: bool,
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
    let expires_at = {
        let t = OffsetDateTime::now_utc() + Duration::days(7);
        format!(
            "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
            t.year(), t.month() as u8, t.day(), t.hour(), t.minute(), t.second()
        )
    };

    let model = session::ActiveModel {
        id: Set(session_id.clone()),
        user_id: Set(user_id),
        expires_at: Set(expires_at),
        ..Default::default()
    };

    model.insert(db).await.expect("Failed to create session");
    session_id
}

pub async fn get_session_user(db: &Db, session_id: &str) -> Option<User> {
    // Find the session and its related user in one query
    let result = session::Entity::find_by_id(session_id.to_string())
        .find_also_related(user::Entity)
        .one(db)
        .await
        .ok()??;

    let (session_model, user_model) = result;
    let user_model = user_model?;

    // Check session expiry: compare expires_at against current time
    let t = OffsetDateTime::now_utc();
    let now = format!(
        "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
        t.year(), t.month() as u8, t.day(), t.hour(), t.minute(), t.second()
    );
    if session_model.expires_at <= now {
        return None;
    }

    // Check ban status
    if user_model.banned_at.is_some() {
        return None;
    }

    Some(User {
        id: user_model.id,
        username: user_model.username,
        email: user_model.email,
        is_admin: user_model.is_admin != 0,
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

        // Find the token by hash, excluding revoked tokens
        let token_model = api_token::Entity::find()
            .filter(api_token::Column::TokenHash.eq(&token_hash))
            .filter(api_token::Column::RevokedAt.is_null())
            .one(&state.db)
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
            .ok_or(StatusCode::UNAUTHORIZED)?;

        let scopes = token_model.scopes.clone();
        let user_id = token_model.user_id;

        // Update last_used_at
        let t = OffsetDateTime::now_utc();
        let now = format!(
            "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
            t.year(), t.month() as u8, t.day(), t.hour(), t.minute(), t.second()
        );
        let mut active_token: api_token::ActiveModel = token_model.into();
        active_token.last_used_at = Set(Some(now));
        let _ = active_token.update(&state.db).await;

        // Find the user, excluding banned users
        let user_model = user::Entity::find_by_id(user_id)
            .filter(user::Column::BannedAt.is_null())
            .one(&state.db)
            .await
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
            .ok_or(StatusCode::UNAUTHORIZED)?;

        Ok(TokenUser {
            user: User {
                id: user_model.id,
                username: user_model.username,
                email: user_model.email,
                is_admin: user_model.is_admin != 0,
            },
            scopes,
        })
    }
}

/// Extractor: requires session auth + is_admin = 1
pub struct AdminUser(pub User);

impl FromRequestParts<Arc<AppState>> for AdminUser {
    type Rejection = StatusCode;

    async fn from_request_parts(
        parts: &mut Parts,
        state: &Arc<AppState>,
    ) -> Result<Self, Self::Rejection> {
        let AuthUser(user) = AuthUser::from_request_parts(parts, state).await?;
        if !user.is_admin {
            return Err(StatusCode::FORBIDDEN);
        }
        Ok(AdminUser(user))
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
