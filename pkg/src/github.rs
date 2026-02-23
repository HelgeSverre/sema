use axum::{
    extract::{Query, State},
    http::{header, StatusCode},
    response::{AppendHeaders, IntoResponse, Redirect},
};
use rand::RngCore;
use serde::Deserialize;
use sqlx::Row;
use std::sync::Arc;

use crate::{auth::create_session, AppState};

fn generate_state() -> String {
    let mut bytes = [0u8; 16];
    rand::thread_rng().fill_bytes(&mut bytes);
    hex::encode(bytes)
}

/// GET /auth/github — redirect to GitHub authorize URL
pub async fn start(State(state): State<Arc<AppState>>) -> impl IntoResponse {
    let (client_id, _) = match github_creds(&state) {
        Some(c) => c,
        None => {
            return (StatusCode::NOT_FOUND, "GitHub OAuth not configured").into_response();
        }
    };

    let oauth_state = generate_state();

    let url = format!(
        "https://github.com/login/oauth/authorize?client_id={}&redirect_uri={}/auth/github/callback&scope=read:user%20user:email&state={}",
        client_id, state.config.base_url, oauth_state,
    );

    // Store state in a cookie so we can verify on callback
    let cookie = format!(
        "github_oauth_state={}; Path=/; HttpOnly; SameSite=Lax; Max-Age=600",
        oauth_state
    );

    ([(header::SET_COOKIE, cookie)], Redirect::to(&url)).into_response()
}

#[derive(Deserialize)]
pub struct CallbackParams {
    pub code: String,
    pub state: String,
}

#[derive(Deserialize)]
struct TokenResponse {
    access_token: String,
}

#[derive(Deserialize)]
struct GitHubUser {
    id: i64,
    login: String,
}

#[derive(Deserialize)]
struct GitHubEmail {
    email: String,
    primary: bool,
    verified: bool,
}

/// GET /auth/github/callback — exchange code for token, find/create user, set session
pub async fn callback(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
    Query(params): Query<CallbackParams>,
) -> impl IntoResponse {
    let (client_id, client_secret) = match github_creds(&state) {
        Some(c) => c,
        None => {
            return (StatusCode::NOT_FOUND, "GitHub OAuth not configured").into_response();
        }
    };

    // Verify state cookie matches
    let cookie_header = headers
        .get(header::COOKIE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");
    let stored_state = cookie_header
        .split(';')
        .filter_map(|c| c.trim().strip_prefix("github_oauth_state="))
        .next()
        .unwrap_or("");

    if stored_state.is_empty() || stored_state != params.state {
        tracing::error!("OAuth state mismatch: stored={:?} vs param={:?}", stored_state, params.state);
        return (StatusCode::BAD_REQUEST, "Invalid OAuth state").into_response();
    }

    // Exchange code for access token
    let client = reqwest::Client::new();
    let token_res = client
        .post("https://github.com/login/oauth/access_token")
        .header("Accept", "application/json")
        .form(&[
            ("client_id", client_id.as_str()),
            ("client_secret", client_secret.as_str()),
            ("code", &params.code),
        ])
        .send()
        .await;

    let token_body = match token_res {
        Ok(r) => match r.json::<TokenResponse>().await {
            Ok(t) => t,
            Err(e) => {
                tracing::error!("Failed to parse GitHub token response: {e}");
                return (StatusCode::BAD_GATEWAY, "Failed to get access token").into_response();
            }
        },
        Err(e) => {
            tracing::error!("GitHub token exchange failed: {e}");
            return (StatusCode::BAD_GATEWAY, "Failed to contact GitHub").into_response();
        }
    };

    // Fetch GitHub user info
    let user_res = client
        .get("https://api.github.com/user")
        .header("Authorization", format!("Bearer {}", token_body.access_token))
        .header("User-Agent", "sema-pkg")
        .send()
        .await;

    let gh_user = match user_res {
        Ok(r) => match r.json::<GitHubUser>().await {
            Ok(u) => u,
            Err(e) => {
                tracing::error!("Failed to parse GitHub user: {e}");
                return (StatusCode::BAD_GATEWAY, "Failed to get user info").into_response();
            }
        },
        Err(e) => {
            tracing::error!("GitHub user fetch failed: {e}");
            return (StatusCode::BAD_GATEWAY, "Failed to contact GitHub").into_response();
        }
    };

    // Fetch primary email
    let email = fetch_primary_email(&client, &token_body.access_token)
        .await
        .unwrap_or_else(|| format!("{}@users.noreply.github.com", gh_user.login));

    // Find or create user
    let user_id = find_or_create_user(&state.db, gh_user.id, &gh_user.login, &email).await;

    let user_id = match user_id {
        Ok(id) => id,
        Err(e) => {
            tracing::error!("Failed to find/create GitHub user: {e}");
            return (StatusCode::INTERNAL_SERVER_ERROR, "Failed to create account")
                .into_response();
        }
    };

    let session_id = create_session(&state.db, user_id).await;
    tracing::info!("GitHub OAuth: created session for user_id={}, redirecting to /account", user_id);

    let session_cookie = format!(
        "session={}; Path=/; HttpOnly; SameSite=Lax; Max-Age=604800",
        session_id
    );
    let clear_state = "github_oauth_state=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0".to_string();

    (
        AppendHeaders([
            (header::SET_COOKIE, session_cookie),
            (header::SET_COOKIE, clear_state),
        ]),
        Redirect::to("/account"),
    )
        .into_response()
}

async fn fetch_primary_email(client: &reqwest::Client, access_token: &str) -> Option<String> {
    let emails: Vec<GitHubEmail> = client
        .get("https://api.github.com/user/emails")
        .header("Authorization", format!("Bearer {access_token}"))
        .header("User-Agent", "sema-pkg")
        .send()
        .await
        .ok()?
        .json()
        .await
        .ok()?;

    // Prefer primary+verified, then any verified (never return unverified)
    emails
        .iter()
        .find(|e| e.primary && e.verified)
        .or_else(|| emails.iter().find(|e| e.verified))
        .map(|e| e.email.clone())
}

async fn find_or_create_user(
    db: &crate::db::Db,
    github_id: i64,
    login: &str,
    email: &str,
) -> Result<i64, String> {
    // Check if user with this github_id already exists
    let existing = sqlx::query("SELECT id FROM users WHERE github_id = ?")
        .bind(github_id)
        .fetch_optional(db)
        .await
        .map_err(|e| e.to_string())?;

    if let Some(row) = existing {
        return Ok(row.get("id"));
    }

    // Check if username is taken — if so, append github id
    let username_taken = sqlx::query("SELECT id FROM users WHERE username = ?")
        .bind(login)
        .fetch_optional(db)
        .await
        .map_err(|e| e.to_string())?;

    let username = if username_taken.is_some() {
        format!("{login}-{github_id}")
    } else {
        login.to_string()
    };

    // Check if email is taken — if so, use a modified email to avoid conflicts
    let email_taken = sqlx::query("SELECT id FROM users WHERE email = ?")
        .bind(email)
        .fetch_optional(db)
        .await
        .map_err(|e| e.to_string())?;

    let user_email = if email_taken.is_some() {
        format!("{github_id}+{email}")
    } else {
        email.to_string()
    };

    // Create new user
    let result = sqlx::query(
        "INSERT INTO users (username, email, github_id) VALUES (?, ?, ?)",
    )
    .bind(&username)
    .bind(&user_email)
    .bind(github_id)
    .execute(db)
    .await
    .map_err(|e| e.to_string())?;

    Ok(result.last_insert_rowid())
}

fn github_creds(state: &AppState) -> Option<(String, String)> {
    let id = state.config.github_client_id.as_ref()?.clone();
    let secret = state.config.github_client_secret.as_ref()?.clone();
    Some((id, secret))
}
