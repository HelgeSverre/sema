use std::env;

pub struct Config {
    pub host: String,
    pub port: u16,
    pub database_url: String,
    pub blob_dir: String,
    pub base_url: String,
    pub github_client_id: Option<String>,
    pub github_client_secret: Option<String>,
    pub session_secret: String,
    pub oauth_token_key: String,
    pub max_tarball_bytes: usize,
}

impl Config {
    pub fn from_env() -> Self {
        Self {
            host: env::var("HOST").unwrap_or_else(|_| "0.0.0.0".into()),
            port: env::var("PORT")
                .ok()
                .and_then(|p| p.parse().ok())
                .unwrap_or(3000),
            database_url: env::var("DATABASE_URL")
                .unwrap_or_else(|_| "sqlite://data/registry.db?mode=rwc".into()),
            blob_dir: env::var("BLOB_DIR").unwrap_or_else(|_| "data/blobs".into()),
            base_url: env::var("BASE_URL")
                .unwrap_or_else(|_| "http://localhost:3000".into()),
            github_client_id: env::var("GITHUB_CLIENT_ID").ok(),
            github_client_secret: env::var("GITHUB_CLIENT_SECRET").ok(),
            session_secret: env::var("SESSION_SECRET")
                .unwrap_or_else(|_| "change-me-in-production".into()),
            oauth_token_key: env::var("OAUTH_TOKEN_KEY")
                .unwrap_or_else(|_| "change-me-32-bytes-in-production!".into()),
            max_tarball_bytes: env::var("MAX_TARBALL_BYTES")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(50 * 1024 * 1024), // 50 MB
        }
    }
}
