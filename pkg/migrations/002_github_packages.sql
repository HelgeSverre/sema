-- OAuth connections (stores encrypted GitHub access tokens)
CREATE TABLE IF NOT EXISTS oauth_connections (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL REFERENCES users(id),
    provider TEXT NOT NULL DEFAULT 'github',
    provider_user_id TEXT NOT NULL,
    provider_login TEXT,
    access_token_enc BLOB NOT NULL,
    scopes TEXT,
    revoked_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_oauth_provider_user
ON oauth_connections(provider, provider_user_id);

CREATE UNIQUE INDEX IF NOT EXISTS idx_oauth_user_provider
ON oauth_connections(user_id, provider);

-- Add source tracking to packages
ALTER TABLE packages ADD COLUMN source TEXT NOT NULL DEFAULT 'upload';
ALTER TABLE packages ADD COLUMN github_repo TEXT;
ALTER TABLE packages ADD COLUMN webhook_secret TEXT;

-- Sync log for GitHub-linked packages
CREATE TABLE IF NOT EXISTS github_sync_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    package_id INTEGER NOT NULL REFERENCES packages(id),
    tag TEXT NOT NULL,
    status TEXT NOT NULL,
    error TEXT,
    synced_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_sync_log_package ON github_sync_log(package_id);
