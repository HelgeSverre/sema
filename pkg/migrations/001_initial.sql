-- Users
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    password_hash TEXT, -- NULL if GitHub-only auth
    github_id INTEGER UNIQUE,
    homepage TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Web sessions
CREATE TABLE IF NOT EXISTS sessions (
    id TEXT PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id),
    expires_at TIMESTAMP NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- CLI API tokens
CREATE TABLE IF NOT EXISTS api_tokens (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL REFERENCES users(id),
    name TEXT NOT NULL,
    token_hash TEXT NOT NULL UNIQUE,
    scopes TEXT NOT NULL DEFAULT 'publish',
    last_used_at TIMESTAMP,
    revoked_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Packages
CREATE TABLE IF NOT EXISTS packages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    description TEXT NOT NULL DEFAULT '',
    repository_url TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Package versions
CREATE TABLE IF NOT EXISTS package_versions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    package_id INTEGER NOT NULL REFERENCES packages(id),
    version TEXT NOT NULL,
    checksum_sha256 TEXT NOT NULL,
    blob_key TEXT NOT NULL,
    size_bytes INTEGER NOT NULL,
    yanked INTEGER NOT NULL DEFAULT 0,
    sema_version_req TEXT,
    published_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(package_id, version)
);

-- Dependencies
CREATE TABLE IF NOT EXISTS dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    version_id INTEGER NOT NULL REFERENCES package_versions(id),
    dependency_name TEXT NOT NULL,
    version_req TEXT NOT NULL
);

-- Package owners
CREATE TABLE IF NOT EXISTS owners (
    package_id INTEGER NOT NULL REFERENCES packages(id),
    user_id INTEGER NOT NULL REFERENCES users(id),
    PRIMARY KEY (package_id, user_id)
);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_versions_package ON package_versions(package_id);
CREATE INDEX IF NOT EXISTS idx_deps_version ON dependencies(version_id);
CREATE INDEX IF NOT EXISTS idx_tokens_user ON api_tokens(user_id);
CREATE INDEX IF NOT EXISTS idx_sessions_user ON sessions(user_id);
