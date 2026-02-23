# sema-pkg

Self-hostable package registry for the [Sema](https://sema-lang.com) programming language. Ships as a single binary with SQLite, serves both a web UI and a REST API for CLI clients.

## Quick Start

```bash
# Run locally
cd pkg
cargo run

# Or with Docker
docker compose up --build
```

Open [http://localhost:3000](http://localhost:3000).

## Configuration

All configuration is via environment variables with sensible defaults:

| Variable | Default | Description |
|---|---|---|
| `HOST` | `0.0.0.0` | Bind address |
| `PORT` | `3000` | Listen port |
| `DATABASE_URL` | `sqlite://data/registry.db?mode=rwc` | SQLite connection string |
| `BLOB_DIR` | `data/blobs` | Directory for package tarballs |
| `BASE_URL` | `http://localhost:3000` | Public URL (used in links) |
| `SESSION_SECRET` | `change-me-in-production` | Secret for session cookies |
| `MAX_TARBALL_BYTES` | `52428800` (50 MB) | Max upload size |
| `GITHUB_CLIENT_ID` | — | GitHub OAuth app client ID (optional) |
| `GITHUB_CLIENT_SECRET` | — | GitHub OAuth app secret (optional) |
| `OAUTH_TOKEN_KEY` | — | 32-char key for encrypting stored GitHub tokens (required for repo linking) |

## API Endpoints

### Auth

| Method | Path | Auth | Description |
|---|---|---|---|
| `POST` | `/api/v1/auth/register` | — | Create account `{username, email, password}` |
| `POST` | `/api/v1/auth/login` | — | Sign in `{username, password}` |
| `POST` | `/api/v1/auth/logout` | — | Clear session |

### Tokens

| Method | Path | Auth | Description |
|---|---|---|---|
| `POST` | `/api/v1/tokens` | Session | Create API token `{name}` |
| `GET` | `/api/v1/tokens` | Session | List your tokens |
| `DELETE` | `/api/v1/tokens/{id}` | Session | Revoke a token |

### Packages

| Method | Path | Auth | Description |
|---|---|---|---|
| `GET` | `/api/v1/search?q=&page=&per_page=` | — | Search packages |
| `GET` | `/api/v1/packages/{name}` | — | Package metadata + versions |
| `PUT` | `/api/v1/packages/{name}/{version}` | Bearer | Publish version (multipart: `tarball` + `metadata`) |
| `GET` | `/api/v1/packages/{name}/{version}/download` | — | Download tarball |
| `POST` | `/api/v1/packages/{name}/{version}/yank` | Bearer | Yank a version |

### Ownership

| Method | Path | Auth | Description |
|---|---|---|---|
| `GET` | `/api/v1/packages/{name}/owners` | — | List owners |
| `PUT` | `/api/v1/packages/{name}/owners` | Bearer | Add owner `{username}` |
| `DELETE` | `/api/v1/packages/{name}/owners` | Bearer | Remove owner `{username}` |

### GitHub-Linked Packages

| Method | Path | Auth | Description |
|---|---|---|---|
| `POST` | `/api/v1/packages/link` | Session | Link a GitHub repo `{repo_url}` |
| `POST` | `/api/v1/packages/{name}/sync` | Session | Manual re-sync from GitHub (owner only) |
| `POST` | `/api/v1/webhooks/github` | HMAC | Webhook receiver for tag events |

## GitHub-Linked Packages

Link a GitHub repository to automatically publish packages from semver tags.

### Prerequisites

- GitHub OAuth configured (`GITHUB_CLIENT_ID`, `GITHUB_CLIENT_SECRET`)
- `OAUTH_TOKEN_KEY` set to a random 32-character string (used to encrypt stored GitHub tokens)

### How It Works

1. User connects their GitHub account via OAuth
2. User pastes a repo URL on the `/link` page
3. Registry validates the repo contains a `sema.toml`, then imports existing semver tags as versions
4. A webhook is registered on the repo — new semver tags are published automatically

### Tag-to-Version Mapping

Git tags are mapped to package versions: `v1.0.0` → `1.0.0`. Tags that don't match semver (e.g., `nightly`, `latest`) are skipped.

### Source Locking

A package is either **CLI-uploaded** or **GitHub-linked**, never both. Once a package is linked to a repo, it cannot be published via `sema publish`, and vice versa.

## Self-Hosting

1. Build: `cargo build --release`
2. Copy `target/release/sema-pkg`, `templates/`, `static/`, and `migrations/` to your server
3. Set `DATABASE_URL`, `BLOB_DIR`, `BASE_URL`, and `SESSION_SECRET`
4. Run `sema-pkg` behind a reverse proxy (nginx/caddy) with TLS

Or use the Docker image:

```bash
docker compose up -d
```

Data is stored in `./data/` (SQLite DB + blob files). Back up this directory.

## GitHub OAuth (Optional)

1. Create a GitHub OAuth App at https://github.com/settings/developers
2. Set callback URL to `{BASE_URL}/auth/github/callback`
3. Set `GITHUB_CLIENT_ID` and `GITHUB_CLIENT_SECRET` environment variables
4. Restart the server — the GitHub login button will appear automatically

## License

[MIT](LICENSE.md)
