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
