use crate::{blob, crypto, db::Db};
use sqlx::Row;

/// Fetch the decrypted GitHub access token for a user.
pub async fn get_github_token(db: &Db, user_id: i64, token_key: &str) -> Option<String> {
    let row = sqlx::query(
        "SELECT access_token_enc FROM oauth_connections WHERE user_id = ? AND provider = 'github' AND revoked_at IS NULL"
    )
    .bind(user_id)
    .fetch_optional(db)
    .await
    .ok()??;

    let enc: Vec<u8> = row.get("access_token_enc");
    crypto::decrypt(&enc, token_key)
}

/// Mark a user's GitHub connection as revoked (e.g. after a 401).
pub async fn mark_token_revoked(db: &Db, user_id: i64) {
    let _ = sqlx::query(
        "UPDATE oauth_connections SET revoked_at = datetime('now') WHERE user_id = ? AND provider = 'github'"
    )
    .bind(user_id)
    .execute(db)
    .await;
}

/// Validate that a GitHub repo exists and contains sema.toml. Returns the parsed manifest.
pub async fn validate_repo(
    client: &reqwest::Client,
    token: &str,
    owner: &str,
    repo: &str,
) -> Result<RepoManifest, String> {
    let resp = client
        .get(format!("https://api.github.com/repos/{owner}/{repo}"))
        .header("Authorization", format!("Bearer {token}"))
        .header("User-Agent", "sema-pkg")
        .send()
        .await
        .map_err(|e| format!("Failed to reach GitHub: {e}"))?;

    if resp.status() == reqwest::StatusCode::UNAUTHORIZED {
        return Err("GitHub token is invalid or revoked".into());
    }
    if !resp.status().is_success() {
        return Err(format!("Repository {owner}/{repo} not found or not accessible"));
    }

    let toml_resp = client
        .get(format!("https://api.github.com/repos/{owner}/{repo}/contents/sema.toml"))
        .header("Authorization", format!("Bearer {token}"))
        .header("User-Agent", "sema-pkg")
        .header("Accept", "application/vnd.github.raw+json")
        .send()
        .await
        .map_err(|e| format!("Failed to fetch sema.toml: {e}"))?;

    if !toml_resp.status().is_success() {
        return Err(format!("No sema.toml found in {owner}/{repo}"));
    }

    let toml_content = toml_resp.text().await.map_err(|e| format!("Failed to read sema.toml: {e}"))?;
    parse_manifest(&toml_content)
}

#[derive(Debug, Clone)]
pub struct RepoManifest {
    pub name: String,
    pub description: String,
    pub repository_url: Option<String>,
    pub sema_version_req: Option<String>,
}

fn parse_manifest(content: &str) -> Result<RepoManifest, String> {
    let doc: toml::Value = toml::from_str(content).map_err(|e| format!("Invalid sema.toml: {e}"))?;
    let pkg = doc.get("package").ok_or("sema.toml missing [package] section")?;
    let pkg = match pkg {
        toml::Value::Table(t) => t,
        _ => return Err("sema.toml [package] must be a table".into()),
    };
    let name = pkg.get("name")
        .and_then(toml::Value::as_str)
        .ok_or("sema.toml [package] missing 'name'")?;
    let description = pkg.get("description")
        .and_then(toml::Value::as_str)
        .unwrap_or("")
        .to_string();
    let repository_url = pkg.get("repository")
        .and_then(toml::Value::as_str)
        .map(str::to_string);
    let sema_version_req = pkg.get("sema_version_req")
        .and_then(toml::Value::as_str)
        .map(str::to_string);
    Ok(RepoManifest { name: name.to_string(), description, repository_url, sema_version_req })
}

/// List semver tags from a GitHub repo. Strips leading 'v' prefix.
/// Returns (tag_name, semver_version) pairs sorted newest-first.
pub async fn list_semver_tags(
    client: &reqwest::Client,
    token: &str,
    owner: &str,
    repo: &str,
) -> Result<Vec<(String, semver::Version)>, String> {
    let mut tags = Vec::new();
    let mut page = 1u32;

    loop {
        let resp = client
            .get(format!("https://api.github.com/repos/{owner}/{repo}/tags?per_page=100&page={page}"))
            .header("Authorization", format!("Bearer {token}"))
            .header("User-Agent", "sema-pkg")
            .send()
            .await
            .map_err(|e| format!("Failed to list tags: {e}"))?;

        if !resp.status().is_success() {
            return Err(format!("Failed to list tags ({})", resp.status()));
        }

        let items: Vec<serde_json::Value> = resp.json().await.map_err(|e| format!("Invalid response: {e}"))?;
        if items.is_empty() {
            break;
        }

        for item in &items {
            if let Some(tag_name) = item.get("name").and_then(|n| n.as_str()) {
                let version_str = tag_name.strip_prefix('v').unwrap_or(tag_name);
                if let Ok(ver) = semver::Version::parse(version_str) {
                    tags.push((tag_name.to_string(), ver));
                }
            }
        }

        if items.len() < 100 {
            break;
        }
        page += 1;
    }

    tags.sort_by(|a, b| b.1.cmp(&a.1));
    Ok(tags)
}

/// Sync a single tag: download tarball from GitHub, store as blob, create version record.
/// Returns Ok(true) if version was created, Ok(false) if it already existed.
pub async fn sync_tag(
    db: &Db,
    client: &reqwest::Client,
    token: &str,
    owner: &str,
    repo: &str,
    tag_name: &str,
    version: &semver::Version,
    package_id: i64,
    blob_dir: &str,
    sema_version_req: Option<&str>,
) -> Result<bool, String> {
    let version_str = version.to_string();

    let exists = sqlx::query(
        "SELECT COUNT(*) as cnt FROM package_versions WHERE package_id = ? AND version = ?"
    )
    .bind(package_id)
    .bind(&version_str)
    .fetch_one(db)
    .await
    .ok()
    .map(|r| r.get::<i32, _>("cnt"))
    .unwrap_or(0);

    if exists > 0 {
        return Ok(false);
    }

    let tarball_url = format!("https://api.github.com/repos/{owner}/{repo}/tarball/{tag_name}");
    let resp = client
        .get(&tarball_url)
        .header("Authorization", format!("Bearer {token}"))
        .header("User-Agent", "sema-pkg")
        .send()
        .await
        .map_err(|e| format!("Failed to download tarball for {tag_name}: {e}"))?;

    if !resp.status().is_success() {
        return Err(format!("Failed to download tarball for {tag_name} ({})", resp.status()));
    }

    let tarball = resp.bytes().await.map_err(|e| format!("Failed to read tarball: {e}"))?;

    let (blob_key, checksum, size) = blob::store(blob_dir, &tarball).await;

    sqlx::query(
        "INSERT INTO package_versions (package_id, version, checksum_sha256, blob_key, size_bytes, sema_version_req) VALUES (?, ?, ?, ?, ?, ?)"
    )
    .bind(package_id)
    .bind(&version_str)
    .bind(&checksum)
    .bind(&blob_key)
    .bind(size as i64)
    .bind(sema_version_req)
    .execute(db)
    .await
    .map_err(|e| format!("Failed to insert version: {e}"))?;

    sqlx::query(
        "INSERT INTO github_sync_log (package_id, tag, status) VALUES (?, ?, 'ok')"
    )
    .bind(package_id)
    .bind(tag_name)
    .execute(db)
    .await
    .ok();

    Ok(true)
}

/// Register a webhook on a GitHub repository.
pub async fn register_webhook(
    client: &reqwest::Client,
    token: &str,
    owner: &str,
    repo: &str,
    webhook_url: &str,
    webhook_secret: &str,
) -> Result<(), String> {
    let body = serde_json::json!({
        "name": "web",
        "active": true,
        "events": ["push"],
        "config": {
            "url": webhook_url,
            "content_type": "json",
            "secret": webhook_secret,
            "insecure_ssl": "0"
        }
    });

    let resp = client
        .post(format!("https://api.github.com/repos/{owner}/{repo}/hooks"))
        .header("Authorization", format!("Bearer {token}"))
        .header("User-Agent", "sema-pkg")
        .json(&body)
        .send()
        .await
        .map_err(|e| format!("Failed to register webhook: {e}"))?;

    if resp.status() == reqwest::StatusCode::UNPROCESSABLE_ENTITY {
        let body: serde_json::Value = resp.json().await.unwrap_or_default();
        let errors = body.get("errors").and_then(|e| e.as_array());
        if let Some(errors) = errors {
            let already_exists = errors.iter().any(|e| {
                e.get("message").and_then(|m| m.as_str()).map(|m| m.contains("already exists")).unwrap_or(false)
            });
            if already_exists {
                return Ok(());
            }
        }
        return Err(format!("Failed to register webhook: {}", body));
    }

    if !resp.status().is_success() {
        let status = resp.status();
        return Err(format!("Failed to register webhook ({status})"));
    }

    Ok(())
}

/// Parse an "owner/repo" string from a GitHub URL.
/// Accepts: "github.com/owner/repo", "https://github.com/owner/repo", "https://github.com/owner/repo.git", "owner/repo"
pub fn parse_github_url(url: &str) -> Option<(String, String)> {
    let url = url.trim();
    let url = url.strip_suffix(".git").unwrap_or(url);
    let url = url.strip_prefix("https://").unwrap_or(url);
    let url = url.strip_prefix("http://").unwrap_or(url);
    let url = url.strip_prefix("github.com/").unwrap_or(url);

    let parts: Vec<&str> = url.splitn(3, '/').collect();
    if parts.len() >= 2 && !parts[0].is_empty() && !parts[1].is_empty() {
        Some((parts[0].to_string(), parts[1].to_string()))
    } else {
        None
    }
}

/// Generate a random webhook secret.
pub fn generate_webhook_secret() -> String {
    use rand::RngCore;
    let mut bytes = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut bytes);
    hex::encode(bytes)
}
