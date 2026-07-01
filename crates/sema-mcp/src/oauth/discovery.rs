//! OAuth authorization-server discovery for MCP (spec `2025-11-25`).
//!
//! The client learns *where* to authenticate by walking a metadata chain:
//! a `401` from the MCP server carries `WWW-Authenticate: Bearer
//! resource_metadata="…"`; that URL (RFC 9728 Protected Resource Metadata)
//! lists the authorization server(s); each server publishes its endpoints via
//! RFC 8414 / OpenID Connect Discovery metadata. All of this is plain `GET`s of
//! `.well-known/*` documents, so it is driven with our own reqwest client.

use serde::Deserialize;
use url::Url;

/// The auth-params parsed from a `401`'s `WWW-Authenticate: Bearer …` header.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct WwwAuthenticate {
    /// RFC 9728 `resource_metadata` — the Protected Resource Metadata URL.
    pub resource_metadata: Option<String>,
    /// RFC 6750 `scope` — when present, authoritative for the failing request.
    pub scope: Option<String>,
    /// RFC 6750 `error` (e.g. `insufficient_scope`).
    pub error: Option<String>,
}

/// Parse a `WWW-Authenticate` header value, pulling the `Bearer` auth-params we
/// care about. Tolerant of ordering, spacing, and unrelated params.
pub fn parse_www_authenticate(header: &str) -> WwwAuthenticate {
    let mut out = WwwAuthenticate::default();
    // Drop the leading scheme token (`Bearer`) if present, then split the
    // comma-separated `key="value"` / `key=value` auth-params.
    let params = header
        .trim()
        .strip_prefix("Bearer")
        .unwrap_or(header)
        .trim();
    for part in params.split(',') {
        let Some((key, value)) = part.split_once('=') else {
            continue;
        };
        let key = key.trim().to_ascii_lowercase();
        let value = value.trim().trim_matches('"').to_string();
        match key.as_str() {
            "resource_metadata" => out.resource_metadata = Some(value),
            "scope" => out.scope = Some(value),
            "error" => out.error = Some(value),
            _ => {}
        }
    }
    out
}

/// RFC 9728 Protected Resource Metadata (only the fields we consume).
#[derive(Debug, Clone, Deserialize)]
pub struct ProtectedResourceMetadata {
    pub resource: String,
    #[serde(default)]
    pub authorization_servers: Vec<String>,
    #[serde(default)]
    pub scopes_supported: Vec<String>,
}

/// RFC 8414 / OIDC Authorization Server Metadata (only the fields we consume).
#[derive(Debug, Clone, Deserialize)]
pub struct AuthorizationServerMetadata {
    pub issuer: String,
    pub authorization_endpoint: String,
    pub token_endpoint: String,
    #[serde(default)]
    pub registration_endpoint: Option<String>,
    #[serde(default)]
    pub device_authorization_endpoint: Option<String>,
    #[serde(default)]
    pub code_challenge_methods_supported: Vec<String>,
    #[serde(default)]
    pub grant_types_supported: Vec<String>,
    #[serde(default)]
    pub scopes_supported: Vec<String>,
}

impl AuthorizationServerMetadata {
    /// RFC 7636 gate: the client MUST verify PKCE-S256 support before using this
    /// server. Metadata that omits `code_challenge_methods_supported` entirely is
    /// treated (per OIDC discovery rules) as not advertising PKCE.
    pub fn supports_pkce_s256(&self) -> bool {
        self.code_challenge_methods_supported
            .iter()
            .any(|m| m.eq_ignore_ascii_case("S256"))
    }

    pub fn supports_device_flow(&self) -> bool {
        self.device_authorization_endpoint.is_some()
            && (self.grant_types_supported.is_empty()
                || self
                    .grant_types_supported
                    .iter()
                    .any(|g| g == "urn:ietf:params:oauth:grant-type:device_code"))
    }
}

/// Candidate Protected Resource Metadata URLs to probe when the `401` did not
/// advertise one (RFC 9728 §3.1): the well-known suffix is inserted between the
/// host and the resource's path, then the origin-root form.
pub fn protected_resource_metadata_urls(mcp_endpoint: &str) -> Vec<String> {
    well_known_urls(mcp_endpoint, "oauth-protected-resource")
}

/// Candidate Authorization Server Metadata URLs for an issuer (RFC 8414 + OIDC
/// Discovery), in the spec's probe order.
pub fn authorization_server_metadata_urls(issuer: &str) -> Vec<String> {
    let Ok(url) = Url::parse(issuer) else {
        return Vec::new();
    };
    let origin = origin_of(&url);
    let path = url.path().trim_end_matches('/');
    if path.is_empty() {
        // Issuer without a path component.
        vec![
            format!("{origin}/.well-known/oauth-authorization-server"),
            format!("{origin}/.well-known/openid-configuration"),
        ]
    } else {
        // Issuer with a path: RFC 8414 / OIDC path-insertion, then OIDC append.
        vec![
            format!("{origin}/.well-known/oauth-authorization-server{path}"),
            format!("{origin}/.well-known/openid-configuration{path}"),
            format!("{origin}{path}/.well-known/openid-configuration"),
        ]
    }
}

/// Insert `/.well-known/<suffix>` between the host and path of `target`, then the
/// origin-root variant.
fn well_known_urls(target: &str, suffix: &str) -> Vec<String> {
    let Ok(url) = Url::parse(target) else {
        return Vec::new();
    };
    let origin = origin_of(&url);
    let path = url.path().trim_end_matches('/');
    let root = format!("{origin}/.well-known/{suffix}");
    if path.is_empty() {
        vec![root]
    } else {
        vec![format!("{origin}/.well-known/{suffix}{path}"), root]
    }
}

/// `scheme://host[:port]` with no trailing slash.
fn origin_of(url: &Url) -> String {
    let mut origin = format!("{}://{}", url.scheme(), url.host_str().unwrap_or(""));
    if let Some(port) = url.port() {
        origin.push_str(&format!(":{port}"));
    }
    origin
}

/// Fetch Protected Resource Metadata. Tries the `401`-advertised URL first (when
/// present), then the well-known fallbacks derived from the MCP endpoint.
pub async fn fetch_protected_resource_metadata(
    client: &reqwest::Client,
    mcp_endpoint: &str,
    advertised_url: Option<&str>,
) -> Result<ProtectedResourceMetadata, String> {
    let mut candidates = Vec::new();
    if let Some(url) = advertised_url {
        candidates.push(url.to_string());
    }
    candidates.extend(protected_resource_metadata_urls(mcp_endpoint));

    fetch_first_json(client, &candidates, "protected resource metadata").await
}

/// Fetch Authorization Server Metadata for an issuer, probing the well-known
/// variants in order.
pub async fn fetch_authorization_server_metadata(
    client: &reqwest::Client,
    issuer: &str,
) -> Result<AuthorizationServerMetadata, String> {
    let candidates = authorization_server_metadata_urls(issuer);
    fetch_first_json(client, &candidates, "authorization server metadata").await
}

/// GET each candidate URL in turn, returning the first that decodes as `T`.
async fn fetch_first_json<T: serde::de::DeserializeOwned>(
    client: &reqwest::Client,
    candidates: &[String],
    what: &str,
) -> Result<T, String> {
    let mut last_err = format!("no {what} URL candidates");
    for url in candidates {
        match client
            .get(url)
            .header("Accept", "application/json")
            .send()
            .await
        {
            Ok(resp) if resp.status().is_success() => match resp.json::<T>().await {
                Ok(value) => return Ok(value),
                Err(err) => last_err = format!("{what} at {url} did not decode: {err}"),
            },
            Ok(resp) => last_err = format!("{what} at {url}: HTTP {}", resp.status().as_u16()),
            Err(err) => last_err = format!("{what} at {url}: {err}"),
        }
    }
    Err(format!("failed to discover {what}: {last_err}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_www_authenticate_params() {
        let h = r#"Bearer resource_metadata="https://mcp.example.com/.well-known/oauth-protected-resource", scope="files:read", error="insufficient_scope""#;
        let parsed = parse_www_authenticate(h);
        assert_eq!(
            parsed.resource_metadata.as_deref(),
            Some("https://mcp.example.com/.well-known/oauth-protected-resource")
        );
        assert_eq!(parsed.scope.as_deref(), Some("files:read"));
        assert_eq!(parsed.error.as_deref(), Some("insufficient_scope"));
    }

    #[test]
    fn parses_www_authenticate_without_scheme_or_quotes() {
        let parsed = parse_www_authenticate("resource_metadata=https://x/y");
        assert_eq!(parsed.resource_metadata.as_deref(), Some("https://x/y"));
        assert!(parsed.scope.is_none());
    }

    #[test]
    fn prm_urls_are_path_aware_then_root() {
        let urls = protected_resource_metadata_urls("https://example.com/public/mcp");
        assert_eq!(
            urls,
            vec![
                "https://example.com/.well-known/oauth-protected-resource/public/mcp".to_string(),
                "https://example.com/.well-known/oauth-protected-resource".to_string(),
            ]
        );
    }

    #[test]
    fn prm_urls_root_only_for_bare_host() {
        let urls = protected_resource_metadata_urls("https://example.com");
        assert_eq!(
            urls,
            vec!["https://example.com/.well-known/oauth-protected-resource".to_string()]
        );
    }

    #[test]
    fn as_urls_for_issuer_with_path() {
        let urls = authorization_server_metadata_urls("https://auth.example.com/tenant1");
        assert_eq!(
            urls,
            vec![
                "https://auth.example.com/.well-known/oauth-authorization-server/tenant1"
                    .to_string(),
                "https://auth.example.com/.well-known/openid-configuration/tenant1".to_string(),
                "https://auth.example.com/tenant1/.well-known/openid-configuration".to_string(),
            ]
        );
    }

    #[test]
    fn as_urls_for_bare_issuer() {
        let urls = authorization_server_metadata_urls("https://auth.example.com");
        assert_eq!(
            urls,
            vec![
                "https://auth.example.com/.well-known/oauth-authorization-server".to_string(),
                "https://auth.example.com/.well-known/openid-configuration".to_string(),
            ]
        );
    }

    #[test]
    fn pkce_gate_requires_s256() {
        let mut md = AuthorizationServerMetadata {
            issuer: "https://a".into(),
            authorization_endpoint: "https://a/authorize".into(),
            token_endpoint: "https://a/token".into(),
            registration_endpoint: None,
            device_authorization_endpoint: None,
            code_challenge_methods_supported: vec![],
            grant_types_supported: vec![],
            scopes_supported: vec![],
        };
        assert!(!md.supports_pkce_s256(), "empty methods = no PKCE");
        md.code_challenge_methods_supported = vec!["plain".into()];
        assert!(!md.supports_pkce_s256(), "plain only = no S256");
        md.code_challenge_methods_supported = vec!["S256".into()];
        assert!(md.supports_pkce_s256());
    }
}
