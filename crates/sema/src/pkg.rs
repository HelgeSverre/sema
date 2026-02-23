use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use sema_core::resolve::packages_dir;

const DEFAULT_REGISTRY: &str = "https://pkg.sema-lang.com";
const PKG_META_FILE: &str = ".sema-pkg.json";

fn run_git(dir: Option<&Path>, args: &[&str]) -> Result<String, String> {
    let mut cmd = Command::new("git");
    cmd.args(args);
    if let Some(dir) = dir {
        cmd.current_dir(dir);
    }
    let output = cmd
        .output()
        .map_err(|e| format!("Failed to run git: {e}"))?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        Err(format!("git {} failed: {stderr}", args.join(" ")))
    }
}

fn current_git_ref(dir: &Path) -> String {
    if let Ok(tag) = run_git(Some(dir), &["describe", "--tags", "--exact-match"]) {
        return tag;
    }
    run_git(Some(dir), &["rev-parse", "--abbrev-ref", "HEAD"])
        .unwrap_or_else(|_| "unknown".to_string())
}

fn find_package_dir(pkg_dir: &Path, name: &str) -> Option<PathBuf> {
    let exact = pkg_dir.join(name);
    if exact.is_dir() {
        return Some(exact);
    }

    find_all_packages(pkg_dir).into_iter().find(|p| {
        p.file_name()
            .map(|n| n.to_string_lossy() == name)
            .unwrap_or(false)
    })
}

fn find_all_packages(pkg_dir: &Path) -> Vec<PathBuf> {
    let mut packages = Vec::new();
    collect_packages(pkg_dir, &mut packages);
    packages
}

fn collect_packages(dir: &Path, packages: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        // Skip symlinks to avoid loops and escaping the packages directory
        if path
            .symlink_metadata()
            .map(|m| m.is_symlink())
            .unwrap_or(false)
        {
            continue;
        }
        if !path.is_dir() {
            continue;
        }
        if path.join("sema.toml").exists()
            || path.join("package.sema").exists()
            || path.join(PKG_META_FILE).exists()
        {
            packages.push(path);
        } else {
            collect_packages(&path, packages);
        }
    }
}

pub fn cmd_add(spec: &str, registry: Option<&str>) -> Result<(), String> {
    if is_git_spec(spec) {
        cmd_add_git(spec)
    } else {
        cmd_add_registry(spec, registry)
    }
}

fn cmd_add_git(spec: &str) -> Result<(), String> {
    let spec = sema_core::resolve::PackageSpec::parse(spec).map_err(|e| e.to_string())?;
    let pkg_dir = packages_dir();
    let dest = spec.dest_dir(&pkg_dir);

    if dest.exists() {
        run_git(Some(&dest), &["fetch", "--tags"])?;
        run_git(Some(&dest), &["checkout", &spec.git_ref])?;
        let current = current_git_ref(&dest);
        println!("✓ Updated {} → {current}", spec.path);
    } else {
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create directory: {e}"))?;
        }
        run_git(None, &["clone", &spec.clone_url(), &dest.to_string_lossy()])?;
        run_git(Some(&dest), &["checkout", &spec.git_ref])?;
        let current = current_git_ref(&dest);
        println!("✓ Installed {} → {current}", spec.path);
    }

    // Add to sema.toml [deps] if present
    let toml_path = Path::new("sema.toml");
    if toml_path.exists() {
        match add_dep_to_toml(toml_path, spec.path.as_str(), &spec.git_ref) {
            Ok(true) => println!("✓ Added {} = \"{}\" to sema.toml", spec.path, spec.git_ref),
            Ok(false) => {}
            Err(e) => eprintln!("Warning: could not update sema.toml: {e}"),
        }
    }

    Ok(())
}

fn cmd_add_registry(spec: &str, registry: Option<&str>) -> Result<(), String> {
    let (name, version) = if let Some((n, v)) = spec.rsplit_once('@') {
        (n.to_string(), Some(v.to_string()))
    } else {
        (spec.to_string(), None)
    };

    let registry_url = effective_registry(registry);

    // Resolve version: use explicit version or find latest
    let version = match version {
        Some(v) => v,
        None => {
            let info = registry_package_info(&name, &registry_url)?;
            latest_version(&info)
                .ok_or_else(|| format!("No published versions found for '{name}'"))?
        }
    };

    println!("Installing {name}@{version} from registry...");
    registry_install(&name, &version, &registry_url)?;
    println!("✓ Installed {name}@{version}");

    // Add to sema.toml [deps] if present
    let toml_path = Path::new("sema.toml");
    if toml_path.exists() {
        match add_dep_to_toml(toml_path, &name, &version) {
            Ok(true) => println!("✓ Added {name} = \"{version}\" to sema.toml"),
            Ok(false) => {}
            Err(e) => eprintln!("Warning: could not update sema.toml: {e}"),
        }
    }

    Ok(())
}

pub fn cmd_install() -> Result<(), String> {
    let toml_path = Path::new("sema.toml");
    if !toml_path.exists() {
        return Err("No sema.toml found in current directory. Run `sema pkg init` first.".into());
    }

    let content =
        std::fs::read_to_string(toml_path).map_err(|e| format!("Failed to read sema.toml: {e}"))?;
    let doc: toml::Value =
        toml::from_str(&content).map_err(|e| format!("Failed to parse sema.toml: {e}"))?;

    let deps = match doc.get("deps").and_then(|d| d.as_table()) {
        Some(table) => table,
        None => {
            println!("No [deps] found in sema.toml, nothing to install.");
            return Ok(());
        }
    };

    for (name, value) in deps {
        let version = match value.as_str() {
            Some(s) => s,
            None => {
                return Err(format!(
                    "dep '{name}': expected a version/ref string (e.g., \"1.0.0\" or \"main\")"
                ));
            }
        };
        let spec = format!("{name}@{version}");
        println!("Installing {name}...");
        // Keys with / are git deps; otherwise registry deps
        cmd_add(&spec, None)?;
    }

    Ok(())
}

pub fn cmd_update(name: Option<&str>) -> Result<(), String> {
    let pkg_dir = packages_dir();

    if let Some(name) = name {
        let dir = find_package_dir(&pkg_dir, name).ok_or_else(|| {
            format!("Package '{name}' not found. Run `sema pkg list` to see installed packages.")
        })?;
        update_single_package(&pkg_dir, &dir)?;
    } else {
        let packages = find_all_packages(&pkg_dir);
        if packages.is_empty() {
            println!("No packages installed.");
            return Ok(());
        }
        for dir in &packages {
            let rel = dir.strip_prefix(&pkg_dir).unwrap_or(dir);
            if let Err(e) = update_single_package(&pkg_dir, dir) {
                eprintln!("✗ Failed to update {}: {e}", rel.display());
            }
        }
    }

    Ok(())
}

fn update_single_package(pkg_dir: &Path, dir: &Path) -> Result<(), String> {
    let rel = dir.strip_prefix(pkg_dir).unwrap_or(dir);

    if let Some(meta) = read_pkg_meta(dir) {
        // Registry package — check for newer version
        let name = meta
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or(&rel.display().to_string())
            .to_string();
        let current_ver = meta
            .get("version")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown");
        let registry = meta
            .get("registry")
            .and_then(|v| v.as_str())
            .unwrap_or(DEFAULT_REGISTRY);

        let info = registry_package_info(&name, registry)?;
        let latest = latest_version(&info)
            .ok_or_else(|| format!("No versions found for '{name}'"))?;

        if latest == current_ver {
            println!("  {} already at latest ({current_ver})", rel.display());
        } else {
            println!("  Updating {} {current_ver} → {latest}...", rel.display());
            registry_install(&name, &latest, registry)?;
            println!("✓ Updated {} → {latest}", rel.display());
        }
    } else if dir.join(".git").is_dir() {
        // Git package
        run_git(Some(dir), &["pull"])?;
        let current = current_git_ref(dir);
        println!("✓ Updated {} → {current}", rel.display());
    } else {
        println!("  {} — unknown source, skipping", rel.display());
    }

    Ok(())
}

pub fn cmd_remove(name: &str) -> Result<(), String> {
    let pkg_dir = packages_dir();
    let dir = find_package_dir(&pkg_dir, name).ok_or_else(|| {
        format!("Package '{name}' not found. Run `sema pkg list` to see installed packages.")
    })?;

    let rel_path = dir
        .strip_prefix(&pkg_dir)
        .unwrap_or(&dir)
        .to_string_lossy()
        .to_string();

    std::fs::remove_dir_all(&dir).map_err(|e| format!("Failed to remove package: {e}"))?;
    println!("✓ Removed {rel_path}");

    // Clean up empty parent directories
    let mut parent = dir.parent();
    while let Some(p) = parent {
        if p == pkg_dir {
            break;
        }
        if p.read_dir().map(|mut d| d.next().is_none()).unwrap_or(true) {
            let _ = std::fs::remove_dir(p);
            parent = p.parent();
        } else {
            break;
        }
    }

    // Remove from sema.toml [deps] if present
    let toml_path = Path::new("sema.toml");
    if toml_path.exists() {
        match remove_dep_from_toml(toml_path, &rel_path) {
            Ok(true) => println!("✓ Removed {rel_path} from sema.toml"),
            Ok(false) => {}
            Err(e) => eprintln!("Warning: could not update sema.toml: {e}"),
        }
    }

    Ok(())
}

/// Add or update a dep entry in a sema.toml file.
/// Returns true if the entry was added or updated, false if already up-to-date.
fn add_dep_to_toml(toml_path: &Path, pkg_path: &str, git_ref: &str) -> Result<bool, String> {
    let content =
        std::fs::read_to_string(toml_path).map_err(|e| format!("Failed to read sema.toml: {e}"))?;
    let mut doc: toml_edit::DocumentMut = content
        .parse()
        .map_err(|e| format!("Failed to parse sema.toml: {e}"))?;

    if doc.get("deps").is_none() {
        doc["deps"] = toml_edit::Item::Table(toml_edit::Table::new());
    }

    let deps = doc["deps"]
        .as_table_mut()
        .ok_or("sema.toml [deps] is not a table")?;

    if let Some(existing) = deps.get(pkg_path).and_then(|v| v.as_str()) {
        if existing == git_ref {
            return Ok(false);
        }
    }

    deps[pkg_path] = toml_edit::value(git_ref);

    std::fs::write(toml_path, doc.to_string())
        .map_err(|e| format!("Failed to write sema.toml: {e}"))?;
    Ok(true)
}

/// Remove a dep entry from a sema.toml file by package path.
/// Returns true if a matching entry was found and removed.
fn remove_dep_from_toml(toml_path: &Path, pkg_path: &str) -> Result<bool, String> {
    let content =
        std::fs::read_to_string(toml_path).map_err(|e| format!("Failed to read sema.toml: {e}"))?;
    let mut doc: toml_edit::DocumentMut = content
        .parse()
        .map_err(|e| format!("Failed to parse sema.toml: {e}"))?;

    let removed = if let Some(deps) = doc.get_mut("deps").and_then(|d| d.as_table_mut()) {
        deps.remove(pkg_path).is_some()
    } else {
        false
    };

    if removed {
        std::fs::write(toml_path, doc.to_string())
            .map_err(|e| format!("Failed to write sema.toml: {e}"))?;
    }

    Ok(removed)
}

pub fn cmd_list() -> Result<(), String> {
    let pkg_dir = packages_dir();
    let packages = find_all_packages(&pkg_dir);

    if packages.is_empty() {
        println!("No packages installed.");
        return Ok(());
    }

    for dir in &packages {
        let rel = dir.strip_prefix(&pkg_dir).unwrap_or(dir);
        if let Some(meta) = read_pkg_meta(dir) {
            let version = meta
                .get("version")
                .and_then(|v| v.as_str())
                .unwrap_or("?");
            let source = meta
                .get("registry")
                .and_then(|v| v.as_str())
                .unwrap_or("registry");
            println!("  {} ({version}) [{}]", rel.display(), source);
        } else {
            let current = current_git_ref(dir);
            println!("  {} ({current}) [git]", rel.display());
        }
    }

    Ok(())
}

pub fn cmd_init() -> Result<(), String> {
    let toml_path = Path::new("sema.toml");
    if toml_path.exists() {
        return Err("sema.toml already exists in current directory.".into());
    }

    let project_name = std::env::current_dir()
        .ok()
        .and_then(|p| p.file_name().map(|n| n.to_string_lossy().to_string()))
        .unwrap_or_else(|| "my-project".to_string());

    let content = format!(
        r#"[package]
name = "{project_name}"
version = "0.1.0"

[deps]
"#
    );

    std::fs::write(toml_path, content).map_err(|e| format!("Failed to write sema.toml: {e}"))?;
    println!("✓ Created sema.toml");

    Ok(())
}

pub fn cmd_login(token: Option<&str>, registry: &str) -> Result<(), String> {
    let token = match token {
        Some(t) => t.to_string(),
        None => {
            eprint!("API token: ");
            let mut input = String::new();
            std::io::stdin()
                .read_line(&mut input)
                .map_err(|e| format!("Failed to read input: {e}"))?;
            let input = input.trim().to_string();
            if input.is_empty() {
                return Err("Token cannot be empty".into());
            }
            input
        }
    };

    if !token.starts_with("sema_pat_") {
        return Err("Invalid token format. Tokens start with 'sema_pat_'".into());
    }

    let creds_path = credentials_path();
    if let Some(parent) = creds_path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("Failed to create config directory: {e}"))?;
    }

    let content = format!("[registry]\ntoken = \"{token}\"\nurl = \"{registry}\"\n");
    std::fs::write(&creds_path, &content)
        .map_err(|e| format!("Failed to write credentials: {e}"))?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(0o600);
        let _ = std::fs::set_permissions(&creds_path, perms);
    }

    println!("✓ Login saved to {}", creds_path.display());
    println!("  Registry: {registry}");
    Ok(())
}

pub fn cmd_logout() -> Result<(), String> {
    let creds_path = credentials_path();
    if creds_path.exists() {
        std::fs::remove_file(&creds_path)
            .map_err(|e| format!("Failed to remove credentials: {e}"))?;
        println!("✓ Logged out (removed {})", creds_path.display());
    } else {
        println!("Not logged in.");
    }
    Ok(())
}

fn credentials_path() -> PathBuf {
    sema_core::home::sema_home().join("credentials.toml")
}

/// Read the stored API token from credentials file, if any.
pub fn read_token() -> Option<String> {
    let path = credentials_path();
    let content = std::fs::read_to_string(path).ok()?;
    let doc: toml::Value = toml::from_str(&content).ok()?;
    doc.get("registry")?
        .get("token")?
        .as_str()
        .map(|s| s.to_string())
}

pub fn cmd_config(key: Option<&str>, value: Option<&str>) -> Result<(), String> {
    match (key, value) {
        // Show all config
        (None, _) => {
            let url = read_registry_url();
            let has_token = read_token().is_some();
            println!("registry.url = {url}");
            println!(
                "registry.token = {}",
                if has_token { "(set)" } else { "(not set)" }
            );
            println!("\nCredentials file: {}", credentials_path().display());
            Ok(())
        }
        // Get a specific key
        (Some(key), None) => match key {
            "registry.url" | "registry" => {
                println!("{}", read_registry_url());
                Ok(())
            }
            _ => Err(format!("Unknown config key: {key}\nAvailable: registry.url")),
        },
        // Set a key
        (Some(key), Some(value)) => match key {
            "registry.url" | "registry" => {
                set_registry_url(value)?;
                println!("✓ Default registry set to {value}");
                Ok(())
            }
            _ => Err(format!("Unknown config key: {key}\nAvailable: registry.url")),
        },
    }
}

/// Update the registry URL in credentials.toml, preserving the token if present.
fn set_registry_url(url: &str) -> Result<(), String> {
    let creds_path = credentials_path();
    let token = read_token().unwrap_or_default();

    if let Some(parent) = creds_path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("Failed to create config directory: {e}"))?;
    }

    let content = if token.is_empty() {
        format!("[registry]\nurl = \"{url}\"\n")
    } else {
        format!("[registry]\ntoken = \"{token}\"\nurl = \"{url}\"\n")
    };

    std::fs::write(&creds_path, &content)
        .map_err(|e| format!("Failed to write credentials: {e}"))?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(0o600);
        let _ = std::fs::set_permissions(&creds_path, perms);
    }

    Ok(())
}

/// Read the stored registry URL from credentials file, or return default.
fn read_registry_url() -> String {
    let path = credentials_path();
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return DEFAULT_REGISTRY.to_string(),
    };
    let doc: toml::Value = match toml::from_str(&content) {
        Ok(d) => d,
        Err(_) => return DEFAULT_REGISTRY.to_string(),
    };
    doc.get("registry")
        .and_then(|r| r.get("url"))
        .and_then(|u| u.as_str())
        .unwrap_or(DEFAULT_REGISTRY)
        .to_string()
}

/// Resolve the effective registry URL: explicit flag > credentials > default.
fn effective_registry(flag: Option<&str>) -> String {
    match flag {
        Some(url) => url.to_string(),
        None => read_registry_url(),
    }
}

/// Determine if a package spec looks like a git URL (has a hostname).
///
/// Heuristic: if the first path segment contains a dot, it's a hostname.
/// e.g., "github.com/user/repo" → true, "http-helpers" → false.
fn is_git_spec(spec: &str) -> bool {
    // Strip @ref suffix for the check
    let path = spec.split('@').next().unwrap_or(spec);
    path.split('/')
        .next()
        .map(|first| first.contains('.'))
        .unwrap_or(false)
}

/// Write registry package metadata to a `.sema-pkg.json` file.
fn write_pkg_meta(
    dir: &Path,
    name: &str,
    version: &str,
    registry: &str,
    checksum: &str,
) -> Result<(), String> {
    let meta = serde_json::json!({
        "source": "registry",
        "name": name,
        "version": version,
        "registry": registry,
        "checksum": checksum,
    });
    let path = dir.join(PKG_META_FILE);
    std::fs::write(&path, serde_json::to_string_pretty(&meta).unwrap())
        .map_err(|e| format!("Failed to write package metadata: {e}"))
}

/// Read registry package metadata from `.sema-pkg.json`, if present.
fn read_pkg_meta(dir: &Path) -> Option<serde_json::Value> {
    let path = dir.join(PKG_META_FILE);
    let content = std::fs::read_to_string(path).ok()?;
    serde_json::from_str(&content).ok()
}

/// Create a tarball of the given directory, excluding .git and target.
fn create_tarball(dir: &str) -> Result<Vec<u8>, String> {
    use flate2::write::GzEncoder;
    use flate2::Compression;

    let dir_path = Path::new(dir);
    let enc = GzEncoder::new(Vec::new(), Compression::default());
    let mut ar = tar::Builder::new(enc);

    let mut files = Vec::new();
    collect_files_for_tar(dir_path, &mut files)?;

    for file in &files {
        let rel = file.strip_prefix(dir_path).unwrap_or(file);
        ar.append_path_with_name(file, rel)
            .map_err(|e| format!("Failed to add {}: {e}", file.display()))?;
    }

    let enc = ar
        .into_inner()
        .map_err(|e| format!("Failed to finalize tar: {e}"))?;
    enc.finish()
        .map_err(|e| format!("Failed to finalize gzip: {e}"))
}

fn collect_files_for_tar(dir: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = std::fs::read_dir(dir)
        .map_err(|e| format!("Failed to read directory {}: {e}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("directory entry error: {e}"))?;
        let path = entry.path();
        if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            if name == ".git" || name == "target" {
                continue;
            }
        }
        if path.is_dir() {
            collect_files_for_tar(&path, files)?;
        } else {
            files.push(path);
        }
    }
    Ok(())
}

/// Extract a tarball into a destination directory.
/// Rejects path traversal, absolute paths, and symlinks.
fn extract_tarball(data: &[u8], dest: &Path) -> Result<(), String> {
    use flate2::read::GzDecoder;

    std::fs::create_dir_all(dest).map_err(|e| format!("Failed to create directory: {e}"))?;

    let decoder = GzDecoder::new(data);
    let mut archive = tar::Archive::new(decoder);

    for entry in archive
        .entries()
        .map_err(|e| format!("Invalid tar archive: {e}"))?
    {
        let mut entry = entry.map_err(|e| format!("Invalid tar entry: {e}"))?;
        let path = entry
            .path()
            .map_err(|e| format!("Invalid entry path: {e}"))?
            .into_owned();

        if path.is_absolute() {
            return Err(format!(
                "Tar entry has absolute path: {}",
                path.display()
            ));
        }

        for component in path.components() {
            if matches!(component, std::path::Component::ParentDir) {
                return Err(format!(
                    "Tar entry contains path traversal: {}",
                    path.display()
                ));
            }
        }

        let entry_type = entry.header().entry_type();
        if entry_type.is_symlink() || entry_type.is_hard_link() {
            return Err(format!(
                "Tar entry is a symlink/hardlink (rejected): {}",
                path.display()
            ));
        }

        let full_path = dest.join(&path);
        if entry_type.is_dir() {
            std::fs::create_dir_all(&full_path)
                .map_err(|e| format!("Failed to create dir {}: {e}", full_path.display()))?;
        } else if entry_type.is_file() {
            if let Some(parent) = full_path.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| format!("Failed to create parent dir: {e}"))?;
            }
            entry
                .unpack(&full_path)
                .map_err(|e| format!("Failed to extract {}: {e}", path.display()))?;
        }
    }

    Ok(())
}

/// Install a package from the registry.
fn registry_install(
    name: &str,
    version: &str,
    registry_url: &str,
) -> Result<(), String> {
    let token = read_token();
    let client = reqwest::blocking::Client::new();
    let base = registry_url.trim_end_matches('/');

    // Download tarball
    let url = format!("{base}/api/v1/packages/{name}/{version}/download");
    let mut req = client.get(&url);
    if let Some(ref t) = token {
        req = req.header("Authorization", format!("Bearer {t}"));
    }

    let resp = req.send().map_err(|e| format!("Download failed: {e}"))?;
    if !resp.status().is_success() {
        let status = resp.status();
        let body: serde_json::Value = resp.json().unwrap_or_default();
        let error = body
            .get("error")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown error");
        return Err(format!("Download failed ({status}): {error}"));
    }

    let tarball = resp
        .bytes()
        .map_err(|e| format!("Failed to read response: {e}"))?;

    // Compute checksum
    use sha2::Digest;
    let checksum = format!("{:x}", sha2::Sha256::digest(&tarball));

    // Extract to packages dir
    let pkg_dir = packages_dir();
    let dest = pkg_dir.join(name);
    if dest.exists() {
        std::fs::remove_dir_all(&dest)
            .map_err(|e| format!("Failed to remove old package: {e}"))?;
    }
    extract_tarball(&tarball, &dest)?;

    // Write metadata
    write_pkg_meta(&dest, name, version, registry_url, &checksum)?;

    Ok(())
}

/// Fetch package info from the registry.
fn registry_package_info(
    name: &str,
    registry_url: &str,
) -> Result<serde_json::Value, String> {
    let client = reqwest::blocking::Client::new();
    let base = registry_url.trim_end_matches('/');
    let url = format!("{base}/api/v1/packages/{name}");

    let mut req = client.get(&url);
    if let Some(t) = read_token() {
        req = req.header("Authorization", format!("Bearer {t}"));
    }

    let resp = req.send().map_err(|e| format!("Request failed: {e}"))?;
    if !resp.status().is_success() {
        let status = resp.status();
        let body: serde_json::Value = resp.json().unwrap_or_default();
        let error = body
            .get("error")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown error");
        return Err(format!("Failed to fetch package ({status}): {error}"));
    }

    resp.json()
        .map_err(|e| format!("Failed to parse response: {e}"))
}

/// Get the latest non-yanked version from a package info response.
fn latest_version(info: &serde_json::Value) -> Option<String> {
    info.get("versions")?
        .as_array()?
        .iter()
        .filter(|v| !v.get("yanked").and_then(|y| y.as_bool()).unwrap_or(false))
        .filter_map(|v| v.get("version").and_then(|s| s.as_str()))
        .next()
        .map(|s| s.to_string())
}

fn validate_version(version: &str) -> Result<semver::Version, String> {
    semver::Version::parse(version)
        .map_err(|_| format!("Invalid semver version: {version} (expected X.Y.Z[-prerelease][+build])"))
}

pub fn cmd_publish(registry: Option<&str>) -> Result<(), String> {
    let toml_path = Path::new("sema.toml");
    if !toml_path.exists() {
        return Err("No sema.toml found. Run `sema pkg init` first.".into());
    }

    let content = std::fs::read_to_string(toml_path)
        .map_err(|e| format!("Failed to read sema.toml: {e}"))?;
    let doc: toml::Value =
        toml::from_str(&content).map_err(|e| format!("Failed to parse sema.toml: {e}"))?;

    let pkg = doc
        .get("package")
        .ok_or("sema.toml missing [package] section")?;
    let name = pkg
        .get("name")
        .and_then(|v| v.as_str())
        .ok_or("sema.toml [package] missing 'name'")?;
    let version = pkg
        .get("version")
        .and_then(|v| v.as_str())
        .ok_or("sema.toml [package] missing 'version'")?;

    validate_version(version)?;

    let token =
        read_token().ok_or("Not logged in. Run `sema pkg login --token <token>` first.")?;
    let registry_url = effective_registry(registry);
    let base = registry_url.trim_end_matches('/');

    // Create tarball
    println!("Packaging...");
    let tarball = create_tarball(".")?;
    println!("  {} bytes compressed", tarball.len());

    // Build metadata
    let metadata = serde_json::json!({
        "description": pkg.get("description").and_then(|v| v.as_str()).unwrap_or(""),
        "repository_url": pkg.get("repository").and_then(|v| v.as_str()),
        "sema_version_req": pkg.get("sema_version_req").and_then(|v| v.as_str()),
    });

    // Upload
    let url = format!("{base}/api/v1/packages/{name}/{version}");
    let form = reqwest::blocking::multipart::Form::new()
        .part(
            "tarball",
            reqwest::blocking::multipart::Part::bytes(tarball)
                .file_name("package.tar.gz")
                .mime_str("application/gzip")
                .unwrap(),
        )
        .part(
            "metadata",
            reqwest::blocking::multipart::Part::text(metadata.to_string()),
        );

    let client = reqwest::blocking::Client::new();
    let resp = client
        .put(&url)
        .header("Authorization", format!("Bearer {token}"))
        .multipart(form)
        .send()
        .map_err(|e| format!("Upload failed: {e}"))?;

    if resp.status().is_success() {
        let body: serde_json::Value = resp
            .json()
            .map_err(|e| format!("Failed to parse response: {e}"))?;
        let checksum = body
            .get("checksum")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown");
        let size = body.get("size").and_then(|v| v.as_u64()).unwrap_or(0);
        println!("✓ Published {name}@{version} ({size} bytes, sha256:{checksum})");
        Ok(())
    } else {
        let status = resp.status();
        let body: serde_json::Value = resp.json().unwrap_or_default();
        let error = body
            .get("error")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown error");
        Err(format!("Publish failed ({status}): {error}"))
    }
}

pub fn cmd_search(query: &str, registry: Option<&str>) -> Result<(), String> {
    let registry_url = effective_registry(registry);
    let base = registry_url.trim_end_matches('/');
    let url = format!("{base}/api/v1/search?q={}", urlencoded(query));

    let client = reqwest::blocking::Client::new();
    let resp = client
        .get(&url)
        .send()
        .map_err(|e| format!("Search failed: {e}"))?;

    if !resp.status().is_success() {
        let status = resp.status();
        return Err(format!("Search failed ({status})"));
    }

    let body: serde_json::Value = resp
        .json()
        .map_err(|e| format!("Failed to parse response: {e}"))?;

    let packages = body
        .get("packages")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    if packages.is_empty() {
        println!("No packages found for '{query}'.");
        return Ok(());
    }

    let total = body.get("total").and_then(|v| v.as_i64()).unwrap_or(0);
    println!("Found {total} package{}:\n", if total == 1 { "" } else { "s" });

    for pkg in &packages {
        let name = pkg.get("name").and_then(|v| v.as_str()).unwrap_or("?");
        let desc = pkg
            .get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("");
        if desc.is_empty() {
            println!("  {name}");
        } else {
            println!("  {name} — {desc}");
        }
    }

    Ok(())
}

pub fn cmd_yank(spec: &str, registry: Option<&str>) -> Result<(), String> {
    let (name, version) = spec
        .rsplit_once('@')
        .ok_or("Expected format: <package>@<version> (e.g., my-package@0.1.0)")?;

    let token =
        read_token().ok_or("Not logged in. Run `sema pkg login --token <token>` first.")?;
    let registry_url = effective_registry(registry);
    let base = registry_url.trim_end_matches('/');

    let url = format!("{base}/api/v1/packages/{name}/{version}/yank");
    let client = reqwest::blocking::Client::new();
    let resp = client
        .post(&url)
        .header("Authorization", format!("Bearer {token}"))
        .send()
        .map_err(|e| format!("Yank failed: {e}"))?;

    if resp.status().is_success() {
        println!("✓ Yanked {name}@{version}");
        Ok(())
    } else {
        let status = resp.status();
        let body: serde_json::Value = resp.json().unwrap_or_default();
        let error = body
            .get("error")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown error");
        Err(format!("Yank failed ({status}): {error}"))
    }
}

pub fn cmd_info(name: &str, registry: Option<&str>) -> Result<(), String> {
    let registry_url = effective_registry(registry);
    let info = registry_package_info(name, &registry_url)?;

    let pkg = info.get("package").unwrap_or(&info);
    let pkg_name = pkg.get("name").and_then(|v| v.as_str()).unwrap_or(name);
    let desc = pkg
        .get("description")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let repo = pkg
        .get("repository_url")
        .and_then(|v| v.as_str())
        .unwrap_or("");

    println!("{pkg_name}");
    if !desc.is_empty() {
        println!("  {desc}");
    }
    if !repo.is_empty() {
        println!("  repo: {repo}");
    }

    let owners = info
        .get("owners")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    if !owners.is_empty() {
        let names: Vec<&str> = owners
            .iter()
            .filter_map(|v| v.as_str())
            .collect();
        println!("  owners: {}", names.join(", "));
    }

    let versions = info
        .get("versions")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    if versions.is_empty() {
        println!("\n  No versions published.");
    } else {
        println!("\n  Versions:");
        for v in &versions {
            let ver = v.get("version").and_then(|s| s.as_str()).unwrap_or("?");
            let yanked = v.get("yanked").and_then(|b| b.as_bool()).unwrap_or(false);
            let size = v.get("size_bytes").and_then(|n| n.as_i64()).unwrap_or(0);
            let published = v
                .get("published_at")
                .and_then(|s| s.as_str())
                .unwrap_or("");
            let yank_mark = if yanked { " (yanked)" } else { "" };
            println!("    {ver} — {size} bytes, {published}{yank_mark}");
        }
    }

    Ok(())
}

/// Minimal URL encoding for query parameters.
fn urlencoded(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            ' ' => result.push_str("%20"),
            '&' => result.push_str("%26"),
            '=' => result.push_str("%3D"),
            '#' => result.push_str("%23"),
            '+' => result.push_str("%2B"),
            '%' => result.push_str("%25"),
            _ => result.push(c),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn tmpdir(name: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!(
            "sema-pkg-test-{name}-{}", std::process::id()
        ));
        let _ = fs::remove_dir_all(&d);
        fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn add_dep_preserves_comments() {
        let dir = tmpdir("add-comments");
        let toml_path = dir.join("sema.toml");
        let input = "# Project config\n[package]\nname = \"my-app\"\n\n# Dependencies\n[deps]\n\"github.com/test/foo\" = \"v1.0.0\"\n";
        fs::write(&toml_path, input).unwrap();

        add_dep_to_toml(&toml_path, "github.com/test/bar", "v2.0.0").unwrap();

        let output = fs::read_to_string(&toml_path).unwrap();
        let doc: toml_edit::DocumentMut = output.parse().unwrap();

        let deps = doc["deps"].as_table().expect("deps table must exist");
        assert_eq!(deps.get("github.com/test/foo").and_then(|v| v.as_str()), Some("v1.0.0"), "existing dep preserved");
        assert_eq!(deps.get("github.com/test/bar").and_then(|v| v.as_str()), Some("v2.0.0"), "new dep added");

        // Comments survived
        assert!(output.contains("# Project config"), "top comment lost");
        assert!(output.contains("# Dependencies"), "deps comment lost");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn add_dep_creates_deps_section_if_missing() {
        let dir = tmpdir("add-no-deps");
        let toml_path = dir.join("sema.toml");
        fs::write(&toml_path, "[package]\nname = \"bare\"\n").unwrap();

        let changed = add_dep_to_toml(&toml_path, "github.com/a/b", "v1.0.0").unwrap();
        assert!(changed);

        let output = fs::read_to_string(&toml_path).unwrap();
        let doc: toml_edit::DocumentMut = output.parse().unwrap();
        assert_eq!(doc["deps"]["github.com/a/b"].as_str(), Some("v1.0.0"));
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn add_dep_updates_existing_version() {
        let dir = tmpdir("add-update");
        let toml_path = dir.join("sema.toml");
        fs::write(&toml_path, "[deps]\n\"github.com/a/b\" = \"v1.0.0\"\n").unwrap();

        let changed = add_dep_to_toml(&toml_path, "github.com/a/b", "v2.0.0").unwrap();
        assert!(changed);

        let output = fs::read_to_string(&toml_path).unwrap();
        let doc: toml_edit::DocumentMut = output.parse().unwrap();
        assert_eq!(doc["deps"]["github.com/a/b"].as_str(), Some("v2.0.0"));
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn add_dep_returns_false_if_already_set() {
        let dir = tmpdir("add-noop");
        let toml_path = dir.join("sema.toml");
        fs::write(&toml_path, "[deps]\n\"github.com/a/b\" = \"v1.0.0\"\n").unwrap();

        let changed = add_dep_to_toml(&toml_path, "github.com/a/b", "v1.0.0").unwrap();
        assert!(!changed);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn remove_dep_removes_entry_preserves_others() {
        let dir = tmpdir("remove");
        let toml_path = dir.join("sema.toml");
        fs::write(&toml_path, "[deps]\n\"github.com/a/b\" = \"v1.0.0\"\n\"github.com/c/d\" = \"v2.0.0\"\n").unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/a/b").unwrap();
        assert!(removed);

        let output = fs::read_to_string(&toml_path).unwrap();
        let doc: toml_edit::DocumentMut = output.parse().unwrap();
        let deps = doc["deps"].as_table().unwrap();
        assert!(deps.get("github.com/a/b").is_none(), "removed dep should be gone");
        assert_eq!(deps.get("github.com/c/d").and_then(|v| v.as_str()), Some("v2.0.0"), "other dep preserved");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn remove_dep_returns_false_if_not_found() {
        let dir = tmpdir("remove-noop");
        let toml_path = dir.join("sema.toml");
        fs::write(&toml_path, "[deps]\n\"github.com/a/b\" = \"v1.0.0\"\n").unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/x/y").unwrap();
        assert!(!removed);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_find_all_packages_empty() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let packages = find_all_packages(&tmp);
        assert!(packages.is_empty());

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_find_all_packages_finds_package_sema() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);

        let pkg = tmp.join("github.com/user/repo");
        std::fs::create_dir_all(&pkg).unwrap();
        std::fs::write(pkg.join("package.sema"), "(define x 1)").unwrap();

        let packages = find_all_packages(&tmp);
        assert_eq!(packages.len(), 1);
        assert!(packages[0].ends_with("github.com/user/repo"));

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_find_all_packages_finds_sema_toml() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find3-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);

        let pkg = tmp.join("github.com/user/lib");
        std::fs::create_dir_all(&pkg).unwrap();
        std::fs::write(pkg.join("sema.toml"), "[package]\nname = \"lib\"\n").unwrap();

        let packages = find_all_packages(&tmp);
        assert_eq!(packages.len(), 1);

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_find_package_dir_by_full_path() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find4-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);

        let pkg = tmp.join("github.com/user/repo");
        std::fs::create_dir_all(&pkg).unwrap();
        std::fs::write(pkg.join("package.sema"), "(define x 1)").unwrap();

        let found = find_package_dir(&tmp, "github.com/user/repo");
        assert!(found.is_some());
        assert!(found.unwrap().ends_with("github.com/user/repo"));

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_find_package_dir_by_name() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find5-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);

        let pkg = tmp.join("github.com/user/mylib");
        std::fs::create_dir_all(&pkg).unwrap();
        std::fs::write(pkg.join("package.sema"), "(define x 1)").unwrap();

        let found = find_package_dir(&tmp, "mylib");
        assert!(found.is_some());

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_find_package_dir_not_found() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find6-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let found = find_package_dir(&tmp, "nonexistent");
        assert!(found.is_none());

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_run_git_checkout_ref_not_as_path() {
        // Verify that `git checkout <ref>` (without `--`) correctly switches
        // to a branch/tag. With `--`, git would interpret the ref as a file path.
        let tmp = std::env::temp_dir().join(format!("sema-pkg-checkout-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        // Init a repo and create a branch
        run_git(Some(&tmp), &["init"]).unwrap();
        run_git(Some(&tmp), &["checkout", "-b", "main"]).unwrap();
        std::fs::write(tmp.join("file.txt"), "hello").unwrap();
        run_git(Some(&tmp), &["add", "."]).unwrap();
        run_git(Some(&tmp), &["commit", "-m", "init"]).unwrap();
        run_git(Some(&tmp), &["branch", "test-branch"]).unwrap();

        // Checkout should succeed for a branch name
        let result = run_git(Some(&tmp), &["checkout", "test-branch"]);
        assert!(result.is_ok(), "checkout branch failed: {result:?}");

        // Verify we're on the right branch
        let branch = run_git(Some(&tmp), &["rev-parse", "--abbrev-ref", "HEAD"]).unwrap();
        assert_eq!(branch, "test-branch");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_cmd_add_rejects_traversal() {
        let result = cmd_add("github.com/../../etc/passwd", None);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("path traversal"), "got: {err}");
    }

    #[test]
    fn test_cmd_add_rejects_scheme() {
        let result = cmd_add("https://github.com/user/repo", None);
        assert!(result.is_err());
    }

    #[test]
    fn test_cmd_init_creates_sema_toml() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-init-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        // Run cmd_init in the temp directory
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(&tmp).unwrap();
        let result = cmd_init();
        std::env::set_current_dir(original_dir).unwrap();

        assert!(result.is_ok());
        let content = std::fs::read_to_string(tmp.join("sema.toml")).unwrap();
        assert!(
            content.contains("[package]"),
            "should use [package], got: {content}"
        );
        assert!(
            content.contains("version = \"0.1.0\""),
            "should have version"
        );
        assert!(content.contains("[deps]"), "should have [deps] section");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_add_dep_to_toml_new_entry() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-adddep1-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            "[package]\nname = \"test\"\nversion = \"0.1.0\"\n\n[deps]\n",
        )
        .unwrap();

        let added = add_dep_to_toml(&toml_path, "github.com/user/repo", "v1.0.0").unwrap();
        assert!(added, "should have added the dep");

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(
            content.contains("\"github.com/user/repo\" = \"v1.0.0\""),
            "dep should be present: {content}"
        );
        assert!(content.contains("[package]"), "package section preserved");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_add_dep_to_toml_updates_existing() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-adddep2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            "[deps]\n\"github.com/user/repo\" = \"v1.0.0\"\n",
        )
        .unwrap();

        let added = add_dep_to_toml(&toml_path, "github.com/user/repo", "v2.0.0").unwrap();
        assert!(added, "should have updated the dep");

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(
            content.contains("\"github.com/user/repo\" = \"v2.0.0\""),
            "dep should be updated: {content}"
        );
        assert!(
            !content.contains("v1.0.0"),
            "old version should be gone: {content}"
        );

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_add_dep_to_toml_already_up_to_date() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-adddep3-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            "[deps]\n\"github.com/user/repo\" = \"v1.0.0\"\n",
        )
        .unwrap();

        let added = add_dep_to_toml(&toml_path, "github.com/user/repo", "v1.0.0").unwrap();
        assert!(!added, "should not change anything");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_add_dep_to_toml_no_deps_section() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-adddep4-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(&toml_path, "[package]\nname = \"test\"\n").unwrap();

        let added = add_dep_to_toml(&toml_path, "github.com/user/repo", "main").unwrap();
        assert!(added, "should have added dep and section");

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(content.contains("[deps]"), "should have [deps]: {content}");
        assert!(
            content.contains("\"github.com/user/repo\" = \"main\""),
            "dep should be present: {content}"
        );

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_add_dep_to_toml_preserves_existing_deps() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-adddep5-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            "[deps]\n\"github.com/user/existing\" = \"v1.0.0\"\n",
        )
        .unwrap();

        let added = add_dep_to_toml(&toml_path, "github.com/user/new", "v2.0.0").unwrap();
        assert!(added);

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(
            content.contains("\"github.com/user/existing\" = \"v1.0.0\""),
            "existing dep preserved: {content}"
        );
        assert!(
            content.contains("\"github.com/user/new\" = \"v2.0.0\""),
            "new dep added: {content}"
        );

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_remove_dep_from_toml_quoted_key() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-rmdep1-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            r#"[package]
name = "myproject"
version = "0.1.0"

[deps]
"github.com/user/repo" = "v1.0.0"
"github.com/user/other" = "main"
"#,
        )
        .unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/user/repo").unwrap();
        assert!(removed, "should have removed the dep");

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(
            !content.contains("github.com/user/repo"),
            "removed dep should be gone: {content}"
        );
        assert!(
            content.contains("github.com/user/other"),
            "other dep should remain: {content}"
        );
        assert!(content.contains("[package]"), "package section preserved");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_remove_dep_from_toml_quoted_key_with_slashes() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-rmdep2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(&toml_path, "[deps]\n\"github.com/user/repo\" = \"v1.0.0\"\n").unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/user/repo").unwrap();
        assert!(removed);

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(
            !content.contains("github.com/user/repo"),
            "dep should be gone: {content}"
        );
        assert!(content.contains("[deps]"), "section header preserved");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_remove_dep_from_toml_not_found() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-rmdep3-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            "[deps]\n\"github.com/user/other\" = \"v1.0.0\"\n",
        )
        .unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/user/repo").unwrap();
        assert!(!removed, "should not have removed anything");

        // File should be unchanged
        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(content.contains("github.com/user/other"));

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_remove_dep_from_toml_no_deps_section() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-rmdep4-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(&toml_path, "[package]\nname = \"test\"\n").unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/user/repo").unwrap();
        assert!(!removed);

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_remove_dep_from_toml_preserves_comments() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-rmdep5-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(
            &toml_path,
            r#"[package]
name = "myproject"

[deps]
# My core dependency
"github.com/user/core" = "v2.0.0"
"github.com/user/remove-me" = "v1.0.0"
"#,
        )
        .unwrap();

        let removed = remove_dep_from_toml(&toml_path, "github.com/user/remove-me").unwrap();
        assert!(removed);

        let content = std::fs::read_to_string(&toml_path).unwrap();
        assert!(
            content.contains("# My core dependency"),
            "comment should be preserved: {content}"
        );
        assert!(content.contains("github.com/user/core"));
        assert!(!content.contains("remove-me"));

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_cmd_init_rejects_existing() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-init2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();
        std::fs::write(tmp.join("sema.toml"), "existing").unwrap();

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(&tmp).unwrap();
        let result = cmd_init();
        std::env::set_current_dir(original_dir).unwrap();

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("already exists"));

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_is_git_spec() {
        assert!(is_git_spec("github.com/user/repo"));
        assert!(is_git_spec("github.com/user/repo@v1.0"));
        assert!(is_git_spec("gitlab.com/org/lib@main"));
        assert!(!is_git_spec("http-helpers"));
        assert!(!is_git_spec("http-helpers@1.0.0"));
        assert!(!is_git_spec("my-package"));
    }

    #[test]
    fn test_write_and_read_pkg_meta() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-meta-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        write_pkg_meta(&tmp, "test-pkg", "1.0.0", "https://registry.example.com", "abc123")
            .unwrap();

        let meta = read_pkg_meta(&tmp);
        assert!(meta.is_some());
        let meta = meta.unwrap();
        assert_eq!(meta["source"], "registry");
        assert_eq!(meta["name"], "test-pkg");
        assert_eq!(meta["version"], "1.0.0");
        assert_eq!(meta["registry"], "https://registry.example.com");
        assert_eq!(meta["checksum"], "abc123");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_read_pkg_meta_missing() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-meta2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        assert!(read_pkg_meta(&tmp).is_none());

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_urlencoded() {
        assert_eq!(urlencoded("hello world"), "hello%20world");
        assert_eq!(urlencoded("a&b=c"), "a%26b%3Dc");
        assert_eq!(urlencoded("simple"), "simple");
    }

    #[test]
    fn test_cmd_yank_requires_at_sign() {
        let result = cmd_yank("my-package", None);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Expected format"));
    }

    #[test]
    fn validate_version_accepts_standard() {
        assert!(validate_version("1.0.0").is_ok());
        assert!(validate_version("0.1.0").is_ok());
        assert!(validate_version("10.20.30").is_ok());
    }

    #[test]
    fn validate_version_accepts_prerelease() {
        assert!(validate_version("1.0.0-alpha.1").is_ok());
        assert!(validate_version("1.0.0-beta").is_ok());
        assert!(validate_version("1.0.0-rc.1").is_ok());
    }

    #[test]
    fn validate_version_accepts_build_metadata() {
        assert!(validate_version("1.0.0+build.123").is_ok());
        assert!(validate_version("1.0.0-alpha+001").is_ok());
    }

    #[test]
    fn validate_version_rejects_invalid() {
        assert!(validate_version("not-a-version").is_err());
        assert!(validate_version("1.0").is_err());
        assert!(validate_version("").is_err());
        assert!(validate_version("v1.0.0").is_err());
    }

    /// Helper: build a tar.gz with a raw path written directly into the header,
    /// bypassing the `tar` crate's own path validation.
    fn make_malicious_tarball(raw_path: &str, data: &[u8]) -> Vec<u8> {
        use flate2::write::GzEncoder;
        use flate2::Compression;

        let mut gz = GzEncoder::new(Vec::new(), Compression::default());

        // Build a 512-byte tar header manually
        let mut header_block = [0u8; 512];
        let path_bytes = raw_path.as_bytes();
        header_block[..path_bytes.len()].copy_from_slice(path_bytes);
        // mode (octal ASCII at offset 100, 8 bytes)
        header_block[100..107].copy_from_slice(b"0000644");
        // size (octal ASCII at offset 124, 12 bytes)
        let size_str = format!("{:011o}", data.len());
        header_block[124..135].copy_from_slice(size_str.as_bytes());
        // typeflag '0' = regular file at offset 156
        header_block[156] = b'0';
        // magic "ustar\0" at offset 257
        header_block[257..263].copy_from_slice(b"ustar\0");
        // version "00" at offset 263
        header_block[263..265].copy_from_slice(b"00");
        // Compute checksum (sum of all bytes, treating checksum field as spaces)
        header_block[148..156].copy_from_slice(b"        ");
        let cksum: u32 = header_block.iter().map(|&b| b as u32).sum();
        let cksum_str = format!("{:06o}\0 ", cksum);
        header_block[148..156].copy_from_slice(cksum_str.as_bytes());

        gz.write_all(&header_block).unwrap();
        gz.write_all(data).unwrap();
        // Pad to 512-byte boundary
        let padding = 512 - (data.len() % 512);
        if padding < 512 {
            gz.write_all(&vec![0u8; padding]).unwrap();
        }
        // Two zero blocks = end of archive
        gz.write_all(&[0u8; 1024]).unwrap();
        gz.finish().unwrap()
    }

    #[test]
    fn extract_tarball_rejects_path_traversal() {
        let malicious = make_malicious_tarball("../pwned.txt", b"pwned!");

        let dir = tmpdir("traversal");
        let dest = dir.join("extracted");
        let parent_file = dir.join("pwned.txt");

        let result = extract_tarball(&malicious, &dest);
        assert!(result.is_err(), "path traversal should be rejected");
        assert!(!parent_file.exists(), "file must NOT be written outside dest");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn extract_tarball_rejects_absolute_paths() {
        let malicious = make_malicious_tarball("/tmp/pwned.txt", b"pwned!");

        let dir = tmpdir("abs-path");
        let dest = dir.join("extracted");

        let result = extract_tarball(&malicious, &dest);
        assert!(result.is_err(), "absolute paths should be rejected");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn extract_tarball_extracts_valid_archive() {
        use flate2::write::GzEncoder;
        use flate2::Compression;

        let mut gz = GzEncoder::new(Vec::new(), Compression::default());
        {
            let mut ar = tar::Builder::new(&mut gz);
            let data = b"(define x 42)";
            let mut header = tar::Header::new_gnu();
            header.set_path("package.sema").unwrap();
            header.set_size(data.len() as u64);
            header.set_mode(0o644);
            header.set_cksum();
            ar.append(&header, &data[..]).unwrap();
            ar.finish().unwrap();
        }
        let tarball = gz.finish().unwrap();

        let dir = tmpdir("valid-tar");
        let dest = dir.join("extracted");

        extract_tarball(&tarball, &dest).unwrap();
        let content = fs::read_to_string(dest.join("package.sema")).unwrap();
        assert_eq!(content, "(define x 42)");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn extract_tarball_rejects_symlinks() {
        use flate2::write::GzEncoder;
        use flate2::Compression;

        let mut gz = GzEncoder::new(Vec::new(), Compression::default());
        {
            let mut ar = tar::Builder::new(&mut gz);
            let mut header = tar::Header::new_gnu();
            header.set_entry_type(tar::EntryType::Symlink);
            header.set_path("evil-link").unwrap();
            header.set_link_name("/etc/passwd").unwrap();
            header.set_size(0);
            header.set_cksum();
            ar.append(&header, &[][..]).unwrap();
            ar.finish().unwrap();
        }
        let malicious = gz.finish().unwrap();

        let dir = tmpdir("symlink");
        let dest = dir.join("extracted");

        let result = extract_tarball(&malicious, &dest);
        assert!(result.is_err(), "symlinks should be rejected");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn extract_tarball_handles_nested_directories() {
        use flate2::write::GzEncoder;
        use flate2::Compression;

        let mut gz = GzEncoder::new(Vec::new(), Compression::default());
        {
            let mut ar = tar::Builder::new(&mut gz);

            let data = b"(define deep 1)";
            let mut header = tar::Header::new_gnu();
            header.set_path("src/lib/deep.sema").unwrap();
            header.set_size(data.len() as u64);
            header.set_mode(0o644);
            header.set_cksum();
            ar.append(&header, &data[..]).unwrap();
            ar.finish().unwrap();
        }
        let tarball = gz.finish().unwrap();

        let dir = tmpdir("nested-dirs");
        let dest = dir.join("extracted");

        extract_tarball(&tarball, &dest).unwrap();
        let content = fs::read_to_string(dest.join("src/lib/deep.sema")).unwrap();
        assert_eq!(content, "(define deep 1)");
        let _ = fs::remove_dir_all(&dir);
    }
}
