use std::path::PathBuf;

use crate::error::SemaError;
use crate::home::sema_home;

/// Returns the packages directory: `sema_home()/packages/`.
pub fn packages_dir() -> PathBuf {
    sema_home().join("packages")
}

/// Determines if an import spec is a package path vs a file path.
///
/// Package paths contain `/` but don't start with `./` or `../`,
/// don't end with `.sema`, and aren't absolute paths.
pub fn is_package_import(spec: &str) -> bool {
    spec.contains('/')
        && !spec.starts_with("./")
        && !spec.starts_with("../")
        && !spec.ends_with(".sema")
        && !spec.starts_with('/')
}

/// Resolves a package spec to a filesystem path.
///
/// Resolution order:
/// 1. `~/.sema/packages/<spec>.sema` (sub-module import)
/// 2. `~/.sema/packages/<spec>/sema.toml` → custom entrypoint
/// 3. `~/.sema/packages/<spec>/mod.sema` (default entrypoint)
pub fn resolve_package_import(spec: &str) -> Result<PathBuf, SemaError> {
    resolve_package_import_in(spec, &packages_dir())
}

/// Resolves a package spec against a given packages directory.
pub fn resolve_package_import_in(spec: &str, base: &std::path::Path) -> Result<PathBuf, SemaError> {

    // 1. Direct file: <packages>/<spec>.sema
    let direct = base.join(format!("{spec}.sema"));
    if direct.is_file() {
        return Ok(direct);
    }

    let pkg_dir = base.join(spec);

    // 2. sema.toml with custom entrypoint
    let toml_path = pkg_dir.join("sema.toml");
    if toml_path.is_file() {
        if let Some(entrypoint) = parse_entrypoint(&toml_path) {
            let entry = pkg_dir.join(entrypoint);
            if entry.is_file() {
                return Ok(entry);
            }
        }
    }

    // 3. Default entrypoint: mod.sema
    let mod_file = pkg_dir.join("mod.sema");
    if mod_file.is_file() {
        return Ok(mod_file);
    }

    Err(SemaError::eval(format!("package not found: {spec}"))
        .with_hint(format!("Run: sema pkg get {spec}")))
}

/// Parse `entrypoint = "..."` from a sema.toml file using simple line parsing.
fn parse_entrypoint(path: &PathBuf) -> Option<String> {
    let contents = std::fs::read_to_string(path).ok()?;
    for line in contents.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("entrypoint") {
            let rest = rest.trim_start();
            if let Some(rest) = rest.strip_prefix('=') {
                let rest = rest.trim();
                // Strip surrounding quotes
                let val = rest
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .or_else(|| rest.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')));
                if let Some(v) = val {
                    return Some(v.to_string());
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    use std::sync::atomic::{AtomicU64, Ordering};

    static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

    /// Create a unique temp packages directory for testing.
    fn temp_packages_dir() -> PathBuf {
        let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        let dir = std::env::temp_dir().join(format!(
            "sema-resolve-test-{}-{}",
            std::process::id(),
            id
        ));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn test_is_package_import() {
        // Package imports
        assert!(is_package_import("github.com/user/repo"));
        assert!(is_package_import("github.com/user/repo/sub"));

        // File imports
        assert!(!is_package_import("./utils.sema"));
        assert!(!is_package_import("../lib/utils.sema"));
        assert!(!is_package_import("utils.sema"));
        assert!(!is_package_import("/absolute/path.sema"));
        assert!(!is_package_import("single-word"));
        assert!(!is_package_import("github.com/user/repo.sema"));
    }

    #[test]
    fn test_resolve_direct_file() {
        let base = temp_packages_dir();
        let pkg_path = base.join("github.com/user");
        fs::create_dir_all(&pkg_path).unwrap();
        fs::write(pkg_path.join("repo.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_path.join("repo.sema"));
    }

    #[test]
    fn test_resolve_mod_sema() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(pkg_dir.join("mod.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_dir.join("mod.sema"));
    }

    #[test]
    fn test_resolve_custom_entrypoint() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(pkg_dir.join("sema.toml"), "entrypoint = \"lib.sema\"\n").unwrap();
        fs::write(pkg_dir.join("lib.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_dir.join("lib.sema"));
    }

    #[test]
    fn test_resolve_custom_entrypoint_single_quotes() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(pkg_dir.join("sema.toml"), "entrypoint = 'main.sema'\n").unwrap();
        fs::write(pkg_dir.join("main.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_dir.join("main.sema"));
    }

    #[test]
    fn test_resolve_not_found() {
        let base = temp_packages_dir();
        let err = resolve_package_import_in("github.com/user/repo", &base).unwrap_err();
        assert!(err.to_string().contains("package not found"));
        assert_eq!(
            err.hint(),
            Some("Run: sema pkg get github.com/user/repo")
        );
    }

    #[test]
    fn test_resolve_priority_direct_over_mod() {
        let base = temp_packages_dir();
        let parent = base.join("github.com/user");
        fs::create_dir_all(&parent).unwrap();
        fs::write(parent.join("repo.sema"), "direct").unwrap();

        let pkg_dir = parent.join("repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(pkg_dir.join("mod.sema"), "mod").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, parent.join("repo.sema"));
    }
}
