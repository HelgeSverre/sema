use std::path::{Path, PathBuf};

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
/// Also rejects URLs with schemes (`://`), backslashes, and colons.
pub fn is_package_import(spec: &str) -> bool {
    spec.contains('/')
        && !spec.starts_with("./")
        && !spec.starts_with("../")
        && !spec.ends_with(".sema")
        && !spec.starts_with('/')
        && !spec.contains("://")
        && !spec.contains('\\')
        && !spec.contains(':')
}

/// Validate that a package spec contains no path traversal or dangerous segments.
///
/// Rejects: `..` segments, empty segments, schemes, backslashes, colons, NUL bytes.
pub fn validate_package_spec(spec: &str) -> Result<(), SemaError> {
    if spec.contains("://") {
        return Err(SemaError::eval(format!(
            "invalid package spec: URL schemes not allowed: {spec}"
        ))
        .with_hint("Use bare host/path format, e.g.: github.com/user/repo"));
    }
    if spec.starts_with('/') {
        return Err(SemaError::eval(format!(
            "invalid package spec: absolute paths not allowed: {spec}"
        ))
        .with_hint("Use bare host/path format, e.g.: github.com/user/repo"));
    }
    if spec.contains('\\') {
        return Err(SemaError::eval(format!(
            "invalid package spec: backslashes not allowed: {spec}"
        )));
    }
    if spec.contains(':') {
        return Err(SemaError::eval(format!(
            "invalid package spec: colons not allowed: {spec}"
        )));
    }
    if spec.contains('\0') {
        return Err(SemaError::eval(
            "invalid package spec: NUL byte not allowed".to_string(),
        ));
    }
    for segment in spec.split('/') {
        if segment.is_empty() || segment == "." || segment == ".." {
            return Err(SemaError::eval(format!(
                "invalid package spec: path traversal not allowed: {spec}"
            )));
        }
    }
    Ok(())
}

/// A validated package path (e.g., "github.com/user/repo").
///
/// Construction via `parse()` ensures the path has no traversal,
/// schemes, backslashes, colons, or empty segments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackagePath(String);

impl PackagePath {
    pub fn parse(s: &str) -> Result<Self, SemaError> {
        validate_package_spec(s)?;
        Ok(Self(s.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for PackagePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// A parsed package spec: validated path + git ref (e.g., "github.com/user/repo@v1.0").
///
/// The git ref defaults to "main" when no `@ref` suffix is present.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageSpec {
    pub path: PackagePath,
    pub git_ref: String,
}

impl PackageSpec {
    pub fn parse(spec: &str) -> Result<Self, SemaError> {
        let (path_str, git_ref) = if let Some((p, r)) = spec.rsplit_once('@') {
            (p, r)
        } else {
            (spec, "main")
        };

        let path = PackagePath::parse(path_str)?;

        if git_ref.is_empty() {
            return Err(
                SemaError::eval(format!("invalid package spec: empty git ref: {spec}"))
                    .with_hint("Provide a ref after @, e.g.: github.com/user/repo@v1.0"),
            );
        }
        if git_ref.contains('\0') {
            return Err(SemaError::eval(
                "invalid package spec: NUL byte in git ref".to_string(),
            ));
        }

        Ok(Self {
            path,
            git_ref: git_ref.to_string(),
        })
    }

    pub fn clone_url(&self) -> String {
        format!("https://{}.git", self.path.as_str())
    }

    pub fn dest_dir(&self, packages_dir: &Path) -> PathBuf {
        packages_dir.join(self.path.as_str())
    }
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
pub fn resolve_package_import_in(spec: &str, base: &Path) -> Result<PathBuf, SemaError> {
    validate_package_spec(spec)?;

    // 1. Direct file: <packages>/<spec>.sema
    let direct = base.join(format!("{spec}.sema"));
    if direct.is_file() {
        verify_path_within(base, &direct)?;
        return Ok(direct);
    }

    let pkg_dir = base.join(spec);

    // 2. sema.toml with custom entrypoint
    let toml_path = pkg_dir.join("sema.toml");
    if toml_path.is_file() {
        if let Some(entrypoint) = parse_entrypoint(&toml_path) {
            // Validate the entrypoint itself doesn't escape the package dir
            if entrypoint.contains("..") || entrypoint.starts_with('/') {
                return Err(SemaError::eval(format!(
                    "invalid entrypoint in {}: {entrypoint}",
                    toml_path.display()
                )));
            }
            let entry = pkg_dir.join(&entrypoint);
            if entry.is_file() {
                verify_path_within(base, &entry)?;
                return Ok(entry);
            }
        }
    }

    // 3. Default entrypoint: mod.sema
    let mod_file = pkg_dir.join("mod.sema");
    if mod_file.is_file() {
        verify_path_within(base, &mod_file)?;
        return Ok(mod_file);
    }

    Err(SemaError::eval(format!("package not found: {spec}"))
        .with_hint(format!("Run: sema pkg get {spec}")))
}

/// Verify that a resolved path stays within the expected base directory.
fn verify_path_within(base: &Path, resolved: &Path) -> Result<(), SemaError> {
    // Use canonicalize if both paths exist, otherwise check lexically
    if let (Ok(canon_base), Ok(canon_resolved)) = (base.canonicalize(), resolved.canonicalize()) {
        if !canon_resolved.starts_with(&canon_base) {
            return Err(SemaError::eval(
                "package path escapes packages directory".to_string(),
            ));
        }
    }
    Ok(())
}

/// Parse `entrypoint = "..."` from a sema.toml file using simple line parsing.
///
/// Looks for `entrypoint = "value"` lines. Handles both double and single quotes,
/// and strips inline comments.
fn parse_entrypoint(path: &Path) -> Option<String> {
    let contents = std::fs::read_to_string(path).ok()?;
    for line in contents.lines() {
        let trimmed = line.trim();
        // Skip comments
        if trimmed.starts_with('#') {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("entrypoint") {
            let rest = rest.trim_start();
            if let Some(rest) = rest.strip_prefix('=') {
                let rest = rest.trim();
                // Try double quotes
                if let Some(inner) = rest.strip_prefix('"') {
                    if let Some(end) = inner.find('"') {
                        return Some(inner[..end].to_string());
                    }
                }
                // Try single quotes
                if let Some(inner) = rest.strip_prefix('\'') {
                    if let Some(end) = inner.find('\'') {
                        return Some(inner[..end].to_string());
                    }
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
        let dir =
            std::env::temp_dir().join(format!("sema-resolve-test-{}-{}", std::process::id(), id));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    // --- is_package_import tests ---

    #[test]
    fn test_is_package_import_valid() {
        assert!(is_package_import("github.com/user/repo"));
        assert!(is_package_import("github.com/user/repo/sub"));
        assert!(is_package_import("gitlab.com/org/project"));
    }

    #[test]
    fn test_is_package_import_file_paths() {
        assert!(!is_package_import("./utils.sema"));
        assert!(!is_package_import("../lib/utils.sema"));
        assert!(!is_package_import("utils.sema"));
        assert!(!is_package_import("/absolute/path.sema"));
        assert!(!is_package_import("single-word"));
        assert!(!is_package_import("github.com/user/repo.sema"));
    }

    #[test]
    fn test_is_package_import_rejects_schemes() {
        assert!(!is_package_import("https://github.com/user/repo"));
        assert!(!is_package_import("http://example.com/pkg"));
        assert!(!is_package_import("ssh://git@github.com/user/repo"));
    }

    #[test]
    fn test_is_package_import_rejects_dangerous() {
        assert!(!is_package_import("github.com\\user\\repo")); // backslash
        assert!(!is_package_import("git@github.com:user/repo")); // colon (scp-style)
        assert!(!is_package_import("C:/Users/path")); // Windows drive
    }

    // --- validate_package_spec tests ---

    #[test]
    fn test_validate_spec_valid() {
        assert!(validate_package_spec("github.com/user/repo").is_ok());
        assert!(validate_package_spec("gitlab.com/org/project/sub").is_ok());
    }

    #[test]
    fn test_validate_spec_traversal() {
        assert!(validate_package_spec("github.com/../../etc/passwd").is_err());
        assert!(validate_package_spec("github.com/user/../../../etc").is_err());
        assert!(validate_package_spec("../escape").is_err());
        assert!(validate_package_spec("github.com/./user/repo").is_err());
    }

    #[test]
    fn test_validate_spec_empty_segments() {
        assert!(validate_package_spec("github.com//user/repo").is_err());
        assert!(validate_package_spec("/github.com/user").is_err());
    }

    #[test]
    fn test_validate_spec_schemes() {
        assert!(validate_package_spec("https://github.com/user/repo").is_err());
        assert!(validate_package_spec("ssh://git@host/repo").is_err());
    }

    #[test]
    fn test_validate_spec_dangerous_chars() {
        assert!(validate_package_spec("github.com\\user").is_err());
        assert!(validate_package_spec("git@github.com:user/repo").is_err());
    }

    // --- resolve_package_import_in tests ---

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
    fn test_resolve_entrypoint_with_inline_comment() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(
            pkg_dir.join("sema.toml"),
            "entrypoint = \"lib.sema\" # the main entry\n",
        )
        .unwrap();
        fs::write(pkg_dir.join("lib.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_dir.join("lib.sema"));
    }

    #[test]
    fn test_resolve_entrypoint_traversal_rejected() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(
            pkg_dir.join("sema.toml"),
            "entrypoint = \"../../etc/passwd\"\n",
        )
        .unwrap();

        let err = resolve_package_import_in("github.com/user/repo", &base).unwrap_err();
        assert!(err.to_string().contains("invalid entrypoint"));
    }

    #[test]
    fn test_resolve_not_found() {
        let base = temp_packages_dir();
        let err = resolve_package_import_in("github.com/user/repo", &base).unwrap_err();
        assert!(err.to_string().contains("package not found"));
        assert_eq!(err.hint(), Some("Run: sema pkg get github.com/user/repo"));
    }

    #[test]
    fn test_resolve_traversal_rejected() {
        let base = temp_packages_dir();
        let err = resolve_package_import_in("github.com/../../etc/passwd", &base).unwrap_err();
        assert!(err.to_string().contains("path traversal"));
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

    #[test]
    fn test_resolve_entrypoint_fallback_to_mod_sema() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        // sema.toml exists but entrypoint file doesn't
        fs::write(
            pkg_dir.join("sema.toml"),
            "entrypoint = \"nonexistent.sema\"\n",
        )
        .unwrap();
        fs::write(pkg_dir.join("mod.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_dir.join("mod.sema"));
    }

    #[test]
    fn test_resolve_sema_toml_without_entrypoint_uses_mod() {
        let base = temp_packages_dir();
        let pkg_dir = base.join("github.com/user/repo");
        fs::create_dir_all(&pkg_dir).unwrap();
        // sema.toml exists but has no entrypoint key
        fs::write(
            pkg_dir.join("sema.toml"),
            "[package]\nname = \"repo\"\nversion = \"1.0\"\n",
        )
        .unwrap();
        fs::write(pkg_dir.join("mod.sema"), "(define x 1)").unwrap();

        let result = resolve_package_import_in("github.com/user/repo", &base).unwrap();
        assert_eq!(result, pkg_dir.join("mod.sema"));
    }

    // --- verify_path_within tests (symlink escape) ---

    #[cfg(unix)]
    #[test]
    fn test_resolve_mod_sema_symlink_escape_rejected() {
        let base = temp_packages_dir();
        // Create a target file outside the packages directory
        let outside = base.parent().unwrap().join(format!(
            "sema-escape-target-{}",
            TEST_COUNTER.fetch_add(1, Ordering::SeqCst)
        ));
        fs::create_dir_all(&outside).unwrap();
        fs::write(outside.join("mod.sema"), "pwned").unwrap();

        // Create a symlink inside packages that points outside
        let pkg_dir = base.join("github.com/user/evil");
        fs::create_dir_all(pkg_dir.parent().unwrap()).unwrap();
        std::os::unix::fs::symlink(&outside, &pkg_dir).unwrap();

        let err = resolve_package_import_in("github.com/user/evil", &base).unwrap_err();
        assert!(
            err.to_string().contains("escapes"),
            "expected escape error, got: {err}"
        );

        let _ = fs::remove_dir_all(&outside);
    }

    #[cfg(unix)]
    #[test]
    fn test_resolve_entrypoint_symlink_escape_rejected() {
        let base = temp_packages_dir();
        // Create a target file outside the packages directory
        let outside_file = base.parent().unwrap().join(format!(
            "sema-escape-entry-{}.sema",
            TEST_COUNTER.fetch_add(1, Ordering::SeqCst)
        ));
        fs::write(&outside_file, "pwned").unwrap();

        // Create a package with a sema.toml pointing to a symlinked file
        let pkg_dir = base.join("github.com/user/tricky");
        fs::create_dir_all(&pkg_dir).unwrap();
        fs::write(pkg_dir.join("sema.toml"), "entrypoint = \"entry.sema\"\n").unwrap();
        std::os::unix::fs::symlink(&outside_file, pkg_dir.join("entry.sema")).unwrap();

        let err = resolve_package_import_in("github.com/user/tricky", &base).unwrap_err();
        assert!(
            err.to_string().contains("escapes"),
            "expected escape error, got: {err}"
        );

        let _ = fs::remove_file(&outside_file);
    }

    // --- PackagePath tests ---

    #[test]
    fn test_package_path_valid() {
        let p = PackagePath::parse("github.com/user/repo").unwrap();
        assert_eq!(p.as_str(), "github.com/user/repo");
    }

    #[test]
    fn test_package_path_rejects_traversal() {
        assert!(PackagePath::parse("github.com/../../etc/passwd").is_err());
    }

    #[test]
    fn test_package_path_display() {
        let p = PackagePath::parse("github.com/user/repo").unwrap();
        assert_eq!(format!("{p}"), "github.com/user/repo");
    }

    // --- PackageSpec tests ---

    #[test]
    fn test_package_spec_with_ref() {
        let s = PackageSpec::parse("github.com/user/repo@v1.0").unwrap();
        assert_eq!(s.path.as_str(), "github.com/user/repo");
        assert_eq!(s.git_ref, "v1.0");
    }

    #[test]
    fn test_package_spec_no_ref_defaults_main() {
        let s = PackageSpec::parse("github.com/user/repo").unwrap();
        assert_eq!(s.git_ref, "main");
    }

    #[test]
    fn test_package_spec_clone_url() {
        let s = PackageSpec::parse("github.com/user/repo@v1.0").unwrap();
        assert_eq!(s.clone_url(), "https://github.com/user/repo.git");
    }

    #[test]
    fn test_package_spec_dest_dir() {
        let s = PackageSpec::parse("github.com/user/repo").unwrap();
        let base = PathBuf::from("/home/user/.sema/packages");
        assert_eq!(
            s.dest_dir(&base),
            PathBuf::from("/home/user/.sema/packages/github.com/user/repo")
        );
    }

    #[test]
    fn test_package_spec_rejects_empty_ref() {
        assert!(PackageSpec::parse("github.com/user/repo@").is_err());
    }

    #[test]
    fn test_package_spec_rejects_traversal_in_path() {
        assert!(PackageSpec::parse("github.com/../../etc/passwd@main").is_err());
    }
}
