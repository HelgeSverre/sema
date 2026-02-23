use std::path::{Path, PathBuf};
use std::process::Command;

use sema_core::resolve::packages_dir;

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
        if path.join("sema.toml").exists() || path.join("mod.sema").exists() {
            packages.push(path);
        } else {
            collect_packages(&path, packages);
        }
    }
}

pub fn cmd_add(spec: &str) -> Result<(), String> {
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
        let git_ref = match value.as_str() {
            Some(s) => s,
            None => {
                return Err(format!(
                    "dep '{name}': expected a git ref string (e.g., \"v1.0.0\" or \"main\")"
                ));
            }
        };
        let spec = format!("{name}@{git_ref}");
        println!("Installing {name}...");
        cmd_add(&spec)?;
    }

    Ok(())
}

pub fn cmd_update(name: Option<&str>) -> Result<(), String> {
    let pkg_dir = packages_dir();

    if let Some(name) = name {
        let dir = find_package_dir(&pkg_dir, name).ok_or_else(|| {
            format!("Package '{name}' not found. Run `sema pkg list` to see installed packages.")
        })?;
        run_git(Some(&dir), &["pull"])?;
        let current = current_git_ref(&dir);
        println!(
            "✓ Updated {} → {current}",
            dir.strip_prefix(&pkg_dir).unwrap_or(&dir).display()
        );
    } else {
        let packages = find_all_packages(&pkg_dir);
        if packages.is_empty() {
            println!("No packages installed.");
            return Ok(());
        }
        for dir in &packages {
            let rel = dir.strip_prefix(&pkg_dir).unwrap_or(dir);
            match run_git(Some(dir), &["pull"]) {
                Ok(_) => {
                    let current = current_git_ref(dir);
                    println!("✓ Updated {} → {current}", rel.display());
                }
                Err(e) => {
                    eprintln!("✗ Failed to update {}: {e}", rel.display());
                }
            }
        }
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

    let new_line = format!("\"{}\" = \"{}\"", pkg_path, git_ref);
    let mut in_deps = false;
    let mut found = false;
    let mut changed = false;
    let mut deps_end = None;
    let mut output: Vec<String> = Vec::new();

    for (i, line) in content.lines().enumerate() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') && !trimmed.starts_with("[[") {
            if in_deps {
                deps_end = None; // We're leaving [deps], insert point was last non-empty line
            }
            in_deps = trimmed == "[deps]";
        }

        if in_deps && !trimmed.starts_with('[') && !trimmed.is_empty() && !trimmed.starts_with('#')
        {
            let key = if let Some(rest) = trimmed.strip_prefix('"') {
                rest.split('"').next()
            } else {
                trimmed.split('=').next().map(|s| s.trim())
            };

            if let Some(key) = key {
                if key == pkg_path {
                    found = true;
                    if trimmed != new_line {
                        output.push(new_line.clone());
                        changed = true;
                    } else {
                        output.push(line.to_string());
                    }
                    continue;
                }
            }
        }

        if in_deps && !trimmed.starts_with('[') {
            deps_end = Some(i);
        }

        output.push(line.to_string());
    }

    // If not found, append to the [deps] section
    if !found {
        if in_deps {
            // [deps] was the last section — append at end
            output.push(new_line);
            changed = true;
        } else if deps_end.is_some() {
            // [deps] exists but another section follows — find where [deps] content ends
            // Re-scan to find correct insertion point
            let mut insert_at = None;
            let mut scanning_deps = false;
            for (i, line) in output.iter().enumerate() {
                let trimmed = line.trim();
                if trimmed.starts_with('[') && !trimmed.starts_with("[[") {
                    if scanning_deps {
                        insert_at = Some(i);
                        break;
                    }
                    scanning_deps = trimmed == "[deps]";
                }
            }
            if let Some(pos) = insert_at {
                output.insert(pos, new_line);
            } else {
                output.push(new_line);
            }
            changed = true;
        } else {
            // No [deps] section at all — append one
            if !content.ends_with('\n') && !output.is_empty() {
                output.push(String::new());
            }
            output.push("[deps]".to_string());
            output.push(new_line);
            changed = true;
        }
    }

    if changed {
        let mut result = output.join("\n");
        if content.ends_with('\n') || !content.contains('\n') {
            result.push('\n');
        }
        std::fs::write(toml_path, result).map_err(|e| format!("Failed to write sema.toml: {e}"))?;
    }

    Ok(changed)
}

/// Remove a dep entry from a sema.toml file by package path.
/// Returns true if a matching entry was found and removed.
fn remove_dep_from_toml(toml_path: &Path, pkg_path: &str) -> Result<bool, String> {
    let content =
        std::fs::read_to_string(toml_path).map_err(|e| format!("Failed to read sema.toml: {e}"))?;

    let mut in_deps = false;
    let mut removed = false;
    let mut output: Vec<&str> = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();

        // Track which section we're in
        if trimmed.starts_with('[') && !trimmed.starts_with("[[") {
            in_deps = trimmed == "[deps]";
        }

        if in_deps && !trimmed.starts_with('[') && !trimmed.is_empty() && !trimmed.starts_with('#')
        {
            // Extract the key, handling both quoted and unquoted forms:
            //   "github.com/user/repo" = "v1.0"
            //   github.com/user/repo = "v1.0"
            let key = if let Some(rest) = trimmed.strip_prefix('"') {
                rest.split('"').next()
            } else {
                trimmed.split('=').next().map(|s| s.trim())
            };

            if let Some(key) = key {
                if key == pkg_path {
                    removed = true;
                    continue;
                }
            }
        }

        output.push(line);
    }

    if removed {
        let mut result = output.join("\n");
        if content.ends_with('\n') {
            result.push('\n');
        }
        std::fs::write(toml_path, result).map_err(|e| format!("Failed to write sema.toml: {e}"))?;
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
        let current = current_git_ref(dir);
        println!("  {} ({})", rel.display(), current);
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

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_find_all_packages_finds_mod_sema() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-find2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);

        let pkg = tmp.join("github.com/user/repo");
        std::fs::create_dir_all(&pkg).unwrap();
        std::fs::write(pkg.join("mod.sema"), "(define x 1)").unwrap();

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
        std::fs::write(pkg.join("mod.sema"), "(define x 1)").unwrap();

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
        std::fs::write(pkg.join("mod.sema"), "(define x 1)").unwrap();

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
        let result = cmd_add("github.com/../../etc/passwd");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("path traversal"), "got: {err}");
    }

    #[test]
    fn test_cmd_add_rejects_scheme() {
        let result = cmd_add("https://github.com/user/repo");
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
    fn test_remove_dep_from_toml_unquoted_key() {
        let tmp = std::env::temp_dir().join(format!("sema-pkg-rmdep2-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let toml_path = tmp.join("sema.toml");
        std::fs::write(&toml_path, "[deps]\ngithub.com/user/repo = \"v1.0.0\"\n").unwrap();

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
}
