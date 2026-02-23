use std::path::{Path, PathBuf};
use std::process::Command;

use sema_core::resolve::packages_dir;

fn run_git(dir: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(dir)
        .output()
        .map_err(|e| format!("Failed to run git: {e}"))?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        Err(format!("git {} failed: {stderr}", args.join(" ")))
    }
}

fn run_git_global(args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .output()
        .map_err(|e| format!("Failed to run git: {e}"))?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        Err(format!("git {} failed: {stderr}", args.join(" ")))
    }
}

fn get_current_ref(dir: &Path) -> String {
    if let Ok(tag) = run_git(dir, &["describe", "--tags", "--exact-match"]) {
        return tag;
    }
    run_git(dir, &["rev-parse", "--abbrev-ref", "HEAD"]).unwrap_or_else(|_| "unknown".to_string())
}

fn parse_url_spec(spec: &str) -> (&str, &str) {
    if let Some((url, git_ref)) = spec.rsplit_once('@') {
        (url, git_ref)
    } else {
        (spec, "main")
    }
}

fn find_package_dir(pkg_dir: &Path, name: &str) -> Option<PathBuf> {
    // Try exact path match first (e.g., github.com/user/repo)
    let exact = pkg_dir.join(name);
    if exact.is_dir() {
        return Some(exact);
    }

    // Search by directory name
    find_all_packages(pkg_dir)
        .into_iter()
        .find(|p| {
            p.file_name()
                .map(|n| n.to_string_lossy() == name)
                .unwrap_or(false)
        })
}

fn find_all_packages(pkg_dir: &Path) -> Vec<PathBuf> {
    let mut packages = Vec::new();
    walk_packages(pkg_dir, &mut packages);
    packages
}

fn walk_packages(dir: &Path, packages: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        // A package root has sema.toml or mod.sema
        if path.join("sema.toml").exists() || path.join("mod.sema").exists() {
            packages.push(path);
        } else {
            walk_packages(&path, packages);
        }
    }
}

pub fn cmd_get(spec: &str) -> Result<(), String> {
    let (url, git_ref) = parse_url_spec(spec);
    let pkg_dir = packages_dir();
    let dest = pkg_dir.join(url);

    if dest.exists() {
        // Update existing package
        run_git(&dest, &["fetch", "--tags"])?;
        run_git(&dest, &["checkout", git_ref])?;
        let current = get_current_ref(&dest);
        println!("✓ Updated {url} → {current}");
    } else {
        // Clone new package
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create directory: {e}"))?;
        }
        let clone_url = format!("https://{url}.git");
        run_git_global(&["clone", &clone_url, &dest.to_string_lossy()])?;
        run_git(&dest, &["checkout", git_ref])?;
        let current = get_current_ref(&dest);
        println!("✓ Installed {url} → {current}");
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
        let spec = match value.as_str() {
            Some(s) => format!("{name}@{s}"),
            None => name.clone(),
        };
        cmd_get(&spec)?;
    }

    Ok(())
}

pub fn cmd_update(name: Option<&str>) -> Result<(), String> {
    let pkg_dir = packages_dir();

    if let Some(name) = name {
        let dir = find_package_dir(&pkg_dir, name)
            .ok_or_else(|| format!("Package '{name}' not found"))?;
        run_git(&dir, &["pull"])?;
        let current = get_current_ref(&dir);
        println!("✓ Updated {} → {current}", dir.strip_prefix(&pkg_dir).unwrap_or(&dir).display());
    } else {
        let packages = find_all_packages(&pkg_dir);
        if packages.is_empty() {
            println!("No packages installed.");
            return Ok(());
        }
        for dir in &packages {
            let rel = dir.strip_prefix(&pkg_dir).unwrap_or(dir);
            match run_git(dir, &["pull"]) {
                Ok(_) => {
                    let current = get_current_ref(dir);
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
    let dir = find_package_dir(&pkg_dir, name)
        .ok_or_else(|| format!("Package '{name}' not found"))?;

    std::fs::remove_dir_all(&dir).map_err(|e| format!("Failed to remove package: {e}"))?;
    println!("✓ Removed {}", dir.strip_prefix(&pkg_dir).unwrap_or(&dir).display());

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

    Ok(())
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
        let current = get_current_ref(dir);
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
        r#"[project]
name = "{project_name}"

[deps]
"#
    );

    std::fs::write(toml_path, content).map_err(|e| format!("Failed to write sema.toml: {e}"))?;
    println!("✓ Created sema.toml");

    Ok(())
}
