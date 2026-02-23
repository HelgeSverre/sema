//! Static import/load tracing for `sema build`.
//!
//! Walks the AST of a root source file and all transitively imported files,
//! collecting their contents into a map suitable for bundling into a VFS
//! archive. Only literal string paths can be resolved statically; dynamic
//! imports produce a warning on stderr.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use sema_core::resolve::{is_package_import, resolve_package_import};
use sema_core::Value;

/// Trace all transitive `(import "...")` and `(load "...")` dependencies
/// starting from `root_file`.
///
/// Returns a map of `relative_path -> file_contents` for every discovered
/// dependency. The root file itself is **not** included (it is compiled to
/// bytecode separately).
pub fn trace_imports(root_file: &Path) -> Result<HashMap<String, Vec<u8>>, String> {
    let root_file = root_file
        .canonicalize()
        .map_err(|e| format!("cannot canonicalize root file {}: {e}", root_file.display()))?;

    let root_dir = root_file
        .parent()
        .ok_or_else(|| format!("root file has no parent directory: {}", root_file.display()))?
        .to_path_buf();

    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut result: HashMap<String, Vec<u8>> = HashMap::new();

    // Mark the root file as visited so we never add it to the result map.
    visited.insert(root_file.clone());

    // Read and parse the root file, then trace its imports.
    let source = std::fs::read_to_string(&root_file)
        .map_err(|e| format!("cannot read root file {}: {e}", root_file.display()))?;

    let exprs = sema_reader::read_many(&source)
        .map_err(|e| format!("parse error in {}: {}", root_file.display(), e.inner()))?;

    trace_file_imports(&exprs, &root_file, &root_dir, &mut visited, &mut result)?;

    Ok(result)
}

/// Parse the expressions from a single file and extract all import/load paths,
/// recursively tracing each discovered dependency.
fn trace_file_imports(
    exprs: &[Value],
    current_file: &Path,
    root_dir: &Path,
    visited: &mut HashSet<PathBuf>,
    result: &mut HashMap<String, Vec<u8>>,
) -> Result<(), String> {
    for expr in exprs {
        extract_imports(expr, current_file, root_dir, visited, result)?;
    }
    Ok(())
}

/// Recursively walk an AST expression looking for `(import "path" ...)`
/// and `(load "path")` forms. For any other list form, recurse into all
/// children to catch imports nested inside `begin`, `let`, `define`, etc.
fn extract_imports(
    expr: &Value,
    current_file: &Path,
    root_dir: &Path,
    visited: &mut HashSet<PathBuf>,
    result: &mut HashMap<String, Vec<u8>>,
) -> Result<(), String> {
    let items = match expr.as_list() {
        Some(items) if !items.is_empty() => items,
        _ => return Ok(()),
    };

    // Check the head of the list.
    if let Some(head) = items[0].as_symbol() {
        match head.as_str() {
            "import" | "load" => {
                if items.len() >= 2 {
                    if let Some(path_str) = items[1].as_str() {
                        process_import(path_str, current_file, root_dir, visited, result)?;
                    } else {
                        // Dynamic import -- cannot resolve statically.
                        eprintln!(
                            "warning: dynamic {} in {} cannot be resolved statically; \
                             use --include to add it manually",
                            head,
                            current_file.display()
                        );
                    }
                }
                // Don't recurse further into import/load forms.
                return Ok(());
            }
            "module" => {
                // (module name ... body ...)
                // The body starts after the module name (index 2+), but there
                // may be an (export ...) form in there too -- just recurse
                // into everything after the name.
                for item in items.iter().skip(2) {
                    extract_imports(item, current_file, root_dir, visited, result)?;
                }
                return Ok(());
            }
            _ => {}
        }
    }

    // For any other list, recurse into all children.
    for item in items.iter() {
        extract_imports(item, current_file, root_dir, visited, result)?;
    }

    Ok(())
}

/// Resolve an import path relative to the importing file, read its contents,
/// add it to the result map, and recursively trace its own imports.
fn process_import(
    import_path: &str,
    current_file: &Path,
    root_dir: &Path,
    visited: &mut HashSet<PathBuf>,
    result: &mut HashMap<String, Vec<u8>>,
) -> Result<(), String> {
    if is_package_import(import_path) {
        return process_package_import(import_path, root_dir, visited, result);
    }

    // Resolve relative to the directory of the importing file.
    let base_dir = current_file
        .parent()
        .ok_or_else(|| format!("file has no parent directory: {}", current_file.display()))?;

    let resolved = base_dir.join(import_path);

    let canonical = resolved.canonicalize().map_err(|e| {
        format!(
            "cannot resolve import \"{}\" (from {}): {e}",
            import_path,
            current_file.display()
        )
    })?;

    // Circular import protection.
    if visited.contains(&canonical) {
        return Ok(());
    }
    visited.insert(canonical.clone());

    // Read file contents.
    let contents = std::fs::read(&canonical)
        .map_err(|e| format!("cannot read {}: {e}", canonical.display()))?;

    // Compute relative path from root_dir for the VFS key.
    let rel_path = canonical
        .strip_prefix(root_dir)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| canonical.to_string_lossy().into_owned());

    result.insert(rel_path, contents.clone());

    // Recursively trace the imported file's own imports.
    // Only parse if it looks like a text file (sema source).
    if let Ok(source) = std::str::from_utf8(&contents) {
        if let Ok(exprs) = sema_reader::read_many(source) {
            trace_file_imports(&exprs, &canonical, root_dir, visited, result)?;
        }
        // If parsing fails, we still included the file -- just don't trace deeper.
    }

    Ok(())
}

/// Resolve a package import via `resolve_package_import`, read its contents,
/// and add it to the result map using the package path as the VFS key.
fn process_package_import(
    import_path: &str,
    root_dir: &Path,
    visited: &mut HashSet<PathBuf>,
    result: &mut HashMap<String, Vec<u8>>,
) -> Result<(), String> {
    let resolved = match resolve_package_import(import_path) {
        Ok(p) => p,
        Err(_) => {
            eprintln!(
                "warning: package \"{}\" is not installed; \
                 skipping for bundling (will be resolved at runtime)",
                import_path
            );
            return Ok(());
        }
    };

    let canonical = resolved.canonicalize().map_err(|e| {
        format!(
            "cannot canonicalize package import \"{}\": {e}",
            import_path
        )
    })?;

    if visited.contains(&canonical) {
        return Ok(());
    }
    visited.insert(canonical.clone());

    let contents = std::fs::read(&canonical)
        .map_err(|e| format!("cannot read {}: {e}", canonical.display()))?;

    // Use the package path as the VFS key for portability.
    result.insert(import_path.to_string(), contents.clone());

    // Recursively trace the package file's own imports.
    if let Ok(source) = std::str::from_utf8(&contents) {
        if let Ok(exprs) = sema_reader::read_many(source) {
            trace_file_imports(&exprs, &canonical, root_dir, visited, result)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn tmpdir(name: &str) -> PathBuf {
        let d =
            std::env::temp_dir().join(format!("sema-tracer-test-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&d);
        fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn test_trace_nonexistent_root() {
        let result = trace_imports(Path::new("/nonexistent/file.sema"));
        assert!(result.is_err());
    }

    #[test]
    fn test_trace_no_imports() {
        let dir = tmpdir("no-imports");
        fs::write(dir.join("main.sema"), "(define x 42)").unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.is_empty());
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_single_import() {
        let dir = tmpdir("single");
        fs::write(dir.join("lib.sema"), "(define y 1)").unwrap();
        fs::write(dir.join("main.sema"), r#"(import "lib.sema")"#).unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(
            result.contains_key("lib.sema"),
            "expected lib.sema in result: {result:?}"
        );
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_circular_imports() {
        let dir = tmpdir("circular");
        fs::write(dir.join("a.sema"), r#"(import "b.sema")"#).unwrap();
        fs::write(dir.join("b.sema"), r#"(import "a.sema")"#).unwrap();
        let result = trace_imports(&dir.join("a.sema")).unwrap();
        assert!(result.contains_key("b.sema"));
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_dynamic_import_warns() {
        let dir = tmpdir("dynamic");
        fs::write(dir.join("main.sema"), "(import some-var)").unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.is_empty());
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_missing_import_errors() {
        let dir = tmpdir("missing");
        fs::write(dir.join("main.sema"), r#"(import "nonexistent.sema")"#).unwrap();
        let result = trace_imports(&dir.join("main.sema"));
        assert!(result.is_err());
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_module_form() {
        let dir = tmpdir("module");
        fs::write(dir.join("lib.sema"), "(define z 99)").unwrap();
        fs::write(
            dir.join("main.sema"),
            r#"(module mymod (export z) (import "lib.sema"))"#,
        )
        .unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.contains_key("lib.sema"));
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_load_form() {
        let dir = tmpdir("load");
        fs::write(dir.join("defs.sema"), "(define loaded 1)").unwrap();
        fs::write(dir.join("main.sema"), r#"(load "defs.sema")"#).unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.contains_key("defs.sema"));
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_binary_file_included() {
        let dir = tmpdir("binary");
        fs::write(dir.join("data.bin"), &[0xDE, 0xAD, 0xBE, 0xEF]).unwrap();
        fs::write(dir.join("main.sema"), r#"(import "data.bin")"#).unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(
            result.contains_key("data.bin"),
            "binary file should be included"
        );
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_nested_imports() {
        let dir = tmpdir("nested");
        fs::create_dir_all(dir.join("lib")).unwrap();
        fs::write(dir.join("lib/deep.sema"), "(define deep 1)").unwrap();
        fs::write(dir.join("lib/mid.sema"), r#"(import "deep.sema")"#).unwrap();
        fs::write(dir.join("main.sema"), r#"(import "lib/mid.sema")"#).unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.contains_key("lib/mid.sema"));
        assert!(result.contains_key("lib/deep.sema"));
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_import_in_begin() {
        let dir = tmpdir("begin");
        fs::write(dir.join("lib.sema"), "(define y 1)").unwrap();
        fs::write(
            dir.join("main.sema"),
            r#"(begin (import "lib.sema") (+ 1 2))"#,
        )
        .unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.contains_key("lib.sema"));
        let _ = fs::remove_dir_all(&dir);
    }

    // All package import tracer tests share a single SEMA_HOME env var and must
    // run sequentially in a single thread to avoid races with other tests.
    #[test]
    fn test_trace_package_imports() {
        std::thread::spawn(|| {
            let dir = tmpdir("pkg-all");
            let sema_home = dir.join("home");

            // --- Set up fake packages ---

            // Basic package with mod.sema
            let mylib = sema_home.join("packages/github.com/test/mylib");
            fs::create_dir_all(&mylib).unwrap();
            fs::write(mylib.join("mod.sema"), "(define pkg-val 42)").unwrap();

            // Package with transitive local imports
            let translib = sema_home.join("packages/github.com/test/translib");
            fs::create_dir_all(&translib).unwrap();
            fs::write(
                translib.join("mod.sema"),
                r#"(import "helpers.sema") (define main-val 1)"#,
            )
            .unwrap();
            fs::write(translib.join("helpers.sema"), "(define helper-val 2)").unwrap();

            // Package with custom entrypoint
            let custom = sema_home.join("packages/github.com/test/custom");
            fs::create_dir_all(&custom).unwrap();
            fs::write(custom.join("sema.toml"), "entrypoint = \"lib.sema\"\n").unwrap();
            fs::write(custom.join("lib.sema"), "(define custom-val 99)").unwrap();

            // Utils package for mixed import test
            let utils = sema_home.join("packages/github.com/test/utils");
            fs::create_dir_all(&utils).unwrap();
            fs::write(utils.join("mod.sema"), "(define util-fn 1)").unwrap();

            // Ensure packages dir exists for not-installed test
            fs::create_dir_all(sema_home.join("packages")).unwrap();

            std::env::set_var("SEMA_HOME", &sema_home);

            // --- Basic package import ---
            {
                fs::write(dir.join("main.sema"), r#"(import "github.com/test/mylib")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/test/mylib"),
                    "package import should be traced: {result:?}"
                );
                assert_eq!(
                    result.get("github.com/test/mylib").unwrap(),
                    b"(define pkg-val 42)"
                );
            }

            // --- Transitive imports through package ---
            {
                fs::write(
                    dir.join("main.sema"),
                    r#"(import "github.com/test/translib")"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/test/translib"),
                    "package should be traced: {result:?}"
                );
                let has_helpers = result.keys().any(|k| k.contains("helpers.sema"));
                assert!(
                    has_helpers,
                    "transitive import from package should be traced: {result:?}"
                );
            }

            // --- Uninstalled package warns but doesn't error ---
            {
                fs::write(
                    dir.join("main.sema"),
                    r#"(import "github.com/nonexistent/pkg")"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.is_empty(),
                    "uninstalled package should be skipped: {result:?}"
                );
            }

            // --- Mixed local and package imports ---
            {
                fs::write(dir.join("local.sema"), "(define local-val 2)").unwrap();
                fs::write(
                    dir.join("main.sema"),
                    "(import \"local.sema\")\n(import \"github.com/test/utils\")",
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("local.sema"),
                    "local import should be traced: {result:?}"
                );
                assert!(
                    result.contains_key("github.com/test/utils"),
                    "package import should be traced: {result:?}"
                );
            }

            // --- Custom entrypoint via sema.toml ---
            {
                fs::write(
                    dir.join("main.sema"),
                    r#"(import "github.com/test/custom")"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/test/custom"),
                    "custom entrypoint package should be traced: {result:?}"
                );
                assert_eq!(
                    result.get("github.com/test/custom").unwrap(),
                    b"(define custom-val 99)"
                );
            }

            // Cleanup
            std::env::remove_var("SEMA_HOME");
            let _ = fs::remove_dir_all(&dir);
        })
        .join()
        .unwrap();
    }
}
