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
            // Quoted data is not evaluated — don't trace imports inside it.
            "quote" | "quasiquote" => return Ok(()),
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

    // Compute relative path for the VFS key.
    // Project files: relative to root_dir. Package files: relative to packages_dir.
    let rel_path = if let Ok(rel) = canonical.strip_prefix(root_dir) {
        rel.to_string_lossy().replace('\\', "/")
    } else {
        let pkg_dir = sema_core::resolve::packages_dir();
        if let Ok(canon_pkg) = pkg_dir.canonicalize() {
            if let Ok(rel) = canonical.strip_prefix(&canon_pkg) {
                rel.to_string_lossy().replace('\\', "/")
            } else {
                return Err(format!(
                    "imported file is outside project and packages directory: {}",
                    canonical.display()
                ));
            }
        } else {
            return Err(format!(
                "cannot resolve packages directory for import: {}",
                canonical.display()
            ));
        }
    };

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
            return Err(format!(
                "package \"{}\" is not installed (hint: sema pkg add {})",
                import_path, import_path
            ));
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

            // Basic package with package.sema
            let mylib = sema_home.join("packages/github.com/test/mylib");
            fs::create_dir_all(&mylib).unwrap();
            fs::write(mylib.join("package.sema"), "(define pkg-val 42)").unwrap();

            // Package with transitive local imports
            let translib = sema_home.join("packages/github.com/test/translib");
            fs::create_dir_all(&translib).unwrap();
            fs::write(
                translib.join("package.sema"),
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
            fs::write(utils.join("package.sema"), "(define util-fn 1)").unwrap();

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

            // --- Transitive imports through package with portable VFS keys ---
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

                // VFS keys must be portable — no absolute paths
                for key in result.keys() {
                    assert!(
                        !key.starts_with('/'),
                        "VFS key should be relative, not absolute: {key}"
                    );
                    assert!(
                        !key.contains(&*sema_home.to_string_lossy()),
                        "VFS key must not contain SEMA_HOME path: {key}"
                    );
                }
            }

            // --- Uninstalled package is a hard error ---
            {
                fs::write(
                    dir.join("main.sema"),
                    r#"(import "github.com/nonexistent/pkg")"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema"));
                assert!(result.is_err(), "missing package should be a hard error");
                let err = result.unwrap_err();
                assert!(
                    err.contains("not installed"),
                    "error should mention 'not installed', got: {err}"
                );
                assert!(
                    err.contains("sema pkg add"),
                    "error should hint 'sema pkg add', got: {err}"
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

    #[test]
    fn test_trace_quoted_import_not_traced() {
        let dir = tmpdir("quoted");
        fs::write(
            dir.join("main.sema"),
            r#"(quote (import "nonexistent.sema"))"#,
        )
        .unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(result.is_empty(), "quoted imports should not be traced");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_quasiquoted_import_not_traced() {
        let dir = tmpdir("quasiquoted");
        fs::write(
            dir.join("main.sema"),
            r#"(quasiquote (import "nonexistent.sema"))"#,
        )
        .unwrap();
        let result = trace_imports(&dir.join("main.sema")).unwrap();
        assert!(
            result.is_empty(),
            "quasiquoted imports should not be traced"
        );
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_trace_package_advanced_scenarios() {
        std::thread::spawn(|| {
            let dir = tmpdir("pkg-advanced");
            // Put sema_home OUTSIDE the project dir so transitive package
            // imports fall through to the packages_dir prefix-strip path
            // (mirrors real-world usage where ~/.sema != project dir).
            let sema_home = tmpdir("pkg-advanced-home");

            // --- Create all package directories up front ---

            // 1. Registry short-name package
            let json_utils = sema_home.join("packages/json-utils");
            fs::create_dir_all(&json_utils).unwrap();
            fs::write(json_utils.join("package.sema"), "(define json-val 42)").unwrap();

            // 2. Registry package with transitive relative imports
            let json_utils2 = sema_home.join("packages/json-utils");
            // Already created above, just add helpers.sema
            fs::write(
                json_utils2.join("package.sema"),
                r#"(import "helpers.sema") (define json-val 1)"#,
            )
            .unwrap();
            fs::write(json_utils2.join("helpers.sema"), "(define helper-fn 2)").unwrap();

            // 3. Package-to-package chain
            let a_lib = sema_home.join("packages/github.com/a/lib");
            fs::create_dir_all(&a_lib).unwrap();
            fs::write(
                a_lib.join("package.sema"),
                r#"(import "github.com/b/util") (define a-val 1)"#,
            )
            .unwrap();
            let b_util = sema_home.join("packages/github.com/b/util");
            fs::create_dir_all(&b_util).unwrap();
            fs::write(b_util.join("package.sema"), "(define b-val 2)").unwrap();

            // 4. Registry package importing a git-style package (cross-type)
            let json_tools = sema_home.join("packages/json-tools");
            fs::create_dir_all(&json_tools).unwrap();
            fs::write(
                json_tools.join("package.sema"),
                r#"(import "github.com/x/parser") (define tool-val 1)"#,
            )
            .unwrap();
            let x_parser = sema_home.join("packages/github.com/x/parser");
            fs::create_dir_all(&x_parser).unwrap();
            fs::write(x_parser.join("package.sema"), "(define parser-val 2)").unwrap();

            // 5. Diamond dependency
            let da_lib = sema_home.join("packages/github.com/a/lib");
            // Already created, overwrite for this scenario later
            let db_lib = sema_home.join("packages/github.com/b/lib");
            fs::create_dir_all(&db_lib).unwrap();
            fs::write(
                db_lib.join("package.sema"),
                r#"(import "github.com/c/shared") (define b-val 2)"#,
            )
            .unwrap();
            let c_shared = sema_home.join("packages/github.com/c/shared");
            fs::create_dir_all(&c_shared).unwrap();
            fs::write(c_shared.join("package.sema"), "(define shared-val 99)").unwrap();

            // 6. Package with nested subdirectory imports
            let deeplib = sema_home.join("packages/github.com/x/deeplib");
            fs::create_dir_all(deeplib.join("src")).unwrap();
            fs::write(
                deeplib.join("package.sema"),
                r#"(import "src/utils.sema") (define deep-val 1)"#,
            )
            .unwrap();
            fs::write(deeplib.join("src/utils.sema"), "(define util-val 2)").unwrap();

            // 7. Custom entrypoint with transitive deps
            let customdeps = sema_home.join("packages/github.com/x/customdeps");
            fs::create_dir_all(&customdeps).unwrap();
            fs::write(customdeps.join("sema.toml"), "entrypoint = \"lib.sema\"\n").unwrap();
            fs::write(
                customdeps.join("lib.sema"),
                r#"(import "internal.sema") (define val 1)"#,
            )
            .unwrap();
            fs::write(customdeps.join("internal.sema"), "(define internal-val 2)").unwrap();

            // 8. Package using load instead of import
            let loadpkg = sema_home.join("packages/github.com/x/loadpkg");
            fs::create_dir_all(&loadpkg).unwrap();
            fs::write(
                loadpkg.join("package.sema"),
                r#"(load "defs.sema") (define val (+ loaded 1))"#,
            )
            .unwrap();
            fs::write(loadpkg.join("defs.sema"), "(define loaded 10)").unwrap();

            // 10. Deeply nested chain (3 levels)
            let l1 = sema_home.join("packages/github.com/l1/pkg");
            fs::create_dir_all(&l1).unwrap();
            fs::write(
                l1.join("package.sema"),
                r#"(import "github.com/l2/pkg") (define l1-val 1)"#,
            )
            .unwrap();
            let l2 = sema_home.join("packages/github.com/l2/pkg");
            fs::create_dir_all(&l2).unwrap();
            fs::write(
                l2.join("package.sema"),
                r#"(import "github.com/l3/pkg") (define l2-val 2)"#,
            )
            .unwrap();
            let l3 = sema_home.join("packages/github.com/l3/pkg");
            fs::create_dir_all(&l3).unwrap();
            fs::write(l3.join("package.sema"), "(define l3-val 3)").unwrap();

            // Ensure packages dir exists
            fs::create_dir_all(sema_home.join("packages")).unwrap();

            std::env::set_var("SEMA_HOME", &sema_home);

            // Helper closure to assert all VFS keys are portable
            let assert_portable_keys = |result: &HashMap<String, Vec<u8>>, scenario: &str| {
                for key in result.keys() {
                    assert!(
                        !key.starts_with('/'),
                        "[{scenario}] VFS key should not start with '/': {key}"
                    );
                    assert!(
                        !key.contains(&*sema_home.to_string_lossy()),
                        "[{scenario}] VFS key must not contain SEMA_HOME path: {key}"
                    );
                    assert!(
                        !key.contains('\\'),
                        "[{scenario}] VFS key must not contain backslashes: {key}"
                    );
                }
            };

            // --- 1. Registry short-name package ---
            {
                // We need to reset json-utils to simple content for this test
                fs::write(json_utils.join("package.sema"), "(define json-val 42)").unwrap();
                fs::remove_file(json_utils.join("helpers.sema")).ok();

                fs::write(dir.join("main.sema"), r#"(import "json-utils")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("json-utils"),
                    "[1] expected key 'json-utils' in result: {result:?}"
                );
                assert_eq!(
                    result.get("json-utils").unwrap(),
                    b"(define json-val 42)",
                    "[1] content mismatch for json-utils"
                );
                assert_portable_keys(&result, "1-registry-short-name");
            }

            // --- 2. Registry package with transitive relative imports ---
            {
                fs::write(
                    json_utils.join("package.sema"),
                    r#"(import "helpers.sema") (define json-val 1)"#,
                )
                .unwrap();
                fs::write(json_utils.join("helpers.sema"), "(define helper-fn 2)").unwrap();

                fs::write(dir.join("main.sema"), r#"(import "json-utils")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("json-utils"),
                    "[2] expected key 'json-utils' in result: {result:?}"
                );
                assert!(
                    result.contains_key("json-utils/helpers.sema"),
                    "[2] expected key 'json-utils/helpers.sema' in result: {result:?}"
                );
                assert_portable_keys(&result, "2-registry-transitive");
            }

            // --- 3. Package-to-package chain ---
            {
                fs::write(dir.join("main.sema"), r#"(import "github.com/a/lib")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/a/lib"),
                    "[3] expected key 'github.com/a/lib': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/b/util"),
                    "[3] expected key 'github.com/b/util': {result:?}"
                );
                assert_portable_keys(&result, "3-pkg-to-pkg-chain");
            }

            // --- 4. Registry package importing a git-style package (cross-type) ---
            {
                fs::write(dir.join("main.sema"), r#"(import "json-tools")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("json-tools"),
                    "[4] expected key 'json-tools': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/x/parser"),
                    "[4] expected key 'github.com/x/parser': {result:?}"
                );
                assert_portable_keys(&result, "4-cross-type");
            }

            // --- 5. Diamond dependency ---
            {
                // Overwrite github.com/a/lib to import c/shared for this scenario
                fs::write(
                    da_lib.join("package.sema"),
                    r#"(import "github.com/c/shared") (define a-val 1)"#,
                )
                .unwrap();

                fs::write(
                    dir.join("main.sema"),
                    r#"(import "github.com/a/lib") (import "github.com/b/lib")"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/a/lib"),
                    "[5] expected key 'github.com/a/lib': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/b/lib"),
                    "[5] expected key 'github.com/b/lib': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/c/shared"),
                    "[5] expected key 'github.com/c/shared': {result:?}"
                );
                assert_eq!(
                    result.get("github.com/c/shared").unwrap(),
                    b"(define shared-val 99)",
                    "[5] shared package content mismatch"
                );
                assert_portable_keys(&result, "5-diamond");
            }

            // --- 6. Package with nested subdirectory imports ---
            {
                fs::write(dir.join("main.sema"), r#"(import "github.com/x/deeplib")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/x/deeplib"),
                    "[6] expected key 'github.com/x/deeplib': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/x/deeplib/src/utils.sema"),
                    "[6] expected key 'github.com/x/deeplib/src/utils.sema': {result:?}"
                );
                assert_portable_keys(&result, "6-nested-subdir");
            }

            // --- 7. Custom entrypoint with transitive deps ---
            {
                fs::write(
                    dir.join("main.sema"),
                    r#"(import "github.com/x/customdeps")"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/x/customdeps"),
                    "[7] expected key 'github.com/x/customdeps': {result:?}"
                );
                // Should contain lib.sema content, not sema.toml content
                let content = result.get("github.com/x/customdeps").unwrap();
                assert!(
                    content != b"entrypoint = \"lib.sema\"\n",
                    "[7] should contain lib.sema content, not sema.toml"
                );
                assert_eq!(
                    content, br#"(import "internal.sema") (define val 1)"#,
                    "[7] content should be lib.sema"
                );
                assert!(
                    result.contains_key("github.com/x/customdeps/internal.sema"),
                    "[7] expected key 'github.com/x/customdeps/internal.sema': {result:?}"
                );
                assert_portable_keys(&result, "7-custom-entrypoint");
            }

            // --- 8. Package using load instead of import ---
            {
                fs::write(dir.join("main.sema"), r#"(import "github.com/x/loadpkg")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/x/loadpkg"),
                    "[8] expected key 'github.com/x/loadpkg': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/x/loadpkg/defs.sema"),
                    "[8] expected key 'github.com/x/loadpkg/defs.sema': {result:?}"
                );
                assert_portable_keys(&result, "8-load-form");
            }

            // --- 9. Quoted import is NOT traced ---
            {
                fs::write(
                    dir.join("main.sema"),
                    r#"(quote (import "nonexistent.sema"))"#,
                )
                .unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.is_empty(),
                    "[9] quoted import should not be traced: {result:?}"
                );
            }

            // --- 10. Deeply nested chain (3 levels of packages) ---
            {
                fs::write(dir.join("main.sema"), r#"(import "github.com/l1/pkg")"#).unwrap();
                let result = trace_imports(&dir.join("main.sema")).unwrap();
                assert!(
                    result.contains_key("github.com/l1/pkg"),
                    "[10] expected key 'github.com/l1/pkg': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/l2/pkg"),
                    "[10] expected key 'github.com/l2/pkg': {result:?}"
                );
                assert!(
                    result.contains_key("github.com/l3/pkg"),
                    "[10] expected key 'github.com/l3/pkg': {result:?}"
                );
                assert_portable_keys(&result, "10-deeply-nested");
            }

            // Cleanup
            std::env::remove_var("SEMA_HOME");
            let _ = fs::remove_dir_all(&dir);
            let _ = fs::remove_dir_all(&sema_home);
        })
        .join()
        .unwrap();
    }
}
