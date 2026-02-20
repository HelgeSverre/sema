//! Static import/load tracing for `sema build`.
//!
//! Walks the AST of a root source file and all transitively imported files,
//! collecting their contents into a map suitable for bundling into a VFS
//! archive. Only literal string paths can be resolved statically; dynamic
//! imports produce a warning on stderr.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

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
                        process_import(
                            path_str,
                            current_file,
                            root_dir,
                            visited,
                            result,
                        )?;
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
    let contents = std::fs::read(&canonical).map_err(|e| {
        format!("cannot read {}: {e}", canonical.display())
    })?;

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
