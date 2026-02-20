use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static EMBEDDED_VFS: RefCell<Option<HashMap<String, Vec<u8>>>> = const { RefCell::new(None) };
}

/// Initialize the VFS with embedded files. Called once at startup for bundled binaries.
pub fn init_vfs(files: HashMap<String, Vec<u8>>) {
    EMBEDDED_VFS.with(|vfs| {
        *vfs.borrow_mut() = Some(files);
    });
}

/// Read a file from the VFS. Returns None if VFS is inactive or file not found.
pub fn vfs_read(path: &str) -> Option<Vec<u8>> {
    EMBEDDED_VFS.with(|vfs| {
        let vfs = vfs.borrow();
        vfs.as_ref()?.get(path).cloned()
    })
}

/// Check if a file exists in the VFS. Returns None if VFS is inactive.
pub fn vfs_exists(path: &str) -> Option<bool> {
    EMBEDDED_VFS.with(|vfs| {
        let vfs = vfs.borrow();
        let map = vfs.as_ref()?;
        Some(map.contains_key(path))
    })
}

/// Check if the VFS is active (has been initialized).
pub fn is_vfs_active() -> bool {
    EMBEDDED_VFS.with(|vfs| vfs.borrow().is_some())
}

/// Try to resolve a path against the VFS, normalizing it.
/// The `base_dir` is used to resolve relative paths.
/// Returns the VFS content if found.
pub fn vfs_resolve_and_read(path: &str, base_dir: Option<&str>) -> Option<Vec<u8>> {
    // Try the path as-is first
    if let Some(data) = vfs_read(path) {
        return Some(data);
    }

    // Try resolving relative to base_dir
    if let Some(base) = base_dir {
        let resolved = std::path::Path::new(base).join(path);
        let normalized = normalize_path(&resolved);
        if let Some(data) = vfs_read(&normalized) {
            return Some(data);
        }
    }

    None
}

/// Normalize a path by resolving `.` and `..` components without hitting the filesystem.
fn normalize_path(path: &std::path::Path) -> String {
    let mut components = Vec::new();
    for comp in path.components() {
        match comp {
            std::path::Component::CurDir => {} // skip "."
            std::path::Component::ParentDir => {
                components.pop(); // handle ".."
            }
            other => components.push(other.as_os_str().to_string_lossy().to_string()),
        }
    }
    components.join("/")
}

/// Validate a VFS path at build time. Rejects unsafe paths.
pub fn validate_vfs_path(path: &str) -> Result<(), String> {
    if path.is_empty() {
        return Err("empty VFS path".to_string());
    }
    if path.starts_with('/') || path.starts_with('\\') {
        return Err(format!("absolute path not allowed in VFS: {path}"));
    }
    if path.contains('\0') {
        return Err(format!("NUL byte in VFS path: {path}"));
    }
    if path.contains("..") {
        return Err(format!("path traversal not allowed in VFS: {path}"));
    }
    // Reject Windows device names
    let stem = path
        .split('/')
        .last()
        .unwrap_or(path)
        .split('.')
        .next()
        .unwrap_or("");
    let upper = stem.to_uppercase();
    if matches!(
        upper.as_str(),
        "CON" | "PRN" | "AUX" | "NUL" | "COM1" | "COM2" | "COM3" | "LPT1" | "LPT2" | "LPT3"
    ) {
        return Err(format!("reserved device name in VFS path: {path}"));
    }
    Ok(())
}
