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
        .next_back()
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

#[cfg(test)]
mod tests {
    use super::*;

    // -- validate_vfs_path --

    #[test]
    fn test_validate_empty_path() {
        assert!(validate_vfs_path("").is_err());
    }

    #[test]
    fn test_validate_absolute_unix() {
        assert!(validate_vfs_path("/etc/passwd").is_err());
    }

    #[test]
    fn test_validate_absolute_windows() {
        assert!(validate_vfs_path("\\windows\\system32").is_err());
    }

    #[test]
    fn test_validate_nul_byte() {
        assert!(validate_vfs_path("foo\0bar").is_err());
    }

    #[test]
    fn test_validate_dotdot() {
        assert!(validate_vfs_path("../etc/passwd").is_err());
        assert!(validate_vfs_path("foo/../bar").is_err());
    }

    #[test]
    fn test_validate_reserved_device_names() {
        for name in &[
            "CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "LPT1", "LPT2", "LPT3",
        ] {
            assert!(
                validate_vfs_path(name).is_err(),
                "{name} should be rejected"
            );
            let with_ext = format!("{name}.txt");
            assert!(
                validate_vfs_path(&with_ext).is_err(),
                "{with_ext} should be rejected"
            );
        }
    }

    #[test]
    fn test_validate_ok_paths() {
        assert!(validate_vfs_path("lib/utils.sema").is_ok());
        assert!(validate_vfs_path("main.sema").is_ok());
        assert!(validate_vfs_path("data/config.json").is_ok());
    }

    // -- normalize_path --

    #[test]
    fn test_normalize_removes_cur_dir() {
        let p = std::path::Path::new("./foo/./bar");
        assert_eq!(normalize_path(p), "foo/bar");
    }

    #[test]
    fn test_normalize_resolves_parent_dir() {
        let p = std::path::Path::new("foo/baz/../bar");
        assert_eq!(normalize_path(p), "foo/bar");
    }

    #[test]
    fn test_normalize_simple_path() {
        let p = std::path::Path::new("lib/utils.sema");
        assert_eq!(normalize_path(p), "lib/utils.sema");
    }

    // -- VFS lifecycle (each test uses a fresh thread for isolation) --

    #[test]
    fn test_vfs_inactive_by_default() {
        std::thread::spawn(|| {
            assert!(!is_vfs_active());
            assert_eq!(vfs_read("anything"), None);
            assert_eq!(vfs_exists("anything"), None);
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_init_and_read() {
        std::thread::spawn(|| {
            let mut files = HashMap::new();
            files.insert("hello.sema".to_string(), b"(+ 1 2)".to_vec());
            init_vfs(files);

            assert!(is_vfs_active());
            assert_eq!(vfs_read("hello.sema"), Some(b"(+ 1 2)".to_vec()));
            assert_eq!(vfs_read("missing.sema"), None);
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_vfs_exists_active() {
        std::thread::spawn(|| {
            let mut files = HashMap::new();
            files.insert("exists.txt".to_string(), vec![]);
            init_vfs(files);

            assert_eq!(vfs_exists("exists.txt"), Some(true));
            assert_eq!(vfs_exists("ghost.txt"), Some(false));
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_resolve_and_read_direct() {
        std::thread::spawn(|| {
            let mut files = HashMap::new();
            files.insert("lib/foo.sema".to_string(), b"data".to_vec());
            init_vfs(files);

            assert_eq!(
                vfs_resolve_and_read("lib/foo.sema", None),
                Some(b"data".to_vec())
            );
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_resolve_and_read_with_base_dir() {
        std::thread::spawn(|| {
            let mut files = HashMap::new();
            files.insert("lib/foo.sema".to_string(), b"data".to_vec());
            init_vfs(files);

            assert_eq!(
                vfs_resolve_and_read("foo.sema", Some("lib")),
                Some(b"data".to_vec())
            );
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_resolve_and_read_miss() {
        std::thread::spawn(|| {
            let mut files = HashMap::new();
            files.insert("lib/foo.sema".to_string(), b"data".to_vec());
            init_vfs(files);

            assert_eq!(vfs_resolve_and_read("bar.sema", Some("other")), None);
            assert_eq!(vfs_resolve_and_read("missing.sema", None), None);
        })
        .join()
        .unwrap();
    }
}
