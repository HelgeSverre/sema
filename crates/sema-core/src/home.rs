use std::path::PathBuf;

/// Returns the sema home directory.
/// Resolution: $SEMA_HOME > $HOME/.sema > %USERPROFILE%\.sema > .sema
pub fn sema_home() -> PathBuf {
    if let Ok(p) = std::env::var("SEMA_HOME") {
        return PathBuf::from(p);
    }
    if let Ok(home) = std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")) {
        return PathBuf::from(home).join(".sema");
    }
    PathBuf::from(".sema")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    // `SEMA_HOME` is a PROCESS-GLOBAL env var — spawning a thread does NOT isolate
    // it, so env-mutating tests race (one sets "/custom/sema" while the other reads
    // it, intermittently failing under parallel/coverage runs). Serialize the
    // env-mutating sections with a shared lock; poison-tolerant so a panic in one
    // test doesn't wedge the other.
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn test_sema_home_from_env_var() {
        let _guard = ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        std::env::set_var("SEMA_HOME", "/custom/sema");
        let p = sema_home();
        std::env::remove_var("SEMA_HOME");
        assert_eq!(p, PathBuf::from("/custom/sema"));
    }

    #[test]
    fn test_sema_home_default_uses_home_dir() {
        let _guard = ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        std::env::remove_var("SEMA_HOME");
        // HOME is expected to be set in normal environments
        if std::env::var("HOME").is_ok() || std::env::var("USERPROFILE").is_ok() {
            let p = sema_home();
            assert!(
                p.to_string_lossy().ends_with(".sema"),
                "expected path ending in .sema, got: {}",
                p.display()
            );
        }
    }
}
