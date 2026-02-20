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
