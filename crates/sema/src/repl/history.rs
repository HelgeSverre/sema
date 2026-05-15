use std::path::PathBuf;

use reedline::{FileBackedHistory, History};

const HISTORY_CAPACITY: usize = 5000;

/// Build a `FileBackedHistory` rooted at `~/.sema/history.txt`.
///
/// If a rustyline-format file already exists at that path, reedline may
/// reject it (rustyline writes raw lines; reedline expects its own
/// `#V2\t...` framing). On parse failure we back the file up to
/// `history.txt.rustyline.bak` so the user keeps their data, then start
/// fresh.
pub fn build_history() -> Box<dyn History> {
    let path = history_path();

    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    if rustyline_format_detected(&path) {
        let backup = path.with_extension("txt.rustyline.bak");
        let _ = std::fs::rename(&path, &backup);
    }

    match FileBackedHistory::with_file(HISTORY_CAPACITY, path) {
        Ok(h) => Box::new(h),
        // Permission / filesystem hiccups: fall back to in-memory so the
        // REPL still starts.
        Err(_) => Box::new(
            FileBackedHistory::new(HISTORY_CAPACITY)
                .expect("in-memory history with non-zero capacity"),
        ),
    }
}

pub fn history_path() -> PathBuf {
    sema_core::sema_home().join("history.txt")
}

/// Reedline's file format starts with a `#V2` header; rustyline's doesn't.
/// If the file exists and lacks that header, treat it as rustyline-legacy.
fn rustyline_format_detected(path: &PathBuf) -> bool {
    if !path.exists() {
        return false;
    }
    let Ok(contents) = std::fs::read_to_string(path) else {
        return false;
    };
    let trimmed = contents.trim_start();
    if trimmed.is_empty() {
        return false;
    }
    !trimmed.starts_with("#V2") && !trimmed.starts_with("{\"")
}
