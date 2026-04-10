//! Embedded browser UI for the notebook.
//!
//! The UI assets (HTML, CSS, JS) live as separate files in the `ui/`
//! directory next to this module. They are embedded into the binary at
//! compile time via `include_str!`, keeping deployment as a single
//! binary while allowing the assets to be edited as normal files.

/// Return the main HTML page.
pub fn index_html() -> String {
    include_str!("ui/index.html").to_string()
}

/// Serve a UI asset by path. Returns (content, content_type).
pub fn asset(path: &str) -> Option<(String, String)> {
    match path {
        "style.css" => Some((css().to_string(), "text/css".to_string())),
        "notebook.js" => Some((js().to_string(), "application/javascript".to_string())),
        _ => None,
    }
}

fn css() -> &'static str {
    include_str!("ui/style.css")
}

fn js() -> &'static str {
    include_str!("ui/notebook.js")
}
