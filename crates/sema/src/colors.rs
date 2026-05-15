use std::io::IsTerminal;

fn enabled() -> bool {
    std::io::stderr().is_terminal() && std::env::var_os("NO_COLOR").is_none()
}

pub fn red_bold(s: &str) -> String {
    if enabled() {
        format!("\x1b[1;31m{s}\x1b[0m")
    } else {
        s.to_string()
    }
}
pub fn yellow(s: &str) -> String {
    if enabled() {
        format!("\x1b[33m{s}\x1b[0m")
    } else {
        s.to_string()
    }
}
pub fn cyan(s: &str) -> String {
    if enabled() {
        format!("\x1b[36m{s}\x1b[0m")
    } else {
        s.to_string()
    }
}
pub fn dim(s: &str) -> String {
    if enabled() {
        format!("\x1b[2m{s}\x1b[0m")
    } else {
        s.to_string()
    }
}
