//! Small UTF-8-safe string helpers shared across crates.

/// Return the prefix of `s` containing at most `max_chars` characters, always
/// landing on a UTF-8 char boundary. Replaces the `&s[..N]` byte-slicing
/// (Pattern B in the 2026-05-29 audit) that panics when byte `N` falls inside a
/// multi-byte character.
pub fn truncate_chars(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
        Some((byte_idx, _)) => &s[..byte_idx],
        None => s,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncate_chars_never_splits_a_char() {
        // 40 lambdas prefixed by one ASCII char: byte 39 lands inside a 2-byte 'λ'.
        let s = format!("x{}", "λ".repeat(40));
        let out = truncate_chars(&s, 39);
        // Must not panic and must be a valid prefix on a char boundary.
        assert!(s.starts_with(out));
        assert_eq!(out.chars().count(), 39);
    }

    #[test]
    fn truncate_chars_returns_whole_string_when_short() {
        assert_eq!(truncate_chars("hello", 39), "hello");
    }
}
