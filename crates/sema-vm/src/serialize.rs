use hashbrown::HashMap;
use sema_core::{resolve, Spur};

/// Builds a deduplicated string table for serialization.
pub struct StringTableBuilder {
    strings: Vec<String>,
    index: HashMap<String, u32>,
}

impl StringTableBuilder {
    pub fn new() -> Self {
        let mut b = StringTableBuilder {
            strings: Vec::new(),
            index: HashMap::new(),
        };
        b.intern_str(""); // index 0 = empty string
        b
    }

    pub fn intern_str(&mut self, s: &str) -> u32 {
        if let Some(&idx) = self.index.get(s) {
            return idx;
        }
        let idx = self.strings.len() as u32;
        self.strings.push(s.to_string());
        self.index.insert(s.to_string(), idx);
        idx
    }

    pub fn intern_spur(&mut self, spur: Spur) -> u32 {
        let s = resolve(spur);
        self.intern_str(&s)
    }

    pub fn finish(self) -> Vec<String> {
        self.strings
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sema_core::intern;

    #[test]
    fn test_string_table_builder() {
        let mut builder = StringTableBuilder::new();
        // Index 0 is always ""
        assert_eq!(builder.intern_str(""), 0);
        let idx_hello = builder.intern_str("hello");
        let idx_world = builder.intern_str("world");
        let idx_hello2 = builder.intern_str("hello");
        assert_eq!(idx_hello, idx_hello2); // deduplication
        assert_ne!(idx_hello, idx_world);

        let table = builder.finish();
        assert_eq!(table.len(), 3); // "", "hello", "world"
        assert_eq!(table[0], "");
        assert_eq!(table[idx_hello as usize], "hello");
        assert_eq!(table[idx_world as usize], "world");
    }

    #[test]
    fn test_string_table_spur_interning() {
        let mut builder = StringTableBuilder::new();
        let spur = intern("my-var");
        let idx = builder.intern_spur(spur);
        assert!(idx > 0);
        let idx2 = builder.intern_spur(spur);
        assert_eq!(idx, idx2);
    }
}
