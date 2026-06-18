use std::collections::HashMap;

use sema_core::{SemaError, Span, Value};

use crate::chunk::Chunk;
use crate::opcodes::Op;

/// Builder for constructing bytecode chunks.
pub struct Emitter {
    chunk: Chunk,
    /// Reverse index for constant pool deduplication: Value → pool index.
    const_dedup: HashMap<Value, u16>,
}

impl Emitter {
    pub fn new() -> Self {
        Emitter {
            chunk: Chunk::new(),
            const_dedup: HashMap::new(),
        }
    }

    pub fn emit_op(&mut self, op: Op) {
        self.chunk.code.push(op as u8);
    }

    pub fn emit_u16(&mut self, val: u16) {
        self.chunk.code.extend_from_slice(&val.to_le_bytes());
    }

    pub fn emit_u32(&mut self, val: u32) {
        self.chunk.code.extend_from_slice(&val.to_le_bytes());
    }

    pub fn emit_i32(&mut self, val: i32) {
        self.chunk.code.extend_from_slice(&val.to_le_bytes());
    }

    /// Add a constant to the pool, deduplicating by value equality.
    /// Returns the u16 index into the constant pool.
    ///
    /// Errors if the pool would exceed `u16::MAX` entries: the index is encoded
    /// as a u16 in `Op::Const`, so a larger pool would wrap the index onto a
    /// previously-used slot and silently load the wrong constant (VM-6).
    pub fn add_const(&mut self, val: Value) -> Result<u16, SemaError> {
        if let Some(&idx) = self.const_dedup.get(&val) {
            return Ok(idx);
        }
        let idx = u16::try_from(self.chunk.consts.len()).map_err(|_| {
            SemaError::eval("constant pool overflow: a single compilation unit cannot hold more than 65536 unique constants")
        })?;
        self.const_dedup.insert(val.clone(), idx);
        self.chunk.consts.push(val);
        Ok(idx)
    }

    /// Emit `Op::Const` followed by the u16 constant index.
    pub fn emit_const(&mut self, val: Value) -> Result<(), SemaError> {
        let idx = self.add_const(val)?;
        self.emit_op(Op::Const);
        self.emit_u16(idx);
        Ok(())
    }

    /// Record a source span at the current PC position.
    pub fn emit_span(&mut self, span: Span) {
        self.chunk.spans.push((self.current_pc(), span));
    }

    /// Current code length as u32.
    pub fn current_pc(&self) -> u32 {
        self.chunk.code.len() as u32
    }

    /// Emit a jump instruction with a placeholder i32 offset.
    /// Returns the PC of the placeholder for later backpatching.
    pub fn emit_jump(&mut self, op: Op) -> u32 {
        self.emit_op(op);
        let placeholder_pc = self.current_pc();
        self.emit_i32(0);
        placeholder_pc
    }

    /// Backpatch the i32 at `placeholder_pc` with the relative offset
    /// from the end of the jump instruction to the current PC.
    pub fn patch_jump(&mut self, placeholder_pc: u32) {
        let jump_end = placeholder_pc + 4; // end of the i32 operand
        let offset = self.current_pc() as i32 - jump_end as i32;
        let bytes = offset.to_le_bytes();
        let pc = placeholder_pc as usize;
        self.chunk.code[pc] = bytes[0];
        self.chunk.code[pc + 1] = bytes[1];
        self.chunk.code[pc + 2] = bytes[2];
        self.chunk.code[pc + 3] = bytes[3];
    }

    /// Consume the emitter and return the finished Chunk.
    pub fn into_chunk(self) -> Chunk {
        self.chunk
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_const() {
        let mut e = Emitter::new();
        e.emit_const(Value::int(42)).unwrap();
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();
        assert_eq!(chunk.code[0], Op::Const as u8);
        assert_eq!(chunk.code[1], 0);
        assert_eq!(chunk.code[2], 0);
        assert_eq!(chunk.code[3], Op::Return as u8);
        assert_eq!(chunk.consts.len(), 1);
        assert_eq!(chunk.consts[0], Value::int(42));
    }

    #[test]
    fn test_emit_jump_and_patch() {
        let mut e = Emitter::new();
        e.emit_op(Op::Nil); // PC 0
        let patch = e.emit_jump(Op::JumpIfFalse); // PC 1 (op), placeholder at PC 2-5
        e.emit_op(Op::True); // PC 6
        e.emit_op(Op::Return); // PC 7
        e.patch_jump(patch); // target is current_pc = 8, jump_end = 6, offset = 8 - 6 = 2
        e.emit_op(Op::False); // PC 8
        e.emit_op(Op::Return); // PC 9
        let chunk = e.into_chunk();
        let offset = i32::from_le_bytes(chunk.code[2..6].try_into().unwrap());
        // jump_end = patch(2) + 4 = 6, target = 8 at time of patch, offset = 8 - 6 = 2
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_const_dedup() {
        let mut e = Emitter::new();
        let idx1 = e.add_const(Value::int(42)).unwrap();
        let idx2 = e.add_const(Value::int(42)).unwrap();
        assert_eq!(idx1, idx2);
        assert_eq!(e.into_chunk().consts.len(), 1);
    }

    #[test]
    fn test_const_dedup_strings() {
        let mut e = Emitter::new();
        let idx1 = e.add_const(Value::string("hello")).unwrap();
        let idx2 = e.add_const(Value::string("hello")).unwrap();
        let idx3 = e.add_const(Value::string("world")).unwrap();
        assert_eq!(idx1, idx2, "same string should dedup");
        assert_ne!(idx1, idx3, "different strings should not dedup");
        assert_eq!(e.into_chunk().consts.len(), 2);
    }

    #[test]
    fn test_const_dedup_floats() {
        let mut e = Emitter::new();
        let idx1 = e.add_const(Value::float(1.25)).unwrap();
        let idx2 = e.add_const(Value::float(1.25)).unwrap();
        let idx3 = e.add_const(Value::float(2.5)).unwrap();
        assert_eq!(idx1, idx2, "same float should dedup");
        assert_ne!(idx1, idx3, "different floats should not dedup");
        assert_eq!(e.into_chunk().consts.len(), 2);
    }

    #[test]
    fn test_const_dedup_neg_zero_vs_pos_zero() {
        let mut e = Emitter::new();
        let idx1 = e.add_const(Value::float(0.0)).unwrap();
        let idx2 = e.add_const(Value::float(-0.0)).unwrap();
        // -0.0 and +0.0 are equal per IEEE 754, so they dedup to the same constant
        assert_eq!(
            idx1, idx2,
            "-0.0 and +0.0 should dedup (equal per IEEE 754)"
        );
        assert_eq!(e.into_chunk().consts.len(), 1);
    }

    #[test]
    fn test_const_dedup_nan() {
        let mut e = Emitter::new();
        let idx1 = e.add_const(Value::float(f64::NAN)).unwrap();
        let idx2 = e.add_const(Value::float(f64::NAN)).unwrap();
        // NaN != NaN per IEEE 754, so each NaN gets its own pool entry.
        // HashMap::get uses Eq, and NaN != NaN, so no dedup occurs.
        assert_ne!(idx1, idx2, "NaN should not dedup with itself");
        assert_eq!(e.into_chunk().consts.len(), 2);
    }

    #[test]
    fn test_const_dedup_keywords() {
        let mut e = Emitter::new();
        let idx1 = e.add_const(Value::keyword("name")).unwrap();
        let idx2 = e.add_const(Value::keyword("name")).unwrap();
        let idx3 = e.add_const(Value::keyword("age")).unwrap();
        assert_eq!(idx1, idx2, "same keyword should dedup");
        assert_ne!(idx1, idx3, "different keywords should not dedup");
        assert_eq!(e.into_chunk().consts.len(), 2);
    }

    #[test]
    fn test_const_dedup_mixed_types_no_collision() {
        let mut e = Emitter::new();
        let idx_int = e.add_const(Value::int(1)).unwrap();
        let idx_float = e.add_const(Value::float(1.0)).unwrap();
        let idx_str = e.add_const(Value::string("1")).unwrap();
        let idx_bool = e.add_const(Value::bool(true)).unwrap();
        // All different types, should not dedup
        assert_ne!(idx_int, idx_float);
        assert_ne!(idx_int, idx_str);
        assert_ne!(idx_int, idx_bool);
        assert_eq!(e.into_chunk().consts.len(), 4);
    }

    #[test]
    fn test_add_const_overflow_errors() {
        // VM-6: the constant index is a u16, so exactly 65536 (0..=u16::MAX)
        // unique constants fit. The 65537th must error rather than wrap the
        // index back onto an already-used slot.
        let mut e = Emitter::new();
        for i in 0..=u16::MAX as i64 {
            let idx = e.add_const(Value::int(i)).expect("first 65536 consts fit");
            assert_eq!(idx as i64, i);
        }
        let err = e
            .add_const(Value::int(u16::MAX as i64 + 1))
            .expect_err("65537th unique constant must overflow");
        assert!(
            err.to_string().contains("constant pool overflow"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn test_emit_span() {
        let mut e = Emitter::new();
        e.emit_span(Span::point(1, 0));
        e.emit_op(Op::Nil);
        e.emit_span(Span::point(2, 4));
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();
        assert_eq!(chunk.spans.len(), 2);
        assert_eq!(chunk.spans[0].0, 0);
        assert_eq!(chunk.spans[0].1.line, 1);
        assert_eq!(chunk.spans[0].1.col, 0);
        assert_eq!(chunk.spans[1].0, 1);
        assert_eq!(chunk.spans[1].1.line, 2);
        assert_eq!(chunk.spans[1].1.col, 4);
    }

    #[test]
    fn test_current_pc() {
        let mut e = Emitter::new();
        assert_eq!(e.current_pc(), 0);
        e.emit_op(Op::Nil);
        assert_eq!(e.current_pc(), 1);
        e.emit_u16(42);
        assert_eq!(e.current_pc(), 3);
        e.emit_u32(100);
        assert_eq!(e.current_pc(), 7);
    }
}
