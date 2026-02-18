use hashbrown::HashMap;
use sema_core::{intern, resolve, SemaError, Span, Spur, Value, ValueView};

use crate::chunk::{Chunk, ExceptionEntry, Function, UpvalueDesc};
use crate::opcodes::Op;

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

// ── Spur remap table ──────────────────────────────────────────────

/// Build a remap table: for each string table index, intern it to get a process-local Spur.
pub fn build_remap_table(table: &[String]) -> Vec<Spur> {
    table.iter().map(|s| intern(s)).collect()
}

// ── Value tag constants (bytecode format) ─────────────────────────

const VAL_NIL: u8 = 0x00;
const VAL_BOOL: u8 = 0x01;
const VAL_INT: u8 = 0x02;
const VAL_FLOAT: u8 = 0x03;
const VAL_STRING: u8 = 0x04;
const VAL_SYMBOL: u8 = 0x05;
const VAL_KEYWORD: u8 = 0x06;
const VAL_CHAR: u8 = 0x07;
const VAL_LIST: u8 = 0x08;
const VAL_VECTOR: u8 = 0x09;
const VAL_MAP: u8 = 0x0A;
const VAL_HASHMAP: u8 = 0x0B;
const VAL_BYTEVECTOR: u8 = 0x0C;

// ── Checked conversions ───────────────────────────────────────────

fn checked_u16(n: usize, what: &str) -> Result<u16, SemaError> {
    u16::try_from(n).map_err(|_| {
        SemaError::eval(format!("{what} exceeds u16::MAX ({n})"))
    })
}

fn checked_u32(n: usize, what: &str) -> Result<u32, SemaError> {
    u32::try_from(n).map_err(|_| {
        SemaError::eval(format!("{what} exceeds u32::MAX ({n})"))
    })
}

// ── Value serialization ───────────────────────────────────────────

pub fn serialize_value(
    val: &Value,
    buf: &mut Vec<u8>,
    stb: &mut StringTableBuilder,
) -> Result<(), SemaError> {
    match val.view() {
        ValueView::Nil => buf.push(VAL_NIL),
        ValueView::Bool(b) => {
            buf.push(VAL_BOOL);
            buf.push(if b { 1 } else { 0 });
        }
        ValueView::Int(n) => {
            buf.push(VAL_INT);
            buf.extend_from_slice(&n.to_le_bytes());
        }
        ValueView::Float(f) => {
            buf.push(VAL_FLOAT);
            buf.extend_from_slice(&f.to_le_bytes());
        }
        ValueView::String(s) => {
            buf.push(VAL_STRING);
            let idx = stb.intern_str(&s);
            buf.extend_from_slice(&idx.to_le_bytes());
        }
        ValueView::Symbol(spur) => {
            buf.push(VAL_SYMBOL);
            let idx = stb.intern_spur(spur);
            buf.extend_from_slice(&idx.to_le_bytes());
        }
        ValueView::Keyword(spur) => {
            buf.push(VAL_KEYWORD);
            let idx = stb.intern_spur(spur);
            buf.extend_from_slice(&idx.to_le_bytes());
        }
        ValueView::Char(c) => {
            buf.push(VAL_CHAR);
            buf.extend_from_slice(&(c as u32).to_le_bytes());
        }
        ValueView::List(items) => {
            let len = checked_u16(items.len(), "list length")?;
            buf.push(VAL_LIST);
            buf.extend_from_slice(&len.to_le_bytes());
            for item in items.iter() {
                serialize_value(item, buf, stb)?;
            }
        }
        ValueView::Vector(items) => {
            let len = checked_u16(items.len(), "vector length")?;
            buf.push(VAL_VECTOR);
            buf.extend_from_slice(&len.to_le_bytes());
            for item in items.iter() {
                serialize_value(item, buf, stb)?;
            }
        }
        ValueView::Map(map) => {
            let len = checked_u16(map.len(), "map length")?;
            buf.push(VAL_MAP);
            buf.extend_from_slice(&len.to_le_bytes());
            for (k, v) in map.iter() {
                serialize_value(k, buf, stb)?;
                serialize_value(v, buf, stb)?;
            }
        }
        ValueView::HashMap(map) => {
            let len = checked_u16(map.len(), "hashmap length")?;
            buf.push(VAL_HASHMAP);
            buf.extend_from_slice(&len.to_le_bytes());
            for (k, v) in map.iter() {
                serialize_value(k, buf, stb)?;
                serialize_value(v, buf, stb)?;
            }
        }
        ValueView::Bytevector(bv) => {
            let len = checked_u32(bv.len(), "bytevector length")?;
            buf.push(VAL_BYTEVECTOR);
            buf.extend_from_slice(&len.to_le_bytes());
            buf.extend_from_slice(&bv);
        }
        // Runtime-only types cannot appear in bytecode constant pools
        _ => {
            return Err(SemaError::eval(format!(
                "cannot serialize {} to bytecode constant pool",
                val.type_name()
            )));
        }
    }
    Ok(())
}

// ── Value deserialization ─────────────────────────────────────────

fn read_u8(buf: &[u8], cursor: &mut usize) -> Result<u8, SemaError> {
    if *cursor >= buf.len() {
        return Err(SemaError::eval("unexpected end of bytecode data"));
    }
    let v = buf[*cursor];
    *cursor += 1;
    Ok(v)
}

fn read_u16_le(buf: &[u8], cursor: &mut usize) -> Result<u16, SemaError> {
    if *cursor + 2 > buf.len() {
        return Err(SemaError::eval("unexpected end of bytecode data"));
    }
    let v = u16::from_le_bytes([buf[*cursor], buf[*cursor + 1]]);
    *cursor += 2;
    Ok(v)
}

fn read_u32_le(buf: &[u8], cursor: &mut usize) -> Result<u32, SemaError> {
    if *cursor + 4 > buf.len() {
        return Err(SemaError::eval("unexpected end of bytecode data"));
    }
    let v = u32::from_le_bytes([buf[*cursor], buf[*cursor + 1], buf[*cursor + 2], buf[*cursor + 3]]);
    *cursor += 4;
    Ok(v)
}

fn read_i64_le(buf: &[u8], cursor: &mut usize) -> Result<i64, SemaError> {
    if *cursor + 8 > buf.len() {
        return Err(SemaError::eval("unexpected end of bytecode data"));
    }
    let v = i64::from_le_bytes(buf[*cursor..*cursor + 8].try_into().unwrap());
    *cursor += 8;
    Ok(v)
}

fn read_f64_le(buf: &[u8], cursor: &mut usize) -> Result<f64, SemaError> {
    if *cursor + 8 > buf.len() {
        return Err(SemaError::eval("unexpected end of bytecode data"));
    }
    let v = f64::from_le_bytes(buf[*cursor..*cursor + 8].try_into().unwrap());
    *cursor += 8;
    Ok(v)
}

fn read_bytes(buf: &[u8], cursor: &mut usize, len: usize) -> Result<Vec<u8>, SemaError> {
    if *cursor + len > buf.len() {
        return Err(SemaError::eval("unexpected end of bytecode data"));
    }
    let v = buf[*cursor..*cursor + len].to_vec();
    *cursor += len;
    Ok(v)
}

pub fn deserialize_value(
    buf: &[u8],
    cursor: &mut usize,
    table: &[String],
    remap: &[Spur],
) -> Result<Value, SemaError> {
    let tag = read_u8(buf, cursor)?;
    match tag {
        VAL_NIL => Ok(Value::nil()),
        VAL_BOOL => {
            let b = read_u8(buf, cursor)?;
            match b {
                0 => Ok(Value::bool(false)),
                1 => Ok(Value::bool(true)),
                _ => Err(SemaError::eval(format!(
                    "invalid bool payload in bytecode: 0x{b:02x}"
                ))),
            }
        }
        VAL_INT => {
            let n = read_i64_le(buf, cursor)?;
            Ok(Value::int(n))
        }
        VAL_FLOAT => {
            let f = read_f64_le(buf, cursor)?;
            Ok(Value::float(f))
        }
        VAL_STRING => {
            let idx = read_u32_le(buf, cursor)? as usize;
            if idx >= table.len() {
                return Err(SemaError::eval(format!(
                    "string table index {idx} out of range (table has {} entries)",
                    table.len()
                )));
            }
            Ok(Value::string(&table[idx]))
        }
        VAL_SYMBOL => {
            let idx = read_u32_le(buf, cursor)? as usize;
            if idx >= remap.len() {
                return Err(SemaError::eval(format!(
                    "string table index {idx} out of range for symbol remap"
                )));
            }
            Ok(Value::symbol_from_spur(remap[idx]))
        }
        VAL_KEYWORD => {
            let idx = read_u32_le(buf, cursor)? as usize;
            if idx >= remap.len() {
                return Err(SemaError::eval(format!(
                    "string table index {idx} out of range for keyword remap"
                )));
            }
            Ok(Value::keyword_from_spur(remap[idx]))
        }
        VAL_CHAR => {
            let cp = read_u32_le(buf, cursor)?;
            let c = char::from_u32(cp).ok_or_else(|| {
                SemaError::eval(format!("invalid unicode code point: {cp}"))
            })?;
            Ok(Value::char(c))
        }
        VAL_LIST => {
            let count = read_u16_le(buf, cursor)? as usize;
            let mut items = Vec::with_capacity(count);
            for _ in 0..count {
                items.push(deserialize_value(buf, cursor, table, remap)?);
            }
            Ok(Value::list(items))
        }
        VAL_VECTOR => {
            let count = read_u16_le(buf, cursor)? as usize;
            let mut items = Vec::with_capacity(count);
            for _ in 0..count {
                items.push(deserialize_value(buf, cursor, table, remap)?);
            }
            Ok(Value::vector(items))
        }
        VAL_MAP => {
            let n_pairs = read_u16_le(buf, cursor)? as usize;
            let mut map = std::collections::BTreeMap::new();
            for _ in 0..n_pairs {
                let k = deserialize_value(buf, cursor, table, remap)?;
                let v = deserialize_value(buf, cursor, table, remap)?;
                map.insert(k, v);
            }
            Ok(Value::map(map))
        }
        VAL_HASHMAP => {
            let n_pairs = read_u16_le(buf, cursor)? as usize;
            let mut entries = Vec::with_capacity(n_pairs);
            for _ in 0..n_pairs {
                let k = deserialize_value(buf, cursor, table, remap)?;
                let v = deserialize_value(buf, cursor, table, remap)?;
                entries.push((k, v));
            }
            Ok(Value::hashmap(entries))
        }
        VAL_BYTEVECTOR => {
            let len = read_u32_le(buf, cursor)? as usize;
            let data = read_bytes(buf, cursor, len)?;
            Ok(Value::bytevector(data))
        }
        _ => Err(SemaError::eval(format!(
            "unknown value tag in bytecode: 0x{tag:02x}"
        ))),
    }
}

// ── Chunk serialization ───────────────────────────────────────────

pub fn serialize_chunk(
    chunk: &Chunk,
    buf: &mut Vec<u8>,
    stb: &mut StringTableBuilder,
) -> Result<(), SemaError> {
    // code — remap Spur operands to string table indices before writing
    let remapped_code = remap_spurs_to_indices(&chunk.code, stb)?;
    let code_len = checked_u32(remapped_code.len(), "bytecode length")?;
    buf.extend_from_slice(&code_len.to_le_bytes());
    buf.extend_from_slice(&remapped_code);

    // constants
    let n_consts = checked_u16(chunk.consts.len(), "constant pool size")?;
    buf.extend_from_slice(&n_consts.to_le_bytes());
    for val in &chunk.consts {
        serialize_value(val, buf, stb)?;
    }

    // spans: Vec<(u32, Span)> where Span { line: usize, col: usize }
    let n_spans = checked_u32(chunk.spans.len(), "span count")?;
    buf.extend_from_slice(&n_spans.to_le_bytes());
    for &(pc, ref span) in &chunk.spans {
        buf.extend_from_slice(&pc.to_le_bytes());
        let line = checked_u32(span.line, "span line")?;
        let col = checked_u32(span.col, "span col")?;
        buf.extend_from_slice(&line.to_le_bytes());
        buf.extend_from_slice(&col.to_le_bytes());
    }

    // max_stack, n_locals
    buf.extend_from_slice(&chunk.max_stack.to_le_bytes());
    buf.extend_from_slice(&chunk.n_locals.to_le_bytes());

    // exception table
    let n_exceptions = checked_u16(chunk.exception_table.len(), "exception table size")?;
    buf.extend_from_slice(&n_exceptions.to_le_bytes());
    for entry in &chunk.exception_table {
        buf.extend_from_slice(&entry.try_start.to_le_bytes());
        buf.extend_from_slice(&entry.try_end.to_le_bytes());
        buf.extend_from_slice(&entry.handler_pc.to_le_bytes());
        buf.extend_from_slice(&entry.stack_depth.to_le_bytes());
        buf.extend_from_slice(&entry.catch_slot.to_le_bytes());
    }

    Ok(())
}

pub fn deserialize_chunk(
    buf: &[u8],
    cursor: &mut usize,
    table: &[String],
    remap: &[Spur],
) -> Result<Chunk, SemaError> {
    // code — remap string table indices back to process-local Spurs
    let code_len = read_u32_le(buf, cursor)? as usize;
    let mut code = read_bytes(buf, cursor, code_len)?;
    remap_indices_to_spurs(&mut code, remap)?;

    // constants
    let n_consts = read_u16_le(buf, cursor)? as usize;
    let mut consts = Vec::with_capacity(n_consts);
    for _ in 0..n_consts {
        consts.push(deserialize_value(buf, cursor, table, remap)?);
    }

    // spans
    let n_spans = read_u32_le(buf, cursor)? as usize;
    let mut spans = Vec::with_capacity(n_spans);
    for _ in 0..n_spans {
        let pc = read_u32_le(buf, cursor)?;
        let line = read_u32_le(buf, cursor)? as usize;
        let col = read_u32_le(buf, cursor)? as usize;
        spans.push((pc, Span { line, col }));
    }

    // max_stack, n_locals
    let max_stack = read_u16_le(buf, cursor)?;
    let n_locals = read_u16_le(buf, cursor)?;

    // exception table
    let n_exceptions = read_u16_le(buf, cursor)? as usize;
    let mut exception_table = Vec::with_capacity(n_exceptions);
    for _ in 0..n_exceptions {
        let try_start = read_u32_le(buf, cursor)?;
        let try_end = read_u32_le(buf, cursor)?;
        let handler_pc = read_u32_le(buf, cursor)?;
        let stack_depth = read_u16_le(buf, cursor)?;
        let catch_slot = read_u16_le(buf, cursor)?;
        exception_table.push(ExceptionEntry {
            try_start,
            try_end,
            handler_pc,
            stack_depth,
            catch_slot,
        });
    }

    Ok(Chunk {
        code,
        consts,
        spans,
        max_stack,
        n_locals,
        exception_table,
    })
}

// ── Function serialization ────────────────────────────────────────

const ANONYMOUS_NAME: u32 = 0xFFFF_FFFF;

pub fn serialize_function(
    func: &Function,
    buf: &mut Vec<u8>,
    stb: &mut StringTableBuilder,
) -> Result<(), SemaError> {
    // name: u32 string table index (0xFFFFFFFF = anonymous)
    match func.name {
        Some(spur) => {
            let idx = stb.intern_spur(spur);
            buf.extend_from_slice(&idx.to_le_bytes());
        }
        None => buf.extend_from_slice(&ANONYMOUS_NAME.to_le_bytes()),
    }

    // arity: u16
    buf.extend_from_slice(&func.arity.to_le_bytes());

    // has_rest: u8
    buf.push(if func.has_rest { 1 } else { 0 });

    // upvalue descriptors
    let n_upvalues = checked_u16(func.upvalue_descs.len(), "upvalue descriptor count")?;
    buf.extend_from_slice(&n_upvalues.to_le_bytes());
    for desc in &func.upvalue_descs {
        match desc {
            UpvalueDesc::ParentLocal(idx) => {
                buf.push(0);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
            UpvalueDesc::ParentUpvalue(idx) => {
                buf.push(1);
                buf.extend_from_slice(&idx.to_le_bytes());
            }
        }
    }

    // chunk
    serialize_chunk(&func.chunk, buf, stb)?;

    // local_names: Vec<(u16, Spur)>
    let n_local_names = checked_u16(func.local_names.len(), "local name count")?;
    buf.extend_from_slice(&n_local_names.to_le_bytes());
    for &(slot, spur) in &func.local_names {
        buf.extend_from_slice(&slot.to_le_bytes());
        let idx = stb.intern_spur(spur);
        buf.extend_from_slice(&idx.to_le_bytes());
    }

    Ok(())
}

pub fn deserialize_function(
    buf: &[u8],
    cursor: &mut usize,
    table: &[String],
    remap: &[Spur],
) -> Result<Function, SemaError> {
    // name
    let name_idx = read_u32_le(buf, cursor)?;
    let name = if name_idx == ANONYMOUS_NAME {
        None
    } else {
        let idx = name_idx as usize;
        if idx >= remap.len() {
            return Err(SemaError::eval(format!(
                "function name string table index {idx} out of range"
            )));
        }
        Some(remap[idx])
    };

    // arity
    let arity = read_u16_le(buf, cursor)?;

    // has_rest
    let has_rest_byte = read_u8(buf, cursor)?;
    let has_rest = match has_rest_byte {
        0 => false,
        1 => true,
        _ => {
            return Err(SemaError::eval(format!(
                "invalid has_rest byte: 0x{has_rest_byte:02x}"
            )));
        }
    };

    // upvalue descriptors
    let n_upvalues = read_u16_le(buf, cursor)? as usize;
    let mut upvalue_descs = Vec::with_capacity(n_upvalues);
    for _ in 0..n_upvalues {
        let kind = read_u8(buf, cursor)?;
        let index = read_u16_le(buf, cursor)?;
        match kind {
            0 => upvalue_descs.push(UpvalueDesc::ParentLocal(index)),
            1 => upvalue_descs.push(UpvalueDesc::ParentUpvalue(index)),
            _ => {
                return Err(SemaError::eval(format!(
                    "invalid upvalue kind: 0x{kind:02x}"
                )));
            }
        }
    }

    // chunk
    let chunk = deserialize_chunk(buf, cursor, table, remap)?;

    // local_names
    let n_local_names = read_u16_le(buf, cursor)? as usize;
    let mut local_names = Vec::with_capacity(n_local_names);
    for _ in 0..n_local_names {
        let slot = read_u16_le(buf, cursor)?;
        let name_idx = read_u32_le(buf, cursor)? as usize;
        if name_idx >= remap.len() {
            return Err(SemaError::eval(format!(
                "local name string table index {name_idx} out of range"
            )));
        }
        local_names.push((slot, remap[name_idx]));
    }

    Ok(Function {
        name,
        chunk,
        upvalue_descs,
        arity,
        has_rest,
        local_names,
    })
}

// ── Spur remapping in bytecode ────────────────────────────────────

fn spur_to_u32(spur: Spur) -> u32 {
    unsafe { std::mem::transmute::<Spur, u32>(spur) }
}

fn u32_to_spur(bits: u32) -> Spur {
    unsafe { std::mem::transmute::<u32, Spur>(bits) }
}

/// Walk bytecode and rewrite global opcodes: Spur u32 → string table index.
/// Returns the rewritten code.
pub fn remap_spurs_to_indices(
    code: &[u8],
    stb: &mut StringTableBuilder,
) -> Result<Vec<u8>, SemaError> {
    let mut out = code.to_vec();
    let mut pc = 0;
    while pc < out.len() {
        let Some(op) = Op::from_u8(out[pc]) else {
            return Err(SemaError::eval(format!(
                "invalid opcode 0x{:02x} at pc {pc}",
                out[pc]
            )));
        };
        match op {
            Op::LoadGlobal | Op::StoreGlobal | Op::DefineGlobal => {
                let spur_bits =
                    u32::from_le_bytes([out[pc + 1], out[pc + 2], out[pc + 3], out[pc + 4]]);
                let spur = u32_to_spur(spur_bits);
                let s = resolve(spur);
                let idx = stb.intern_str(&s);
                let bytes = idx.to_le_bytes();
                out[pc + 1] = bytes[0];
                out[pc + 2] = bytes[1];
                out[pc + 3] = bytes[2];
                out[pc + 4] = bytes[3];
                pc += 5;
            }
            Op::MakeClosure => {
                let n_upvalues =
                    u16::from_le_bytes([out[pc + 3], out[pc + 4]]) as usize;
                pc += 1 + 2 + 2 + n_upvalues * 4;
            }
            Op::Const
            | Op::LoadLocal
            | Op::StoreLocal
            | Op::LoadUpvalue
            | Op::StoreUpvalue
            | Op::Call
            | Op::TailCall
            | Op::MakeList
            | Op::MakeVector
            | Op::MakeMap
            | Op::MakeHashMap => {
                pc += 3; // op + u16
            }
            Op::CallNative => {
                pc += 5; // op + u16 + u16
            }
            Op::Jump | Op::JumpIfFalse | Op::JumpIfTrue => {
                pc += 5; // op + i32
            }
            _ => {
                pc += 1;
            }
        }
    }
    Ok(out)
}

/// Walk bytecode and rewrite global opcodes: string table index → process-local Spur u32.
pub fn remap_indices_to_spurs(
    code: &mut [u8],
    remap: &[Spur],
) -> Result<(), SemaError> {
    let mut pc = 0;
    while pc < code.len() {
        let Some(op) = Op::from_u8(code[pc]) else {
            return Err(SemaError::eval(format!(
                "invalid opcode 0x{:02x} at pc {pc}",
                code[pc]
            )));
        };
        match op {
            Op::LoadGlobal | Op::StoreGlobal | Op::DefineGlobal => {
                let idx =
                    u32::from_le_bytes([code[pc + 1], code[pc + 2], code[pc + 3], code[pc + 4]])
                        as usize;
                if idx >= remap.len() {
                    return Err(SemaError::eval(format!(
                        "global spur remap index {idx} out of range at pc {pc}"
                    )));
                }
                let spur_bits = spur_to_u32(remap[idx]);
                let bytes = spur_bits.to_le_bytes();
                code[pc + 1] = bytes[0];
                code[pc + 2] = bytes[1];
                code[pc + 3] = bytes[2];
                code[pc + 4] = bytes[3];
                pc += 5;
            }
            Op::MakeClosure => {
                let n_upvalues =
                    u16::from_le_bytes([code[pc + 3], code[pc + 4]]) as usize;
                pc += 1 + 2 + 2 + n_upvalues * 4;
            }
            Op::Const
            | Op::LoadLocal
            | Op::StoreLocal
            | Op::LoadUpvalue
            | Op::StoreUpvalue
            | Op::Call
            | Op::TailCall
            | Op::MakeList
            | Op::MakeVector
            | Op::MakeMap
            | Op::MakeHashMap => {
                pc += 3;
            }
            Op::CallNative => {
                pc += 5;
            }
            Op::Jump | Op::JumpIfFalse | Op::JumpIfTrue => {
                pc += 5;
            }
            _ => {
                pc += 1;
            }
        }
    }
    Ok(())
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

    #[test]
    fn test_chunk_roundtrip() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let mut e = Emitter::new();
        e.emit_const(Value::int(42));
        e.emit_const(Value::string("hello"));
        e.emit_op(Op::Add);
        e.emit_op(Op::Return);
        let mut chunk = e.into_chunk();
        chunk.n_locals = 2;
        chunk.max_stack = 4;

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_chunk(&chunk, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let chunk2 = deserialize_chunk(&buf, &mut cursor, &table, &remap).unwrap();

        assert_eq!(chunk2.code, chunk.code);
        assert_eq!(chunk2.consts.len(), chunk.consts.len());
        assert_eq!(chunk2.n_locals, 2);
        assert_eq!(chunk2.max_stack, 4);
    }

    // ── Float edge cases ────────────────────────────────────────

    #[test]
    fn test_serialize_float_nan() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_value(&Value::float(f64::NAN), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let v = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert!(v.as_float().unwrap().is_nan());
    }

    #[test]
    fn test_serialize_float_neg_zero() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        let neg_zero = Value::float(-0.0);
        serialize_value(&neg_zero, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let v = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        let f = v.as_float().unwrap();
        assert!(f.is_sign_negative());
        assert_eq!(f.to_bits(), (-0.0f64).to_bits());
    }

    #[test]
    fn test_serialize_float_infinities() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_value(&Value::float(f64::INFINITY), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::float(f64::NEG_INFINITY), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let v1 = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(v1.as_float(), Some(f64::INFINITY));
        let v2 = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(v2.as_float(), Some(f64::NEG_INFINITY));
    }

    // ── Int edge cases ───────────────────────────────────────────

    #[test]
    fn test_serialize_int_extremes() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_value(&Value::int(i64::MIN), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::int(i64::MAX), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::int(0), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::int(-1), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::int(i64::MIN)
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::int(i64::MAX)
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::int(0)
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::int(-1)
        );
    }

    // ── Empty collections ────────────────────────────────────────

    #[test]
    fn test_serialize_empty_collections() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();

        serialize_value(&Value::list(vec![]), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::vector(vec![]), &mut buf, &mut stb).unwrap();
        serialize_value(
            &Value::map(std::collections::BTreeMap::new()),
            &mut buf,
            &mut stb,
        )
        .unwrap();
        serialize_value(&Value::hashmap(vec![]), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::bytevector(vec![]), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;

        let l = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(l.as_list().unwrap().len(), 0);
        let v = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(v.as_vector().unwrap().len(), 0);
        let m = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(m.as_map_rc().unwrap().len(), 0);
        let hm = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(hm.as_hashmap_rc().unwrap().len(), 0);
        let bv = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(bv.as_bytevector().unwrap().len(), 0);
    }

    // ── Nested collections ───────────────────────────────────────

    #[test]
    fn test_serialize_nested_collections() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();

        // vector of lists
        let nested = Value::vector(vec![
            Value::list(vec![Value::int(1), Value::int(2)]),
            Value::list(vec![Value::string("a"), Value::symbol("b")]),
        ]);
        serialize_value(&nested, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let v = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(v, nested);
    }

    // ── Char roundtrip ───────────────────────────────────────────

    #[test]
    fn test_serialize_char() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_value(&Value::char('A'), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::char('🦀'), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::char('A')
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::char('🦀')
        );
    }

    // ── Bytevector roundtrip ─────────────────────────────────────

    #[test]
    fn test_serialize_bytevector() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        let data = vec![0u8, 1, 2, 255, 128, 64];
        serialize_value(&Value::bytevector(data.clone()), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let v = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(v.as_bytevector().unwrap(), &data);
    }

    // ── Invalid data deserialization ─────────────────────────────

    #[test]
    fn test_deserialize_invalid_bool() {
        let buf = vec![VAL_BOOL, 0x02]; // invalid: not 0 or 1
        let table: Vec<String> = vec![];
        let remap: Vec<Spur> = vec![];
        let mut cursor = 0;
        let result = deserialize_value(&buf, &mut cursor, &table, &remap);
        assert!(result.is_err());
    }

    #[test]
    fn test_deserialize_invalid_char() {
        // 0xD800 is a surrogate — not a valid Unicode scalar value
        let mut buf = vec![VAL_CHAR];
        buf.extend_from_slice(&0xD800u32.to_le_bytes());
        let table: Vec<String> = vec![];
        let remap: Vec<Spur> = vec![];
        let mut cursor = 0;
        let result = deserialize_value(&buf, &mut cursor, &table, &remap);
        assert!(result.is_err());
    }

    #[test]
    fn test_deserialize_unknown_tag() {
        let buf = vec![0xFF];
        let table: Vec<String> = vec![];
        let remap: Vec<Spur> = vec![];
        let mut cursor = 0;
        let result = deserialize_value(&buf, &mut cursor, &table, &remap);
        assert!(result.is_err());
    }

    #[test]
    fn test_deserialize_truncated_data() {
        // Int tag but only 3 bytes of payload instead of 8
        let buf = vec![VAL_INT, 0x01, 0x02, 0x03];
        let table: Vec<String> = vec![];
        let remap: Vec<Spur> = vec![];
        let mut cursor = 0;
        let result = deserialize_value(&buf, &mut cursor, &table, &remap);
        assert!(result.is_err());
    }

    #[test]
    fn test_deserialize_string_index_out_of_range() {
        let mut buf = vec![VAL_STRING];
        buf.extend_from_slice(&99u32.to_le_bytes()); // index 99, but table is smaller
        let table = vec!["".to_string()];
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let result = deserialize_value(&buf, &mut cursor, &table, &remap);
        assert!(result.is_err());
    }

    // ── Runtime-only types rejected ──────────────────────────────

    #[test]
    fn test_serialize_runtime_only_type_rejected() {
        use sema_core::{Env, Lambda};
        let lambda = Value::lambda(Lambda {
            params: vec![],
            rest_param: None,
            body: vec![],
            env: Env::new(),
            name: None,
        });
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        let result = serialize_value(&lambda, &mut buf, &mut stb);
        assert!(result.is_err());
    }

    // ── Chunk edge cases ─────────────────────────────────────────

    #[test]
    fn test_chunk_roundtrip_with_exceptions() {
        use crate::chunk::ExceptionEntry;
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let mut e = Emitter::new();
        e.emit_op(Op::Nil);
        e.emit_op(Op::Return);
        let mut chunk = e.into_chunk();
        chunk.exception_table = vec![
            ExceptionEntry {
                try_start: 0,
                try_end: 10,
                handler_pc: 20,
                stack_depth: 3,
                catch_slot: 5,
            },
            ExceptionEntry {
                try_start: 100,
                try_end: 200,
                handler_pc: 300,
                stack_depth: 0,
                catch_slot: 7,
            },
        ];

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_chunk(&chunk, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let chunk2 = deserialize_chunk(&buf, &mut cursor, &table, &remap).unwrap();

        assert_eq!(chunk2.exception_table.len(), 2);
        assert_eq!(chunk2.exception_table[0].try_start, 0);
        assert_eq!(chunk2.exception_table[0].try_end, 10);
        assert_eq!(chunk2.exception_table[0].handler_pc, 20);
        assert_eq!(chunk2.exception_table[0].stack_depth, 3);
        assert_eq!(chunk2.exception_table[0].catch_slot, 5);
        assert_eq!(chunk2.exception_table[1].try_start, 100);
        assert_eq!(chunk2.exception_table[1].handler_pc, 300);
    }

    #[test]
    fn test_chunk_roundtrip_with_spans() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let mut e = Emitter::new();
        e.emit_op(Op::Nil);
        e.emit_op(Op::Return);
        let mut chunk = e.into_chunk();
        chunk.spans = vec![
            (0, Span { line: 1, col: 5 }),
            (1, Span { line: 2, col: 10 }),
        ];

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_chunk(&chunk, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let chunk2 = deserialize_chunk(&buf, &mut cursor, &table, &remap).unwrap();

        assert_eq!(chunk2.spans.len(), 2);
        assert_eq!(chunk2.spans[0].0, 0);
        assert_eq!(chunk2.spans[0].1.line, 1);
        assert_eq!(chunk2.spans[0].1.col, 5);
        assert_eq!(chunk2.spans[1].0, 1);
        assert_eq!(chunk2.spans[1].1.line, 2);
        assert_eq!(chunk2.spans[1].1.col, 10);
    }

    #[test]
    fn test_chunk_deserialize_truncated() {
        // A chunk with code_len=100 but only a few bytes in the buffer
        let mut buf = Vec::new();
        buf.extend_from_slice(&100u32.to_le_bytes()); // claims 100 bytes of code
        buf.extend_from_slice(&[0u8; 4]); // only 4 bytes, not 100

        let table: Vec<String> = vec![];
        let remap: Vec<Spur> = vec![];
        let mut cursor = 0;
        let result = deserialize_chunk(&buf, &mut cursor, &table, &remap);
        assert!(result.is_err());
    }

    // ── Spur remapping ─────────────────────────────────────────

    #[test]
    fn test_spur_remapping_in_bytecode() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let spur = intern("my-global");
        let mut e = Emitter::new();
        e.emit_op(Op::LoadGlobal);
        e.emit_u32(spur_to_u32(spur));
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_chunk(&chunk, &mut buf, &mut stb).unwrap();

        // Deserialize — the spur in the deserialized bytecode should resolve to "my-global"
        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let chunk2 = deserialize_chunk(&buf, &mut cursor, &table, &remap).unwrap();

        let spur2_bits = u32::from_le_bytes([
            chunk2.code[1],
            chunk2.code[2],
            chunk2.code[3],
            chunk2.code[4],
        ]);
        let spur2 = u32_to_spur(spur2_bits);
        assert_eq!(sema_core::resolve(spur2), "my-global");
    }

    #[test]
    fn test_spur_remapping_multiple_globals() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let spur_a = intern("alpha");
        let spur_b = intern("beta");
        let mut e = Emitter::new();
        e.emit_op(Op::LoadGlobal);
        e.emit_u32(spur_to_u32(spur_a));
        e.emit_op(Op::DefineGlobal);
        e.emit_u32(spur_to_u32(spur_b));
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_chunk(&chunk, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let chunk2 = deserialize_chunk(&buf, &mut cursor, &table, &remap).unwrap();

        // Check both globals resolved correctly
        let bits_a = u32::from_le_bytes([
            chunk2.code[1],
            chunk2.code[2],
            chunk2.code[3],
            chunk2.code[4],
        ]);
        assert_eq!(sema_core::resolve(u32_to_spur(bits_a)), "alpha");

        let bits_b = u32::from_le_bytes([
            chunk2.code[6],
            chunk2.code[7],
            chunk2.code[8],
            chunk2.code[9],
        ]);
        assert_eq!(sema_core::resolve(u32_to_spur(bits_b)), "beta");
    }

    // ── Function serialization ─────────────────────────────────

    #[test]
    fn test_function_roundtrip() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let mut e = Emitter::new();
        e.emit_op(Op::LoadLocal0);
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();

        let func = Function {
            name: Some(intern("my-func")),
            chunk,
            upvalue_descs: vec![UpvalueDesc::ParentLocal(0), UpvalueDesc::ParentUpvalue(1)],
            arity: 2,
            has_rest: true,
            local_names: vec![(0, intern("x")), (1, intern("y"))],
        };

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_function(&func, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let func2 = deserialize_function(&buf, &mut cursor, &table, &remap).unwrap();

        assert_eq!(func2.arity, 2);
        assert!(func2.has_rest);
        assert_eq!(func2.upvalue_descs.len(), 2);
        assert_eq!(func2.local_names.len(), 2);
        assert!(func2.name.is_some());
        assert_eq!(sema_core::resolve(func2.name.unwrap()), "my-func");
        assert_eq!(sema_core::resolve(func2.local_names[0].1), "x");
        assert_eq!(sema_core::resolve(func2.local_names[1].1), "y");
    }

    #[test]
    fn test_function_roundtrip_anonymous() {
        use crate::emit::Emitter;
        use crate::opcodes::Op;

        let mut e = Emitter::new();
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();

        let func = Function {
            name: None,
            chunk,
            upvalue_descs: vec![],
            arity: 0,
            has_rest: false,
            local_names: vec![],
        };

        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();
        serialize_function(&func, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        let func2 = deserialize_function(&buf, &mut cursor, &table, &remap).unwrap();

        assert!(func2.name.is_none());
        assert_eq!(func2.arity, 0);
        assert!(!func2.has_rest);
        assert_eq!(func2.upvalue_descs.len(), 0);
    }

    // ── Unicode string table ─────────────────────────────────────

    #[test]
    fn test_string_table_unicode() {
        let mut builder = StringTableBuilder::new();
        let idx1 = builder.intern_str("こんにちは");
        let idx2 = builder.intern_str("🦀");
        let idx3 = builder.intern_str("café");

        let table = builder.finish();
        assert_eq!(table[idx1 as usize], "こんにちは");
        assert_eq!(table[idx2 as usize], "🦀");
        assert_eq!(table[idx3 as usize], "café");
    }

    #[test]
    fn test_serialize_value_roundtrip_primitives() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();

        serialize_value(&Value::nil(), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::bool(true), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::bool(false), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::int(42), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::float(3.14), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::string("hello"), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::symbol("foo"), &mut buf, &mut stb).unwrap();
        serialize_value(&Value::keyword("bar"), &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::nil()
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::bool(true)
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::bool(false)
        );
        assert_eq!(
            deserialize_value(&buf, &mut cursor, &table, &remap).unwrap(),
            Value::int(42)
        );
        let f = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(f.as_float(), Some(3.14));
        let s = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(s.as_str().unwrap(), "hello");
        let sym = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert!(sym.as_symbol().is_some());
        let kw = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert!(kw.as_keyword().is_some());
    }

    #[test]
    fn test_serialize_value_roundtrip_collections() {
        let mut buf = Vec::new();
        let mut stb = StringTableBuilder::new();

        let list = Value::list(vec![Value::int(1), Value::int(2), Value::int(3)]);
        serialize_value(&list, &mut buf, &mut stb).unwrap();

        let vec = Value::vector(vec![Value::string("a"), Value::string("b")]);
        serialize_value(&vec, &mut buf, &mut stb).unwrap();

        let table = stb.finish();
        let remap = build_remap_table(&table);
        let mut cursor = 0;

        let list2 = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(list2, list);

        let vec2 = deserialize_value(&buf, &mut cursor, &table, &remap).unwrap();
        assert_eq!(vec2, vec);
    }
}
