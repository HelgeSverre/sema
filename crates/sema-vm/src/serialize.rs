use hashbrown::HashMap;
use sema_core::{intern, resolve, SemaError, Spur, Value, ValueView};

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
            buf.push(VAL_LIST);
            buf.extend_from_slice(&(items.len() as u16).to_le_bytes());
            for item in items.iter() {
                serialize_value(item, buf, stb)?;
            }
        }
        ValueView::Vector(items) => {
            buf.push(VAL_VECTOR);
            buf.extend_from_slice(&(items.len() as u16).to_le_bytes());
            for item in items.iter() {
                serialize_value(item, buf, stb)?;
            }
        }
        ValueView::Map(map) => {
            buf.push(VAL_MAP);
            buf.extend_from_slice(&(map.len() as u16).to_le_bytes());
            for (k, v) in map.iter() {
                serialize_value(k, buf, stb)?;
                serialize_value(v, buf, stb)?;
            }
        }
        ValueView::HashMap(map) => {
            buf.push(VAL_HASHMAP);
            buf.extend_from_slice(&(map.len() as u16).to_le_bytes());
            for (k, v) in map.iter() {
                serialize_value(k, buf, stb)?;
                serialize_value(v, buf, stb)?;
            }
        }
        ValueView::Bytevector(bv) => {
            buf.push(VAL_BYTEVECTOR);
            buf.extend_from_slice(&(bv.len() as u32).to_le_bytes());
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
            Ok(Value::bool(b != 0))
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
