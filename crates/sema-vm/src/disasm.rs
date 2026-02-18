use std::fmt::Write;

use sema_core::resolve;

use crate::chunk::Chunk;
use crate::opcodes::Op;

fn read_u16(code: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([code[offset], code[offset + 1]])
}

fn read_u32(code: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([
        code[offset],
        code[offset + 1],
        code[offset + 2],
        code[offset + 3],
    ])
}

fn read_i32(code: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes([
        code[offset],
        code[offset + 1],
        code[offset + 2],
        code[offset + 3],
    ])
}

fn op_name(op: Op) -> &'static str {
    match op {
        Op::Const => "CONST",
        Op::Nil => "NIL",
        Op::True => "TRUE",
        Op::False => "FALSE",
        Op::Pop => "POP",
        Op::Dup => "DUP",
        Op::LoadLocal => "LOAD_LOCAL",
        Op::StoreLocal => "STORE_LOCAL",
        Op::LoadUpvalue => "LOAD_UPVALUE",
        Op::StoreUpvalue => "STORE_UPVALUE",
        Op::LoadGlobal => "LOAD_GLOBAL",
        Op::StoreGlobal => "STORE_GLOBAL",
        Op::DefineGlobal => "DEFINE_GLOBAL",
        Op::Jump => "JUMP",
        Op::JumpIfFalse => "JUMP_IF_FALSE",
        Op::JumpIfTrue => "JUMP_IF_TRUE",
        Op::Call => "CALL",
        Op::TailCall => "TAIL_CALL",
        Op::Return => "RETURN",
        Op::MakeClosure => "MAKE_CLOSURE",
        Op::CallNative => "CALL_NATIVE",
        Op::MakeList => "MAKE_LIST",
        Op::MakeVector => "MAKE_VECTOR",
        Op::MakeMap => "MAKE_MAP",
        Op::MakeHashMap => "MAKE_HASHMAP",
        Op::Throw => "THROW",
        Op::Add => "ADD",
        Op::Sub => "SUB",
        Op::Mul => "MUL",
        Op::Div => "DIV",
        Op::Negate => "NEGATE",
        Op::Not => "NOT",
        Op::Eq => "EQ",
        Op::Lt => "LT",
        Op::Gt => "GT",
        Op::Le => "LE",
        Op::Ge => "GE",
        Op::AddInt => "ADD_INT",
        Op::SubInt => "SUB_INT",
        Op::MulInt => "MUL_INT",
        Op::LtInt => "LT_INT",
        Op::EqInt => "EQ_INT",
        Op::LoadLocal0 => "LOAD_LOCAL_0",
        Op::LoadLocal1 => "LOAD_LOCAL_1",
        Op::LoadLocal2 => "LOAD_LOCAL_2",
        Op::LoadLocal3 => "LOAD_LOCAL_3",
    }
}

/// Produce a human-readable disassembly of a Chunk.
pub fn disassemble(chunk: &Chunk, name: Option<&str>) -> String {
    let mut out = String::new();
    let label = name.unwrap_or("<script>");
    writeln!(out, "== {label} ==").unwrap();

    let code = &chunk.code;
    let mut pc = 0usize;

    while pc < code.len() {
        let op_byte = code[pc];
        let op = match op_from_u8(op_byte) {
            Some(op) => op,
            None => {
                writeln!(out, "{pc:04}  UNKNOWN({op_byte:#04x})").unwrap();
                pc += 1;
                continue;
            }
        };

        match op {
            Op::Const => {
                let idx = read_u16(code, pc + 1);
                let val = &chunk.consts[idx as usize];
                writeln!(out, "{pc:04}  {:<16} {idx:<4} ; {val}", op_name(op)).unwrap();
                pc += 3;
            }

            Op::LoadLocal | Op::StoreLocal | Op::LoadUpvalue | Op::StoreUpvalue => {
                let slot = read_u16(code, pc + 1);
                writeln!(out, "{pc:04}  {:<16} {slot}", op_name(op)).unwrap();
                pc += 3;
            }

            Op::LoadGlobal | Op::StoreGlobal | Op::DefineGlobal => {
                let spur_bits = read_u32(code, pc + 1);
                let spur = unsafe { std::mem::transmute::<u32, lasso::Spur>(spur_bits) };
                let name_str = resolve(spur);
                writeln!(
                    out,
                    "{pc:04}  {:<16} {spur_bits:<4} ; {name_str}",
                    op_name(op)
                )
                .unwrap();
                pc += 5;
            }

            Op::Jump | Op::JumpIfFalse | Op::JumpIfTrue => {
                let offset = read_i32(code, pc + 1);
                let target = (pc as i32 + 5 + offset) as u32;
                writeln!(
                    out,
                    "{pc:04}  {:<16} {offset:<4} ; -> {target:04}",
                    op_name(op)
                )
                .unwrap();
                pc += 5;
            }

            Op::Call | Op::TailCall => {
                let argc = read_u16(code, pc + 1);
                writeln!(out, "{pc:04}  {:<16} {argc}", op_name(op)).unwrap();
                pc += 3;
            }

            Op::CallNative => {
                let native_id = read_u16(code, pc + 1);
                let argc = read_u16(code, pc + 3);
                writeln!(
                    out,
                    "{pc:04}  {:<16} native={native_id} argc={argc}",
                    op_name(op)
                )
                .unwrap();
                pc += 5;
            }

            Op::MakeClosure => {
                let func_id = read_u16(code, pc + 1);
                let n_upvalues = read_u16(code, pc + 3);
                writeln!(
                    out,
                    "{pc:04}  {:<16} func={func_id} upvalues={n_upvalues}",
                    op_name(op)
                )
                .unwrap();
                pc += 5;
                for _ in 0..n_upvalues {
                    let is_local = read_u16(code, pc);
                    let idx = read_u16(code, pc + 2);
                    let kind = if is_local != 0 { "local" } else { "upvalue" };
                    writeln!(out, "        | {kind} {idx}").unwrap();
                    pc += 4;
                }
            }

            Op::MakeList | Op::MakeVector | Op::MakeMap | Op::MakeHashMap => {
                let count = read_u16(code, pc + 1);
                writeln!(out, "{pc:04}  {:<16} {count}", op_name(op)).unwrap();
                pc += 3;
            }

            // All zero-operand opcodes
            _ => {
                writeln!(out, "{pc:04}  {}", op_name(op)).unwrap();
                pc += 1;
            }
        }
    }

    out
}

fn op_from_u8(byte: u8) -> Option<Op> {
    const MAX_OP: u8 = Op::LoadLocal3 as u8;
    if byte > MAX_OP {
        return None;
    }
    // Safety: Op is repr(u8) and we've bounds-checked
    Some(unsafe { std::mem::transmute::<u8, Op>(byte) })
}

#[cfg(test)]
mod tests {
    use sema_core::Value;

    use super::*;
    use crate::emit::Emitter;

    #[test]
    fn test_disassemble_simple() {
        let mut e = Emitter::new();
        e.emit_const(Value::int(1));
        e.emit_const(Value::int(2));
        e.emit_op(Op::AddInt);
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();
        let output = disassemble(&chunk, Some("test"));
        assert!(output.contains("== test =="));
        assert!(output.contains("CONST"));
        assert!(output.contains("ADD_INT"));
        assert!(output.contains("RETURN"));
        assert!(output.contains("1"));
        assert!(output.contains("2"));
    }

    #[test]
    fn test_disassemble_jump() {
        let mut e = Emitter::new();
        let patch = e.emit_jump(Op::JumpIfFalse);
        e.emit_op(Op::Nil);
        e.patch_jump(patch);
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();
        let output = disassemble(&chunk, Some("jump_test"));
        assert!(output.contains("JUMP_IF_FALSE"));
        assert!(output.contains("->"));
    }

    #[test]
    fn test_disassemble_no_name() {
        let mut e = Emitter::new();
        e.emit_op(Op::Return);
        let chunk = e.into_chunk();
        let output = disassemble(&chunk, None);
        assert!(output.contains("== <script> =="));
    }
}
