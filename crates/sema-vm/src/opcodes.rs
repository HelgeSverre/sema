/// Bytecode opcodes for the Sema VM.
///
/// Stack-based: operands are pushed/popped from the value stack.
/// Variable-length encoding: opcode (1 byte) + operands (u16/u32/i32).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    // Constants & stack
    Const, // u16 const_index → push constants[i]
    Nil,   // push nil
    True,  // push #t
    False, // push #f
    Pop,   // discard TOS
    Dup,   // duplicate TOS

    // Locals (slot-addressed within call frame)
    LoadLocal,  // u16 slot → push locals[slot]
    StoreLocal, // u16 slot → locals[slot] = pop

    // Upvalues (captured variables from enclosing scopes)
    LoadUpvalue,  // u16 index → push upvalues[i].get()
    StoreUpvalue, // u16 index → upvalues[i].set(pop)

    // Globals (module-level bindings, keyed by Spur)
    LoadGlobal,   // u32 spur → push globals[spur]
    StoreGlobal,  // u32 spur → globals[spur] = pop
    DefineGlobal, // u32 spur → globals[spur] = pop (define, not set!)

    // Control flow
    Jump,        // i32 relative offset
    JumpIfFalse, // i32 relative offset (pop condition)
    JumpIfTrue,  // i32 relative offset (pop condition)

    // Function calls
    Call,     // u16 argc → call TOS-argc with argc args
    TailCall, // u16 argc → tail call (reuse frame)
    Return,   // return TOS

    // Closures
    MakeClosure, // u16 func_id, u16 n_upvalues, then n * (u16 is_local, u16 idx)

    // Native function calls (fast path)
    CallNative, // u16 native_id, u16 argc

    // Data constructors
    MakeList,    // u16 n → pop n values, push list
    MakeVector,  // u16 n → pop n values, push vector
    MakeMap,     // u16 n_pairs → pop 2n values, push map
    MakeHashMap, // u16 n_pairs → pop 2n values, push hashmap

    // Exception handling
    Throw, // pop value, throw as exception

    // Generic arithmetic & comparison
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Not,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,

    // Specialized int arithmetic (fast paths)
    AddInt,
    SubInt,
    MulInt,
    LtInt,
    EqInt,

    // Specialized zero-operand locals (most common slots)
    LoadLocal0,  // = 42
    LoadLocal1,  // = 43
    LoadLocal2,  // = 44
    LoadLocal3,  // = 45
    StoreLocal0, // = 46
    StoreLocal1, // = 47
    StoreLocal2, // = 48
    StoreLocal3, // = 49

    // Fused global call (LOAD_GLOBAL + CALL in one instruction)
    CallGlobal, // u32 spur, u16 argc → lookup global, call with argc args

    // Inline stdlib intrinsics (bypass CallGlobal overhead)
    Car,    // pop list, push first element (or nil if empty)
    Cdr,    // pop list, push rest (tail)
    Cons,   // pop head, pop tail → push new list
    IsNull, // pop value, push #t if nil or empty list
    IsPair, // pop value, push #t if non-empty list
    IsList, // pop value, push #t if list
    IsNumber, // pop value, push #t if int or float
    IsString, // pop value, push #t if string
    IsSymbol, // pop value, push #t if symbol
    Length, // pop collection, push its length as int
}

impl Op {
    /// Convert a raw byte to an Op. Valid because the enum is `#[repr(u8)]` with
    /// dense variants from 0 through `CallGlobal`. If new variants are added with
    /// gaps, this must be updated.
    pub fn from_u8(byte: u8) -> Option<Op> {
        if byte <= Op::Length as u8 {
            // SAFETY: Op is #[repr(u8)] with dense, contiguous variants 0..=CallGlobal.
            Some(unsafe { std::mem::transmute::<u8, Op>(byte) })
        } else {
            None
        }
    }
}

/// Opcode constants for use in match patterns (avoids `Op::from_u8` overhead).
pub mod op {
    use super::Op;
    pub const CONST: u8 = Op::Const as u8;
    pub const NIL: u8 = Op::Nil as u8;
    pub const TRUE: u8 = Op::True as u8;
    pub const FALSE: u8 = Op::False as u8;
    pub const POP: u8 = Op::Pop as u8;
    pub const DUP: u8 = Op::Dup as u8;
    pub const LOAD_LOCAL: u8 = Op::LoadLocal as u8;
    pub const STORE_LOCAL: u8 = Op::StoreLocal as u8;
    pub const LOAD_UPVALUE: u8 = Op::LoadUpvalue as u8;
    pub const STORE_UPVALUE: u8 = Op::StoreUpvalue as u8;
    pub const LOAD_GLOBAL: u8 = Op::LoadGlobal as u8;
    pub const STORE_GLOBAL: u8 = Op::StoreGlobal as u8;
    pub const DEFINE_GLOBAL: u8 = Op::DefineGlobal as u8;
    pub const JUMP: u8 = Op::Jump as u8;
    pub const JUMP_IF_FALSE: u8 = Op::JumpIfFalse as u8;
    pub const JUMP_IF_TRUE: u8 = Op::JumpIfTrue as u8;
    pub const CALL: u8 = Op::Call as u8;
    pub const TAIL_CALL: u8 = Op::TailCall as u8;
    pub const RETURN: u8 = Op::Return as u8;
    pub const MAKE_CLOSURE: u8 = Op::MakeClosure as u8;
    pub const CALL_NATIVE: u8 = Op::CallNative as u8;
    pub const MAKE_LIST: u8 = Op::MakeList as u8;
    pub const MAKE_VECTOR: u8 = Op::MakeVector as u8;
    pub const MAKE_MAP: u8 = Op::MakeMap as u8;
    pub const MAKE_HASH_MAP: u8 = Op::MakeHashMap as u8;
    pub const THROW: u8 = Op::Throw as u8;
    pub const ADD: u8 = Op::Add as u8;
    pub const SUB: u8 = Op::Sub as u8;
    pub const MUL: u8 = Op::Mul as u8;
    pub const DIV: u8 = Op::Div as u8;
    pub const NEGATE: u8 = Op::Negate as u8;
    pub const NOT: u8 = Op::Not as u8;
    pub const EQ: u8 = Op::Eq as u8;
    pub const LT: u8 = Op::Lt as u8;
    pub const GT: u8 = Op::Gt as u8;
    pub const LE: u8 = Op::Le as u8;
    pub const GE: u8 = Op::Ge as u8;
    pub const ADD_INT: u8 = Op::AddInt as u8;
    pub const SUB_INT: u8 = Op::SubInt as u8;
    pub const MUL_INT: u8 = Op::MulInt as u8;
    pub const LT_INT: u8 = Op::LtInt as u8;
    pub const EQ_INT: u8 = Op::EqInt as u8;
    pub const LOAD_LOCAL0: u8 = Op::LoadLocal0 as u8;
    pub const LOAD_LOCAL1: u8 = Op::LoadLocal1 as u8;
    pub const LOAD_LOCAL2: u8 = Op::LoadLocal2 as u8;
    pub const LOAD_LOCAL3: u8 = Op::LoadLocal3 as u8;
    pub const STORE_LOCAL0: u8 = Op::StoreLocal0 as u8;
    pub const STORE_LOCAL1: u8 = Op::StoreLocal1 as u8;
    pub const STORE_LOCAL2: u8 = Op::StoreLocal2 as u8;
    pub const STORE_LOCAL3: u8 = Op::StoreLocal3 as u8;
    pub const CALL_GLOBAL: u8 = Op::CallGlobal as u8;
    pub const CAR: u8 = Op::Car as u8;
    pub const CDR: u8 = Op::Cdr as u8;
    pub const CONS: u8 = Op::Cons as u8;
    pub const IS_NULL: u8 = Op::IsNull as u8;
    pub const IS_PAIR: u8 = Op::IsPair as u8;
    pub const IS_LIST: u8 = Op::IsList as u8;
    pub const IS_NUMBER: u8 = Op::IsNumber as u8;
    pub const IS_STRING: u8 = Op::IsString as u8;
    pub const IS_SYMBOL: u8 = Op::IsSymbol as u8;
    pub const LENGTH: u8 = Op::Length as u8;

    // Instruction sizes (opcode byte + operand bytes)
    /// Size of a bare opcode with no operands: 1
    pub const SIZE_OP: usize = 1;
    /// Size of an instruction with a u16 operand (e.g., CALL, LOAD_LOCAL): 1 + 2 = 3
    pub const SIZE_OP_U16: usize = 3;
    /// Size of an instruction with a u32 operand (e.g., LOAD_GLOBAL): 1 + 4 = 5
    pub const SIZE_OP_U32: usize = 5;
    /// Size of CALL_GLOBAL: 1 + u32 spur + u16 argc = 7
    pub const SIZE_CALL_GLOBAL: usize = 7;
}
