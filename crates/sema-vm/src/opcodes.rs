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
    LoadLocal0, // = 42
    LoadLocal1, // = 43
    LoadLocal2, // = 44
    LoadLocal3, // = 45
}

impl Op {
    pub fn from_u8(byte: u8) -> Option<Op> {
        match byte {
            0 => Some(Op::Const),
            1 => Some(Op::Nil),
            2 => Some(Op::True),
            3 => Some(Op::False),
            4 => Some(Op::Pop),
            5 => Some(Op::Dup),
            6 => Some(Op::LoadLocal),
            7 => Some(Op::StoreLocal),
            8 => Some(Op::LoadUpvalue),
            9 => Some(Op::StoreUpvalue),
            10 => Some(Op::LoadGlobal),
            11 => Some(Op::StoreGlobal),
            12 => Some(Op::DefineGlobal),
            13 => Some(Op::Jump),
            14 => Some(Op::JumpIfFalse),
            15 => Some(Op::JumpIfTrue),
            16 => Some(Op::Call),
            17 => Some(Op::TailCall),
            18 => Some(Op::Return),
            19 => Some(Op::MakeClosure),
            20 => Some(Op::CallNative),
            21 => Some(Op::MakeList),
            22 => Some(Op::MakeVector),
            23 => Some(Op::MakeMap),
            24 => Some(Op::MakeHashMap),
            25 => Some(Op::Throw),
            26 => Some(Op::Add),
            27 => Some(Op::Sub),
            28 => Some(Op::Mul),
            29 => Some(Op::Div),
            30 => Some(Op::Negate),
            31 => Some(Op::Not),
            32 => Some(Op::Eq),
            33 => Some(Op::Lt),
            34 => Some(Op::Gt),
            35 => Some(Op::Le),
            36 => Some(Op::Ge),
            37 => Some(Op::AddInt),
            38 => Some(Op::SubInt),
            39 => Some(Op::MulInt),
            40 => Some(Op::LtInt),
            41 => Some(Op::EqInt),
            42 => Some(Op::LoadLocal0),
            43 => Some(Op::LoadLocal1),
            44 => Some(Op::LoadLocal2),
            45 => Some(Op::LoadLocal3),
            _ => None,
        }
    }
}
