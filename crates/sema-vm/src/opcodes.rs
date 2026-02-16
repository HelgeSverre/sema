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
    MakeClosure, // u16 func_id, u16 n_upvalues, then n * (u8 is_local, u16 idx)

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
}
