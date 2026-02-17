use sema_core::{Spur, Value};

/// How a variable reference was resolved by the resolver pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarResolution {
    /// Local variable in the current function frame.
    Local { slot: u16 },
    /// Captured variable from an enclosing function scope.
    Upvalue { index: u16 },
    /// Module-level / global binding.
    Global { spur: Spur },
}

/// A resolved variable reference (name preserved for debugging).
#[derive(Debug, Clone, Copy)]
pub struct VarRef {
    pub name: Spur,
    pub resolution: VarResolution,
}

/// Desugared core language. Variables referenced by name (Spur).
/// The variable resolver replaces names with slot indices in a later pass.
#[derive(Debug, Clone)]
pub enum CoreExpr {
    /// Literal constant
    Const(Value),
    /// Variable reference
    Var(Spur),
    /// if-then-else
    If {
        test: Box<CoreExpr>,
        then: Box<CoreExpr>,
        else_: Box<CoreExpr>,
    },
    /// Sequence of expressions
    Begin(Vec<CoreExpr>),
    /// Variable mutation (set!)
    Set(Spur, Box<CoreExpr>),
    /// Closure creation
    Lambda(LambdaDef),
    /// Function call (tail flag for TCO)
    Call {
        func: Box<CoreExpr>,
        args: Vec<CoreExpr>,
        tail: bool,
    },
    /// Variable definition (define)
    Define(Spur, Box<CoreExpr>),
    /// Parallel binding
    Let {
        bindings: Vec<(Spur, CoreExpr)>,
        body: Vec<CoreExpr>,
    },
    /// Sequential binding
    LetStar {
        bindings: Vec<(Spur, CoreExpr)>,
        body: Vec<CoreExpr>,
    },
    /// Recursive binding
    Letrec {
        bindings: Vec<(Spur, CoreExpr)>,
        body: Vec<CoreExpr>,
    },
    /// Named let (loop construct)
    NamedLet {
        name: Spur,
        bindings: Vec<(Spur, CoreExpr)>,
        body: Vec<CoreExpr>,
    },
    /// Do loop
    Do(DoLoop),
    /// Try/catch
    Try {
        body: Vec<CoreExpr>,
        catch_var: Spur,
        handler: Vec<CoreExpr>,
    },
    /// Throw exception
    Throw(Box<CoreExpr>),
    /// Short-circuit and
    And(Vec<CoreExpr>),
    /// Short-circuit or
    Or(Vec<CoreExpr>),
    /// Quoted value (no evaluation)
    Quote(Value),
    /// List constructor (evaluate elements)
    MakeList(Vec<CoreExpr>),
    /// Vector constructor (evaluate elements)
    MakeVector(Vec<CoreExpr>),
    /// Map constructor (evaluate key-value pairs)
    MakeMap(Vec<(CoreExpr, CoreExpr)>),
    /// Macro definition (runtime)
    Defmacro {
        name: Spur,
        params: Vec<Spur>,
        rest: Option<Spur>,
        body: Vec<CoreExpr>,
    },
    /// Record type definition
    DefineRecordType {
        type_name: Spur,
        ctor_name: Spur,
        pred_name: Spur,
        field_names: Vec<Spur>,
        field_specs: Vec<(Spur, Spur)>,
    },
    /// Module declaration
    Module {
        name: Spur,
        exports: Vec<Spur>,
        body: Vec<CoreExpr>,
    },
    /// Import a module
    Import {
        path: Box<CoreExpr>,
        selective: Vec<Spur>,
    },
    /// Load a file in current env
    Load(Box<CoreExpr>),
    /// Dynamic eval
    Eval(Box<CoreExpr>),
    /// Prompt (LLM data constructor)
    Prompt(Vec<PromptEntry>),
    /// Message (LLM data constructor)
    Message {
        role: Box<CoreExpr>,
        parts: Vec<CoreExpr>,
    },
    /// Tool definition (LLM)
    Deftool {
        name: Spur,
        description: Box<CoreExpr>,
        parameters: Box<CoreExpr>,
        handler: Box<CoreExpr>,
    },
    /// Agent definition (LLM)
    Defagent { name: Spur, options: Box<CoreExpr> },
    /// Delay (create thunk)
    Delay(Box<CoreExpr>),
    /// Force (evaluate thunk)
    Force(Box<CoreExpr>),
    /// Macroexpand
    Macroexpand(Box<CoreExpr>),
}

/// A prompt entry: either a role-content form or an expression.
#[derive(Debug, Clone)]
pub enum PromptEntry {
    RoleContent { role: String, parts: Vec<CoreExpr> },
    Expr(CoreExpr),
}

#[derive(Debug, Clone)]
pub struct LambdaDef {
    pub name: Option<Spur>,
    pub params: Vec<Spur>,
    pub rest: Option<Spur>,
    pub body: Vec<CoreExpr>,
}

#[derive(Debug, Clone)]
pub struct DoLoop {
    pub vars: Vec<DoVar>,
    pub test: Box<CoreExpr>,
    pub result: Vec<CoreExpr>,
    pub body: Vec<CoreExpr>,
}

#[derive(Debug, Clone)]
pub struct DoVar {
    pub name: Spur,
    pub init: CoreExpr,
    pub step: Option<CoreExpr>,
}

// --- Resolved IR (output of variable resolution pass) ---

/// Resolved expression: like CoreExpr but variables are resolved to slots/upvalues/globals.
#[derive(Debug, Clone)]
pub enum ResolvedExpr {
    Const(Value),
    Var(VarRef),
    If {
        test: Box<ResolvedExpr>,
        then: Box<ResolvedExpr>,
        else_: Box<ResolvedExpr>,
    },
    Begin(Vec<ResolvedExpr>),
    Set(VarRef, Box<ResolvedExpr>),
    Lambda(ResolvedLambda),
    Call {
        func: Box<ResolvedExpr>,
        args: Vec<ResolvedExpr>,
        tail: bool,
    },
    Define(Spur, Box<ResolvedExpr>),
    Let {
        bindings: Vec<(VarRef, ResolvedExpr)>,
        body: Vec<ResolvedExpr>,
    },
    LetStar {
        bindings: Vec<(VarRef, ResolvedExpr)>,
        body: Vec<ResolvedExpr>,
    },
    Letrec {
        bindings: Vec<(VarRef, ResolvedExpr)>,
        body: Vec<ResolvedExpr>,
    },
    NamedLet {
        name: VarRef,
        bindings: Vec<(VarRef, ResolvedExpr)>,
        body: Vec<ResolvedExpr>,
    },
    Do(ResolvedDoLoop),
    Try {
        body: Vec<ResolvedExpr>,
        catch_var: VarRef,
        handler: Vec<ResolvedExpr>,
    },
    Throw(Box<ResolvedExpr>),
    And(Vec<ResolvedExpr>),
    Or(Vec<ResolvedExpr>),
    Quote(Value),
    MakeList(Vec<ResolvedExpr>),
    MakeVector(Vec<ResolvedExpr>),
    MakeMap(Vec<(ResolvedExpr, ResolvedExpr)>),
    Defmacro {
        name: Spur,
        params: Vec<Spur>,
        rest: Option<Spur>,
        body: Vec<ResolvedExpr>,
    },
    DefineRecordType {
        type_name: Spur,
        ctor_name: Spur,
        pred_name: Spur,
        field_names: Vec<Spur>,
        field_specs: Vec<(Spur, Spur)>,
    },
    Module {
        name: Spur,
        exports: Vec<Spur>,
        body: Vec<ResolvedExpr>,
    },
    Import {
        path: Box<ResolvedExpr>,
        selective: Vec<Spur>,
    },
    Load(Box<ResolvedExpr>),
    Eval(Box<ResolvedExpr>),
    Prompt(Vec<ResolvedPromptEntry>),
    Message {
        role: Box<ResolvedExpr>,
        parts: Vec<ResolvedExpr>,
    },
    Deftool {
        name: Spur,
        description: Box<ResolvedExpr>,
        parameters: Box<ResolvedExpr>,
        handler: Box<ResolvedExpr>,
    },
    Defagent {
        name: Spur,
        options: Box<ResolvedExpr>,
    },
    Delay(Box<ResolvedExpr>),
    Force(Box<ResolvedExpr>),
    Macroexpand(Box<ResolvedExpr>),
}

#[derive(Debug, Clone)]
pub enum ResolvedPromptEntry {
    RoleContent {
        role: String,
        parts: Vec<ResolvedExpr>,
    },
    Expr(ResolvedExpr),
}

#[derive(Debug, Clone)]
pub struct ResolvedLambda {
    pub name: Option<Spur>,
    pub params: Vec<Spur>,
    pub rest: Option<Spur>,
    pub body: Vec<ResolvedExpr>,
    pub upvalues: Vec<UpvalueDesc>,
    pub n_locals: u16,
}

// Re-export UpvalueDesc from chunk.rs — used by both ResolvedLambda and Function.
pub use crate::chunk::UpvalueDesc;

#[derive(Debug, Clone)]
pub struct ResolvedDoLoop {
    pub vars: Vec<ResolvedDoVar>,
    pub test: Box<ResolvedExpr>,
    pub result: Vec<ResolvedExpr>,
    pub body: Vec<ResolvedExpr>,
}

#[derive(Debug, Clone)]
pub struct ResolvedDoVar {
    pub name: VarRef,
    pub init: ResolvedExpr,
    pub step: Option<ResolvedExpr>,
}
