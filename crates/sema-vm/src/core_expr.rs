use sema_core::{Spur, Value};

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
    /// With-budget scope
    WithBudget {
        options: Box<CoreExpr>,
        body: Vec<CoreExpr>,
    },
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
