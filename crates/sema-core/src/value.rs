use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use hashbrown::HashMap as SpurMap;
use lasso::{Rodeo, Spur};

use crate::error::SemaError;
use crate::EvalContext;

thread_local! {
    static INTERNER: RefCell<Rodeo> = RefCell::new(Rodeo::default());
}

/// Intern a string, returning a Spur key.
pub fn intern(s: &str) -> Spur {
    INTERNER.with(|r| r.borrow_mut().get_or_intern(s))
}

/// Resolve a Spur key back to a String.
pub fn resolve(spur: Spur) -> String {
    INTERNER.with(|r| r.borrow().resolve(&spur).to_string())
}

/// Resolve a Spur and call f with the &str, avoiding allocation.
pub fn with_resolved<F, R>(spur: Spur, f: F) -> R
where
    F: FnOnce(&str) -> R,
{
    INTERNER.with(|r| {
        let interner = r.borrow();
        f(interner.resolve(&spur))
    })
}

/// Compare two Spurs by their resolved string content (lexicographic).
pub fn compare_spurs(a: Spur, b: Spur) -> std::cmp::Ordering {
    if a == b {
        return std::cmp::Ordering::Equal;
    }
    INTERNER.with(|r| {
        let interner = r.borrow();
        interner.resolve(&a).cmp(interner.resolve(&b))
    })
}

/// A native function callable from Sema.
pub type NativeFnInner = dyn Fn(&EvalContext, &[Value]) -> Result<Value, SemaError>;

pub struct NativeFn {
    pub name: String,
    pub func: Box<NativeFnInner>,
}

impl NativeFn {
    pub fn simple(
        name: impl Into<String>,
        f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static,
    ) -> Self {
        Self {
            name: name.into(),
            func: Box::new(move |_ctx, args| f(args)),
        }
    }

    pub fn with_ctx(
        name: impl Into<String>,
        f: impl Fn(&EvalContext, &[Value]) -> Result<Value, SemaError> + 'static,
    ) -> Self {
        Self {
            name: name.into(),
            func: Box::new(f),
        }
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native-fn {}>", self.name)
    }
}

/// A user-defined lambda.
#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<Spur>,
    pub rest_param: Option<Spur>,
    pub body: Vec<Value>,
    pub env: Env,
    pub name: Option<Spur>,
}

/// A macro definition.
#[derive(Debug, Clone)]
pub struct Macro {
    pub params: Vec<Spur>,
    pub rest_param: Option<Spur>,
    pub body: Vec<Value>,
    pub name: Spur,
}

/// A lazy promise: delay/force with memoization.
pub struct Thunk {
    pub body: Value,
    pub forced: RefCell<Option<Value>>,
}

impl fmt::Debug for Thunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.forced.borrow().is_some() {
            write!(f, "<promise (forced)>")
        } else {
            write!(f, "<promise>")
        }
    }
}

impl Clone for Thunk {
    fn clone(&self) -> Self {
        Thunk {
            body: self.body.clone(),
            forced: RefCell::new(self.forced.borrow().clone()),
        }
    }
}

/// A record: tagged product type created by define-record-type.
#[derive(Debug, Clone)]
pub struct Record {
    pub type_tag: Spur,
    pub fields: Vec<Value>,
}

/// A message role in a conversation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Role {
    System,
    User,
    Assistant,
    Tool,
}

impl fmt::Display for Role {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Role::System => write!(f, "system"),
            Role::User => write!(f, "user"),
            Role::Assistant => write!(f, "assistant"),
            Role::Tool => write!(f, "tool"),
        }
    }
}

/// A single message in a conversation.
#[derive(Debug, Clone)]
pub struct Message {
    pub role: Role,
    pub content: String,
}

/// A prompt: a structured list of messages.
#[derive(Debug, Clone)]
pub struct Prompt {
    pub messages: Vec<Message>,
}

/// A conversation: immutable history + provider config.
#[derive(Debug, Clone)]
pub struct Conversation {
    pub messages: Vec<Message>,
    pub model: String,
    pub metadata: BTreeMap<String, String>,
}

/// A tool definition for LLM function calling.
#[derive(Debug, Clone)]
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    pub parameters: Value,
    pub handler: Value,
}

/// An agent: system prompt + tools + config for autonomous loops.
#[derive(Debug, Clone)]
pub struct Agent {
    pub name: String,
    pub system: String,
    pub tools: Vec<Value>,
    pub max_turns: usize,
    pub model: String,
}

/// The core Value type for all Sema data.
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Rc<String>),
    Symbol(Spur),
    Keyword(Spur),
    Char(char),
    List(Rc<Vec<Value>>),
    Vector(Rc<Vec<Value>>),
    Map(Rc<BTreeMap<Value, Value>>),
    HashMap(Rc<hashbrown::HashMap<Value, Value>>),
    Lambda(Rc<Lambda>),
    Macro(Rc<Macro>),
    NativeFn(Rc<NativeFn>),
    Prompt(Rc<Prompt>),
    Message(Rc<Message>),
    Conversation(Rc<Conversation>),
    ToolDef(Rc<ToolDefinition>),
    Agent(Rc<Agent>),
    Thunk(Rc<Thunk>),
    Record(Rc<Record>),
    Bytevector(Rc<Vec<u8>>),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Keyword(_) => "keyword",
            Value::Char(_) => "char",
            Value::List(_) => "list",
            Value::Vector(_) => "vector",
            Value::Map(_) => "map",
            Value::HashMap(_) => "hashmap",
            Value::Lambda(_) => "lambda",
            Value::Macro(_) => "macro",
            Value::NativeFn(_) => "native-fn",
            Value::Prompt(_) => "prompt",
            Value::Message(_) => "message",
            Value::Conversation(_) => "conversation",
            Value::ToolDef(_) => "tool",
            Value::Agent(_) => "agent",
            Value::Thunk(_) => "promise",
            Value::Record(_) => "record",
            Value::Bytevector(_) => "bytevector",
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            Value::Int(n) => Some(*n as f64),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<String> {
        match self {
            Value::Symbol(s) => Some(resolve(*s)),
            _ => None,
        }
    }

    pub fn as_keyword(&self) -> Option<String> {
        match self {
            Value::Keyword(s) => Some(resolve(*s)),
            _ => None,
        }
    }

    pub fn as_symbol_spur(&self) -> Option<Spur> {
        match self {
            Value::Symbol(s) => Some(*s),
            _ => None,
        }
    }

    pub fn as_keyword_spur(&self) -> Option<Spur> {
        match self {
            Value::Keyword(s) => Some(*s),
            _ => None,
        }
    }

    pub fn as_char(&self) -> Option<char> {
        match self {
            Value::Char(c) => Some(*c),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<&[Value]> {
        match self {
            Value::List(v) => Some(v),
            _ => None,
        }
    }

    pub fn symbol(s: &str) -> Value {
        Value::Symbol(intern(s))
    }

    pub fn keyword(s: &str) -> Value {
        Value::Keyword(intern(s))
    }

    pub fn char(c: char) -> Value {
        Value::Char(c)
    }

    pub fn string(s: &str) -> Value {
        Value::String(Rc::new(s.to_string()))
    }

    pub fn list(v: Vec<Value>) -> Value {
        Value::List(Rc::new(v))
    }

    pub fn vector(v: Vec<Value>) -> Value {
        Value::Vector(Rc::new(v))
    }

    pub fn hashmap(entries: Vec<(Value, Value)>) -> Value {
        let map: hashbrown::HashMap<Value, Value> = entries.into_iter().collect();
        Value::HashMap(Rc::new(map))
    }

    pub fn as_record(&self) -> Option<&Rc<Record>> {
        match self {
            Value::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn as_bytevector(&self) -> Option<&Rc<Vec<u8>>> {
        match self {
            Value::Bytevector(bv) => Some(bv),
            _ => None,
        }
    }

    pub fn bytevector(bytes: Vec<u8>) -> Value {
        Value::Bytevector(Rc::new(bytes))
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Nil => {}
            Value::Bool(b) => b.hash(state),
            Value::Int(n) => n.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::Symbol(s) => s.hash(state),
            Value::Keyword(s) => s.hash(state),
            Value::Char(c) => c.hash(state),
            Value::List(l) => l.hash(state),
            Value::Vector(v) => v.hash(state),
            Value::Record(r) => {
                r.type_tag.hash(state);
                r.fields.hash(state);
            }
            Value::Bytevector(bv) => bv.hash(state),
            _ => {}
        }
    }
}

// Implement Eq/Ord for Value so it can be used as BTreeMap key.
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Keyword(a), Value::Keyword(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::HashMap(a), Value::HashMap(b)) => a == b,
            (Value::Record(a), Value::Record(b)) => {
                a.type_tag == b.type_tag && a.fields == b.fields
            }
            (Value::Bytevector(a), Value::Bytevector(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        fn type_order(v: &Value) -> u8 {
            match v {
                Value::Nil => 0,
                Value::Bool(_) => 1,
                Value::Int(_) => 2,
                Value::Float(_) => 3,
                Value::Char(_) => 4,
                Value::String(_) => 5,
                Value::Symbol(_) => 6,
                Value::Keyword(_) => 7,
                Value::List(_) => 8,
                Value::Vector(_) => 9,
                Value::Map(_) => 10,
                Value::HashMap(_) => 11,
                Value::Record(_) => 12,
                Value::Bytevector(_) => 13,
                _ => 14,
            }
        }
        match (self, other) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => a.to_bits().cmp(&b.to_bits()),
            (Value::String(a), Value::String(b)) => a.cmp(b),
            (Value::Symbol(a), Value::Symbol(b)) => compare_spurs(*a, *b),
            (Value::Keyword(a), Value::Keyword(b)) => compare_spurs(*a, *b),
            (Value::Char(a), Value::Char(b)) => a.cmp(b),
            (Value::List(a), Value::List(b)) => a.cmp(b),
            (Value::Vector(a), Value::Vector(b)) => a.cmp(b),
            (Value::Record(a), Value::Record(b)) => {
                compare_spurs(a.type_tag, b.type_tag).then_with(|| a.fields.cmp(&b.fields))
            }
            (Value::Bytevector(a), Value::Bytevector(b)) => a.cmp(b),
            _ => type_order(self).cmp(&type_order(other)),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{n:.1}")
                } else {
                    write!(f, "{n}")
                }
            }
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => with_resolved(*s, |name| write!(f, "{name}")),
            Value::Keyword(s) => with_resolved(*s, |name| write!(f, ":{name}")),
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                '\r' => write!(f, "#\\return"),
                '\0' => write!(f, "#\\nul"),
                _ => write!(f, "#\\{c}"),
            },
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Vector(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                for (i, (k, v)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{k} {v}")?;
                }
                write!(f, "}}")
            }
            Value::HashMap(map) => {
                let mut entries: Vec<_> = map.iter().collect();
                entries.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
                write!(f, "{{")?;
                for (i, (k, v)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{k} {v}")?;
                }
                write!(f, "}}")
            }
            Value::Lambda(l) => {
                if let Some(name) = &l.name {
                    with_resolved(*name, |n| write!(f, "<lambda {n}>"))
                } else {
                    write!(f, "<lambda>")
                }
            }
            Value::Macro(m) => with_resolved(m.name, |n| write!(f, "<macro {n}>")),
            Value::NativeFn(n) => write!(f, "<native-fn {}>", n.name),
            Value::Prompt(p) => write!(f, "<prompt {} messages>", p.messages.len()),
            Value::Message(m) => write!(f, "<message {} \"{}\">", m.role, truncate(&m.content, 40)),
            Value::Conversation(c) => {
                write!(f, "<conversation {} messages>", c.messages.len())
            }
            Value::ToolDef(t) => write!(f, "<tool {}>", t.name),
            Value::Agent(a) => write!(f, "<agent {}>", a.name),
            Value::Thunk(t) => {
                if t.forced.borrow().is_some() {
                    write!(f, "<promise (forced)>")
                } else {
                    write!(f, "<promise>")
                }
            }
            Value::Record(r) => {
                with_resolved(r.type_tag, |tag| write!(f, "#<record {tag}"))?;
                for field in &r.fields {
                    write!(f, " {field}")?;
                }
                write!(f, ">")
            }
            Value::Bytevector(bv) => {
                write!(f, "#u8(")?;
                for (i, byte) in bv.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{byte}")?;
                }
                write!(f, ")")
            }
        }
    }
}

fn truncate(s: &str, max: usize) -> String {
    let mut iter = s.chars();
    let prefix: String = iter.by_ref().take(max).collect();
    if iter.next().is_none() {
        prefix
    } else {
        format!("{prefix}...")
    }
}

/// A Sema environment: a chain of scopes with bindings.
#[derive(Debug, Clone)]
pub struct Env {
    pub bindings: Rc<RefCell<SpurMap<Spur, Value>>>,
    pub parent: Option<Rc<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            bindings: Rc::new(RefCell::new(SpurMap::new())),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Env>) -> Self {
        Env {
            bindings: Rc::new(RefCell::new(SpurMap::new())),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: Spur) -> Option<Value> {
        if let Some(val) = self.bindings.borrow().get(&name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn get_str(&self, name: &str) -> Option<Value> {
        self.get(intern(name))
    }

    pub fn set(&self, name: Spur, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    pub fn set_str(&self, name: &str, val: Value) {
        self.set(intern(name), val);
    }

    /// Update a binding that already exists in the current scope, avoiding key allocation.
    pub fn update(&self, name: Spur, val: Value) {
        let mut bindings = self.bindings.borrow_mut();
        if let Some(entry) = bindings.get_mut(&name) {
            *entry = val;
        } else {
            bindings.insert(name, val);
        }
    }

    /// Remove and return a binding from the current scope only (not parents).
    pub fn take(&self, name: Spur) -> Option<Value> {
        self.bindings.borrow_mut().remove(&name)
    }

    /// Remove and return a binding from any scope in the parent chain.
    pub fn take_anywhere(&self, name: Spur) -> Option<Value> {
        if let Some(val) = self.bindings.borrow_mut().remove(&name) {
            Some(val)
        } else if let Some(parent) = &self.parent {
            parent.take_anywhere(name)
        } else {
            None
        }
    }

    /// Set a variable in the scope where it's defined (for set!).
    pub fn set_existing(&self, name: Spur, val: Value) -> bool {
        let mut bindings = self.bindings.borrow_mut();
        if let Some(entry) = bindings.get_mut(&name) {
            *entry = val;
            true
        } else {
            drop(bindings);
            if let Some(parent) = &self.parent {
                parent.set_existing(name, val)
            } else {
                false
            }
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::EvalContext;

    #[test]
    fn test_native_fn_simple() {
        let f = NativeFn::simple("add1", |args| Ok(args[0].clone()));
        let ctx = EvalContext::new();
        assert!((f.func)(&ctx, &[Value::Int(42)]).is_ok());
    }

    #[test]
    fn test_native_fn_with_ctx() {
        let f = NativeFn::with_ctx("get-depth", |ctx, _args| {
            Ok(Value::Int(ctx.eval_depth.get() as i64))
        });
        let ctx = EvalContext::new();
        assert_eq!((f.func)(&ctx, &[]).unwrap(), Value::Int(0));
    }
}
