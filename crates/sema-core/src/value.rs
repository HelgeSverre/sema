use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use crate::error::SemaError;

/// A native function callable from Sema.
pub type NativeFnInner = dyn Fn(&[Value]) -> Result<Value, SemaError>;

pub struct NativeFn {
    pub name: String,
    pub func: Box<NativeFnInner>,
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native-fn {}>", self.name)
    }
}

/// A user-defined lambda.
#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub rest_param: Option<String>,
    pub body: Vec<Value>,
    pub env: Env,
    pub name: Option<String>,
}

/// A macro definition.
#[derive(Debug, Clone)]
pub struct Macro {
    pub params: Vec<String>,
    pub rest_param: Option<String>,
    pub body: Vec<Value>,
    pub name: String,
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
    Symbol(Rc<String>),
    Keyword(Rc<String>),
    Char(char),
    List(Rc<Vec<Value>>),
    Vector(Rc<Vec<Value>>),
    Map(Rc<BTreeMap<Value, Value>>),
    Lambda(Rc<Lambda>),
    Macro(Rc<Macro>),
    NativeFn(Rc<NativeFn>),
    Prompt(Rc<Prompt>),
    Message(Rc<Message>),
    Conversation(Rc<Conversation>),
    ToolDef(Rc<ToolDefinition>),
    Agent(Rc<Agent>),
    Thunk(Rc<Thunk>),
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
            Value::Lambda(_) => "lambda",
            Value::Macro(_) => "macro",
            Value::NativeFn(_) => "native-fn",
            Value::Prompt(_) => "prompt",
            Value::Message(_) => "message",
            Value::Conversation(_) => "conversation",
            Value::ToolDef(_) => "tool",
            Value::Agent(_) => "agent",
            Value::Thunk(_) => "promise",
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

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Value::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_keyword(&self) -> Option<&str> {
        match self {
            Value::Keyword(s) => Some(s),
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
        Value::Symbol(Rc::new(s.to_string()))
    }

    pub fn keyword(s: &str) -> Value {
        Value::Keyword(Rc::new(s.to_string()))
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
                _ => 11,
            }
        }
        match (self, other) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => a.to_bits().cmp(&b.to_bits()),
            (Value::String(a), Value::String(b)) => a.cmp(b),
            (Value::Symbol(a), Value::Symbol(b)) => a.cmp(b),
            (Value::Keyword(a), Value::Keyword(b)) => a.cmp(b),
            (Value::Char(a), Value::Char(b)) => a.cmp(b),
            (Value::List(a), Value::List(b)) => a.cmp(b),
            (Value::Vector(a), Value::Vector(b)) => a.cmp(b),
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
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Keyword(s) => write!(f, ":{s}"),
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
            Value::Lambda(l) => {
                if let Some(name) = &l.name {
                    write!(f, "<lambda {name}>")
                } else {
                    write!(f, "<lambda>")
                }
            }
            Value::Macro(m) => write!(f, "<macro {}>", m.name),
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
        }
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max])
    }
}

/// A Sema environment: a chain of scopes with bindings.
#[derive(Debug, Clone)]
pub struct Env {
    pub bindings: Rc<RefCell<BTreeMap<String, Value>>>,
    pub parent: Option<Rc<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            bindings: Rc::new(RefCell::new(BTreeMap::new())),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Env>) -> Self {
        Env {
            bindings: Rc::new(RefCell::new(BTreeMap::new())),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: String, val: Value) {
        self.bindings.borrow_mut().insert(name, val);
    }

    /// Set a variable in the scope where it's defined (for set!).
    pub fn set_existing(&self, name: &str, val: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), val);
            true
        } else if let Some(parent) = &self.parent {
            parent.set_existing(name, val)
        } else {
            false
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
