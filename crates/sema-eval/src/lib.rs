#![allow(clippy::mutable_key_type)]
mod eval;
mod special_forms;

pub use eval::{
    call_value, create_module_env, eval, eval_string, eval_value, EvalResult, Interpreter,
    Trampoline,
};
pub use sema_core::EvalContext;
