#![allow(clippy::mutable_key_type)]
mod destructure;
mod eval;
mod prelude;
mod special_forms;

pub use eval::{
    call_value, create_module_env, eval, eval_string, eval_value, EvalResult, Interpreter,
    Trampoline,
};
pub use sema_core::EvalContext;
pub use special_forms::SPECIAL_FORM_NAMES;
