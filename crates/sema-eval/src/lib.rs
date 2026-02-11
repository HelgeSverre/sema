#![allow(clippy::mutable_key_type)]
mod eval;
mod special_forms;

pub use eval::{
    cache_module, call_stack_depth, capture_stack_trace, clear_module_exports, create_module_env,
    current_file_dir, current_file_name, eval, eval_string, eval_value, get_cached_module,
    merge_span_table, pop_file_path, push_call_frame, push_file_path, set_module_exports,
    take_module_exports, truncate_call_stack, Interpreter,
};
