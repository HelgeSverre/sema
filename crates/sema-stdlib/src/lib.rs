#![allow(clippy::mutable_key_type, clippy::cloned_ref_to_slice_refs)]
mod arithmetic;
mod bitwise;
mod bytevector;
mod comparison;
mod crypto;
mod csv_ops;
mod datetime;
#[cfg(not(target_arch = "wasm32"))]
mod http;
#[cfg(not(target_arch = "wasm32"))]
mod io;
pub(crate) mod json;
mod list;
mod map;
mod math;
mod meta;
mod predicates;
mod regex_ops;
mod string;
#[cfg(not(target_arch = "wasm32"))]
mod system;
#[cfg(not(target_arch = "wasm32"))]
mod terminal;

use sema_core::{Env, Value};
use std::rc::Rc;

pub fn register_stdlib(env: &Env) {
    arithmetic::register(env);
    comparison::register(env);
    list::register(env);
    string::register(env);
    predicates::register(env);
    map::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    io::register(env);
    math::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    system::register(env);
    json::register(env);
    meta::register(env);
    regex_ops::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    http::register(env);
    bitwise::register(env);
    crypto::register(env);
    datetime::register(env);
    csv_ops::register(env);
    bytevector::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    terminal::register(env);
}

fn register_fn(
    env: &Env,
    name: &str,
    f: impl Fn(&[Value]) -> Result<Value, sema_core::SemaError> + 'static,
) {
    env.set(
        sema_core::intern(name),
        Value::NativeFn(Rc::new(sema_core::NativeFn {
            name: name.to_string(),
            func: Box::new(f),
        })),
    );
}
