#![allow(clippy::mutable_key_type, clippy::cloned_ref_to_slice_refs)]
mod arithmetic;
mod bitwise;
mod bytevector;
mod comparison;
mod context;
mod crypto;
mod csv_ops;
mod datetime;
#[cfg(not(target_arch = "wasm32"))]
mod http;
#[cfg(not(target_arch = "wasm32"))]
mod io;
pub(crate) mod json;
#[cfg(not(target_arch = "wasm32"))]
mod kv;
mod list;
mod map;
mod math;
mod meta;
#[cfg(not(target_arch = "wasm32"))]
mod pdf;
mod predicates;
mod regex_ops;
#[cfg(not(target_arch = "wasm32"))]
mod server;
mod string;
#[cfg(not(target_arch = "wasm32"))]
mod system;
#[cfg(not(target_arch = "wasm32"))]
mod terminal;
mod text;

use sema_core::{Caps, Env, Sandbox, Value};

pub fn register_stdlib(env: &Env, sandbox: &Sandbox) {
    arithmetic::register(env);
    comparison::register(env);
    context::register(env);
    list::register(env);
    string::register(env);
    predicates::register(env);
    map::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    io::register(env, sandbox);
    math::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    system::register(env, sandbox);
    json::register(env);
    meta::register(env);
    regex_ops::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    http::register(env, sandbox);
    #[cfg(not(target_arch = "wasm32"))]
    server::register(env, sandbox);
    bitwise::register(env);
    crypto::register(env);
    datetime::register(env);
    csv_ops::register(env);
    bytevector::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    terminal::register(env);
    text::register(env);
    #[cfg(not(target_arch = "wasm32"))]
    kv::register(env, sandbox);
    #[cfg(not(target_arch = "wasm32"))]
    pdf::register(env, sandbox);
}

fn register_fn_gated(
    env: &Env,
    sandbox: &Sandbox,
    cap: Caps,
    name: &str,
    f: impl Fn(&[Value]) -> Result<Value, sema_core::SemaError> + 'static,
) {
    if sandbox.is_unrestricted() {
        register_fn(env, name, f);
    } else {
        let sandbox = sandbox.clone();
        let fn_name = name.to_string();
        register_fn(env, name, move |args| {
            sandbox.check(cap, &fn_name)?;
            f(args)
        });
    }
}

fn register_fn_path_gated(
    env: &Env,
    sandbox: &Sandbox,
    cap: Caps,
    name: &str,
    path_args: &[usize],
    f: impl Fn(&[Value]) -> Result<Value, sema_core::SemaError> + 'static,
) {
    if sandbox.is_unrestricted() {
        register_fn(env, name, f);
    } else {
        let sandbox = sandbox.clone();
        let fn_name = name.to_string();
        let path_indices: Vec<usize> = path_args.to_vec();
        register_fn(env, name, move |args| {
            sandbox.check(cap, &fn_name)?;
            for &idx in &path_indices {
                if let Some(val) = args.get(idx) {
                    if let Some(p) = val.as_str() {
                        sandbox.check_path(p, &fn_name)?;
                    }
                }
            }
            f(args)
        });
    }
}

fn register_fn(
    env: &Env,
    name: &str,
    f: impl Fn(&[Value]) -> Result<Value, sema_core::SemaError> + 'static,
) {
    env.set(
        sema_core::intern(name),
        Value::native_fn(sema_core::NativeFn::simple(name, f)),
    );
}
