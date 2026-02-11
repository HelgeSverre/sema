mod arithmetic;
mod comparison;
mod io;
mod json;
mod list;
mod map;
mod math;
mod predicates;
mod string;
mod system;

use sema_core::{Env, Value};
use std::rc::Rc;

pub fn register_stdlib(env: &Env) {
    arithmetic::register(env);
    comparison::register(env);
    list::register(env);
    string::register(env);
    predicates::register(env);
    map::register(env);
    io::register(env);
    math::register(env);
    system::register(env);
    json::register(env);
}

fn register_fn(env: &Env, name: &str, f: impl Fn(&[Value]) -> Result<Value, sema_core::SemaError> + 'static) {
    env.set(
        name.to_string(),
        Value::NativeFn(Rc::new(sema_core::NativeFn {
            name: name.to_string(),
            func: Box::new(f),
        })),
    );
}
