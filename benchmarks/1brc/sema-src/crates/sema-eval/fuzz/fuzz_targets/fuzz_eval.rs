#![no_main]
use libfuzzer_sys::fuzz_target;
use std::rc::Rc;

/// Max eval steps per fuzz input — prevents infinite loops from timing out the fuzzer.
const FUZZ_STEP_LIMIT: usize = 100_000;

thread_local! {
    /// Reuse a single stdlib environment across fuzz iterations.
    /// LLM builtins are intentionally excluded (no network in fuzzer).
    static ENV: Rc<sema_core::Env> = {
        let env = sema_core::Env::new();
        sema_stdlib::register_stdlib(&env);
        sema_eval::set_eval_step_limit(FUZZ_STEP_LIMIT);
        Rc::new(env)
    };
}

fuzz_target!(|data: &[u8]| {
    let input = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    ENV.with(|global_env| {
        // Eval in a child env so fuzz iterations don't leak defines into each other
        let env = sema_core::Env::with_parent(global_env.clone());
        // Must not panic — Ok or Err are both fine
        let _ = sema_eval::eval_string(input, &env);
    });
});
