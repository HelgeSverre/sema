#![no_main]
use libfuzzer_sys::fuzz_target;

/// Max eval steps per fuzz input — prevents infinite loops from timing out the fuzzer.
const FUZZ_STEP_LIMIT: usize = 100_000;

thread_local! {
    /// Reuse a single interpreter across fuzz iterations. The VM is the sole
    /// evaluator, so we fuzz the real compile + execute pipeline via the public
    /// entry point. Defines persist across iterations (acceptable — we only
    /// assert "must not panic").
    static INTERP: sema_eval::Interpreter = {
        let interp = sema_eval::Interpreter::new();
        interp.ctx.set_eval_step_limit(FUZZ_STEP_LIMIT);
        interp
    };
}

fuzz_target!(|data: &[u8]| {
    let input = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    INTERP.with(|interp| {
        // Must not panic — Ok or Err are both fine.
        let _ = interp.eval_str_compiled(input);
    });
});
