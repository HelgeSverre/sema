//! Gap-1 regression: the gRPC (tonic) OTLP path builds a real provider on the static
//! tokio runtime, emits spans, and shuts down within the bounded budget even with no
//! collector listening (connect_lazy) — proving the fix for the dropped-runtime bug
//! and the fail-safe "a dead collector never blocks" contract. Own binary (global
//! provider is process-global).

#![cfg(not(target_arch = "wasm32"))]

use std::time::{Duration, Instant};

#[test]
fn grpc_provider_builds_emits_and_shuts_down_bounded() {
    // SAFETY: single-threaded test setup before any otel init.
    unsafe {
        std::env::remove_var("SEMA_OTEL_FILE");
        std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://127.0.0.1:4317");
        std::env::set_var("OTEL_EXPORTER_OTLP_PROTOCOL", "grpc");
        std::env::set_var("OTEL_EXPORTER_OTLP_TIMEOUT", "1000");
    }

    let start = Instant::now();
    let guard = sema_otel::init_from_env();
    assert!(
        guard.is_some(),
        "a gRPC OTLP endpoint must install a provider (no panic, no hang at build)"
    );

    for i in 0..20 {
        let s = sema_otel::llm_span("chat");
        s.set_dispatch("openai", "gpt-x");
        s.set_response(&sema_otel::ResponseFacts {
            input_tokens: i,
            output_tokens: i,
            ..Default::default()
        });
        drop(s);
    }

    // Bounded flush + shutdown watchdog — must return well under the budget even
    // though nothing is listening on 4317.
    drop(guard);

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(12),
        "gRPC path hung for {elapsed:?} — fail-safe / bounded-shutdown violated"
    );
}
