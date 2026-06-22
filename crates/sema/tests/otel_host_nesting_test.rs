//! Gap-3: embedded host nesting. With `TelemetryMode::UseHostGlobal`, Sema emits
//! against the host's global provider and its root span nests under the host's active
//! span (Decision #13). Own binary (global provider is process-global).

#![cfg(not(target_arch = "wasm32"))]

use opentelemetry::global;
use opentelemetry::trace::{Span, TraceContextExt, Tracer};
use opentelemetry::Context;
use sema::InterpreterBuilder;
use sema_otel::TelemetryMode;

#[test]
fn sema_span_nests_under_host_span() {
    // The host installs its own global provider with an in-memory exporter.
    let cap = sema_otel::testing::install();

    // Build an embedded interpreter that uses the host's global provider (installs none).
    let interp = InterpreterBuilder::new()
        .with_telemetry(TelemetryMode::UseHostGlobal)
        .build();

    // The host opens a request span and makes it current...
    let host_tracer = global::tracer("host");
    let host_span = host_tracer.start("host.request");
    let host_span_id = format!("{:016x}", host_span.span_context().span_id());
    let host_trace_id = format!("{:032x}", host_span.span_context().trace_id());
    let guard = Context::current_with_span(host_span).attach();

    // ...then runs Sema code that emits a span.
    interp
        .eval_str(r#"(otel/span "sema-work" (fn () (+ 1 2)))"#)
        .expect("otel/span should run");

    drop(guard); // ends the host span

    let spans = cap.spans_json();
    let sema_span = spans
        .iter()
        .find(|s| s["name"] == "sema-work")
        .expect("the Sema span should be captured");

    // The Sema span parents under the host span and shares its trace.
    assert_eq!(
        sema_span["parent_span_id"], host_span_id,
        "Sema span must nest under the host's current span"
    );
    assert_eq!(sema_span["trace_id"], host_trace_id);

    // No-leak invariant: the embedded/telemetry-mode machinery must never emit a stray
    // diagnostic probe span into the host's pipeline.
    assert!(
        !spans.iter().any(|s| s["name"] == "sema.otel.probe"),
        "no probe span may leak into the host pipeline"
    );
}
