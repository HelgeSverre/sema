//! Gap-3: `TelemetryMode::OwnProvider` emits against a host-supplied provider WITHOUT
//! installing a global one. Verified by routing: spans land in the OWNED exporter and
//! NOT in a separately-installed global exporter. Own binary (process-global state).

#![cfg(not(target_arch = "wasm32"))]

use opentelemetry_sdk::trace::{InMemorySpanExporter, SdkTracerProvider};
use sema::InterpreterBuilder;
use sema_otel::TelemetryMode;

#[test]
fn own_provider_routes_without_touching_global() {
    // A global in-memory provider is present (it must receive NOTHING from OwnProvider).
    let global_cap = sema_otel::testing::install();

    // The host hands Sema its own separate provider/exporter.
    let owned_exporter = InMemorySpanExporter::default();
    let owned_provider = SdkTracerProvider::builder()
        .with_simple_exporter(owned_exporter.clone())
        .build();

    let interp = InterpreterBuilder::new()
        .with_telemetry(TelemetryMode::OwnProvider(owned_provider))
        .build();

    interp
        .eval_str(r#"(otel/span "owned-work" (fn () (+ 1 2)))"#)
        .expect("otel/span should run");

    // The span landed in the OWNED exporter...
    let owned = owned_exporter
        .get_finished_spans()
        .expect("owned spans readable");
    assert!(
        owned.iter().any(|s| s.name == "owned-work"),
        "the Sema span should land in the host-supplied provider"
    );
    // ...and NOT in the global one (OwnProvider installs no global / routes around it).
    let global_spans = global_cap.spans_json();
    assert!(
        !global_spans.iter().any(|s| s["name"] == "owned-work"),
        "OwnProvider must not emit into the global provider"
    );
}
