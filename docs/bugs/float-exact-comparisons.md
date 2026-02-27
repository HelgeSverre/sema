# Fragile Exact Float Comparisons

## Problem

Several tests compare computed floating-point results using exact equality (`==`). While many of these are safe because both sides go through the same Rust `f64` parse path (e.g., `(+ 1.5 2.5)` compared to `"4.0"`), some involve genuine computation where IEEE 754 does not guarantee bit-exact results across all platforms and library implementations.

Most of these tests pass on current hardware (x86-64, aarch64) with the current Rust version, but they represent a latent fragility that could surface on different platforms, with different compiler optimizations, or after dependency updates.

## Affected Tests

- `crates/sema/tests/dual_eval_test.rs:398` -- `toml_decode_float` -- TOML parser may use a different float parsing path than Sema's reader, so the roundtripped float may not be bit-identical
- `crates/sema/tests/dual_eval_types_test.rs:97` -- `emb_similarity` -- cosine similarity of a vector with itself should theoretically be 1.0, but accumulated floating-point error in dot product and magnitude calculations may produce a value slightly off from 1.0
- `crates/sema/tests/integration_test.rs:2145` -- `math/log10 100 == 2.0` -- while most implementations return exactly 2.0 for log10(100), IEEE 754 does not mandate this; some platforms may return 1.9999999999999998 or similar

## Suggested Fix

1. For computed results (not just parsed literals), use epsilon-based comparison: `(result - expected).abs() < 1e-10`.
2. Add a Sema-level `approx=` or `float-close?` test helper that wraps epsilon comparison for use in test assertions.
3. Keep exact comparisons only for tests that specifically verify the parse-display roundtrip of float literals (where both sides use the same code path).
4. For the cosine similarity test, assert `result > 0.9999` rather than `result == 1.0`.
