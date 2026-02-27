# Process-Global Environment Variable Mutation

## Problem

`test_sys_set_env` calls `std::env::set_var` via `(sys/set-env! ...)`, which mutates the process-global environment. The env var `SEMA_TEST_VAR_12345` persists for the entire lifetime of the test process. Since Rust's `cargo test` runs tests in parallel within the same process, any other test that inspects environment variables could observe this leftover state, creating a cross-test contamination vector.

The test currently passes in isolation, but its correctness depends on no other test using the same env var name, and it leaves permanent side effects in the process.

## Affected Tests

- `crates/sema/tests/integration_test.rs:3514` -- `test_sys_set_env` -- sets `SEMA_TEST_VAR_12345` process-wide, never cleans it up

## Suggested Fix

Either:
1. Generate a unique env var name per test invocation (e.g., include a UUID or timestamp suffix) and clean it up with `std::env::remove_var` at the end.
2. Use the `serial_test` crate and annotate this test with `#[serial]` to prevent parallel execution with other env-sensitive tests.
3. At minimum, restore the previous value (or remove the var) in a cleanup step after the assertion.
