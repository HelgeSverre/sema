# Order-Dependent Map Output Assertions

## Problem

Many tests assert the exact string representation of maps (BTreeMap-backed). While BTreeMap provides deterministic sorted-key ordering today, these tests are tightly coupled to both the iteration order and the display format of the map implementation. Any change to the map backing structure (e.g., switching to a different ordered map), changes to the `Display` impl for maps, or changes to how keys sort would cause widespread test breakage even if the semantics are correct.

This is a design fragility rather than a current bug -- the tests pass today but are brittle against future refactoring.

## Affected Tests

- `crates/sema/tests/integration_test.rs:155` -- `test_json` -- asserts exact JSON-round-tripped map string
- `crates/sema/tests/integration_test.rs:3388` -- `test_frequencies` -- asserts exact frequency map output
- `crates/sema/tests/integration_test.rs:3213` -- `test_map_map_keys` -- asserts exact map-keys output
- `crates/sema/tests/integration_test.rs:3222` -- `test_map_from_entries` -- asserts exact from-entries output
- `crates/sema/tests/integration_test.rs:7417` -- `test_map_sort_keys` -- asserts exact sorted-keys output
- `crates/sema/tests/integration_test.rs:7433` -- `test_map_except` -- asserts exact map-except output
- `crates/sema/tests/dual_eval_collections_test.rs` -- `mode_tie_sorted`, `duplicates_basic` -- assert exact collection output strings
- `crates/sema/tests/dual_eval_stdlib_test.rs:32-43` -- ANSI terminal tests -- assert exact map display in styled output

## Suggested Fix

1. For tests that care about map contents, parse the output back into a data structure and compare structurally rather than via string equality.
2. Alternatively, sort both the expected and actual output lines before comparing.
3. For tests that specifically test display formatting, keep them as-is but isolate them into dedicated display/format tests so they are clearly expected to break on format changes.
4. Consider a test helper like `assert_map_contains(result, &[("key1", "val1"), ("key2", "val2")])` that checks entries without relying on display order.
