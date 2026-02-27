# Fragile Error Message String Matching

## Problem

Dozens of tests assert error conditions by checking whether the error's display string contains a specific substring (e.g., `.contains("expects")`, `.contains("division by zero")`, `.contains("cannot parse")`). This couples tests to the exact wording of error messages, which are user-facing text that should be freely improvable without breaking the test suite.

Any rewording, typo fix, or internationalization effort will cause cascading test failures across multiple test files, even though the underlying error semantics are unchanged.

Some tests go further and use `assert_eq!` on the full error display string, making them even more brittle.

## Affected Tests

- `crates/sema/tests/integration_test.rs` -- widespread use of `.contains(...)` on error messages throughout the file
- `crates/sema/tests/dual_eval_test.rs` -- `dual_eval_error_tests!` macro entries that match on error substrings
- `crates/sema/tests/dual_eval_types_test.rs` -- type error tests matching on display strings
- `crates/sema-core/src/error.rs:461-475` -- tests using `assert_eq!` on full `Display` output of `SemaError` variants
- `crates/sema/tests/dual_eval_collections_test.rs` -- collection error tests matching on message text

## Suggested Fix

1. Prefer matching on structured error variants where possible: match on `SemaError::TypeError { .. }`, `SemaError::Arity { .. }`, etc., rather than on the display string.
2. Add helper methods or test utilities for asserting error kinds: `assert_error_kind!(result, SemaError::TypeError)`.
3. Where the error variant does not carry enough information to distinguish cases, add structured fields (e.g., an error code or sub-kind enum) rather than relying on message text.
4. Keep display-string assertions only in dedicated formatting tests in `error.rs` that explicitly test the `Display` impl. Feature tests should not depend on message wording.
5. For the `dual_eval_error_tests!` macro, consider accepting either a substring match or an error variant pattern, defaulting to variant matching.
