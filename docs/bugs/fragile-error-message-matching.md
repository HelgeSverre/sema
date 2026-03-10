# Fragile Error Message String Matching

## Status: MOSTLY RESOLVED

The worst offenders (`.contains("expects")`, `.contains("Type error")`) in `integration_test.rs` have been converted to structured variant matching using `assert_arity_error()` and `assert_type_error()` helpers that match on `SemaError::Arity { .. }` and `SemaError::Type { .. }` via `.inner()`.

## Remaining

A handful of `.contains()` checks remain for `SemaError::Eval(String)` messages where the error variant is generic and the message content is the only distinguishing factor (e.g., `.contains("empty")`, `.contains("pair")`, `.contains("division by zero")`). These are acceptable since `Eval` is a catch-all variant — the alternative would be adding new error variants for each semantic case, which is over-engineering for now.

## What Was Fixed

- All `.contains("expects")` → `assert_arity_error()` (matches `SemaError::Arity { .. }`)
- All `.contains("Type error")` → `assert_type_error()` (matches `SemaError::Type { .. }`)
- IO error checks → `matches!(err.inner(), SemaError::Io(_))`
- Division by zero → `matches!(err.inner(), SemaError::Eval(msg) if msg.contains(...))`
