# Tree-Walker Oracle Circularity in Dual-Eval Tests

## Problem

Many dual-eval tests use `common::eval_tw(...)` to produce the expected value, then compare the VM result against it. This means the tree-walker acts as the oracle for correctness. If the tree-walker has a bug in a fundamental operation (e.g., quote evaluation, list construction, numeric parsing), both the expected value and the tree-walker test result will be identically wrong, and the test will pass.

The VM variant partially mitigates this by providing an independent second implementation, but if both evaluators share the same reader/parser bug, or if the expected value is constructed through the tree-walker, the bug is invisible to the test suite.

Example: `eval_tw("'(2 3)")` constructs the expected list via the tree-walker's quote handling. If quote evaluation had a bug that, say, reversed list elements, the expected value would also be reversed, and the comparison would pass.

## Affected Tests

- `crates/sema/tests/dual_eval_test.rs` -- numerous tests using `common::eval_tw(...)` as expected value
- `crates/sema/tests/dual_eval_collections_test.rs` -- collection tests using eval_tw for expected list/map values
- `crates/sema/tests/dual_eval_types_test.rs` -- type tests using eval_tw for expected values

## Suggested Fix

1. For critical tests (basic data types, quote, list construction, arithmetic), construct expected `Value` objects directly in Rust: `Value::list(vec![Value::int(2), Value::int(3)])` instead of `eval_tw("'(2 3)")`.
2. Keep `eval_tw` as oracle for higher-level feature tests where constructing the expected Value by hand would be impractical.
3. Add a small set of "ground truth" tests that compare eval output against hand-constructed Values for fundamental operations (integers, floats, strings, lists, maps, quote, quasiquote).
4. This does not need to be applied everywhere -- just to the foundational tests that other tests implicitly depend on being correct.
