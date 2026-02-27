# VFS OnceLock Race Condition Between Tests

## Problem

The virtual filesystem (VFS) initialization uses `OnceLock`, which is write-once per process. Two unit tests in the same module test different lifecycle stages of the VFS, but since `OnceLock` can only be initialized once, the tests are order-dependent. If `test_vfs_lifecycle` runs first and initializes the VFS, then `test_vfs_inactive_before_init` will observe an already-initialized VFS and fail its assertion that the VFS should be inactive.

Since Rust runs tests within a module in parallel by default, this is a race condition: the outcome depends on which test thread reaches `init_vfs` first.

## Affected Tests

- `crates/sema-core/src/vfs.rs:225` -- `test_vfs_inactive_before_init` -- asserts VFS is inactive, but may find it already initialized
- `crates/sema-core/src/vfs.rs:225` -- `test_vfs_lifecycle` -- initializes the VFS via `OnceLock`, permanently changing process state

## Suggested Fix

Either:
1. Combine both tests into a single test that first asserts inactivity, then initializes, then asserts the full lifecycle. This guarantees ordering within a single test function.
2. Use `#[serial]` from the `serial_test` crate and ensure `test_vfs_inactive_before_init` runs before `test_vfs_lifecycle`.
3. Refactor VFS initialization to use a resettable mechanism in test builds (e.g., a `#[cfg(test)]` reset function that clears the `OnceLock` state).
