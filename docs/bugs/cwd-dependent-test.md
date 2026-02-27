# Working-Directory-Dependent Glob Test

## Problem

`test_file_glob_returns_list_of_strings` uses the relative glob pattern `"Cargo.*"` to test the `file/glob` function. This pattern matches files relative to the current working directory at the time the test runs. While `cargo test` typically sets the working directory to the workspace root (where `Cargo.toml` lives), this is not guaranteed by all test runners, IDE integrations, or CI configurations.

If the test is run from a different directory (e.g., from within a crate subdirectory, or via a custom test harness), the glob will match different files or no files at all, causing a spurious failure.

## Affected Tests

- `crates/sema/tests/integration_test.rs:6961` -- `test_file_glob_returns_list_of_strings` -- uses relative glob `"Cargo.*"` that depends on CWD

## Suggested Fix

1. Use `env!("CARGO_MANIFEST_DIR")` to construct an absolute path to a known file pattern, e.g., `format!("{}/../Cargo.*", env!("CARGO_MANIFEST_DIR"))` to reach the workspace root reliably.
2. Alternatively, create a temporary directory with known files in a test fixture and glob against that, removing any dependency on CWD.
3. At minimum, document the CWD assumption with a comment explaining why the test expects to run from the workspace root.
