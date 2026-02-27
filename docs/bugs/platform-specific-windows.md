# Platform-Specific Tests Assume Unix

## Problem

Multiple tests assume a Unix-like environment (macOS/Linux) and will fail on Windows. This is not an immediate concern since development and CI currently target Darwin/Linux, but it prevents Windows portability and would cause failures if Windows CI were added.

The assumptions span several categories: shell commands, path separators, binary naming, and cleanup commands.

## Affected Tests

- `crates/sema/tests/integration_test.rs` -- `test_sys_which` -- looks up `"sh"` which does not exist on Windows (would need `"cmd.exe"` or similar)
- `crates/sema/tests/integration_test.rs` -- `path_join` tests -- hardcode `/` as path separator; Windows uses `\`
- `crates/sema/tests/integration_test.rs` -- `test_shell_command` -- invokes `"echo"` as a shell command, which behaves differently on Windows cmd vs Unix sh
- `crates/sema-dap/src/` -- `sema_binary()` -- constructs the path to the sema binary without `.exe` extension on Windows
- `crates/sema/tests/integration_test.rs` -- `file_mkdir` cleanup -- uses `rm -rf` which is a Unix command (Windows equivalent is `rmdir /s /q`)

## Suggested Fix

1. Use `#[cfg(not(target_os = "windows"))]` to skip Unix-specific tests on Windows, or provide platform-specific variants.
2. For path tests, use `std::path::MAIN_SEPARATOR` or `Path::join` instead of hardcoded `/` in expected output.
3. For `sema_binary()`, append `.exe` when `cfg!(target_os = "windows")` is true.
4. Replace direct `rm -rf` calls with `std::fs::remove_dir_all` in Rust test cleanup code.
5. For shell command tests, use platform-agnostic commands or `cfg`-gate the command strings.
