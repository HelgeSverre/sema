use sema_core::Value;
use sema_eval::Interpreter;

/// Evaluate via tree-walker
pub fn eval_tw(input: &str) -> Value {
    let interp = Interpreter::new();
    interp
        .eval_str(input)
        .unwrap_or_else(|e| panic!("tree-walker failed for `{input}`: {e}"))
}

/// Evaluate via bytecode VM
pub fn eval_vm(input: &str) -> Value {
    let interp = Interpreter::new();
    interp
        .eval_str_compiled(input)
        .unwrap_or_else(|e| panic!("VM failed for `{input}`: {e}"))
}

/// Generate tests for both tree-walker and VM backends.
///
/// Usage:
/// ```ignore
/// dual_eval_tests! {
///     test_name: "sema expression" => expected_value,
///     test_name2: "sema expression" => expected_value2,
/// }
/// ```
///
/// This generates `test_name_tw` and `test_name_vm` test functions.
#[macro_export]
macro_rules! dual_eval_tests {
    ($($name:ident : $input:expr => $expected:expr),* $(,)?) => {
        $(
            paste::paste! {
                #[test]
                fn [<$name _tw>]() {
                    let result = common::eval_tw($input);
                    assert_eq!(result, $expected, "tree-walker: {}", $input);
                }

                #[test]
                fn [<$name _vm>]() {
                    let result = common::eval_vm($input);
                    assert_eq!(result, $expected, "VM: {}", $input);
                }
            }
        )*
    };
}

/// Generate error tests for both tree-walker and VM backends.
///
/// Supports two per-entry forms (mix freely within one invocation):
///
/// ```ignore
/// dual_eval_error_tests! {
///     // Strong form: assert the error message contains an expected substring
///     // (matched case-insensitively against the full Display'd error).
///     name1: "(bad-expr)" => "expected substring",
///
///     // Legacy form: only assert that evaluation errors. Prefer the strong
///     // form for new tests; keep this only when no informative substring is
///     // available (mark with a TODO so future error-UX work can revisit).
///     name2: "(other-bad)",
/// }
/// ```
#[macro_export]
macro_rules! dual_eval_error_tests {
    // Entry point: parse a comma-separated list of mixed entries.
    ($($body:tt)*) => {
        $crate::__dual_eval_error_tests_parse!($($body)*);
    };
}

/// Internal: recursive muncher over `name: input` and `name: input => substr` entries.
#[macro_export]
#[doc(hidden)]
macro_rules! __dual_eval_error_tests_parse {
    // Empty
    () => {};

    // Strong form, trailing comma
    ($name:ident : $input:expr => $expected:expr , $($rest:tt)*) => {
        $crate::__dual_eval_error_test_strong!($name, $input, $expected);
        $crate::__dual_eval_error_tests_parse!($($rest)*);
    };
    // Strong form, no trailing comma
    ($name:ident : $input:expr => $expected:expr) => {
        $crate::__dual_eval_error_test_strong!($name, $input, $expected);
    };

    // Legacy form, trailing comma
    ($name:ident : $input:expr , $($rest:tt)*) => {
        $crate::__dual_eval_error_test_legacy!($name, $input);
        $crate::__dual_eval_error_tests_parse!($($rest)*);
    };
    // Legacy form, no trailing comma
    ($name:ident : $input:expr) => {
        $crate::__dual_eval_error_test_legacy!($name, $input);
    };
}

/// Internal: emit a strong (substring-checked) error test pair.
#[macro_export]
#[doc(hidden)]
macro_rules! __dual_eval_error_test_strong {
    ($name:ident, $input:expr, $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<$name _tw>]() {
                let interp = sema_eval::Interpreter::new();
                let result = interp.eval_str($input);
                let err = result.expect_err(concat!(
                    "tree-walker should error for: ", stringify!($name)
                ));
                let msg = err.to_string().to_lowercase();
                let expected = ($expected).to_lowercase();
                assert!(
                    msg.contains(&expected),
                    "tree-walker error for `{}` did not contain `{}`\n  full error: {}",
                    $input, $expected, err
                );
            }

            #[test]
            fn [<$name _vm>]() {
                let interp = sema_eval::Interpreter::new();
                let result = interp.eval_str_compiled($input);
                let err = result.expect_err(concat!(
                    "VM should error for: ", stringify!($name)
                ));
                let msg = err.to_string().to_lowercase();
                let expected = ($expected).to_lowercase();
                assert!(
                    msg.contains(&expected),
                    "VM error for `{}` did not contain `{}`\n  full error: {}",
                    $input, $expected, err
                );
            }
        }
    };
}

/// Internal: emit a legacy (existence-only) error test pair.
#[macro_export]
#[doc(hidden)]
macro_rules! __dual_eval_error_test_legacy {
    ($name:ident, $input:expr) => {
        paste::paste! {
            #[test]
            fn [<$name _tw>]() {
                let interp = sema_eval::Interpreter::new();
                assert!(interp.eval_str($input).is_err(),
                    "tree-walker should error for: {}", $input);
            }

            #[test]
            fn [<$name _vm>]() {
                let interp = sema_eval::Interpreter::new();
                assert!(interp.eval_str_compiled($input).is_err(),
                    "VM should error for: {}", $input);
            }
        }
    };
}
