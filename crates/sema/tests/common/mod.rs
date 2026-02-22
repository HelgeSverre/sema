use sema_core::{SemaError, Value};
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

/// Assert both evaluators produce the same result and return it
pub fn eval_both(input: &str) -> Value {
    let tw = eval_tw(input);
    let vm = eval_vm(input);
    assert_eq!(tw, vm, "tree-walker vs VM mismatch for: {input}");
    tw
}

/// Assert both evaluators produce an error
pub fn eval_both_err(input: &str) {
    let interp_tw = Interpreter::new();
    let interp_vm = Interpreter::new();
    assert!(
        interp_tw.eval_str(input).is_err(),
        "tree-walker should error for: {input}"
    );
    assert!(
        interp_vm.eval_str_compiled(input).is_err(),
        "VM should error for: {input}"
    );
}

/// Evaluate via tree-walker, expecting error
pub fn eval_tw_err(input: &str) -> SemaError {
    let interp = Interpreter::new();
    interp
        .eval_str(input)
        .expect_err(&format!("expected error for: {input}"))
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
#[macro_export]
macro_rules! dual_eval_error_tests {
    ($($name:ident : $input:expr),* $(,)?) => {
        $(
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
        )*
    };
}
