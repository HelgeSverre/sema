//! Doctest parser and runner.
//!
//! Extracts executable examples from docstrings:
//! - `>>>` lines are expressions to evaluate
//! - The next non-blank line is the expected result
//! - `!! substring` means expect an error containing substring
//! - `>>>!` means evaluate but don't check result (setup)

use sema_core::{SemaError, Value};

/// A single doctest example.
#[derive(Debug, Clone)]
pub struct DocTest {
    pub input: String,
    pub expected: Expected,
    pub line: usize,
}

/// Expected result of a doctest.
#[derive(Debug, Clone)]
pub enum Expected {
    /// Compare return value (the expected expression as a string).
    Value(String),
    /// Expect an error containing this substring.
    Error(String),
    /// Don't check the result (setup step).
    Skip,
}

/// Result of running a single doctest.
#[derive(Debug)]
pub struct DocTestResult {
    pub input: String,
    pub passed: bool,
    pub expected: String,
    pub actual: String,
    pub line: usize,
}

/// Parse doctests from a docstring.
pub fn parse_doctests(docstring: &str) -> Vec<DocTest> {
    let mut tests = Vec::new();
    let lines: Vec<&str> = docstring.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let trimmed = lines[i].trim();

        if let Some(expr) = trimmed.strip_prefix(">>>!") {
            // Setup-only: evaluate but don't check
            let expr = expr.trim().to_string();
            if !expr.is_empty() {
                tests.push(DocTest {
                    input: expr,
                    expected: Expected::Skip,
                    line: i + 1,
                });
            }
            i += 1;
        } else if let Some(expr) = trimmed.strip_prefix(">>>") {
            let expr = expr.trim().to_string();
            if expr.is_empty() {
                i += 1;
                continue;
            }

            // Look at next non-blank line for expected result
            i += 1;
            while i < lines.len() && lines[i].trim().is_empty() {
                i += 1;
            }

            if i < lines.len() {
                let expected_line = lines[i].trim();
                if let Some(err_match) = expected_line.strip_prefix("!!") {
                    tests.push(DocTest {
                        input: expr,
                        expected: Expected::Error(err_match.trim().to_string()),
                        line: i + 1,
                    });
                    i += 1;
                } else if expected_line.starts_with(">>>") {
                    // No expected value, next line is another test
                    tests.push(DocTest {
                        input: expr,
                        expected: Expected::Skip,
                        line: i,
                    });
                    // Don't increment i, re-process this line
                } else {
                    tests.push(DocTest {
                        input: expr,
                        expected: Expected::Value(expected_line.to_string()),
                        line: i + 1,
                    });
                    i += 1;
                }
            } else {
                // No expected line — treat as skip
                tests.push(DocTest {
                    input: expr,
                    expected: Expected::Skip,
                    line: i,
                });
            }
        } else {
            i += 1;
        }
    }

    tests
}

/// Run all doctests, using the provided eval function for execution.
pub fn run_doctests<F>(docstring: &str, eval_fn: &mut F) -> Vec<DocTestResult>
where
    F: FnMut(&str) -> Result<Value, SemaError>,
{
    let tests = parse_doctests(docstring);
    let mut results = Vec::new();

    for test in tests {
        match &test.expected {
            Expected::Skip => match eval_fn(&test.input) {
                Ok(_) => {
                    results.push(DocTestResult {
                        input: test.input,
                        passed: true,
                        expected: "(skip)".to_string(),
                        actual: "(skip)".to_string(),
                        line: test.line,
                    });
                }
                Err(e) => {
                    results.push(DocTestResult {
                        input: test.input,
                        passed: false,
                        expected: "(skip)".to_string(),
                        actual: format!("ERROR: {e}"),
                        line: test.line,
                    });
                }
            },
            Expected::Value(expected_str) => match eval_fn(&test.input) {
                Ok(actual) => {
                    let actual_str = format!("{actual}");
                    let passed = actual_str.trim() == expected_str.trim();
                    results.push(DocTestResult {
                        input: test.input,
                        passed,
                        expected: expected_str.clone(),
                        actual: actual_str,
                        line: test.line,
                    });
                }
                Err(e) => {
                    results.push(DocTestResult {
                        input: test.input,
                        passed: false,
                        expected: expected_str.clone(),
                        actual: format!("ERROR: {e}"),
                        line: test.line,
                    });
                }
            },
            Expected::Error(expected_substring) => match eval_fn(&test.input) {
                Ok(val) => {
                    results.push(DocTestResult {
                        input: test.input,
                        passed: false,
                        expected: format!("!! {expected_substring}"),
                        actual: format!("{val}"),
                        line: test.line,
                    });
                }
                Err(e) => {
                    let err_str = e.to_string().to_lowercase();
                    let expected_lower = expected_substring.to_lowercase();
                    let passed = err_str.contains(&expected_lower);
                    results.push(DocTestResult {
                        input: test.input,
                        passed,
                        expected: format!("!! {expected_substring}"),
                        actual: format!("!! {e}"),
                        line: test.line,
                    });
                }
            },
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic() {
        let doc = r#"Does something.

   >>> (+ 1 2)
   3

   >>> (string/trim "  hi  ")
   "hi""#;
        let tests = parse_doctests(doc);
        assert_eq!(tests.len(), 2);
        assert_eq!(tests[0].input, "(+ 1 2)");
        assert!(matches!(tests[0].expected, Expected::Value(ref s) if s == "3"));
        assert_eq!(tests[1].input, r#"(string/trim "  hi  ")"#);
    }

    #[test]
    fn test_parse_error_expected() {
        let doc = r#"Fails on bad input.

   >>> (/ 1 0)
   !! division by zero"#;
        let tests = parse_doctests(doc);
        assert_eq!(tests.len(), 1);
        assert!(matches!(tests[0].expected, Expected::Error(ref s) if s == "division by zero"));
    }

    #[test]
    fn test_parse_setup_step() {
        let doc = r#"Needs setup.

   >>>! (define x 42)
   >>> x
   42"#;
        let tests = parse_doctests(doc);
        assert_eq!(tests.len(), 2);
        assert!(matches!(tests[0].expected, Expected::Skip));
        assert_eq!(tests[1].input, "x");
    }

    #[test]
    fn test_parse_empty_docstring() {
        let tests = parse_doctests("Just a description, no examples.");
        assert!(tests.is_empty());
    }
}
