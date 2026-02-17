use sema_core::Value;
use sema_eval::Interpreter;

fn eval_with_llm(input: &str) -> Value {
    let interp = Interpreter::new();
    interp
        .eval_str(&format!("(llm/auto-configure) {input}"))
        .expect(&format!("failed to eval: {input}"))
}

// === LLM completion ===

#[test]
#[ignore] // requires ANTHROPIC_API_KEY or OPENAI_API_KEY
fn test_llm_complete_basic() {
    let result = eval_with_llm(r#"(llm/complete "What is 2+2? Reply with just the number.")"#);
    let s = format!("{}", result);
    assert!(
        s.contains('4'),
        "expected response to contain '4', got: {s}"
    );
}

// === LLM extract ===

#[test]
#[ignore] // requires ANTHROPIC_API_KEY or OPENAI_API_KEY
fn test_llm_extract_basic() {
    let result =
        eval_with_llm(r#"(llm/extract {:answer :string} "The capital of France is Paris.")"#);
    let s = format!("{}", result);
    assert!(
        s.contains("Paris"),
        "expected result to contain 'Paris', got: {s}"
    );
}

// === llm/extract-from-image error paths (no API key needed) ===

#[test]
fn test_llm_extract_from_image_arity() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(llm/extract-from-image {:x :string})"#);
    assert!(result.is_err());
}

#[test]
fn test_llm_extract_from_image_invalid_path() {
    let interp = Interpreter::new();
    let result =
        interp.eval_str(r#"(llm/extract-from-image {:x :string} "/nonexistent/file.png")"#);
    assert!(result.is_err());
}

// === llm/extract-from-image with bytevector (requires API key) ===

#[test]
#[ignore] // requires ANTHROPIC_API_KEY or OPENAI_API_KEY
fn test_llm_extract_from_image_with_bytevector() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (llm/auto-configure)
            (define img (bytevector 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1 0 0 0 1 8 2 0 0 0 144 119 83 222 0 0 0 12 73 68 65 84 120 156 99 248 207 192 0 0 3 1 1 0 201 254 146 239 0 0 0 0 73 69 78 68 174 66 96 130))
            (llm/extract-from-image {:description :string} img))"#,
    );
    println!("extract-from-image result: {:?}", result);
    assert!(result.is_ok(), "llm/extract-from-image should not panic");
}

// === message/with-image + llm/chat (requires API key) ===

#[test]
#[ignore] // requires ANTHROPIC_API_KEY or OPENAI_API_KEY
fn test_message_with_image_in_llm_chat() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (llm/auto-configure)
            (define img (bytevector 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1 0 0 0 1 8 2 0 0 0 144 119 83 222 0 0 0 12 73 68 65 84 120 156 99 248 207 192 0 0 3 1 1 0 201 254 146 239 0 0 0 0 73 69 78 68 174 66 96 130))
            (define msg (message/with-image :user "Describe this image briefly." img))
            (llm/chat (list msg)))"#,
    );
    println!("llm/chat with image result: {:?}", result);
    assert!(result.is_ok(), "llm/chat with image should not panic");
}
