mod common;

use sema_core::Value;

// ============================================================
// Text processing — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    text_word_count: r#"(text/word-count "hello world foo bar")"# => Value::int(4),
    text_word_count_empty: r#"(text/word-count "")"# => Value::int(0),
    text_clean_whitespace: r#"(text/clean-whitespace "  hello   world  ")"# => Value::string("hello world"),
    text_strip_html: r#"(text/strip-html "<p>Hello <b>world</b></p>")"# => Value::string("Hello world"),
    text_strip_html_entities: r#"(text/strip-html "a &amp; b &lt; c")"# => Value::string("a & b < c"),
    text_truncate_short: r#"(text/truncate "hello" 10)"# => Value::string("hello"),
    text_truncate_long: r#"(text/truncate "hello world" 5)"# => Value::string("he..."),
    text_truncate_custom: r#"(text/truncate "hello world" 8 "…")"# => Value::string("hello w…"),
    text_split_sentences_count: r#"(length (text/split-sentences "Hello world. How are you? Fine."))"# => Value::int(3),
    text_split_sentences_empty: r#"(length (text/split-sentences ""))"# => Value::int(0),
    text_normalize_newlines: "(text/normalize-newlines \"a\\r\\nb\\rc\")" => Value::string("a\nb\nc"),
    text_excerpt: r#"(string? (text/excerpt "the quick brown fox jumps over" "fox" 5))"# => Value::bool(true),
    text_chunk_default: r#"(length (text/chunk "short text"))"# => Value::int(1),
    text_chunk_empty: r#"(length (text/chunk ""))"# => Value::int(0),
    text_chunk_separator: r#"(length (text/chunk-by-separator "a\nb\nc" "\n"))"# => Value::int(3),
}

// ============================================================
// Terminal / ANSI — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    term_bold: r#"(term/bold "hello")"# => Value::string("\x1b[1mhello\x1b[0m"),
    term_dim: r#"(term/dim "text")"# => Value::string("\x1b[2mtext\x1b[0m"),
    term_red: r#"(term/red "error")"# => Value::string("\x1b[31merror\x1b[0m"),
    term_green: r#"(term/green "ok")"# => Value::string("\x1b[32mok\x1b[0m"),
    term_cyan: r#"(term/cyan "info")"# => Value::string("\x1b[36minfo\x1b[0m"),
    term_gray: r#"(term/gray "muted")"# => Value::string("\x1b[90mmuted\x1b[0m"),
    // NOTE: This test depends on the exact order of ANSI SGR codes (bold=1, red=31 => "1;31").
    // If the implementation reorders style attributes, this will break. ANSI ordering IS
    // significant for some terminals, so we intentionally test the exact byte sequence.
    term_style_compound: r#"(term/style "text" :bold :red)"# => Value::string("\x1b[1;31mtext\x1b[0m"),
    term_style_plain: r#"(term/style "plain")"# => Value::string("plain"),
    term_strip: r#"(term/strip (term/bold "hello"))"# => Value::string("hello"),
    term_strip_compound: r#"(term/strip (term/style "text" :bold :red))"# => Value::string("text"),
    term_rgb: r#"(term/rgb "hi" 255 100 0)"# => Value::string("\x1b[38;2;255;100;0mhi\x1b[0m"),
    term_strip_rgb: r#"(term/strip (term/rgb "hi" 255 100 0))"# => Value::string("hi"),
}

// ============================================================
// Pretty print — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    pprint_returns_nil: r#"(pprint '(1 2 3))"# => Value::nil(),
}

// ============================================================
// Context operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    ctx_set_get: r#"(begin (context/set :name "alice") (context/get :name))"# => Value::string("alice"),
    ctx_get_missing: "(context/get :missing)" => Value::nil(),
    ctx_has: "(begin (context/set :x 1) (context/has? :x))" => Value::bool(true),
    ctx_has_missing: "(context/has? :nope)" => Value::bool(false),
    ctx_remove: "(begin (context/set :x 1) (context/remove :x) (context/has? :x))" => Value::bool(false),
    ctx_pull: r#"(begin (context/set :temp "val") (context/pull :temp))"# => Value::string("val"),
    ctx_with_scoped: r#"(begin (context/set :x "outer") (context/with {:x "inner"} (lambda () (context/get :x))))"# => Value::string("inner"),
    ctx_hidden: r#"(begin (context/set-hidden :secret "s3cret") (context/get-hidden :secret))"# => Value::string("s3cret"),
    ctx_hidden_not_visible: r#"(begin (context/set-hidden :secret "s3cret") (context/get :secret))"# => Value::nil(),
    ctx_stack_push: r#"(begin (context/push :trail "a") (context/push :trail "b") (length (context/stack :trail)))"# => Value::int(2),
    ctx_stack_pop: r#"(begin (context/push :trail "a") (context/push :trail "b") (context/pop :trail))"# => Value::string("b"),
    ctx_stack_empty: "(length (context/stack :empty))" => Value::int(0),
    ctx_merge: "(begin (context/merge {:a 1 :b 2}) (context/get :b))" => Value::int(2),
    ctx_clear: "(begin (context/set :x 1) (context/clear) (count (context/all)))" => Value::int(0),
}

// ============================================================
// Prompt/Message primitives — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    prompt_pred_false: "(prompt? 42)" => Value::bool(false),
    prompt_render: r#"(prompt/render "Hello {{name}}" {:name "Alice"})"# => Value::string("Hello Alice"),
    prompt_render_missing: r#"(prompt/render "Hello {{name}}, {{x}}." {:name "Bob"})"# => Value::string("Hello Bob, {{x}}."),
    prompt_render_number: r#"(prompt/render "Count: {{n}}" {:n 42})"# => Value::string("Count: 42"),
    prompt_render_repeated: r#"(prompt/render "{{x}} and {{x}}" {:x "hi"})"# => Value::string("hi and hi"),
    message_pred_false: "(message? 42)" => Value::bool(false),
    prompt_pred: r#"(prompt? (prompt (user "hello")))"# => Value::bool(true),
    prompt_messages_count: r#"(length (prompt/messages (prompt (user "hello") (assistant "hi"))))"# => Value::int(2),
    prompt_three_roles: r#"(length (prompt/messages (prompt (system "be helpful") (user "hello") (assistant "hi"))))"# => Value::int(3),
    prompt_append_count: r#"(length (prompt/messages (prompt/append (prompt (user "a")) (prompt (assistant "b")))))"# => Value::int(2),
    prompt_append_variadic: r#"(length (prompt/messages (prompt/append (prompt (user "a")) (prompt (user "b")) (prompt (user "c")))))"# => Value::int(3),
    prompt_concat_count: r#"(length (prompt/messages (prompt/concat (prompt (user "a")) (prompt (assistant "b")))))"# => Value::int(2),
    prompt_concat_variadic: r#"(length (prompt/messages (prompt/concat (prompt (system "s")) (prompt (user "u")) (prompt (assistant "a")))))"# => Value::int(3),
    prompt_set_system: r#"(length (prompt/messages (prompt/set-system (prompt (system "old") (user "hello")) "new")))"# => Value::int(2),
    prompt_fill_basic: r#"(message/content (car (prompt/messages (prompt/fill (prompt (user "Hello {{name}}")) {:name "Alice"}))))"# => Value::string("Hello Alice"),
    prompt_fill_missing: r#"(message/content (car (prompt/messages (prompt/fill (prompt (user "Hi {{x}}")) {:y "z"}))))"# => Value::string("Hi {{x}}"),
    prompt_fill_multi: r#"(length (prompt/messages (prompt/fill (prompt (system "You are {{role}}") (user "{{query}}")) {:role "helpful" :query "hi"})))"# => Value::int(2),
    prompt_slots_basic: r#"(length (prompt/slots (prompt (user "Hello {{name}}, age {{age}}"))))"# => Value::int(2),
    prompt_slots_empty: r#"(length (prompt/slots (prompt (user "no slots here"))))"# => Value::int(0),
    prompt_slots_dedup: r#"(length (prompt/slots (prompt (user "{{x}} and {{x}}"))))"# => Value::int(1),
    prompt_slots_multi_msg: r#"(length (prompt/slots (prompt (system "{{role}}") (user "{{query}}"))))"# => Value::int(2),
    message_pred: r#"(message? (message :user "hi"))"# => Value::bool(true),
    message_role: r#"(message/role (message :user "hi"))"# => Value::keyword("user"),
    message_content: r#"(message/content (message :user "hello world"))"# => Value::string("hello world"),
    message_from_prompt: r#"(message/content (car (prompt/messages (prompt (user "test input")))))"# => Value::string("test input"),
}

// ============================================================
// Conversation — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    conv_pred: r#"(conversation? (conversation/new))"# => Value::bool(true),
    conv_pred_false: "(conversation? 42)" => Value::bool(false),
    conv_empty_msgs: r#"(length (conversation/messages (conversation/new)))"# => Value::int(0),
    conv_model: r#"(conversation/model (conversation/new {:model "gpt-4"}))"# => Value::string("gpt-4"),
    conv_model_empty: r#"(conversation/model (conversation/new))"# => Value::string(""),
    conv_add_msg: r#"(length (conversation/messages (conversation/add-message (conversation/new) :user "hello")))"# => Value::int(1),
    conv_fork: r#"(conversation? (conversation/fork (conversation/new)))"# => Value::bool(true),
    conv_system_nil: r#"(nil? (conversation/system (conversation/new)))"# => Value::bool(true),
    conv_system_get: r#"(conversation/system (conversation/add-message (conversation/new) :system "be helpful"))"# => Value::string("be helpful"),
    conv_set_system: r#"(conversation/system (conversation/set-system (conversation/new) "be nice"))"# => Value::string("be nice"),
    conv_set_system_replace: r#"(conversation/system (conversation/set-system (conversation/add-message (conversation/new) :system "old") "new"))"# => Value::string("new"),
    conv_set_system_preserves: r#"(length (conversation/messages (conversation/set-system (conversation/add-message (conversation/add-message (conversation/new) :system "old") :user "hi") "new")))"# => Value::int(2),
    conv_filter: r#"(length (conversation/messages (conversation/filter (-> (conversation/new) (conversation/add-message :user "hello") (conversation/add-message :assistant "hi") (conversation/add-message :user "bye")) (fn (m) (= (message/role m) :user)))))"# => Value::int(2),
    conv_filter_empty: r#"(length (conversation/messages (conversation/filter (conversation/new) (fn (m) #t))))"# => Value::int(0),
    conv_map: r#"(length (conversation/map (-> (conversation/new) (conversation/add-message :user "hello") (conversation/add-message :assistant "hi")) message/content))"# => Value::int(2),
    conv_map_content: r#"(car (conversation/map (conversation/add-message (conversation/new) :user "hello") message/content))"# => Value::string("hello"),
    conv_token_count: r#"(> (conversation/token-count (conversation/add-message (conversation/new {:model "test"}) :user "hello world this is a test")) 0)"# => Value::bool(true),
    conv_token_count_empty: r#"(conversation/token-count (conversation/new))"# => Value::int(0),
}

// ============================================================
// Document operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    doc_text: r#"(document/text (document/create "hello" {:source "x"}))"# => Value::string("hello"),
    doc_metadata_source: r#"(get (document/metadata (document/create "hello" {:source "x"})) :source)"# => Value::string("x"),
}

// ============================================================
// Tool/Agent definitions — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    tool_pred: r#"(begin (deftool add-numbers "Add" {:a {:type :number}} (lambda (a) a)) (tool? add-numbers))"# => Value::bool(true),
    tool_name: r#"(begin (deftool add-numbers "Add" {:a {:type :number}} (lambda (a) a)) (tool/name add-numbers))"# => Value::string("add-numbers"),
    tool_desc: r#"(begin (deftool add-numbers "Add two" {:a {:type :number}} (lambda (a) a)) (tool/description add-numbers))"# => Value::string("Add two"),
    agent_pred: r#"(begin (deftool greet "Greet" {:name {:type :string}} (lambda (name) name)) (defagent greeter {:system "You greet." :tools [greet]}) (agent? greeter))"# => Value::bool(true),
    agent_name: r#"(begin (deftool greet "Greet" {:name {:type :string}} (lambda (name) name)) (defagent greeter {:system "You greet." :tools [greet]}) (agent/name greeter))"# => Value::string("greeter"),
    agent_system: r#"(begin (deftool greet "Greet" {:name {:type :string}} (lambda (name) name)) (defagent greeter {:system "You greet." :tools [greet]}) (agent/system greeter))"# => Value::string("You greet."),
}

// ============================================================
// LLM utility functions (no API calls) — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    llm_token_count_empty: r#"(llm/token-count "")"# => Value::int(0),
    llm_similarity_identical: "(llm/similarity '(1.0 0.0 0.0) '(1.0 0.0 0.0))" => Value::float(1.0),
    llm_similarity_orthogonal: "(llm/similarity '(1.0 0.0) '(0.0 1.0))" => Value::float(0.0),
    llm_reset_usage: "(llm/reset-usage)" => Value::nil(),
    llm_set_pricing: r#"(llm/set-pricing "my-model" 1.0 2.0)"# => Value::nil(),
}

// ============================================================
// Retry — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    retry_basic: r#"(retry (lambda () 42))"# => Value::int(42),
    retry_with_opts: r#"(retry (lambda () 42) {:max-attempts 3})"# => Value::int(42),
}

// ============================================================
// Log (side-effect, returns nil) — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    log_info: r#"(log/info "hello")"# => Value::nil(),
    log_warn: r#"(log/warn "caution")"# => Value::nil(),
}
