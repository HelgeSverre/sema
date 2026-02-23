use sema_core::SemaError;
use sema_reader::lexer::{tokenize, FStringPart, SpannedToken, Token};

// ---------------------------------------------------------------------------
// Node tree — lightweight structure built from the flat token stream
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Node {
    /// A single semantic token (symbol, number, string, keyword, bool, char, dot, etc.)
    Atom(Token),
    /// A comment (already includes leading semicolons)
    Comment(String),
    /// A newline separator (used to track blank lines between forms)
    Newline,
    /// `(` ... `)`
    List(Vec<Node>),
    /// `[` ... `]`
    Vector(Vec<Node>),
    /// `{` ... `}`
    Map(Vec<Node>),
    /// `#(` ... `)`
    ShortLambda(Vec<Node>),
    /// `#u8(` ... `)`
    ByteVector(Vec<Node>),
    /// Quote / quasiquote / unquote / unquote-splice prefix attached to the
    /// following node.
    Prefix(Token, Box<Node>),
}

// ---------------------------------------------------------------------------
// Building the node tree from the flat token stream
// ---------------------------------------------------------------------------

fn build_nodes(tokens: &[SpannedToken]) -> Result<Vec<Node>, SemaError> {
    let mut pos = 0;
    let mut nodes = Vec::new();
    while pos < tokens.len() {
        let (node, next) = build_one(tokens, pos)?;
        nodes.push(node);
        pos = next;
    }
    Ok(nodes)
}

/// Parse one node starting at `pos`, returning `(node, next_pos)`.
fn build_one(tokens: &[SpannedToken], pos: usize) -> Result<(Node, usize), SemaError> {
    if pos >= tokens.len() {
        return Err(SemaError::eval("unexpected end of token stream"));
    }
    let st = &tokens[pos];
    match &st.token {
        Token::Comment(text) => Ok((Node::Comment(text.clone()), pos + 1)),
        Token::Newline => Ok((Node::Newline, pos + 1)),

        // Prefix tokens — attach to the following node
        Token::Quote | Token::Quasiquote | Token::Unquote | Token::UnquoteSplice => {
            let prefix_tok = st.token.clone();
            if pos + 1 >= tokens.len() {
                return Err(SemaError::eval("prefix token at end of input"));
            }
            let (inner, next) = build_one(tokens, pos + 1)?;
            Ok((Node::Prefix(prefix_tok, Box::new(inner)), next))
        }

        // Grouped forms
        Token::LParen => build_group(tokens, pos + 1, Token::RParen, |children| {
            Node::List(children)
        }),
        Token::LBracket => build_group(tokens, pos + 1, Token::RBracket, |children| {
            Node::Vector(children)
        }),
        Token::LBrace => build_group(tokens, pos + 1, Token::RBrace, |children| {
            Node::Map(children)
        }),
        Token::ShortLambdaStart => build_group(tokens, pos + 1, Token::RParen, |children| {
            Node::ShortLambda(children)
        }),
        Token::BytevectorStart => build_group(tokens, pos + 1, Token::RParen, |children| {
            Node::ByteVector(children)
        }),

        // Closing delimiters — should not appear here at top-level
        Token::RParen | Token::RBracket | Token::RBrace => {
            Err(SemaError::eval("unexpected closing delimiter"))
        }

        // Everything else is an atom
        _ => Ok((Node::Atom(st.token.clone()), pos + 1)),
    }
}

fn build_group<F>(
    tokens: &[SpannedToken],
    start: usize,
    closer: Token,
    make: F,
) -> Result<(Node, usize), SemaError>
where
    F: FnOnce(Vec<Node>) -> Node,
{
    let mut pos = start;
    let mut children = Vec::new();
    while pos < tokens.len() {
        if std::mem::discriminant(&tokens[pos].token) == std::mem::discriminant(&closer) {
            return Ok((make(children), pos + 1));
        }
        let (node, next) = build_one(tokens, pos)?;
        children.push(node);
        pos = next;
    }
    Err(SemaError::eval("unclosed delimiter"))
}

// ---------------------------------------------------------------------------
// Form classification
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
enum FormKind {
    Body,      // define, defn, fn, lambda, do, begin, when, unless, module, etc.
    Binding,   // let, let*, letrec, when-let, if-let
    Clause,    // cond, case, match
    Threading, // ->, ->>, as->, some->
    TryCatch,  // try
    Cond,      // if
    Import,    // import, load, require
    Call,      // default function call
}

fn classify_form(children: &[Node]) -> FormKind {
    // Find the first non-trivia child; only classify if it's a symbol
    let head = children
        .iter()
        .find(|n| !is_trivia(n))
        .and_then(|n| match n {
            Node::Atom(Token::Symbol(s)) => Some(s.as_str()),
            _ => None,
        });

    match head {
        Some(
            "define"
            | "defn"
            | "defun"
            | "defmacro"
            | "fn"
            | "lambda"
            | "do"
            | "begin"
            | "when"
            | "unless"
            | "module"
            | "defagent"
            | "deftool"
            | "prompt"
            | "message"
            | "export"
            | "for"
            | "for-each"
            | "while"
            | "with-open-file"
            | "with-exception-handler"
            | "define-record-type"
            | "define-syntax"
            | "syntax-rules",
        ) => FormKind::Body,
        Some("let" | "let*" | "letrec" | "when-let" | "if-let") => FormKind::Binding,
        Some("cond" | "case" | "match") => FormKind::Clause,
        Some("->" | "->>" | "as->" | "some->") => FormKind::Threading,
        Some("try") => FormKind::TryCatch,
        Some("if") => FormKind::Cond,
        Some("import" | "load" | "require") => FormKind::Import,
        _ => FormKind::Call,
    }
}

fn is_trivia(n: &Node) -> bool {
    matches!(n, Node::Comment(_) | Node::Newline)
}



// ---------------------------------------------------------------------------
// Measuring the flat (single-line) width of a node
// ---------------------------------------------------------------------------

fn flat_width(node: &Node) -> usize {
    match node {
        Node::Atom(tok) => token_text(tok).len(),
        Node::Comment(text) => text.len(),
        Node::Newline => 0,
        Node::List(children) => grouped_flat_width(children, "(", ")"),
        Node::Vector(children) => grouped_flat_width(children, "[", "]"),
        Node::Map(children) => grouped_flat_width(children, "{", "}"),
        Node::ShortLambda(children) => grouped_flat_width(children, "#(", ")"),
        Node::ByteVector(children) => grouped_flat_width(children, "#u8(", ")"),
        Node::Prefix(tok, inner) => prefix_text(tok).len() + flat_width(inner),
    }
}

fn grouped_flat_width(children: &[Node], open: &str, close: &str) -> usize {
    let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();
    if semantic.is_empty() {
        return open.len() + close.len();
    }
    let inner: usize = semantic.iter().map(|n| flat_width(n)).sum::<usize>();
    let spaces = semantic.len().saturating_sub(1);
    open.len() + inner + spaces + close.len()
}

// ---------------------------------------------------------------------------
// Token → source text
// ---------------------------------------------------------------------------

fn token_text(tok: &Token) -> String {
    match tok {
        Token::Symbol(s) => s.clone(),
        Token::Keyword(s) => format!(":{s}"),
        Token::String(s) => format!("\"{}\"", escape_string(s)),
        Token::FString(parts) => format_fstring(parts),
        Token::Regex(s) => format!("#\"{}\"", escape_regex(s)),
        Token::Int(n) => n.to_string(),
        Token::Float(f) => format_float(*f),
        Token::Bool(true) => "#t".to_string(),
        Token::Bool(false) => "#f".to_string(),
        Token::Char(c) => format_char(*c),
        Token::Dot => ".".to_string(),
        // These shouldn't normally appear as atoms but handle them:
        Token::LParen => "(".to_string(),
        Token::RParen => ")".to_string(),
        Token::LBracket => "[".to_string(),
        Token::RBracket => "]".to_string(),
        Token::LBrace => "{".to_string(),
        Token::RBrace => "}".to_string(),
        Token::Quote => "'".to_string(),
        Token::Quasiquote => "`".to_string(),
        Token::Unquote => ",".to_string(),
        Token::UnquoteSplice => ",@".to_string(),
        Token::ShortLambdaStart => "#(".to_string(),
        Token::BytevectorStart => "#u8(".to_string(),
        Token::Comment(text) => text.clone(),
        Token::Newline => "\n".to_string(),
    }
}

fn prefix_text(tok: &Token) -> &'static str {
    match tok {
        Token::Quote => "'",
        Token::Quasiquote => "`",
        Token::Unquote => ",",
        Token::UnquoteSplice => ",@",
        _ => "",
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\0' => out.push_str("\\0"),
            _ => out.push(c),
        }
    }
    out
}

fn escape_regex(s: &str) -> String {
    // For regex, we only need to escape literal double-quotes
    s.replace('"', "\\\"")
}

fn format_fstring(parts: &[FStringPart]) -> String {
    let mut out = String::from("f\"");
    for part in parts {
        match part {
            FStringPart::Literal(s) => {
                // Escape string content but also need to preserve $ that isn't
                // before { in the original.
                for c in s.chars() {
                    match c {
                        '\n' => out.push_str("\\n"),
                        '\t' => out.push_str("\\t"),
                        '\r' => out.push_str("\\r"),
                        '\\' => out.push_str("\\\\"),
                        '"' => out.push_str("\\\""),
                        '\0' => out.push_str("\\0"),
                        _ => out.push(c),
                    }
                }
            }
            FStringPart::Expr(expr) => {
                out.push_str("${");
                out.push_str(expr);
                out.push('}');
            }
        }
    }
    out.push('"');
    out
}

fn format_float(f: f64) -> String {
    if f.fract() == 0.0 && !f.is_infinite() && !f.is_nan() {
        format!("{f:.1}")
    } else {
        let s = format!("{f}");
        s
    }
}

fn format_char(c: char) -> String {
    match c {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        '\t' => "#\\tab".to_string(),
        '\r' => "#\\return".to_string(),
        '\0' => "#\\nul".to_string(),
        _ => format!("#\\{c}"),
    }
}

// ---------------------------------------------------------------------------
// Formatting engine
// ---------------------------------------------------------------------------

struct Formatter {
    width: usize,
    output: String,
}

impl Formatter {
    fn new(width: usize) -> Self {
        Self {
            width,
            output: String::new(),
        }
    }

    /// Format a sequence of top-level nodes.
    fn format_top_level(&mut self, nodes: &[Node]) {
        let mut i = 0;
        let len = nodes.len();
        // Track whether we've emitted any content yet
        let mut first_content = true;
        // Track consecutive newline count for blank line collapsing
        let mut pending_blank_lines: usize = 0;

        while i < len {
            match &nodes[i] {
                Node::Newline => {
                    pending_blank_lines += 1;
                    i += 1;
                }
                Node::Comment(text) => {
                    if !first_content {
                        if pending_blank_lines > 1 {
                            // Collapse multiple blank lines to 1
                            self.output.push('\n');
                        }
                        // Always start the comment on a new line
                        if !self.output.ends_with('\n') {
                            self.output.push('\n');
                        }
                    }
                    pending_blank_lines = 0;
                    self.output.push_str(text);
                    self.output.push('\n');
                    first_content = false;
                    i += 1;
                }
                _ => {
                    if !first_content {
                        if pending_blank_lines > 1 {
                            // There was at least one blank line between forms
                            self.output.push('\n');
                        }
                        if !self.output.ends_with('\n') {
                            self.output.push('\n');
                        }
                    }
                    pending_blank_lines = 0;

                    // Collect any trailing comment on the same "logical line"
                    // (i.e. the next non-newline token after the form, if it's
                    // a comment and there's no newline in between)
                    let trailing_comment = self.find_trailing_comment(nodes, i + 1);

                    self.format_node(&nodes[i], 0);
                    if let Some((comment_text, skip_to)) = trailing_comment {
                        self.output.push(' ');
                        self.output.push_str(&comment_text);
                        i = skip_to;
                    } else {
                        i += 1;
                    }
                    if !self.output.ends_with('\n') {
                        self.output.push('\n');
                    }
                    first_content = false;
                }
            }
        }
    }

    /// Look ahead from `start` to see if there is a trailing comment
    /// (a comment that was on the same line as the preceding form).
    /// Returns Some((comment_text, next_pos_after_comment)) if found.
    fn find_trailing_comment(&self, nodes: &[Node], start: usize) -> Option<(String, usize)> {
        // A trailing comment is: possibly nothing, then a Comment, with no
        // Newline in between.
        if start < nodes.len() {
            if let Node::Comment(text) = &nodes[start] {
                return Some((text.clone(), start + 1));
            }
        }
        None
    }

    /// Format a single node at the given indentation level.
    fn format_node(&mut self, node: &Node, indent: usize) {
        match node {
            Node::Atom(tok) => {
                self.output.push_str(&token_text(tok));
            }
            Node::Comment(text) => {
                self.output.push_str(text);
            }
            Node::Newline => {
                // At the formatting level, newlines are handled by the parent logic
            }
            Node::List(children) => {
                self.format_list(children, indent, "(", ")");
            }
            Node::Vector(children) => {
                self.format_collection(children, indent, "[", "]");
            }
            Node::Map(children) => {
                self.format_map(children, indent, "{", "}");
            }
            Node::ShortLambda(children) => {
                self.format_list(children, indent, "#(", ")");
            }
            Node::ByteVector(children) => {
                self.format_collection(children, indent, "#u8(", ")");
            }
            Node::Prefix(tok, inner) => {
                self.output.push_str(prefix_text(tok));
                self.format_node(inner, indent);
            }
        }
    }

    /// Format a list form with Lisp-aware indentation.
    fn format_list(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        // Empty form
        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        let kind = classify_form(children);
        let has_comments = children.iter().any(|n| matches!(n, Node::Comment(_)));

        // Try one-line format first (only if no inner comments).
        // House style: if the form fits on one line, keep it — even body forms
        // like (fn (a) (fn (b) (f a b))). Only break when it exceeds width.
        if !has_comments {
            let one_line = self.format_flat(&semantic, open, close);
            if indent + one_line.len() <= self.width {
                self.output.push_str(&one_line);
                return;
            }
        }

        // Multi-line: dispatch based on form kind
        match kind {
            FormKind::Body => self.format_body(children, indent, open, close),
            FormKind::Binding => self.format_binding(children, indent, open, close),
            FormKind::Clause => self.format_clause(children, indent, open, close),
            FormKind::Threading => self.format_threading(children, indent, open, close),
            FormKind::TryCatch => self.format_body(children, indent, open, close),
            FormKind::Cond => self.format_conditional(children, indent, open, close),
            FormKind::Import => self.format_import(children, indent, open, close),
            FormKind::Call => self.format_call(children, indent, open, close),
        }
    }

    /// Render nodes flat (single line) with spaces between them.
    fn format_flat(&self, nodes: &[&Node], open: &str, close: &str) -> String {
        let mut out = String::new();
        out.push_str(open);
        let mut first = true;
        for node in nodes {
            if !first {
                out.push(' ');
            }
            out.push_str(&self.node_to_flat_string(node));
            first = false;
        }
        out.push_str(close);
        out
    }

    /// Render a single node as a flat (single-line) string.
    fn node_to_flat_string(&self, node: &Node) -> String {
        match node {
            Node::Atom(tok) => token_text(tok),
            Node::Comment(text) => text.clone(),
            Node::Newline => String::new(),
            Node::List(children) => {
                let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();
                self.format_flat(&semantic, "(", ")")
            }
            Node::Vector(children) => {
                let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();
                self.format_flat(&semantic, "[", "]")
            }
            Node::Map(children) => {
                let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();
                self.format_flat(&semantic, "{", "}")
            }
            Node::ShortLambda(children) => {
                let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();
                self.format_flat(&semantic, "#(", ")")
            }
            Node::ByteVector(children) => {
                let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();
                self.format_flat(&semantic, "#u8(", ")")
            }
            Node::Prefix(tok, inner) => {
                format!("{}{}", prefix_text(tok), self.node_to_flat_string(inner))
            }
        }
    }

    // -----------------------------------------------------------------------
    // Body forms: (define name ...\n  body...)
    // -----------------------------------------------------------------------

    fn format_body(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<(usize, &Node)> = children
            .iter()
            .enumerate()
            .filter(|(_, n)| !is_trivia(n))
            .collect();

        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        // Determine how many "distinguished" args go on the first line.
        // For define/defn: head + name + params (up to 3)
        // For fn/lambda: head + params (up to 2)
        // For do/begin/when/unless: just head (1)
        let head_name = match &semantic[0].1 {
            Node::Atom(Token::Symbol(s)) => s.as_str(),
            _ => "",
        };
        let first_line_count = match head_name {
            "define" => {
                // (define x 42) — head + name + value = 3 on first line
                // (define (f x) body...) — head + signature = 2 on first line
                if semantic.len() > 1 && matches!(semantic[1].1, Node::List(_)) {
                    // Scheme-style: (define (name args...) body...)
                    2
                } else {
                    // (define name value) or (define name)
                    semantic.len().min(3)
                }
            }
            "defn" | "defun" | "defmacro" | "defagent" | "deftool" | "define-record-type"
            | "define-syntax" => {
                // (defn name [params] body...) — head + name + params
                let mut count = 1; // the head symbol
                for (_idx, node) in semantic.iter().skip(1) {
                    match node {
                        Node::Atom(_) | Node::Vector(_) | Node::Prefix(_, _) | Node::List(_) => {
                            count += 1;
                        }
                        _ => break,
                    }
                }
                count
            }
            "fn" | "lambda" => {
                // head + params
                if semantic.len() > 1 {
                    2
                } else {
                    1
                }
            }
            _ => 1, // do, begin, when, unless, etc: just the head
        };

        let first_count = first_line_count.min(semantic.len());

        self.output.push_str(open);

        // Emit first-line args
        for (j, (_orig_idx, node)) in semantic.iter().enumerate().take(first_count) {
            if j > 0 {
                self.output.push(' ');
            }
            self.format_node(node, indent + open.len());
        }

        // Emit body forms with interleaved comments preserved
        let body_indent = indent + 2;
        let body_start = Self::index_after_nth_semantic(children, first_count);
        self.emit_body_with_comments(children, body_start, body_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Binding forms: (let ([x 1] [y 2])\n  body...)
    // -----------------------------------------------------------------------

    fn format_binding(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.len() < 2 {
            // Degenerate, just format as call
            return self.format_call(children, indent, open, close);
        }

        self.output.push_str(open);

        // head (let/let*/letrec)
        self.format_node(semantic[0], indent + open.len());
        self.output.push(' ');

        // bindings (the second semantic child, usually a vector or list)
        let bindings_indent = indent + open.len() + flat_width(semantic[0]) + 1;
        self.format_node(semantic[1], bindings_indent);

        // body forms with interleaved comments preserved
        let body_indent = indent + 2;
        let body_start = Self::index_after_nth_semantic(children, 2);
        self.emit_body_with_comments(children, body_start, body_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Clause forms: (cond\n  (test1 expr1)\n  (test2 expr2))
    // -----------------------------------------------------------------------

    fn format_clause(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        self.output.push_str(open);
        // head
        self.format_node(semantic[0], indent + open.len());

        // clauses with interleaved comments preserved
        let clause_indent = indent + 2;
        let clause_start = Self::index_after_nth_semantic(children, 1);
        self.emit_body_with_comments(children, clause_start, clause_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Threading macros: (-> val\n  step1\n  step2)
    // -----------------------------------------------------------------------

    fn format_threading(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.len() < 2 {
            return self.format_call(children, indent, open, close);
        }

        self.output.push_str(open);
        // head (->)
        self.format_node(semantic[0], indent + open.len());
        self.output.push(' ');
        // first value
        self.format_node(
            semantic[1],
            indent + open.len() + flat_width(semantic[0]) + 1,
        );

        // steps with interleaved comments preserved
        let step_indent = indent + 2;
        let step_start = Self::index_after_nth_semantic(children, 2);
        self.emit_body_with_comments(children, step_start, step_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Conditional: (if test then else)
    // -----------------------------------------------------------------------

    fn format_conditional(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        // Try: head + test on first line, then/else indented
        self.output.push_str(open);
        // head (if)
        self.format_node(semantic[0], indent + open.len());

        if semantic.len() > 1 {
            self.output.push(' ');
            // test
            self.format_node(
                semantic[1],
                indent + open.len() + flat_width(semantic[0]) + 1,
            );
        }

        // then/else branches with interleaved comments preserved
        let body_indent = indent + 2;
        let body_start = Self::index_after_nth_semantic(children, 2);
        self.emit_body_with_comments(children, body_start, body_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Import: (import "module") or (import\n  "mod1"\n  "mod2")
    // -----------------------------------------------------------------------

    fn format_import(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        // Same as body with first_count = 1
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        // If children contain comments, force multi-line to preserve them
        let has_comments = children.iter().any(|n| matches!(n, Node::Comment(_)));

        // Try one-line first (only if no inner comments)
        if !has_comments {
            let one_line = self.format_flat(&semantic, open, close);
            if indent + one_line.len() <= self.width {
                self.output.push_str(&one_line);
                return;
            }
        }

        self.output.push_str(open);
        self.format_node(semantic[0], indent + open.len());

        // args with interleaved comments preserved
        let arg_indent = indent + 2;
        let arg_start = Self::index_after_nth_semantic(children, 1);
        self.emit_body_with_comments(children, arg_start, arg_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Default call: (f arg1 arg2 ...) — align args with first arg
    // -----------------------------------------------------------------------

    fn format_call(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        self.output.push_str(open);
        // head
        self.format_node(semantic[0], indent + open.len());

        if semantic.len() == 1 {
            self.output.push_str(close);
            return;
        }

        // Try: head + first arg on same line
        let head_width = flat_width(semantic[0]);
        let first_arg_col = indent + open.len() + head_width + 1;
        let arg_indent = indent + 2;

        // Check if head + first arg fits on one line
        if first_arg_col + flat_width(semantic[1]) <= self.width {
            // First arg on same line
            self.output.push(' ');
            self.format_node(semantic[1], first_arg_col);

            // Remaining args indented 2 from opening paren, with comments preserved
            let rest_start = Self::index_after_nth_semantic(children, 2);
            self.emit_body_with_comments(children, rest_start, arg_indent);
        } else {
            // Everything indented 2 from opening paren, with comments preserved
            let rest_start = Self::index_after_nth_semantic(children, 1);
            self.emit_body_with_comments(children, rest_start, arg_indent);
        }

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Collection (vector): [a b c] — one-line or one-per-line
    // -----------------------------------------------------------------------

    fn format_collection(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        // If children contain comments, force multi-line to preserve them
        let has_comments = children.iter().any(|n| matches!(n, Node::Comment(_)));

        // Try one-line (only if no inner comments)
        if !has_comments {
            let one_line = self.format_flat(&semantic, open, close);
            if indent + one_line.len() <= self.width {
                self.output.push_str(&one_line);
                return;
            }
        }

        // One per line, with comments preserved
        let elem_indent = indent + open.len();
        self.output.push_str(open);
        self.format_node(semantic[0], elem_indent);

        let rest_start = Self::index_after_nth_semantic(children, 1);
        self.emit_body_with_comments(children, rest_start, elem_indent);

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Map: {:a 1 :b 2} — key-value pairs, one per line if doesn't fit
    // -----------------------------------------------------------------------

    fn format_map(&mut self, children: &[Node], indent: usize, open: &str, close: &str) {
        let semantic: Vec<&Node> = children.iter().filter(|n| !is_trivia(n)).collect();

        if semantic.is_empty() {
            self.output.push_str(open);
            self.output.push_str(close);
            return;
        }

        // If children contain comments, force multi-line to preserve them
        let has_comments = children.iter().any(|n| matches!(n, Node::Comment(_)));

        // Try one-line (only if no inner comments)
        if !has_comments {
            let one_line = self.format_flat(&semantic, open, close);
            if indent + one_line.len() <= self.width {
                self.output.push_str(&one_line);
                return;
            }
        }

        // Multi-line: each key-value pair on its own line, with comments preserved
        let pair_indent = indent + open.len();
        self.output.push_str(open);

        // Iterate through all children, tracking pair state
        // semantic_count: 0 = expecting key (start of pair), 1 = expecting value
        let mut semantic_count = 0;
        let mut first_pair = true;
        for child in children.iter() {
            match child {
                Node::Newline => {}
                Node::Comment(text) => {
                    self.output.push('\n');
                    self.push_indent(pair_indent);
                    self.output.push_str(text);
                }
                _ if is_trivia(child) => {}
                _ => {
                    if semantic_count % 2 == 0 {
                        // Key position — start a new pair
                        if !first_pair {
                            self.output.push('\n');
                            self.push_indent(pair_indent);
                        }
                        self.format_node(child, pair_indent);
                        first_pair = false;
                    } else {
                        // Value position — on same line as key
                        self.output.push(' ');
                        self.format_node(child, pair_indent);
                    }
                    semantic_count += 1;
                }
            }
        }

        self.output.push_str(close);
    }

    // -----------------------------------------------------------------------
    // Helper: emit body children with interleaved comments preserved
    // -----------------------------------------------------------------------

    /// Find the index in `all_children` just past the `n`th semantic (non-trivia) node.
    /// Returns `all_children.len()` if fewer than `n` semantic nodes exist.
    fn index_after_nth_semantic(all_children: &[Node], n: usize) -> usize {
        let mut count = 0;
        for (i, child) in all_children.iter().enumerate() {
            if !is_trivia(child) {
                count += 1;
                if count == n {
                    return i + 1;
                }
            }
        }
        all_children.len()
    }

    /// Emit all children starting from `start_idx`, preserving comments inline.
    /// Semantic nodes are formatted on their own lines at `body_indent`.
    /// Comments are emitted at `body_indent`. Multiple blank lines are collapsed.
    fn emit_body_with_comments(
        &mut self,
        all_children: &[Node],
        start_idx: usize,
        body_indent: usize,
    ) {
        let mut prev_was_newline = false;
        for child in &all_children[start_idx..] {
            match child {
                Node::Newline => {
                    prev_was_newline = true;
                }
                Node::Comment(text) => {
                    if prev_was_newline {
                        // blank line before the comment — already emitting newlines
                    }
                    self.output.push('\n');
                    self.push_indent(body_indent);
                    self.output.push_str(text);
                    prev_was_newline = false;
                }
                _ if is_trivia(child) => {}
                _ => {
                    self.output.push('\n');
                    self.push_indent(body_indent);
                    self.format_node(child, body_indent);
                    prev_was_newline = false;
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Utilities
    // -----------------------------------------------------------------------

    fn push_indent(&mut self, n: usize) {
        for _ in 0..n {
            self.output.push(' ');
        }
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Format Sema source code, targeting the given line width.
///
/// The formatter preserves all comments, handles shebang lines, and produces
/// idempotent output.
pub fn format_source(input: &str, width: usize) -> Result<String, SemaError> {
    if input.is_empty() {
        return Ok(String::new());
    }

    // 1. Handle shebang: if input starts with "#!", extract the first line
    let (shebang, rest) = if input.starts_with("#!") {
        match input.find('\n') {
            Some(pos) => (Some(&input[..pos]), &input[pos + 1..]),
            None => (Some(input), ""),
        }
    } else {
        (None, input)
    };

    // 2. Tokenize the remaining source
    if rest.trim().is_empty() {
        let mut result = String::new();
        if let Some(shebang_line) = shebang {
            result.push_str(shebang_line);
            result.push('\n');
        }
        return Ok(result);
    }

    let tokens = tokenize(rest)?;

    // 3. Build node tree from tokens
    let nodes = build_nodes(&tokens)?;

    // 4. Format node tree to string
    let mut fmt = Formatter::new(width);
    fmt.format_top_level(&nodes);

    // 5. Assemble result
    let mut result = String::new();
    if let Some(shebang_line) = shebang {
        result.push_str(shebang_line);
        result.push('\n');
    }
    result.push_str(&fmt.output);

    // 6. Remove trailing whitespace on each line
    let cleaned: Vec<&str> = result.lines().map(|l| l.trim_end()).collect();
    let mut final_result = cleaned.join("\n");

    // 7. Ensure exactly one trailing newline
    while final_result.ends_with('\n') {
        final_result.pop();
    }
    if !final_result.is_empty() {
        final_result.push('\n');
    }

    Ok(final_result)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(input: &str) -> String {
        format_source(input, 80).unwrap()
    }

    fn fmt_narrow(input: &str) -> String {
        format_source(input, 40).unwrap()
    }

    // 1. Simple atom formatting
    #[test]
    fn test_simple_form() {
        assert_eq!(fmt("(+ 1 2)"), "(+ 1 2)\n");
    }

    #[test]
    fn test_simple_form_extra_spaces() {
        assert_eq!(fmt("(+   1    2)"), "(+ 1 2)\n");
    }

    #[test]
    fn test_atom() {
        assert_eq!(fmt("42"), "42\n");
    }

    #[test]
    fn test_string() {
        assert_eq!(fmt(r#""hello world""#), "\"hello world\"\n");
    }

    #[test]
    fn test_keyword() {
        assert_eq!(fmt(":name"), ":name\n");
    }

    #[test]
    fn test_boolean() {
        assert_eq!(fmt("#t"), "#t\n");
        assert_eq!(fmt("#f"), "#f\n");
    }

    // 2. Line breaking
    #[test]
    fn test_line_break_long_call() {
        let input =
            "(some-very-long-function-name arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)";
        let result = fmt(input);
        // Should break since it exceeds 80 chars
        assert!(result.contains('\n'));
        // Should still have matching parens
        assert_eq!(
            result.chars().filter(|c| *c == '(').count(),
            result.chars().filter(|c| *c == ')').count()
        );
    }

    #[test]
    fn test_short_form_stays_one_line() {
        assert_eq!(fmt("(+ 1 2 3)"), "(+ 1 2 3)\n");
    }

    // 3. Comment preservation
    #[test]
    fn test_trailing_comment() {
        assert_eq!(fmt("(+ 1 2) ; add"), "(+ 1 2) ; add\n");
    }

    #[test]
    fn test_standalone_comment() {
        assert_eq!(
            fmt("; this is a comment\n(+ 1 2)"),
            "; this is a comment\n(+ 1 2)\n"
        );
    }

    // 4. Blank line preservation
    #[test]
    fn test_blank_line_between_forms() {
        let input = "(define x 1)\n\n(define y 2)";
        let result = fmt(input);
        assert_eq!(result, "(define x 1)\n\n(define y 2)\n");
    }

    #[test]
    fn test_multiple_blank_lines_collapsed() {
        let input = "(define x 1)\n\n\n\n(define y 2)";
        let result = fmt(input);
        assert_eq!(result, "(define x 1)\n\n(define y 2)\n");
    }

    // 5. Define with body indentation
    #[test]
    fn test_define_body() {
        let input = "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))";
        let result = fmt_narrow(input);
        assert!(result.starts_with("(define (factorial n)"));
        // Body should be indented 2 spaces
        let lines: Vec<&str> = result.lines().collect();
        if lines.len() > 1 {
            assert!(lines[1].starts_with("  "));
        }
    }

    #[test]
    fn test_defn_body() {
        let result = fmt_narrow("(defn greet [name] (string-append \"Hello, \" name))");
        assert!(result.starts_with("(defn greet [name]"));
    }

    #[test]
    fn test_lambda() {
        let result = fmt_narrow("(lambda (x y) (+ x y) (* x y))");
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].contains("lambda"));
        if lines.len() > 1 {
            assert!(lines[1].starts_with("  "));
        }
    }

    // 6. Let with binding indentation
    #[test]
    fn test_let_binding() {
        let result = fmt_narrow("(let ([x 1] [y 2]) (+ x y))");
        assert!(result.starts_with("(let"));
    }

    // 7. Cond with clause formatting
    #[test]
    fn test_cond_clauses() {
        let input = "(cond ((= x 1) \"one\") ((= x 2) \"two\") (else \"other\"))";
        let result = fmt_narrow(input);
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].starts_with("(cond"));
        // Clauses indented 2 from opening paren
        if lines.len() > 1 {
            assert!(lines[1].starts_with("  "));
        }
    }

    // 8. Map formatting
    #[test]
    fn test_map_short() {
        assert_eq!(fmt("{:a 1 :b 2}"), "{:a 1 :b 2}\n");
    }

    #[test]
    fn test_map_long() {
        let input = "{:name \"Alice\" :age 30 :email \"alice@example.com\" :city \"Wonderland\"}";
        let result = fmt_narrow(input);
        // Should break into multiple lines
        assert!(result.contains('\n'));
    }

    // 9. Vector formatting
    #[test]
    fn test_vector_short() {
        assert_eq!(fmt("[1 2 3]"), "[1 2 3]\n");
    }

    #[test]
    fn test_vector_long() {
        let input =
            "[very-long-element-1 very-long-element-2 very-long-element-3 very-long-element-4]";
        let result = fmt_narrow(input);
        assert!(result.contains('\n'));
    }

    // 10. Quote/quasiquote
    #[test]
    fn test_quote() {
        assert_eq!(fmt("'(a b c)"), "'(a b c)\n");
    }

    #[test]
    fn test_quasiquote_unquote() {
        assert_eq!(fmt("`(a ,b ,@c)"), "`(a ,b ,@c)\n");
    }

    // 11. F-string preservation
    #[test]
    fn test_fstring() {
        assert_eq!(fmt("f\"Hello ${name}!\""), "f\"Hello ${name}!\"\n");
    }

    // 12. Regex preservation
    #[test]
    fn test_regex() {
        let input = "#\"\\d+\"";
        let result = fmt(input);
        assert_eq!(result, "#\"\\d+\"\n");
    }

    #[test]
    fn test_regex_in_form() {
        let input = "(regex/match? #\"[a-z]+\" \"hello\")";
        let result = fmt(input);
        assert_eq!(result, "(regex/match? #\"[a-z]+\" \"hello\")\n");
    }

    // 13. Idempotency
    #[test]
    fn test_idempotency_simple() {
        let input =
            "(define (factorial n)\n  (if (<= n 1)\n    1\n    (* n (factorial (- n 1)))))\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "formatting should be idempotent");
    }

    #[test]
    fn test_idempotency_with_comments() {
        let input = "; header comment\n\n(define x 42) ; the answer\n\n(define y 7)\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(
            first, second,
            "formatting with comments should be idempotent"
        );
    }

    #[test]
    fn test_idempotency_multiline() {
        let input =
            "(some-very-long-function-name argument1 argument2 argument3 argument4 argument5)";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "multiline formatting should be idempotent");
    }

    // 14. Nested forms
    #[test]
    fn test_nested_forms() {
        assert_eq!(fmt("(+ (* 2 3) (- 4 1))"), "(+ (* 2 3) (- 4 1))\n");
    }

    #[test]
    fn test_deeply_nested() {
        let input = "(a (b (c (d (e 1)))))";
        let result = fmt(input);
        assert_eq!(result, "(a (b (c (d (e 1)))))\n");
    }

    // 15. Threading macros
    #[test]
    fn test_threading_short() {
        assert_eq!(fmt("(-> x f g)"), "(-> x f g)\n");
    }

    #[test]
    fn test_threading_long() {
        let input =
            "(-> some-value (map some-function) (filter some-predicate?) (reduce some-reducer 0))";
        let result = fmt_narrow(input);
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].starts_with("(-> some-value"));
        if lines.len() > 1 {
            assert!(lines[1].starts_with("  "));
        }
    }

    // Edge cases
    #[test]
    fn test_empty_input() {
        assert_eq!(format_source("", 80).unwrap(), "");
    }

    #[test]
    fn test_whitespace_only() {
        assert_eq!(format_source("   \n  \n  ", 80).unwrap(), "");
    }

    #[test]
    fn test_empty_list() {
        assert_eq!(fmt("()"), "()\n");
    }

    #[test]
    fn test_empty_vector() {
        assert_eq!(fmt("[]"), "[]\n");
    }

    #[test]
    fn test_empty_map() {
        assert_eq!(fmt("{}"), "{}\n");
    }

    #[test]
    fn test_shebang() {
        let input = "#!/usr/bin/env sema\n(+ 1 2)";
        let result = fmt(input);
        assert_eq!(result, "#!/usr/bin/env sema\n(+ 1 2)\n");
    }

    #[test]
    fn test_char_literal() {
        assert_eq!(fmt("#\\a"), "#\\a\n");
        assert_eq!(fmt("#\\space"), "#\\space\n");
        assert_eq!(fmt("#\\newline"), "#\\newline\n");
    }

    #[test]
    fn test_short_lambda() {
        assert_eq!(fmt("#(+ %1 1)"), "#(+ %1 1)\n");
    }

    #[test]
    fn test_dot() {
        assert_eq!(fmt("(a . b)"), "(a . b)\n");
    }

    #[test]
    fn test_no_trailing_whitespace() {
        let input = "(define x 1)   ";
        let result = fmt(input);
        for line in result.lines() {
            assert_eq!(
                line,
                line.trim_end(),
                "line should have no trailing whitespace"
            );
        }
    }

    #[test]
    fn test_trailing_newline() {
        let result = fmt("42");
        assert!(result.ends_with('\n'));
        assert!(!result.ends_with("\n\n"));
    }

    #[test]
    fn test_multiple_top_level_forms() {
        let input = "(define x 1)\n(define y 2)\n(+ x y)";
        let result = fmt(input);
        assert_eq!(result, "(define x 1)\n(define y 2)\n(+ x y)\n");
    }

    #[test]
    fn test_bytevector() {
        assert_eq!(fmt("#u8(1 2 3)"), "#u8(1 2 3)\n");
    }

    #[test]
    fn test_string_escaping_roundtrip() {
        let input = "\"hello\\nworld\\t!\"";
        let result = fmt(input);
        assert_eq!(result, "\"hello\\nworld\\t!\"\n");
    }

    #[test]
    fn test_match_form() {
        let input = "(match x (1 \"one\") (2 \"two\") (_ \"other\"))";
        let result = fmt_narrow(input);
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].starts_with("(match"));
    }

    #[test]
    fn test_do_form() {
        let result = fmt_narrow("(do (display \"hello\") (display \"world\") (newline))");
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].starts_with("(do"));
        if lines.len() > 1 {
            assert!(lines[1].starts_with("  "));
        }
    }

    #[test]
    fn test_when_form() {
        let result = fmt_narrow("(when (> x 0) (display x) (newline))");
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].starts_with("(when"));
    }

    #[test]
    fn test_if_form_short() {
        assert_eq!(fmt("(if #t 1 0)"), "(if #t 1 0)\n");
    }

    #[test]
    fn test_if_form_long() {
        let input =
            "(if (some-very-long-predicate? x y z) (do-something-complex x) (do-other-thing y))";
        let result = fmt(input);
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[0].starts_with("(if"));
    }

    #[test]
    fn test_nil() {
        assert_eq!(fmt("nil"), "nil\n");
    }

    #[test]
    fn test_negative_number() {
        assert_eq!(fmt("-42"), "-42\n");
    }

    #[test]
    fn test_float() {
        assert_eq!(fmt("3.14"), "3.14\n");
    }

    #[test]
    fn test_closing_parens_not_on_own_line() {
        let input = "(define (foo x)\n  (+ x 1))";
        let result = fmt(input);
        // No line should be just closing parens
        for line in result.lines() {
            let trimmed = line.trim();
            assert!(
                !trimmed.chars().all(|c| c == ')' || c == ']' || c == '}'),
                "closing delimiters should not be on their own line: {:?}",
                line
            );
        }
    }

    #[test]
    fn test_real_world_hello_sema() {
        let input = r#";; hello.sema — Basic Sema demo

;; Factorial
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(display "Factorial of 10: ")
(println (factorial 10))

;; Fibonacci
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

(display "Fibonacci of 10: ")
(println (fib 10))

;; Map, filter, fold
(define numbers (range 1 11))
(display "Numbers: ")
(println numbers)

(define squares (map (lambda (x) (* x x)) numbers))
(display "Squares: ")
(println squares)

(define evens (filter even? numbers))
(display "Evens: ")
(println evens)

(define sum (foldl + 0 numbers))
(display "Sum 1-10: ")
(println sum)"#;
        let result = fmt(input);
        // Should be idempotent
        let result2 = fmt(&result);
        assert_eq!(
            result, result2,
            "real-world formatting should be idempotent"
        );
        // Comments should be preserved
        assert!(result.contains(";; hello.sema"));
        assert!(result.contains(";; Factorial"));
        assert!(result.contains(";; Fibonacci"));
        // Blank lines between sections preserved
        assert!(result.contains("\n\n"));
    }

    #[test]
    fn test_idempotency_cond_multiline() {
        let input =
            "(cond\n  ((= x 0) 0)\n  ((= x 1) 1)\n  (else (+ (fib (- x 1)) (fib (- x 2)))))\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "cond formatting should be idempotent");
    }

    #[test]
    fn test_idempotency_let_multiline() {
        let input = "(let ((x 10)\n      (y 20))\n  (+ x y))\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "let formatting should be idempotent");
    }

    #[test]
    fn test_idempotency_define_function() {
        let input = "(define (factorial n)\n  (if (<= n 1) 1 (* n (factorial (- n 1)))))\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(
            first, second,
            "define function formatting should be idempotent"
        );
    }

    // Bug fix tests: inner comments preserved

    #[test]
    fn test_inner_comment_in_define() {
        let input = "(define (foo x)\n  ;; compute result\n  (+ x 1))";
        let result = format_source(input, 80).unwrap();
        assert!(
            result.contains(";; compute result"),
            "inner comment should be preserved, got: {result}"
        );
        assert!(
            result.contains("(+ x 1)"),
            "body should be preserved, got: {result}"
        );
    }

    #[test]
    fn test_inner_comment_in_let() {
        let input = "(let ((x 1))\n  ;; use x\n  (+ x 2))";
        let result = format_source(input, 80).unwrap();
        assert!(
            result.contains(";; use x"),
            "inner comment in let should be preserved, got: {result}"
        );
    }

    #[test]
    fn test_inner_comment_in_cond() {
        let input = "(cond\n  ;; first case\n  ((= x 1) \"one\")\n  ;; second case\n  ((= x 2) \"two\"))";
        let result = format_source(input, 80).unwrap();
        assert!(
            result.contains(";; first case"),
            "comment before first clause preserved, got: {result}"
        );
        assert!(
            result.contains(";; second case"),
            "comment before second clause preserved, got: {result}"
        );
    }

    #[test]
    fn test_classify_form_non_symbol_head() {
        // (42 define x) should NOT be classified as Body
        let input = "(42 define x)";
        let result = format_source(input, 80).unwrap();
        // Should be formatted as a function call, not as a body form
        assert_eq!(result.trim(), "(42 define x)");
    }

    #[test]
    fn test_inner_comment_idempotency() {
        let input = "(define (foo x)\n  ;; compute result\n  (+ x 1))";
        let first = format_source(input, 80).unwrap();
        let second = format_source(&first, 80).unwrap();
        assert_eq!(
            first, second,
            "formatting with inner comments should be idempotent"
        );
    }
}
