/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// Tree-sitter grammar for Sema — a Lisp dialect.
// Token rules derived from crates/sema-reader/src/lexer.rs.

// Symbol character classes matching is_symbol_start / is_symbol_char in lexer.rs:
//   start: alphabetic | + - * / ! ? < > = _ & % ^ ~ .
//   rest:  start | ascii digit | - / .
// Note: `-` and `.` appear in both sets; digits only in rest.
const SYMBOL_START = /[a-zA-Z\u00C0-\u024F+\-*\/!?<>=_&%^~.]/;
const SYMBOL_CHAR = /[a-zA-Z\u00C0-\u024F+\-*\/!?<>=_&%^~.#0-9]/;

module.exports = grammar({
  name: 'sema',

  externals: $ => [
    $.block_comment,
  ],

  extras: $ => [
    /\s/,
    $.comment,
    $.block_comment,
  ],

  word: $ => $.symbol,

  rules: {
    source_file: $ => repeat($._form),

    _form: $ => choice(
      $.list,
      $.vector,
      $.hash_map,
      $.quote,
      $.quasiquote,
      $.unquote,
      $.unquote_splicing,
      $.byte_vector,
      $._atom,
    ),

    // ── Compound forms ──────────────────────────────────────────────

    list: $ => choice(
      seq('(', repeat($._form), ')'),
      // Dotted pair: (a b . c)
      seq('(', repeat1($._form), '.', $._form, ')'),
    ),

    vector: $ => seq('[', repeat($._form), ']'),

    hash_map: $ => seq('{', repeat($._form), '}'),

    // ── Quote / unquote ─────────────────────────────────────────────

    quote: $ => seq("'", $._form),

    quasiquote: $ => seq('`', $._form),

    unquote: $ => seq(',', $._form),

    unquote_splicing: $ => seq(',@', $._form),

    // ── Byte vector: #u8( num* ) ────────────────────────────────────

    byte_vector: $ => seq('#u8(', repeat($.number), ')'),

    // ── Atoms ───────────────────────────────────────────────────────

    _atom: $ => choice(
      $.number,
      $.string,
      $.symbol,
      $.keyword,
      $.boolean,
      $.character,
    ),

    // Numbers: integer or float, optional leading minus.
    // Negative numbers only when `-` is immediately followed by a digit
    // (otherwise `-` is a symbol).
    // Float requires digit(s) on both sides of the dot: 3.14, -0.5
    number: _$ => token(
      seq(
        optional('-'),
        /[0-9]+/,
        optional(seq('.', /[0-9]+/)),
      ),
    ),

    // Strings with escape sequences ──────────────────────────────────

    string: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        /[^"\\]+/,
      )),
      '"',
    ),

    escape_sequence: _$ => token.immediate(seq(
      '\\',
      choice(
        /[ntr\\"0]/,          // \n \t \r \\ \" \0
        /x[0-9a-fA-F]+;/,    // \xHH;  (R7RS hex escape)
        /u[0-9a-fA-F]{4}/,   // \uHHHH
        /U[0-9a-fA-F]{8}/,   // \UHHHHHHHH
      ),
    )),

    // Symbols ────────────────────────────────────────────────────────
    // Must not match "true", "false", or a bare "." (those are
    // handled by `boolean` and the dot in `list`).

    symbol: _$ => token(
      seq(SYMBOL_START, repeat(SYMBOL_CHAR)),
    ),

    // Keywords: colon followed by symbol chars ───────────────────────

    keyword: _$ => token(
      seq(':', SYMBOL_CHAR, repeat(SYMBOL_CHAR)),
    ),

    // Booleans ───────────────────────────────────────────────────────

    boolean: _$ => token(prec(1, choice('#t', '#f', 'true', 'false'))),

    // Character literals ─────────────────────────────────────────────
    // #\a  #\space  #\newline  #\tab  #\return  #\nul
    // Single non-alpha char: #\(  #\)  etc.

    character: _$ => token(
      seq(
        '#\\',
        choice(
          'space',
          'newline',
          'tab',
          'return',
          'nul',
          /./,   // any single character
        ),
      ),
    ),

    // Line comment ───────────────────────────────────────────────────

    comment: _$ => token(seq(';', /[^\n]*/)),
  },
});
