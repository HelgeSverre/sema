// External scanner for tree-sitter-sema.
// Handles nestable block comments: #| ... |#

#include "tree_sitter/parser.h"

enum TokenType {
    BLOCK_COMMENT,
};

void *tree_sitter_sema_external_scanner_create(void) {
    return NULL;
}

void tree_sitter_sema_external_scanner_destroy(void *payload) {
    (void)payload;
}

unsigned tree_sitter_sema_external_scanner_serialize(void *payload, char *buffer) {
    (void)payload;
    (void)buffer;
    return 0;
}

void tree_sitter_sema_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {
    (void)payload;
    (void)buffer;
    (void)length;
}

static void advance(TSLexer *lexer) {
    lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
    lexer->advance(lexer, true);
}

bool tree_sitter_sema_external_scanner_scan(void *payload,
                                            TSLexer *lexer,
                                            const bool *valid_symbols) {
    (void)payload;

    if (!valid_symbols[BLOCK_COMMENT]) {
        return false;
    }

    // Skip leading whitespace so tree-sitter can find the comment start.
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
           lexer->lookahead == '\r' || lexer->lookahead == '\n') {
        skip(lexer);
    }

    // Look for #|
    if (lexer->lookahead != '#') {
        return false;
    }
    advance(lexer);

    if (lexer->lookahead != '|') {
        return false;
    }
    advance(lexer);

    // We are inside the block comment. Track nesting depth.
    int depth = 1;

    while (depth > 0) {
        if (lexer->eof(lexer)) {
            return false;
        }

        if (lexer->lookahead == '#') {
            advance(lexer);
            if (lexer->lookahead == '|') {
                advance(lexer);
                depth++;
            }
        } else if (lexer->lookahead == '|') {
            advance(lexer);
            if (lexer->lookahead == '#') {
                advance(lexer);
                depth--;
            }
        } else {
            advance(lexer);
        }
    }

    lexer->result_symbol = BLOCK_COMMENT;
    return true;
}
