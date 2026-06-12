package com.sema.intellij

import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import org.junit.Assert.*
import org.junit.Test

class SemaLexerTest {
    private fun tokensOf(source: String): List<Pair<IElementType?, String>> {
        val lexer = SemaLexer()
        lexer.start(source)
        val result = mutableListOf<Pair<IElementType?, String>>()
        while (lexer.tokenType != null) {
            result.add(lexer.tokenType to source.substring(lexer.tokenStart, lexer.tokenEnd))
            lexer.advance()
        }
        return result
    }

    @Test
    fun emptyFile() {
        assertTrue(tokensOf("").isEmpty())
    }

    @Test
    fun whitespace() {
        val tokens = tokensOf("   \t\n  ")
        assertTrue(tokens.all { it.first == TokenType.WHITE_SPACE })
        assertEquals(1, tokens.size)
    }

    @Test
    fun delimiters() {
        val types = tokensOf("()[]{}").map { it.first }
        assertEquals(listOf(SemaTokenTypes.LPAREN, SemaTokenTypes.RPAREN,
            SemaTokenTypes.LBRACKET, SemaTokenTypes.RBRACKET,
            SemaTokenTypes.LBRACE, SemaTokenTypes.RBRACE), types)
    }

    @Test
    fun nestedDelimiters() {
        val types = tokensOf("([{}])").map { it.first }
        assertEquals(listOf(SemaTokenTypes.LPAREN, SemaTokenTypes.LBRACKET,
            SemaTokenTypes.LBRACE, SemaTokenTypes.RBRACE,
            SemaTokenTypes.RBRACKET, SemaTokenTypes.RPAREN), types)
    }

    @Test
    fun quoteFamily() {
        val types = tokensOf("' ` , ,@").filter { it.first != TokenType.WHITE_SPACE }.map { it.first }
        assertEquals(listOf(SemaTokenTypes.QUOTE, SemaTokenTypes.QUASIQUOTE, SemaTokenTypes.SPLICE), types)
    }

    @Test
    fun commaIsWhitespaceNotUnquote() {
        val symbols = tokensOf(",foo ,bar").filter { it.first == SemaTokenTypes.SYMBOL }.map { it.second }
        assertEquals(listOf("foo", "bar"), symbols)
    }

    @Test
    fun commaBeforeSymbolIsWhitespace() {
        val types = tokensOf(",x").filter { it.first != TokenType.WHITE_SPACE }.map { it.first }
        assertEquals(listOf(SemaTokenTypes.SYMBOL), types)
    }

    @Test
    fun stringWithEscapes() {
        val strings = tokensOf("\"hello \\\"world\\\"\"").filter { it.first == SemaTokenTypes.STRING }
        assertEquals(1, strings.size)
        assertEquals("\"hello \\\"world\\\"\"", strings[0].second)
    }

    @Test
    fun unterminatedString() {
        assertEquals(SemaTokenTypes.STRING, tokensOf("\"unclosed").last().first)
    }

    @Test
    fun fString() {
        val strings = tokensOf("f\"hello {name}\"").filter { it.first == SemaTokenTypes.STRING }
        assertEquals(1, strings.size)
        assertEquals("f\"hello {name}\"", strings[0].second)
    }

    @Test
    fun regexString() {
        val strings = tokensOf("#\"[a-z]+\"").filter { it.first == SemaTokenTypes.STRING }
        assertEquals(1, strings.size)
        assertEquals("#\"[a-z]+\"", strings[0].second)
    }

    @Test
    fun integers() {
        val numbers = tokensOf("0 42 -7 1000000").filter { it.first == SemaTokenTypes.NUMBER }
        assertEquals(4, numbers.size)
        assertEquals(listOf("0", "42", "-7", "1000000"), numbers.map { it.second })
    }

    @Test
    fun floats() {
        val numbers = tokensOf("3.14 -0.5 1.0").filter { it.first == SemaTokenTypes.NUMBER }
        assertEquals(3, numbers.size)
        assertEquals(listOf("3.14", "-0.5", "1.0"), numbers.map { it.second })
    }

    @Test
    fun standaloneMinusIsSymbol() {
        val types = tokensOf("(- x y)").filter { it.first != TokenType.WHITE_SPACE }.map { it.first }
        assertTrue(SemaTokenTypes.SYMBOL in types)
        assertFalse(SemaTokenTypes.NUMBER in types)
    }

    @Test
    fun plainSymbol() {
        val tokens = tokensOf("my-function")
        val sym = tokens.singleOrNull { it.first != TokenType.WHITE_SPACE }
        assertNotNull(sym)
        assertEquals(SemaTokenTypes.SYMBOL, sym!!.first)
        assertEquals("my-function", sym.second)
    }

    @Test
    fun specialForms() {
        val tokens = tokensOf("if when let lambda defun define defmacro import module")
        val specials = tokens.filter { it.first == SemaTokenTypes.SPECIAL_FORM }
        val defs = tokens.filter { it.first == SemaTokenTypes.DEFINITION_KEYWORD }
        assertEquals(6, specials.size)
        assertEquals(3, defs.size)
    }

    @Test
    fun dotToken() {
        val dot = tokensOf("(foo . bar)").find { it.first == SemaTokenTypes.DOT }
        assertNotNull(dot)
        assertEquals(".", dot!!.second)
    }

    @Test
    fun dotInSymbol() {
        val symbols = tokensOf("foo.bar").filter { it.first == SemaTokenTypes.SYMBOL }
        assertEquals(1, symbols.size)
        assertEquals("foo.bar", symbols[0].second)
    }

    @Test
    fun keywordColon() {
        val keywords = tokensOf(":my-keyword :another").filter { it.first == SemaTokenTypes.KEYWORD }
        assertEquals(2, keywords.size)
        assertEquals(listOf(":my-keyword", ":another"), keywords.map { it.second })
    }

    @Test
    fun booleansAndNil() {
        val tokens = tokensOf("#t #f true false nil")
        val booleans = tokens.filter { it.first == SemaTokenTypes.BOOLEAN }
        val nils = tokens.filter { it.first == SemaTokenTypes.NIL }
        assertEquals(4, booleans.size)
        assertEquals(1, nils.size)
    }

    @Test
    fun characterLiterals() {
        for (src in listOf("#\\a", "#\\space", "#\\newline", "#\\tab")) {
            assertEquals(SemaTokenTypes.CHARACTER, tokensOf(src)[0].first)
        }
    }

    @Test
    fun hashDispatch() {
        val dispatches = tokensOf("#(1 2 3) #u8(1 2 3)").filter { it.first == SemaTokenTypes.HASH_DISPATCH }
        assertEquals(2, dispatches.size)
        assertEquals("#(", dispatches[0].second)
        assertEquals("#u8(", dispatches[1].second)
    }

    @Test
    fun nestedBlockComments() {
        val comments = tokensOf("#|outer #|inner|# text|#").filter { it.first == SemaTokenTypes.BLOCK_COMMENT }
        assertEquals(1, comments.size)
        assertTrue(comments[0].second.contains("inner"))
    }

    @Test
    fun unterminatedBlockComment() {
        assertEquals(SemaTokenTypes.BLOCK_COMMENT, tokensOf("#|outer #|inner|#").last().first)
    }

    @Test
    fun lineComment() {
        val comments = tokensOf("; comment\nafter").filter { it.first == SemaTokenTypes.LINE_COMMENT }
        assertEquals(1, comments.size)
        assertEquals("; comment", comments[0].second)
    }

    @Test
    fun badCharacter() {
        assertEquals(TokenType.BAD_CHARACTER, tokensOf("@")[0].first)
    }

    @Test
    fun veryLongSymbol() {
        val longName = "x".repeat(1000)
        val tokens = tokensOf(longName)
        assertEquals(SemaTokenTypes.SYMBOL, tokens[0].first)
        assertEquals(longName, tokens[0].second)
    }

    @Test
    fun manyStrings() {
        val source = (1..100).joinToString(" ") { "\"str$it\"" }
        val strings = tokensOf(source).filter { it.first == SemaTokenTypes.STRING }
        assertEquals(100, strings.size)
    }

    @Test
    fun symbolChars() {
        val symbols = tokensOf("+ - * / < > = _! ? & % ^ ~").filter { it.first == SemaTokenTypes.SYMBOL }
        assertEquals(13, symbols.size)
    }
}
