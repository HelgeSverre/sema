package com.sema.intellij

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.tree.IElementType
import org.junit.Assert.*
import org.junit.Test

class SemaSyntaxHighlighterTest {
    private val highlighter = SemaSyntaxHighlighter()

    private fun highlightFor(tokenType: IElementType?): Array<TextAttributesKey> =
        highlighter.getTokenHighlights(tokenType)

    @Test
    fun tokenTypesMapToColors() {
        val pairs = listOf(
            SemaTokenTypes.LINE_COMMENT to SemaColors.LINE_COMMENT,
            SemaTokenTypes.BLOCK_COMMENT to SemaColors.BLOCK_COMMENT,
            SemaTokenTypes.STRING to SemaColors.STRING,
            SemaTokenTypes.NUMBER to SemaColors.NUMBER,
            SemaTokenTypes.KEYWORD to SemaColors.KEYWORD,
            SemaTokenTypes.SYMBOL to SemaColors.SYMBOL,
            SemaTokenTypes.BOOLEAN to SemaColors.BOOLEAN,
            SemaTokenTypes.NIL to SemaColors.NIL,
            SemaTokenTypes.CHARACTER to SemaColors.CHARACTER,
            SemaTokenTypes.SPECIAL_FORM to SemaColors.SPECIAL_FORM,
            SemaTokenTypes.DEFINITION_KEYWORD to SemaColors.DEFINITION_KEYWORD,
        )
        for ((token, expected) in pairs) {
            val highlights = highlightFor(token)
            assertTrue(highlights.isNotEmpty())
            assertEquals(expected, highlights[0])
        }
    }

    @Test
    fun delimiterColors() {
        assertEquals(SemaColors.PARENS, highlightFor(SemaTokenTypes.LPAREN)[0])
        assertEquals(SemaColors.BRACKETS, highlightFor(SemaTokenTypes.LBRACKET)[0])
        assertEquals(SemaColors.BRACES, highlightFor(SemaTokenTypes.LBRACE)[0])
    }

    @Test
    fun unknownTokenReturnsEmpty() {
        assertTrue(highlightFor(null).isEmpty())
    }

    @Test
    fun colorKeysHaveExternalNames() {
        val colors = listOf(SemaColors.LINE_COMMENT, SemaColors.BLOCK_COMMENT,
            SemaColors.STRING, SemaColors.NUMBER, SemaColors.KEYWORD,
            SemaColors.SYMBOL, SemaColors.BOOLEAN, SemaColors.NIL,
            SemaColors.CHARACTER, SemaColors.PARENS, SemaColors.BRACKETS,
            SemaColors.BRACES, SemaColors.SPECIAL_FORM, SemaColors.DEFINITION_KEYWORD)
        for (color in colors) assertNotNull(color.externalName)
    }
}
