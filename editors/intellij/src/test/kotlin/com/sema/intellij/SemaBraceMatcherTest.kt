package com.sema.intellij

import org.junit.Assert.*
import org.junit.Test

class SemaBraceMatcherTest {
    private val matcher = SemaBraceMatcher()

    @Test
    fun threePairs() = assertEquals(3, matcher.pairs.size)

    @Test
    fun parenPair() {
        val pair = matcher.pairs.find { it.leftBraceType == SemaTokenTypes.LPAREN }
        assertNotNull(pair)
        assertEquals(SemaTokenTypes.RPAREN, pair!!.rightBraceType)
        assertFalse(pair.isStructural)
    }

    @Test
    fun bracketPair() {
        val pair = matcher.pairs.find { it.leftBraceType == SemaTokenTypes.LBRACKET }
        assertNotNull(pair)
        assertEquals(SemaTokenTypes.RBRACKET, pair!!.rightBraceType)
    }

    @Test
    fun bracePair() {
        val pair = matcher.pairs.find { it.leftBraceType == SemaTokenTypes.LBRACE }
        assertNotNull(pair)
        assertEquals(SemaTokenTypes.RBRACE, pair!!.rightBraceType)
    }

    @Test
    fun allowsBraceBeforeOtherTokens() {
        assertTrue(matcher.isPairedBracesAllowedBeforeType(SemaTokenTypes.LPAREN, SemaTokenTypes.SYMBOL))
        assertTrue(matcher.isPairedBracesAllowedBeforeType(SemaTokenTypes.LBRACKET, SemaTokenTypes.RPAREN))
    }
}
