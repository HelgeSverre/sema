package com.sema.intellij

import org.junit.Assert.*
import org.junit.Test

class SemaLexerStressTest {
    private fun lexAll(source: String): Int {
        val lexer = SemaLexer()
        lexer.start(source)
        var count = 0
        while (lexer.tokenType != null) {
            count++
            lexer.advance()
        }
        return count
    }

    @Test
    fun test10kForms() {
        val source = (1..10_000).joinToString("\n") { "(define x$it $it)" }
        val start = System.currentTimeMillis()
        val count = lexAll(source)
        val elapsed = System.currentTimeMillis() - start
        assertTrue(count > 30_000)
        assertTrue(elapsed < 500)
    }

    @Test
    fun testDeepNesting() {
        val depth = 5000
        val source = "(".repeat(depth) + "x" + ")".repeat(depth)
        val start = System.currentTimeMillis()
        val count = lexAll(source)
        val elapsed = System.currentTimeMillis() - start
        assertEquals(depth * 2 + 1, count)
        assertTrue(elapsed < 200)
    }

    @Test
    fun test100kCharsMixed() {
        val forms = listOf("(define x 42)", "(defun f (x) (* x x))",
            "\"a string\"", "; comment\n", "(let ((a 1) (b 2)) (+ a b))")
        val sb = StringBuilder()
        repeat(10_000) { sb.append(forms[it % forms.size]).append("\n") }
        val source = sb.toString()
        assertTrue(source.length > 100_000)
        val start = System.currentTimeMillis()
        val count = lexAll(source)
        val elapsed = System.currentTimeMillis() - start
        assertTrue(count > 50_000)
        assertTrue(elapsed < 1000)
    }

    @Test
    fun testRepeatedLexing() {
        val source = TestHelpers.sampleSemaContent().repeat(100)
        repeat(5) { lexAll(source) }
        val start = System.currentTimeMillis()
        repeat(10) { lexAll(source) }
        val elapsed = System.currentTimeMillis() - start
        assertTrue(elapsed < 500)
    }
}
