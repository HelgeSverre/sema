package com.sema.intellij

import com.intellij.lang.LanguageBraceMatching
import com.intellij.lang.LanguageCommenters
import com.intellij.psi.PsiFileFactory
import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaPluginSmokeTest : LightPlatformTestCase() {

    @Test
    fun testSyntaxHighlighterFactory() {
        val factory = SemaSyntaxHighlighterFactory()
        val hl = factory.getSyntaxHighlighter(null, null)
        assertNotNull(hl)
    }

    @Test
    fun testBraceMatcher() {
        val matcher = LanguageBraceMatching.INSTANCE.forLanguage(SemaLanguage)
        assertNotNull(matcher)
        assertTrue(matcher is SemaBraceMatcher)
    }

    @Test
    fun testCommenter() {
        val commenter = LanguageCommenters.INSTANCE.forLanguage(SemaLanguage)
        assertNotNull(commenter)
        assertEquals(";", commenter.lineCommentPrefix)
    }

    @Test
    fun testAssociatedFileType() {
        assertEquals(SemaFileType, SemaLanguage.associatedFileType)
    }

    @Test
    fun testRepeatedParseDoesNotThrow() {
        val source = TestHelpers.sampleSemaContent().repeat(500)
        repeat(20) {
            val psiFile = PsiFileFactory.getInstance(project)
                .createFileFromText("test-${it}.sema", SemaLanguage, source)
            assertNotNull(psiFile)
        }
    }

    @Test
    fun testRapidDocumentChanges() {
        val lexer = SemaLexer()
        for (i in 1..5000) {
            lexer.start("(define x $i)")
            while (lexer.tokenType != null) { lexer.advance() }
        }
    }
}
