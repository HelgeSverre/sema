package com.sema.intellij

import com.intellij.psi.PsiFileFactory
import com.intellij.psi.tree.IElementType
import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaPsiTest : LightPlatformTestCase() {

    private fun parseSema(text: String) =
        PsiFileFactory.getInstance(project).createFileFromText("test.sema", SemaLanguage, text)

    private fun countTokensOfType(psiFile: com.intellij.psi.PsiFile, type: IElementType): Int {
        var count = 0
        walkTree(psiFile.node) { elementType -> if (elementType == type) count++ }
        return count
    }

    private fun walkTree(node: com.intellij.lang.ASTNode, visitor: (IElementType) -> Unit) {
        visitor(node.elementType)
        var child = node.firstChildNode
        while (child != null) {
            walkTree(child, visitor)
            child = child.treeNext
        }
    }

    @Test
    fun testParsesValidFile() {
        val psiFile = parseSema(TestHelpers.sampleSemaContent())
        assertNotNull(psiFile)
        assertEquals(SemaLanguage, psiFile.language)
        assertNotNull(psiFile.firstChild)
        assertEquals(0, countTokensOfType(psiFile, com.intellij.psi.TokenType.BAD_CHARACTER))
    }

    @Test
    fun testParsesEmptyFile() {
        assertNotNull(parseSema(""))
    }

    @Test
    fun testParsesCommentOnly() {
        val psiFile = parseSema(";; comment\n#|block|#")
        assertNotNull(psiFile)
        assertEquals(0, countTokensOfType(psiFile, com.intellij.psi.TokenType.BAD_CHARACTER))
    }

    @Test
    fun testParsesDeepNesting() {
        val depth = 100
        assertNotNull(parseSema("(".repeat(depth) + "x" + ")".repeat(depth)))
    }

    @Test
    fun testTokenCount() {
        val psiFile = parseSema("(define x 42)")
        var count = 0
        walkTree(psiFile.node) { elementType ->
            if (elementType !is com.intellij.psi.TokenType) count++
        }
        assertTrue(count >= 5)
    }
}
