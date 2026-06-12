package com.sema.intellij.lsp

import com.intellij.psi.PsiFileFactory
import com.intellij.testFramework.LightPlatformTestCase
import com.redhat.devtools.lsp4ij.features.semanticTokens.SemanticTokensHighlightingColors
import com.sema.intellij.SemaLanguage
import org.junit.Assert.*
import org.junit.Test

class SemaSemanticTokensColorsProviderTest : LightPlatformTestCase() {
    private val provider = SemaSemanticTokensColorsProvider()

    private fun dummyFile() = PsiFileFactory.getInstance(project)
        .createFileFromText("test.sema", SemaLanguage, "x")

    @Test
    fun testKnownTokenTypesMapToColors() {
        val file = dummyFile()
        for (type in listOf("keyword", "function", "variable", "parameter", "macro")) {
            val key = provider.getTextAttributesKey(type, mutableListOf(), file)
            assertNotNull(key)
        }
    }

    @Test
    fun testDefaultLibraryModifier() {
        val key = provider.getTextAttributesKey("function", mutableListOf("defaultLibrary"), dummyFile())
        assertNotNull(key)
    }

    @Test
    fun testDefinitionModifierOnFunctionUsesDeclarationColor() {
        val file = dummyFile()
        val plain = provider.getTextAttributesKey("function", mutableListOf(), file)
        val definition = provider.getTextAttributesKey("function", mutableListOf("definition"), file)
        assertEquals(SemanticTokensHighlightingColors.FUNCTION, plain)
        assertEquals(SemanticTokensHighlightingColors.FUNCTION_DECLARATION, definition)
        assertNotEquals(plain, definition)
    }

    @Test
    fun testUnknownTokenReturnsNull() {
        assertNull(provider.getTextAttributesKey("nonexistent_type_xyz", mutableListOf(), dummyFile()))
    }
}
