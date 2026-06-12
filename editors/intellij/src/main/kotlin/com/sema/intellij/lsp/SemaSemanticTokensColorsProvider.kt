package com.sema.intellij.lsp

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.PsiFile
import com.redhat.devtools.lsp4ij.features.semanticTokens.SemanticTokensColorsProvider
import com.redhat.devtools.lsp4ij.features.semanticTokens.SemanticTokensHighlightingColors
import com.sema.intellij.SemaColors

class SemaSemanticTokensColorsProvider : SemanticTokensColorsProvider {
    override fun getTextAttributesKey(
        tokenType: String,
        tokenModifiers: MutableList<String>,
        file: PsiFile,
    ): TextAttributesKey? {
        val defaultLibrary = tokenModifiers.contains("defaultLibrary")
        val definition = tokenModifiers.contains("definition")
        return when (tokenType) {
            "keyword" -> SemaColors.SPECIAL_FORM
            "function" -> when {
                defaultLibrary -> SemanticTokensHighlightingColors.DEFAULT_LIBRARY_FUNCTION
                definition -> SemanticTokensHighlightingColors.FUNCTION_DECLARATION
                else -> SemanticTokensHighlightingColors.FUNCTION
            }
            "variable" -> if (definition) SemaColors.DEFINITION_KEYWORD else SemaColors.SYMBOL
            "parameter" -> SemanticTokensHighlightingColors.PARAMETER
            "macro" -> SemanticTokensHighlightingColors.MACRO
            else -> null
        }
    }
}
