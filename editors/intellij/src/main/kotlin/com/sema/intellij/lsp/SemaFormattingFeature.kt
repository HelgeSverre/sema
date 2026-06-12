package com.sema.intellij.lsp

import com.intellij.psi.PsiFile
import com.redhat.devtools.lsp4ij.client.features.LSPFormattingFeature
import com.sema.intellij.config.SemaSettings

/**
 * Gates LSP-backed formatting (Reformat Code) behind the user setting.
 * The server advertises `documentFormattingProvider`; this lets users turn it off
 * without disabling the rest of the language server.
 */
class SemaFormattingFeature : LSPFormattingFeature() {
    override fun isEnabled(file: PsiFile): Boolean = SemaSettings.getInstance().formattingEnabled
}
