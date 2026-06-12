package com.sema.intellij.lsp

import com.intellij.psi.PsiFile
import com.redhat.devtools.lsp4ij.client.features.LSPCompletionFeature

/**
 * Customizes how Sema completion items are rendered/sorted in the IntelliJ lookup.
 *
 * The server supplies `detail` (the function signature) inline and full documentation lazily via
 * `completionItem/resolve`, which LSP4IJ's default rendering already surfaces. On top of that we
 * enable client-side context-aware sorting so the most relevant candidates rank first.
 */
class SemaCompletionFeature : LSPCompletionFeature() {
    override fun useContextAwareSorting(file: PsiFile): Boolean = true
}
