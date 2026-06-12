package com.sema.intellij.lsp

import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.LanguageServerFactory
import com.redhat.devtools.lsp4ij.client.LanguageClientImpl
import com.redhat.devtools.lsp4ij.client.features.LSPClientFeatures
import com.redhat.devtools.lsp4ij.server.StreamConnectionProvider

class SemaLanguageServerFactory : LanguageServerFactory {
    override fun createConnectionProvider(project: Project): StreamConnectionProvider {
        return SemaLanguageServer(project)
    }

    override fun createLanguageClient(project: Project): LanguageClientImpl {
        return SemaLanguageClient(project)
    }

    override fun createClientFeatures(): LSPClientFeatures =
        SemaClientFeatures()
            .setCompletionFeature(SemaCompletionFeature())
            .setFormattingFeature(SemaFormattingFeature())
}
