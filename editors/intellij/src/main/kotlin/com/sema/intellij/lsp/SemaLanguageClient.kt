package com.sema.intellij.lsp

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.client.IndexAwareLanguageClient
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

// Extends IndexAwareLanguageClient (a subclass of LanguageClientImpl) per LSP4IJ guidance for
// clients that handle custom LSP messages — keeps indexing from interfering with the custom
// `sema/evalResult` notification below.
class SemaLanguageClient(project: Project) : IndexAwareLanguageClient(project) {

    @JsonNotification("sema/evalResult")
    fun evalResult(params: EvalResultParams) {
        ApplicationManager.getApplication().invokeLater {
            if (project.isDisposed) return@invokeLater
            val service = EvalResultService.getInstance(project)
            service.handleResult(params)
        }
    }
}
