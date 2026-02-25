package com.sema.intellij.lsp

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.client.LanguageClientImpl
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

class SemaLanguageClient(project: Project) : LanguageClientImpl(project) {

    @JsonNotification("sema/evalResult")
    fun evalResult(params: EvalResultParams) {
        ApplicationManager.getApplication().invokeLater {
            if (project.isDisposed) return@invokeLater
            val service = EvalResultService.getInstance(project)
            service.handleResult(params)
        }
    }
}
