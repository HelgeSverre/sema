package com.sema.intellij.lsp

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent

class ClearEvalResultsAction : AnAction("Clear Sema Results") {
    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        EvalResultService.getInstance(project).clearAllResults()
    }

    override fun update(e: AnActionEvent) {
        e.presentation.isEnabledAndVisible = e.project != null
    }
}
