package com.sema.intellij.lsp

import com.intellij.openapi.project.Project
import com.redhat.devtools.lsp4ij.server.OSProcessStreamConnectionProvider
import com.sema.intellij.config.SemaCommandLine

class SemaLanguageServer(project: Project) : OSProcessStreamConnectionProvider() {
    init {
        commandLine = SemaCommandLine.lsp(workingDirectory = project.basePath)
    }
}
