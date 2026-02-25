package com.sema.intellij.lsp

import com.intellij.execution.configurations.GeneralCommandLine
import com.redhat.devtools.lsp4ij.server.OSProcessStreamConnectionProvider

class SemaLanguageServer : OSProcessStreamConnectionProvider() {
    init {
        val semaPath = System.getenv("SEMA_PATH") ?: "sema"
        commandLine = GeneralCommandLine(semaPath, "lsp")
    }
}
