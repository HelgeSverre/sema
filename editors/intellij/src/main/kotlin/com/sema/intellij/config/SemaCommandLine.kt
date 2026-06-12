package com.sema.intellij.config

import com.intellij.execution.configurations.GeneralCommandLine
import java.io.File

object SemaCommandLine {
    fun lsp(
        semaPath: String = SemaSettings.getInstance().semaPath,
        workingDirectory: String? = null,
    ): GeneralCommandLine =
        command(semaPath, workingDirectory, "lsp")

    fun dap(
        semaPath: String = SemaSettings.getInstance().semaPath,
        workingDirectory: String? = null,
    ): GeneralCommandLine =
        command(semaPath, workingDirectory, "dap")

    fun notebookServe(
        semaPath: String = SemaSettings.getInstance().semaPath,
        notebookPath: String,
        workingDirectory: String? = null,
        port: Int,
    ): GeneralCommandLine =
        command(semaPath, workingDirectory, "notebook", "serve", notebookPath, "--host", "127.0.0.1", "--port", port.toString())

    fun notebookNew(
        semaPath: String = SemaSettings.getInstance().semaPath,
        notebookPath: String,
        workingDirectory: String? = null,
    ): GeneralCommandLine =
        command(semaPath, workingDirectory, "notebook", "new", notebookPath)

    fun notebookRun(
        semaPath: String = SemaSettings.getInstance().semaPath,
        notebookPath: String,
        workingDirectory: String? = null,
    ): GeneralCommandLine =
        command(semaPath, workingDirectory, "notebook", "run", notebookPath)

    fun notebookExport(
        semaPath: String = SemaSettings.getInstance().semaPath,
        notebookPath: String,
        workingDirectory: String? = null,
        outputPath: String? = null,
    ): GeneralCommandLine {
        val args = mutableListOf("notebook", "export", notebookPath)
        if (!outputPath.isNullOrBlank()) {
            args += "--output"
            args += outputPath
        }
        return command(semaPath, workingDirectory, *args.toTypedArray())
    }

    fun runFile(
        semaPath: String = SemaSettings.getInstance().semaPath,
        scriptPath: String,
        arguments: List<String>,
        workingDirectory: String? = null,
    ): GeneralCommandLine =
        command(semaPath, workingDirectory, scriptPath, *arguments.toTypedArray())

    private fun command(semaPath: String, workingDirectory: String?, vararg parameters: String): GeneralCommandLine =
        GeneralCommandLine(semaPath.ifBlank { SemaSettings.DEFAULT_SEMA_PATH }, *parameters).apply {
            charset = Charsets.UTF_8
            if (!workingDirectory.isNullOrBlank()) {
                workDirectory = File(workingDirectory)
            }
        }
}
