package com.sema.intellij.notebook.actions

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessTerminatedListener
import com.intellij.ide.BrowserUtil
import com.intellij.openapi.actionSystem.ActionUpdateThread
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.progress.Task
import com.intellij.openapi.project.DumbAwareAction
import com.intellij.openapi.ui.Messages
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.openapi.vfs.VirtualFile
import com.sema.intellij.config.SemaCommandLine
import com.sema.intellij.notebook.SemaNotebookFileType
import com.sema.intellij.notebook.SemaNotebookSessionService
import java.nio.file.Path
import kotlin.io.path.nameWithoutExtension

class NewSemaNotebookAction : DumbAwareAction() {
    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        val basePath = project.basePath ?: return
        val fileName = Messages.showInputDialog(
            project,
            "Notebook file name:",
            "New Sema Notebook",
            Messages.getQuestionIcon(),
            "notebook.sema-nb",
            null,
        ) ?: return

        val safeFileName = Path.of(fileName).fileName?.toString()?.takeIf { it.isNotBlank() } ?: return
        val normalizedName = if (safeFileName.endsWith(".sema-nb")) safeFileName else "$safeFileName.sema-nb"
        val path = Path.of(basePath, normalizedName)

        object : Task.Backgroundable(project, "Creating Sema Notebook", false) {
            override fun run(indicator: ProgressIndicator) {
                runNotebookCommandAndWait(
                    SemaCommandLine.notebookNew(notebookPath = path.toString(), workingDirectory = basePath),
                )
            }

            override fun onSuccess() {
                val file = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(path) ?: return
                FileEditorManager.getInstance(project).openFile(file, true)
            }
        }.queue()
    }

    override fun update(e: AnActionEvent) {
        e.presentation.isEnabledAndVisible = e.project?.basePath != null
    }

    override fun getActionUpdateThread(): ActionUpdateThread = ActionUpdateThread.BGT
}

class OpenSemaNotebookAction : DumbAwareAction() {
    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        val file = notebookFile(e) ?: return
        val session = SemaNotebookSessionService.getInstance(project).start(file)
        BrowserUtil.browse(session.url, project)
    }

    override fun update(e: AnActionEvent) {
        e.presentation.isEnabledAndVisible = e.project != null && notebookFile(e) != null
    }

    override fun getActionUpdateThread(): ActionUpdateThread = ActionUpdateThread.BGT
}

class RunAllSemaNotebookCellsAction : DumbAwareAction() {
    override fun actionPerformed(e: AnActionEvent) {
        e.project ?: return
        val file = notebookFile(e) ?: return
        runNotebookCommand(SemaCommandLine.notebookRun(notebookPath = file.path, workingDirectory = file.parent?.path))
    }

    override fun update(e: AnActionEvent) {
        e.presentation.isEnabledAndVisible = e.project != null && notebookFile(e) != null
    }

    override fun getActionUpdateThread(): ActionUpdateThread = ActionUpdateThread.BGT
}

class ExportSemaNotebookToMarkdownAction : DumbAwareAction() {
    override fun actionPerformed(e: AnActionEvent) {
        e.project ?: return
        val file = notebookFile(e) ?: return
        val outputPath = "${file.parent.path}/${Path.of(file.name).nameWithoutExtension}.md"
        runNotebookCommand(
            SemaCommandLine.notebookExport(
                notebookPath = file.path,
                workingDirectory = file.parent?.path,
                outputPath = outputPath,
            ),
        )
    }

    override fun update(e: AnActionEvent) {
        e.presentation.isEnabledAndVisible = e.project != null && notebookFile(e) != null
    }

    override fun getActionUpdateThread(): ActionUpdateThread = ActionUpdateThread.BGT
}

private fun notebookFile(e: AnActionEvent): VirtualFile? =
    e.getData(CommonDataKeys.VIRTUAL_FILE)?.takeIf { it.fileType == SemaNotebookFileType }

private fun runNotebookCommand(commandLine: GeneralCommandLine) {
    val handler = OSProcessHandler(commandLine)
    ProcessTerminatedListener.attach(handler)
    handler.startNotify()
}

private fun runNotebookCommandAndWait(commandLine: GeneralCommandLine) {
    commandLine.createProcess().waitFor()
}
