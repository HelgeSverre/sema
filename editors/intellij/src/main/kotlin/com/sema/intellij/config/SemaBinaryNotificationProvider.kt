package com.sema.intellij.config

import com.intellij.openapi.fileEditor.FileEditor
import com.intellij.openapi.options.ShowSettingsUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.ui.EditorNotificationPanel
import com.intellij.ui.EditorNotificationProvider
import com.sema.intellij.SemacFileType
import com.sema.intellij.SemaFileType
import com.sema.intellij.notebook.SemaNotebookFileType
import java.util.function.Function
import javax.swing.JComponent

class SemaBinaryNotificationProvider : EditorNotificationProvider {
    private val noNotification = Function<FileEditor, JComponent?> { null }

    override fun collectNotificationData(
        project: Project,
        file: VirtualFile,
    ): Function<in FileEditor, out JComponent?> {
        if (file.fileType !in setOf(SemaFileType, SemacFileType, SemaNotebookFileType)) return noNotification

        val status = SemaBinary.currentStatus()
        if (status.available) return noNotification

        return Function {
            EditorNotificationPanel().apply {
                text(status.errorText ?: SemaBinary.missingBinaryMessage())
                createActionLabel("Configure") {
                    ShowSettingsUtil.getInstance().showSettingsDialog(project, SemaConfigurable::class.java)
                }
            }
        }
    }
}
