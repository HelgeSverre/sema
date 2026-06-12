package com.sema.intellij.notebook

import com.intellij.ide.BrowserUtil
import com.intellij.openapi.fileEditor.FileEditor
import com.intellij.openapi.fileEditor.FileEditorState
import com.intellij.openapi.fileEditor.FileEditorStateLevel
import com.intellij.openapi.options.ShowSettingsUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.ui.components.JBLabel
import com.intellij.ui.jcef.JBCefApp
import com.intellij.ui.jcef.JBCefBrowser
import com.intellij.util.ui.JBUI
import java.awt.BorderLayout
import java.beans.PropertyChangeListener
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JPanel
import com.sema.intellij.config.SemaBinary
import com.sema.intellij.config.SemaConfigurable

class SemaNotebookFileEditor(
    private val project: Project,
    private val file: VirtualFile,
) : UserDataHolderBase(), FileEditor {
    private var browser: JBCefBrowser? = null
    private val component: JComponent = createComponent()

    private fun createComponent(): JComponent {
        val status = SemaBinary.currentStatus()
        if (!status.available) {
            return JPanel(BorderLayout()).apply {
                border = JBUI.Borders.empty(16)
                add(JBLabel(status.errorText ?: SemaBinary.missingBinaryMessage()), BorderLayout.NORTH)
                add(
                    JButton("Configure").apply {
                        addActionListener {
                            ShowSettingsUtil.getInstance().showSettingsDialog(project, SemaConfigurable::class.java)
                        }
                    },
                    BorderLayout.SOUTH,
                )
            }
        }

        val session = SemaNotebookSessionService.getInstance(project).start(file)
        if (JBCefApp.isSupported()) {
            return JBCefBrowser(session.url).also { browser = it }.component
        }

        return JPanel(BorderLayout()).apply {
            border = JBUI.Borders.empty(16)
            add(JBLabel("JCEF is not available in this IDE runtime."), BorderLayout.NORTH)
            add(
                JButton("Open in Browser").apply {
                    addActionListener { BrowserUtil.browse(session.url, project) }
                },
                BorderLayout.SOUTH,
            )
        }
    }

    override fun getComponent(): JComponent = component

    override fun getPreferredFocusedComponent(): JComponent = component

    override fun getName(): String = "Notebook"

    override fun setState(state: FileEditorState) = Unit

    override fun isModified(): Boolean = false

    override fun isValid(): Boolean = file.isValid

    override fun addPropertyChangeListener(listener: PropertyChangeListener) = Unit

    override fun removePropertyChangeListener(listener: PropertyChangeListener) = Unit

    override fun getFile(): VirtualFile = file

    override fun getState(level: FileEditorStateLevel): FileEditorState =
        FileEditorState.INSTANCE

    override fun dispose() {
        browser?.dispose()
        SemaNotebookSessionService.getInstance(project).stop(file)
    }
}
