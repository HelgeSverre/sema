package com.sema.intellij.config

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.ui.components.JBCheckBox
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.FormBuilder
import javax.swing.JComponent

class SemaConfigurable : Configurable {
    private val settings = SemaSettings.getInstance()
    private val semaPathField = TextFieldWithBrowseButton().apply {
        addBrowseFolderListener(null, FileChooserDescriptorFactory.createSingleFileOrExecutableAppDescriptor())
    }
    private val statusLabel = JBLabel()
    private val keepServerAliveCheckbox = JBCheckBox("Keep language server running when no Sema files are open")
    private val formattingEnabledCheckbox = JBCheckBox("Enable code formatting (Reformat Code)")
    private var panel: JComponent? = null

    override fun getDisplayName(): String = "Sema"

    override fun createComponent(): JComponent {
        if (panel == null) {
            panel = FormBuilder.createFormBuilder()
                .addLabeledComponent("Sema binary:", semaPathField)
                .addComponent(statusLabel)
                .addComponent(keepServerAliveCheckbox)
                .addComponent(formattingEnabledCheckbox)
                .addComponentFillVertically(JBLabel(), 0)
                .panel
        }
        reset()
        return panel!!
    }

    override fun isModified(): Boolean =
        semaPathField.text != settings.semaPath ||
            keepServerAliveCheckbox.isSelected != settings.keepLspServerAlive ||
            formattingEnabledCheckbox.isSelected != settings.formattingEnabled

    override fun apply() {
        settings.semaPath = semaPathField.text
        settings.keepLspServerAlive = keepServerAliveCheckbox.isSelected
        settings.formattingEnabled = formattingEnabledCheckbox.isSelected
        updateStatus()
    }

    override fun reset() {
        semaPathField.text = settings.semaPath
        keepServerAliveCheckbox.isSelected = settings.keepLspServerAlive
        formattingEnabledCheckbox.isSelected = settings.formattingEnabled
        updateStatus()
    }

    private fun updateStatus() {
        val status = SemaBinary.check(semaPathField.text)
        statusLabel.text = if (status.available) {
            "Using ${status.resolvedPath}"
        } else {
            status.errorText ?: SemaBinary.missingBinaryMessage(semaPathField.text)
        }
    }
}
