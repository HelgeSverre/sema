package com.sema.intellij.run

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.util.ui.FormBuilder
import javax.swing.JComponent
import javax.swing.JTextField

class SemaRunConfigurationEditor(private val project: Project) : SettingsEditor<SemaRunConfiguration>() {

    private val scriptPathField = TextFieldWithBrowseButton().apply {
        addBrowseFolderListener(
            "Select Sema Script",
            "Choose the .sema file to run",
            project,
            FileChooserDescriptorFactory.createSingleFileDescriptor("sema")
        )
    }
    private val argumentsField = JTextField()
    private val workingDirectoryField = TextFieldWithBrowseButton().apply {
        addBrowseFolderListener(
            "Select Working Directory",
            "Choose working directory for the script",
            project,
            FileChooserDescriptorFactory.createSingleFolderDescriptor()
        )
    }

    override fun createEditor(): JComponent {
        return FormBuilder.createFormBuilder()
            .addLabeledComponent("Script path:", scriptPathField)
            .addLabeledComponent("Arguments:", argumentsField)
            .addLabeledComponent("Working directory:", workingDirectoryField)
            .panel
    }

    override fun applyEditorTo(config: SemaRunConfiguration) {
        config.scriptPath = scriptPathField.text
        config.arguments = argumentsField.text
        config.workingDirectory = workingDirectoryField.text
    }

    override fun resetEditorFrom(config: SemaRunConfiguration) {
        scriptPathField.text = config.scriptPath
        argumentsField.text = config.arguments
        workingDirectoryField.text = config.workingDirectory
    }
}
