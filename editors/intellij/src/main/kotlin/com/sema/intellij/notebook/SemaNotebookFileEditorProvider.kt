package com.sema.intellij.notebook

import com.intellij.openapi.fileEditor.FileEditor
import com.intellij.openapi.fileEditor.FileEditorPolicy
import com.intellij.openapi.fileEditor.FileEditorProvider
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

class SemaNotebookFileEditorProvider : FileEditorProvider {
    override fun accept(project: Project, file: VirtualFile): Boolean =
        file.fileType == SemaNotebookFileType

    override fun createEditor(project: Project, file: VirtualFile): FileEditor =
        SemaNotebookFileEditor(project, file)

    override fun getEditorTypeId(): String = "sema-notebook-editor"

    override fun getPolicy(): FileEditorPolicy = FileEditorPolicy.HIDE_DEFAULT_EDITOR

    override fun disposeEditor(editor: FileEditor) {
        editor.dispose()
    }
}
