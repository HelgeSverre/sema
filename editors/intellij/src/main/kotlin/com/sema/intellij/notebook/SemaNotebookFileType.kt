package com.sema.intellij.notebook

import com.intellij.openapi.fileTypes.FileType
import com.sema.intellij.SemaIcons
import javax.swing.Icon

object SemaNotebookFileType : FileType {
    override fun getName(): String = "Sema Notebook"
    override fun getDescription(): String = "Sema notebook"
    override fun getDefaultExtension(): String = "sema-nb"
    override fun getIcon(): Icon = SemaIcons.NOTEBOOK_FILE
    override fun isBinary(): Boolean = false
}
