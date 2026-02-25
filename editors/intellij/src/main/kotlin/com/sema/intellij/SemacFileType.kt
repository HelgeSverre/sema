package com.sema.intellij

import com.intellij.openapi.fileTypes.FileType
import javax.swing.Icon

object SemacFileType : FileType {
    override fun getName(): String = "Semac"
    override fun getDescription(): String = "Sema compiled bytecode"
    override fun getDefaultExtension(): String = "semac"
    override fun getIcon(): Icon = SemaIcons.COMPILED_FILE
    override fun isBinary(): Boolean = true
    override fun isReadOnly(): Boolean = true
}
