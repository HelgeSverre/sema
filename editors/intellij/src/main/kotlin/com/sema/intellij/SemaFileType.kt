package com.sema.intellij

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

object SemaFileType : LanguageFileType(SemaLanguage) {
    override fun getName(): String = "Sema"
    override fun getDescription(): String = "Sema language source file"
    override fun getDefaultExtension(): String = "sema"
    override fun getIcon(): Icon = SemaIcons.FILE
}
