package com.sema.intellij

import com.intellij.extapi.psi.PsiFileBase
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider

class SemaFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, SemaLanguage) {
    override fun getFileType(): FileType = SemaFileType
}
