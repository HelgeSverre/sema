package com.sema.intellij

import com.intellij.lang.Language

object SemaLanguage : Language("Sema") {
    private fun readResolve(): Any = SemaLanguage
}
