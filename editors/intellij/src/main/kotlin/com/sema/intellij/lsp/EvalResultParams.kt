package com.sema.intellij.lsp

import org.eclipse.lsp4j.Range

data class EvalResultParams(
    val uri: String = "",
    val range: Range = Range(),
    val kind: String = "",
    val value: String? = null,
    val stdout: String = "",
    val stderr: String = "",
    val ok: Boolean = false,
    val error: String? = null,
    val elapsedMs: Long = 0,
)
