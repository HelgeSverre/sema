package com.sema.intellij.lsp

import com.google.gson.JsonObject
import com.redhat.devtools.lsp4ij.client.features.LSPClientFeatures
import com.sema.intellij.config.SemaBinary
import com.sema.intellij.config.SemaSettings
import org.eclipse.lsp4j.InitializeParams

/**
 * Customizes the Sema LSP client.
 *
 * - Sends the configured Sema binary path as `initializationOptions.semaPath` so the server's
 *   code-lens eval subprocess uses the same binary the user configured (not just `sema` on PATH).
 * - Honors the "keep language server alive" setting.
 */
class SemaClientFeatures : LSPClientFeatures() {

    override fun initializeParams(params: InitializeParams) {
        super.initializeParams(params)
        // Prefer the resolved absolute path; fall back to the configured value.
        val status = SemaBinary.currentStatus()
        val semaPath = status.resolvedPath ?: SemaSettings.getInstance().semaPath
        val options = (params.initializationOptions as? JsonObject) ?: JsonObject()
        options.addProperty("semaPath", semaPath)
        params.initializationOptions = options
    }

    override fun keepServerAlive(): Boolean = SemaSettings.getInstance().keepLspServerAlive
}
