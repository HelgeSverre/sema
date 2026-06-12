package com.sema.intellij.lsp

import com.google.gson.JsonObject
import com.intellij.testFramework.LightPlatformTestCase
import com.sema.intellij.config.SemaSettings
import org.eclipse.lsp4j.InitializeParams
import org.junit.Assert.*
import org.junit.Test

class SemaClientFeaturesTest : LightPlatformTestCase() {

    @Test
    fun testInitializeParamsSendsSemaPath() {
        val features = SemaClientFeatures()
        val params = InitializeParams()
        features.initializeParams(params)

        val options = params.initializationOptions
        assertTrue("initializationOptions should be a JsonObject", options is JsonObject)
        val semaPath = (options as JsonObject).get("semaPath")
        assertNotNull("semaPath must be sent so the eval subprocess uses the configured binary", semaPath)
        assertTrue(semaPath.asString.isNotBlank())
    }

    @Test
    fun testInitializeParamsPreservesExistingOptions() {
        val features = SemaClientFeatures()
        val params = InitializeParams()
        params.initializationOptions = JsonObject().apply { addProperty("existing", "value") }
        features.initializeParams(params)

        val options = params.initializationOptions as JsonObject
        assertEquals("value", options.get("existing").asString)
        assertNotNull(options.get("semaPath"))
    }

    @Test
    fun testKeepServerAliveReflectsSetting() {
        val settings = SemaSettings.getInstance()
        val original = settings.keepLspServerAlive
        try {
            val features = SemaClientFeatures()
            settings.keepLspServerAlive = true
            assertTrue(features.keepServerAlive())
            settings.keepLspServerAlive = false
            assertFalse(features.keepServerAlive())
        } finally {
            settings.keepLspServerAlive = original
        }
    }
}
