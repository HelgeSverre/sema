package com.sema.intellij.lsp

import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaLanguageClientTest : LightPlatformTestCase() {
    @Test
    fun testLanguageClientConstructed() {
        val client = SemaLanguageClient(project)
        assertNotNull(client)
    }
}
