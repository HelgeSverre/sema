package com.sema.intellij.lsp

import com.intellij.testFramework.LightPlatformTestCase
import com.sema.intellij.TestHelpers
import org.junit.Assert.*
import org.junit.Test

class SemaLanguageServerTest : LightPlatformTestCase() {

    @Test
    fun testFactoryCreatesConnectionProvider() {
        val factory = SemaLanguageServerFactory()
        val provider = factory.createConnectionProvider(project)
        assertNotNull(provider)
        assertTrue(provider is SemaLanguageServer)
    }

    @Test
    fun testFactoryCreatesLanguageClient() {
        val factory = SemaLanguageServerFactory()
        val client = factory.createLanguageClient(project)
        assertNotNull(client)
        assertTrue(client is SemaLanguageClient)
    }

    @Test
    fun testLspServerCommandLine() {
        TestHelpers.requireSema()
        val server = SemaLanguageServer(project)
        assertNotNull(server.commandLine)
        assertEquals("lsp", server.commandLine!!.parametersList.list.first())
    }
}
