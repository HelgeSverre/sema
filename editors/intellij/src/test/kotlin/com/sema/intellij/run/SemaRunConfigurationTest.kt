package com.sema.intellij.run

import com.intellij.testFramework.LightPlatformTestCase
import org.jdom.Element
import org.junit.Assert.*
import org.junit.Test

class SemaRunConfigurationTest : LightPlatformTestCase() {

    private fun createConfig(): SemaRunConfiguration {
        val factory = SemaRunConfigurationType().configurationFactories.first()
        return SemaRunConfiguration(project, factory, "Test Config")
    }

    @Test
    fun testDefaultValues() {
        val config = createConfig()
        assertEquals("", config.scriptPath)
        assertEquals("", config.arguments)
    }

    @Test
    fun testSerializationRoundtrip() {
        val config = createConfig()
        config.scriptPath = "/path/to/main.sema"
        config.arguments = "--verbose arg1 arg2"
        config.workingDirectory = "/path/to/project"
        val element = Element("configuration")
        config.writeExternal(element)
        val restored = createConfig()
        restored.readExternal(element)
        assertEquals("/path/to/main.sema", restored.scriptPath)
        assertEquals("--verbose arg1 arg2", restored.arguments)
        assertEquals("/path/to/project", restored.workingDirectory)
    }

    @Test
    fun testDeserializationHandlesMissingAttributes() {
        val restored = createConfig()
        restored.scriptPath = "/path/to/main.sema"
        restored.readExternal(Element("configuration"))
        assertEquals("", restored.scriptPath)
        assertEquals("", restored.arguments)
    }

    @Test
    fun testRunConfigTypeId() {
        val type = SemaRunConfigurationType()
        assertTrue(type.id.isNotEmpty())
        assertEquals("Sema", type.displayName)
        assertEquals("Run a Sema script", type.configurationTypeDescription)
    }

    @Test
    fun testFactoryProducesCorrectType() {
        val type = SemaRunConfigurationType()
        val factory = type.configurationFactories.first()
        val config = factory.createTemplateConfiguration(project)
        assertTrue(config is SemaRunConfiguration)
    }
}
