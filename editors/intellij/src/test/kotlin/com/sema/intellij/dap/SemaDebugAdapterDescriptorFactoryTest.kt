package com.sema.intellij.dap

import com.intellij.openapi.vfs.VirtualFile
import com.intellij.testFramework.LightPlatformTestCase
import com.sema.intellij.SemaFileType
import org.junit.Assert.*
import org.junit.Test

class SemaDebugAdapterDescriptorFactoryTest : LightPlatformTestCase() {
    private val factory = SemaDebugAdapterDescriptorFactory()

    private fun virtualFile(name: String, fileType: com.intellij.openapi.fileTypes.FileType = SemaFileType): VirtualFile =
        object : VirtualFile() {
            override fun getName() = name
            override fun getFileSystem() = throw UnsupportedOperationException()
            override fun getPath() = name
            override fun isDirectory() = false
            override fun isValid() = true
            override fun getParent(): VirtualFile? = null
            override fun getChildren(): Array<VirtualFile> = emptyArray()
            override fun getOutputStream(o: Any, n1: Long, n2: Long) = throw UnsupportedOperationException()
            override fun contentsToByteArray() = ByteArray(0)
            override fun getTimeStamp() = 0L
            override fun getLength() = 0L
            override fun refresh(a: Boolean, b: Boolean, r: Runnable?) {}
            override fun getInputStream() = throw UnsupportedOperationException()
            override fun isWritable() = false
            override fun getModificationStamp() = 0L
            override fun getFileType() = fileType
        }

    @Test
    fun testSemaFilesAreDebuggable() {
        assertTrue(factory.isDebuggableFile(virtualFile("test.sema"), project))
    }

    @Test
    fun testNonSemaFilesNotDebuggable() {
        val nonSemaFile = virtualFile("test.txt", com.sema.intellij.SemacFileType)
        assertFalse(factory.isDebuggableFile(nonSemaFile, project))
    }

    @Test
    fun testLaunchModeSupported() = assertTrue(factory.canRun("launch"))

    @Test
    fun testOnlyLaunchModeSupported() {
        assertTrue(factory.canRun("launch"))
    }

    @Test
    fun testSingleLaunchConfiguration() {
        val configs = factory.launchConfigurations
        assertEquals(1, configs.size)
        assertEquals("sema-launch", configs[0].id)
        assertEquals("Debug Sema file", configs[0].name)
    }

    @Test
    fun testLaunchConfigIsValid() {
        val config = factory.launchConfigurations.first()
        assertNotNull(config)
        assertTrue(config.id.isNotEmpty())
        assertTrue(config.name.isNotEmpty())
    }

    @Test
    fun testLaunchParametersIncludeTypeForAdapterId() {
        // getDapParameters() must carry "type": "sema-dap" so the DAP adapterID matches the
        // launch-config template (LSP4IJ derives adapterID from the "type" field).
        val params = SemaDebugAdapterDescriptorFactory.launchParameters("/work/main.sema", "/work")
        assertEquals("sema-dap", params["type"])
        assertEquals("launch", params["request"])
        assertEquals("/work/main.sema", params["program"])
        assertEquals("/work", params["cwd"])
        assertEquals(true, params["stopOnEntry"])
    }

    @Test
    fun testLaunchConfigContainsExpectedDapParameters() {
        // The launch config drives the DAP "launch" request; assert the parameters the manual smoke
        // gate checks (program, cwd, stopOnEntry) are present and wired to the IDE placeholders.
        val content = factory.launchConfigurations.first().content
        assertTrue("should be a launch request", content.contains("\"request\": \"launch\""))
        assertTrue("program should resolve to the open file", content.contains("\"program\": \"\${file}\""))
        assertTrue("cwd should resolve to the workspace folder", content.contains("\"cwd\": \"\${workspaceFolder}\""))
        assertTrue("should stop on entry", content.contains("\"stopOnEntry\": true"))
        assertTrue("should target the Sema DAP server", content.contains("\"type\": \"sema-dap\""))
    }
}
