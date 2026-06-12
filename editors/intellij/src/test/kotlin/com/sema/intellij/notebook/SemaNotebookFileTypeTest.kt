package com.sema.intellij.notebook

import com.intellij.openapi.fileTypes.FileTypeManager
import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaNotebookFileTypeTest : LightPlatformTestCase() {
    @Test
    fun testExtension() = assertEquals("sema-nb", SemaNotebookFileType.defaultExtension)

    @Test
    fun testNameAndDescription() {
        assertEquals("Sema Notebook", SemaNotebookFileType.name)
        assertTrue(SemaNotebookFileType.description.contains("notebook"))
    }

    @Test
    fun testIcon() = assertNotNull(SemaNotebookFileType.icon)

    @Test
    fun testExtensionMapping() {
        assertEquals(SemaNotebookFileType, FileTypeManager.getInstance().getFileTypeByExtension("sema-nb"))
    }
}
