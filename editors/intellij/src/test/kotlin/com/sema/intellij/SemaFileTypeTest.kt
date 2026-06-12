package com.sema.intellij

import com.intellij.openapi.fileTypes.FileTypeManager
import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaFileTypeTest : LightPlatformTestCase() {
    @Test
    fun testSemaExtension() = assertEquals("sema", SemaFileType.defaultExtension)

    @Test
    fun testSemaName() {
        assertEquals("Sema", SemaFileType.name)
        assertEquals("Sema language source file", SemaFileType.description)
    }

    @Test
    fun testSemaIcon() = assertNotNull(SemaFileType.icon)

    @Test
    fun testSemacExtension() = assertEquals("semac", SemacFileType.defaultExtension)

    @Test
    fun testExtensionMapping() {
        val ftm = FileTypeManager.getInstance()
        assertEquals(SemaFileType, ftm.getFileTypeByExtension("sema"))
        assertEquals(SemacFileType, ftm.getFileTypeByExtension("semac"))
    }

    @Test
    fun testLanguageRegistered() {
        assertNotNull(SemaLanguage)
        assertEquals("Sema", SemaLanguage.id)
    }
}
