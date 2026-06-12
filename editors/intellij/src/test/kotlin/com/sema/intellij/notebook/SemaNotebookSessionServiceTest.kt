package com.sema.intellij.notebook

import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaNotebookSessionServiceTest : LightPlatformTestCase() {
    @Test
    fun testServiceAccessible() {
        assertNotNull(SemaNotebookSessionService.getInstance(project))
    }
}
