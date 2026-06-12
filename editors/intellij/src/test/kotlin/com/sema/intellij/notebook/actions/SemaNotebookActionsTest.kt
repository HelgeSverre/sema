package com.sema.intellij.notebook.actions

import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.testFramework.LightPlatformTestCase
import org.junit.Assert.*
import org.junit.Test

class SemaNotebookActionsTest : LightPlatformTestCase() {
    @Test
    fun testNewNotebookAction() {
        val action = ActionManager.getInstance().getAction("sema.notebook.new")
        assertNotNull(action)
        assertTrue(action is NewSemaNotebookAction)
    }

    @Test
    fun testOpenExternalAction() {
        val action = ActionManager.getInstance().getAction("sema.notebook.openExternal")
        assertNotNull(action)
        assertTrue(action is OpenSemaNotebookAction)
    }

    @Test
    fun testRunAllCellsAction() {
        val action = ActionManager.getInstance().getAction("sema.notebook.runAll")
        assertNotNull(action)
        assertTrue(action is RunAllSemaNotebookCellsAction)
    }

    @Test
    fun testExportMarkdownAction() {
        val action = ActionManager.getInstance().getAction("sema.notebook.exportMarkdown")
        assertNotNull(action)
        assertTrue(action is ExportSemaNotebookToMarkdownAction)
    }

    @Test
    fun testClearResultsAction() {
        assertNotNull(ActionManager.getInstance().getAction("sema.clearResults"))
    }
}
