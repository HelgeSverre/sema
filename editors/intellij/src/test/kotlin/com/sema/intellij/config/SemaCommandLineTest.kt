package com.sema.intellij.config

import org.junit.Assert.*
import org.junit.Test

class SemaCommandLineTest {
    @Test
    fun lspCommand() {
        val cl = SemaCommandLine.lsp("/opt/sema", "/work/project")
        assertEquals("/opt/sema", cl.exePath)
        assertEquals(listOf("lsp"), cl.parametersList.list)
        assertEquals("/work/project", cl.workDirectory.path)
        assertEquals(Charsets.UTF_8, cl.charset)
    }

    @Test
    fun dapCommand() {
        val cl = SemaCommandLine.dap("/opt/sema", "/work/project")
        assertEquals("/opt/sema", cl.exePath)
        assertEquals(listOf("dap"), cl.parametersList.list)
        assertEquals("/work/project", cl.workDirectory.path)
        assertEquals(Charsets.UTF_8, cl.charset)
    }

    @Test
    fun notebookServeCommand() {
        val cl = SemaCommandLine.notebookServe(
            semaPath = "/opt/sema", notebookPath = "/work/project/demo.sema-nb",
            workingDirectory = "/work/project", port = 9123)
        assertEquals(listOf("notebook", "serve", "/work/project/demo.sema-nb",
            "--host", "127.0.0.1", "--port", "9123"), cl.parametersList.list)
        assertEquals("/opt/sema", cl.exePath)
        assertEquals("/work/project", cl.workDirectory.path)
    }

    @Test
    fun notebookNewRunExport() {
        val nb = "/work/project/demo.sema-nb"
        assertEquals(listOf("notebook", "new", nb), SemaCommandLine.notebookNew("/opt/sema", nb, "/work/project").parametersList.list)
        assertEquals(listOf("notebook", "run", nb), SemaCommandLine.notebookRun("/opt/sema", nb, "/work/project").parametersList.list)
        assertEquals(listOf("notebook", "export", nb, "--output", "/work/project/demo.md"),
            SemaCommandLine.notebookExport("/opt/sema", nb, "/work/project", outputPath = "/work/project/demo.md").parametersList.list)
    }

    @Test
    fun runFileCommand() {
        val cl = SemaCommandLine.runFile("/opt/sema", "/work/project/main.sema",
            listOf("--verbose", "arg1"), "/work/project")
        assertEquals("/opt/sema", cl.exePath)
        assertEquals(listOf("/work/project/main.sema", "--verbose", "arg1"), cl.parametersList.list)
        assertEquals("/work/project", cl.workDirectory.path)
        assertEquals(Charsets.UTF_8, cl.charset)
    }

    @Test
    fun blankSemaPathDefaults() {
        val cl = SemaCommandLine.lsp(semaPath = "  ", workingDirectory = "/project")
        assertEquals("sema", cl.exePath)
        assertEquals(listOf("lsp"), cl.parametersList.list)
    }

    @Test
    fun notebookExportWithoutOutput() {
        val cl = SemaCommandLine.notebookExport("/opt/sema", "/project/demo.sema-nb", "/project", outputPath = null)
        assertEquals(listOf("notebook", "export", "/project/demo.sema-nb"), cl.parametersList.list)
    }

    @Test
    fun nullWorkingDir() {
        val cl = SemaCommandLine.lsp(semaPath = "/opt/sema", workingDirectory = null)
        assertEquals("/opt/sema", cl.exePath)
        assertNull(cl.workDirectory)
    }
}
