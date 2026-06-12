package com.sema.intellij.config

import kotlin.io.path.createFile
import kotlin.io.path.createTempDirectory
import kotlin.io.path.name
import kotlin.io.path.pathString
import org.junit.Assert.*
import org.junit.Test

class SemaBinaryTest {
    @Test
    fun settingsDefaultToSemaOnPath() {
        assertEquals("sema", SemaSettings.State().semaPath)
    }

    @Test
    fun resolvesAbsoluteExecutablePath() {
        val executable = createTempDirectory("sema-binary").resolve("sema").createFile()
        executable.toFile().setExecutable(true)
        val status = SemaBinary.check(executable.pathString, emptyList())
        assertTrue(status.available)
        assertEquals(executable.pathString, status.resolvedPath)
    }

    @Test
    fun resolvesBinaryFromPathEntries() {
        val dir = createTempDirectory("sema-path")
        val executable = dir.resolve("sema").createFile()
        executable.toFile().setExecutable(true)
        val status = SemaBinary.check("sema", listOf(dir.pathString))
        assertTrue(status.available)
        assertEquals(executable.pathString, status.resolvedPath)
    }

    @Test
    fun reportsUnavailableBinaryWithSharedMessage() {
        val status = SemaBinary.check("missing-sema-${createTempDirectory().name}", emptyList())
        assertFalse(status.available)
        assertNotNull(status.errorText)
        assertTrue(status.errorText!!.contains("Settings | Languages & Frameworks | Sema"))
    }

    @Test
    fun resolvesBinaryCustomName() {
        val dir = createTempDirectory("sema-custom")
        val executable = dir.resolve("sema-custom").createFile()
        executable.toFile().setExecutable(true)
        val status = SemaBinary.check("sema-custom", listOf(dir.pathString))
        assertTrue(status.available)
        assertEquals(executable.pathString, status.resolvedPath)
    }

    @Test
    fun multiplePathEntriesSearchedInOrder() {
        val dir1 = createTempDirectory("sema-path1")
        val dir2 = createTempDirectory("sema-path2")
        val executable = dir2.resolve("sema").createFile()
        executable.toFile().setExecutable(true)
        val status = SemaBinary.check("sema", listOf(dir1.pathString, dir2.pathString))
        assertTrue(status.available)
        assertNotNull(status.resolvedPath)
        assertTrue(status.resolvedPath!!.contains(dir2.name))
    }

    @Test
    fun firstPathEntryWins() {
        val dir1 = createTempDirectory("sema-path1")
        val dir2 = createTempDirectory("sema-path2")
        val exe1 = dir1.resolve("sema").createFile().also { it.toFile().setExecutable(true) }
        dir2.resolve("sema").createFile().also { it.toFile().setExecutable(true) }
        val status = SemaBinary.check("sema", listOf(dir1.pathString, dir2.pathString))
        assertTrue(status.available)
        assertEquals(exe1.pathString, status.resolvedPath)
    }

    @Test
    fun pathWithSlashIsAbsoluteLike() {
        val dir = createTempDirectory("sema-path")
        dir.resolve("sema").createFile().also { it.toFile().setExecutable(true) }
        assertFalse(SemaBinary.check("./sema", listOf(dir.pathString)).available)
    }

    @Test
    fun blankPathUsesDefaultName() {
        val dir = createTempDirectory("sema-blank")
        dir.resolve("sema").createFile().also { it.toFile().setExecutable(true) }
        assertTrue(SemaBinary.check("", listOf(dir.pathString)).available)
    }
}
