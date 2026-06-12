package com.sema.intellij.config

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.pathString

data class SemaBinaryStatus(
    val configuredPath: String,
    val available: Boolean,
    val resolvedPath: String? = null,
    val errorText: String? = null,
)

object SemaBinary {
    const val SETTINGS_PATH = "Settings | Languages & Frameworks | Sema"

    fun currentStatus(): SemaBinaryStatus = check(SemaSettings.getInstance().semaPath)

    fun check(
        configuredPath: String,
        pathEntries: List<String> = pathEntriesFromEnvironment(),
    ): SemaBinaryStatus {
        val normalized = configuredPath.ifBlank { SemaSettings.DEFAULT_SEMA_PATH }
        val resolved = resolve(normalized, pathEntries)
        return if (resolved != null) {
            SemaBinaryStatus(normalized, true, resolved.pathString)
        } else {
            SemaBinaryStatus(normalized, false, errorText = missingBinaryMessage(normalized))
        }
    }

    fun missingBinaryMessage(configuredPath: String = SemaSettings.getInstance().semaPath): String {
        return "Sema executable '$configuredPath' was not found. Configure the Sema binary path in $SETTINGS_PATH."
    }

    private fun resolve(configuredPath: String, pathEntries: List<String>): Path? {
        val file = File(configuredPath)
        if (file.isAbsolute || configuredPath.contains('/') || configuredPath.contains('\\')) {
            return file.toPath().takeIf(::isExecutable)
        }

        val candidates = executableNames(configuredPath)
        return pathEntries.asSequence()
            .filter { it.isNotBlank() }
            .flatMap { entry -> candidates.map { Path.of(entry, it) }.asSequence() }
            .firstOrNull(::isExecutable)
    }

    private fun isExecutable(path: Path): Boolean =
        Files.isRegularFile(path) && Files.isExecutable(path)

    private fun executableNames(name: String): List<String> {
        if (!System.getProperty("os.name").startsWith("Windows", ignoreCase = true)) return listOf(name)

        val extensions = System.getenv("PATHEXT")
            ?.split(File.pathSeparatorChar)
            ?.filter { it.isNotBlank() }
            ?: listOf(".EXE", ".BAT", ".CMD")

        return if (name.contains('.')) {
            listOf(name)
        } else {
            extensions.map { name + it.lowercase() } + extensions.map { name + it.uppercase() }
        }
    }

    fun pathEntriesFromEnvironment(): List<String> =
        System.getenv("PATH")?.split(File.pathSeparatorChar) ?: emptyList()
}
