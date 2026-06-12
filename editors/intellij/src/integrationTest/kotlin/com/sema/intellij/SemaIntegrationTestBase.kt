package com.sema.intellij

import com.intellij.ide.starter.driver.engine.runIdeWithDriver
import com.intellij.ide.starter.ide.IdeProductProvider
import com.intellij.ide.starter.ide.IDETestContext
import com.intellij.ide.starter.models.TestCase
import com.intellij.ide.starter.plugins.PluginConfigurator
import com.intellij.ide.starter.project.LocalProjectInfo
import com.intellij.ide.starter.runner.Starter
import java.io.File
import java.nio.file.Path
import kotlin.io.path.createDirectory
import kotlin.io.path.writeText

abstract class SemaIntegrationTestBase {

    companion object {
        const val IDE_VERSION = "2024.3"

        const val SEMA_FIXTURE = "main.sema"
        const val NOTEBOOK_FIXTURE = "notebook.sema-nb"

        fun createContext(testName: String, projectDir: Path): IDETestContext {
            val projectPath = projectDir.resolve("test-project")
            projectPath.createDirectory()
            projectPath.resolve(SEMA_FIXTURE).writeText(
                """
                (define greeting "Hello from IntelliJ test!")
                (defun square (x) (* x x))
                (print (square 5))
                """.trimIndent(),
            )
            // Minimal valid .sema-nb so the notebook editor provider has something to open.
            projectPath.resolve(NOTEBOOK_FIXTURE).writeText(
                """
                {
                  "version": 1,
                  "metadata": { "title": "Integration Test Notebook" },
                  "cells": [
                    { "id": "c1", "type": "markdown", "source": "# Integration test" }
                  ]
                }
                """.trimIndent(),
            )

            return Starter.newContext(
                testName = testName,
                testCase = TestCase(
                    IdeProductProvider.IC,
                    LocalProjectInfo(projectPath),
                ).withVersion(IDE_VERSION),
            ).apply {
                val pathToPlugin = System.getProperty("path.to.build.plugin")
                    ?: error("path.to.build.plugin system property must be set")
                PluginConfigurator(this).installPluginFromFolder(File(pathToPlugin))
            }
        }
    }
}
