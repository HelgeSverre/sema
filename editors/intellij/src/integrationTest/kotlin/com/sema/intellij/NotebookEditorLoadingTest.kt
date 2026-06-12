package com.sema.intellij

import com.intellij.driver.sdk.isProjectOpened
import com.intellij.driver.sdk.openFile
import com.intellij.driver.sdk.waitForIndicators
import com.intellij.ide.starter.driver.engine.runIdeWithDriver
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path
import kotlin.time.Duration.Companion.minutes

/**
 * Verifies that a `.sema-nb` file opens through [com.sema.intellij.notebook.SemaNotebookFileEditorProvider]
 * without error. In this headless run no `sema` binary is installed, so the editor renders its
 * missing-binary fallback panel (no JCEF, no notebook server) — which is exactly the wiring this test
 * exercises. JCEF rendering, the loopback server lifecycle, and run-all/export actions are covered by
 * the manual smoke gate in RELEASING.md, not here.
 */
class NotebookEditorLoadingTest : SemaIntegrationTestBase() {
    @Test
    fun `notebook file opens in the notebook editor`(@TempDir projectDir: Path) {
        createContext("notebookOpens", projectDir)
            .runIdeWithDriver()
            .useDriverAndCloseIde {
                waitForIndicators(5.minutes)
                // openFile throws if no provider can open the file; reaching the assertion means
                // the notebook file type + editor provider loaded and accepted the .sema-nb file.
                openFile(NOTEBOOK_FIXTURE, waitForCodeAnalysis = false)
                assertTrue(isProjectOpened(), "project should remain open after opening a .sema-nb file")
            }
    }
}
