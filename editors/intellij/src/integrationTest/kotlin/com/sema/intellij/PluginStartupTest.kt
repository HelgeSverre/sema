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

class PluginStartupTest : SemaIntegrationTestBase() {
    @Test
    fun `plugin starts without errors in IDE`(@TempDir projectDir: Path) {
        createContext("pluginStartup", projectDir)
            .runIdeWithDriver()
            .useDriverAndCloseIde {
                waitForIndicators(3.minutes)
            }
    }

    @Test
    fun `sema file opens in an editor`(@TempDir projectDir: Path) {
        createContext("semaFileOpens", projectDir)
            .runIdeWithDriver()
            .useDriverAndCloseIde {
                waitForIndicators(5.minutes)
                // openFile throws if the file can't be opened in an editor; reaching the
                // assertion means the Sema file type + editor wiring loaded without error.
                openFile(SEMA_FIXTURE, waitForCodeAnalysis = false)
                assertTrue(isProjectOpened(), "project should remain open after opening a .sema file")
            }
    }
}
