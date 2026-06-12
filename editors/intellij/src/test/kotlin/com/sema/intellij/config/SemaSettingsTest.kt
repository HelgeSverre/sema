package com.sema.intellij.config

import org.junit.Assert.*
import org.junit.Test

class SemaSettingsTest {
    @Test
    fun defaultPath() = assertEquals("sema", SemaSettings.State().semaPath)

    @Test
    fun blankPathResets() {
        val s = SemaSettings()
        s.semaPath = "   "
        assertEquals("sema", s.semaPath)
    }

    @Test
    fun getStateReturnsTheSameObject() {
        val s = SemaSettings()
        s.semaPath = "/custom/sema"
        val state = s.state
        assertEquals("/custom/sema", state.semaPath)
        state.semaPath = "/hacked/sema"
        assertEquals("/hacked/sema", s.semaPath)
    }

    @Test
    fun loadStateReplaces() {
        val s = SemaSettings()
        s.semaPath = "/original/sema"
        val ns = SemaSettings.State()
        ns.semaPath = "/loaded/sema"
        s.loadState(ns)
        assertEquals("/loaded/sema", s.semaPath)
    }

    @Test
    fun keepLspServerAliveDefaultsToTrue() = assertTrue(SemaSettings.State().keepLspServerAlive)

    @Test
    fun formattingEnabledDefaultsToTrue() = assertTrue(SemaSettings.State().formattingEnabled)

    @Test
    fun booleanSettingsRoundTrip() {
        val s = SemaSettings()
        s.keepLspServerAlive = false
        s.formattingEnabled = false
        assertFalse(s.keepLspServerAlive)
        assertFalse(s.formattingEnabled)
        s.keepLspServerAlive = true
        s.formattingEnabled = true
        assertTrue(s.keepLspServerAlive)
        assertTrue(s.formattingEnabled)
    }

    @Test
    fun loadStateReplacesBooleans() {
        val s = SemaSettings()
        s.keepLspServerAlive = true
        s.formattingEnabled = true
        val ns = SemaSettings.State()
        ns.keepLspServerAlive = false
        ns.formattingEnabled = false
        s.loadState(ns)
        assertFalse(s.keepLspServerAlive)
        assertFalse(s.formattingEnabled)
    }
}
