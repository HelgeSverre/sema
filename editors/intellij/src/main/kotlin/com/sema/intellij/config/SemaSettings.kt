package com.sema.intellij.config

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.components.Service
import com.intellij.openapi.components.State
import com.intellij.openapi.components.Storage
import com.intellij.util.xmlb.XmlSerializerUtil

@Service
@State(name = "SemaSettings", storages = [Storage("sema.xml")])
class SemaSettings : PersistentStateComponent<SemaSettings.State> {
    private var state = State()

    override fun getState(): State = state

    override fun loadState(state: State) {
        XmlSerializerUtil.copyBean(state, this.state)
    }

    var semaPath: String
        get() = state.semaPath.ifBlank { DEFAULT_SEMA_PATH }
        set(value) {
            state.semaPath = value.ifBlank { DEFAULT_SEMA_PATH }
        }

    /** Keep the LSP server process running even after all Sema files are closed. */
    var keepLspServerAlive: Boolean
        get() = state.keepLspServerAlive
        set(value) {
            state.keepLspServerAlive = value
        }

    /** Enable LSP-backed code formatting (Reformat Code) for Sema files. */
    var formattingEnabled: Boolean
        get() = state.formattingEnabled
        set(value) {
            state.formattingEnabled = value
        }

    class State {
        var semaPath: String = DEFAULT_SEMA_PATH
        var keepLspServerAlive: Boolean = true
        var formattingEnabled: Boolean = true
    }

    companion object {
        const val DEFAULT_SEMA_PATH = "sema"

        fun getInstance(): SemaSettings =
            ApplicationManager.getApplication().getService(SemaSettings::class.java)
    }
}
