package com.sema.intellij.notebook

import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.process.ProcessTerminatedListener
import com.intellij.openapi.Disposable
import com.intellij.openapi.components.Service
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.sema.intellij.config.SemaCommandLine
import java.net.ServerSocket

data class SemaNotebookSession(val url: String, val processHandler: ProcessHandler)

@Service(Service.Level.PROJECT)
class SemaNotebookSessionService(private val project: Project) : Disposable {
    private val sessions = mutableMapOf<String, SemaNotebookSession>()

    fun start(file: VirtualFile): SemaNotebookSession {
        sessions[file.path]?.let { return it }

        val port = allocatePort()
        val commandLine = SemaCommandLine.notebookServe(
            notebookPath = file.path,
            workingDirectory = file.parent?.path ?: project.basePath,
            port = port,
        )
        val handler = OSProcessHandler(commandLine)
        ProcessTerminatedListener.attach(handler)
        handler.startNotify()

        return SemaNotebookSession("http://127.0.0.1:$port", handler).also {
            sessions[file.path] = it
        }
    }

    fun stop(file: VirtualFile) {
        sessions.remove(file.path)?.processHandler?.destroyProcess()
    }

    override fun dispose() {
        sessions.values.forEach { it.processHandler.destroyProcess() }
        sessions.clear()
    }

    private fun allocatePort(): Int {
        ServerSocket(0).use { socket ->
            socket.reuseAddress = true
            return socket.localPort
        }
    }

    companion object {
        fun getInstance(project: Project): SemaNotebookSessionService =
            project.getService(SemaNotebookSessionService::class.java)
    }
}
