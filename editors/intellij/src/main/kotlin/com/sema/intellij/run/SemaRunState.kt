package com.sema.intellij.run

import com.intellij.execution.configurations.CommandLineState
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.configurations.ParametersList
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.process.ProcessTerminatedListener
import com.intellij.execution.runners.ExecutionEnvironment
import java.io.File

class SemaRunState(
    environment: ExecutionEnvironment,
    private val config: SemaRunConfiguration,
) : CommandLineState(environment) {

    override fun startProcess(): ProcessHandler {
        val semaPath = System.getenv("SEMA_PATH") ?: "sema"
        val cmd = GeneralCommandLine(semaPath, config.scriptPath)

        if (config.arguments.isNotBlank()) {
            cmd.addParameters(*ParametersList.parse(config.arguments))
        }

        if (config.workingDirectory.isNotBlank()) {
            cmd.workDirectory = File(config.workingDirectory)
        }

        cmd.charset = Charsets.UTF_8

        val handler = OSProcessHandler(cmd)
        ProcessTerminatedListener.attach(handler)
        return handler
    }
}
