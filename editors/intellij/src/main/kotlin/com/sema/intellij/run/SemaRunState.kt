package com.sema.intellij.run

import com.intellij.execution.configurations.CommandLineState
import com.intellij.execution.configurations.ParametersList
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.process.ProcessTerminatedListener
import com.intellij.execution.runners.ExecutionEnvironment
import com.sema.intellij.config.SemaCommandLine

class SemaRunState(
    environment: ExecutionEnvironment,
    private val config: SemaRunConfiguration,
) : CommandLineState(environment) {

    override fun startProcess(): ProcessHandler {
        val arguments = if (config.arguments.isBlank()) emptyList() else ParametersList.parse(config.arguments).toList()
        val cmd = SemaCommandLine.runFile(
            scriptPath = config.scriptPath,
            arguments = arguments,
            workingDirectory = config.workingDirectory,
        )

        val handler = OSProcessHandler(cmd)
        ProcessTerminatedListener.attach(handler)
        return handler
    }
}
