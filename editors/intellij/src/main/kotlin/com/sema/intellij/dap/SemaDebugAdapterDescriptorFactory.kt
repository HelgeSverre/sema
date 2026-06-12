package com.sema.intellij.dap

import com.intellij.execution.ExecutionException
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.execution.configurations.RunConfigurationOptions
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.fileTypes.FileType
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.redhat.devtools.lsp4ij.dap.DebugMode
import com.redhat.devtools.lsp4ij.dap.LaunchConfiguration
import com.redhat.devtools.lsp4ij.dap.configurations.DAPRunConfiguration
import com.redhat.devtools.lsp4ij.dap.configurations.DAPRunConfigurationOptions
import com.redhat.devtools.lsp4ij.dap.definitions.DebugAdapterServerDefinition
import com.redhat.devtools.lsp4ij.dap.descriptors.DebugAdapterDescriptor
import com.redhat.devtools.lsp4ij.dap.descriptors.DebugAdapterDescriptorFactory
import com.redhat.devtools.lsp4ij.templates.ServerMappingSettings
import com.sema.intellij.SemaFileType
import com.sema.intellij.config.SemaCommandLine

/**
 * DAP integration for Sema, built on LSP4IJ's debug-adapter framework.
 *
 * Note: [DebugAdapterDescriptorFactory] and [DebugAdapterDescriptor] (and the related DAP
 * configuration/descriptor types) are marked `@ApiStatus.Experimental` in LSP4IJ 0.19.4. We use
 * them intentionally — LSP4IJ exposes no stable DAP API, and this is the only supported path for
 * DAP support today. The IntelliJ Plugin Verifier reports these (alongside our other LSP4IJ
 * feature usages) as *experimental API usage* warnings; they are expected and non-blocking (not
 * part of the build's verifier failure level). Revisit when LSP4IJ stabilizes these APIs.
 */
class SemaDebugAdapterDescriptorFactory : DebugAdapterDescriptorFactory() {
    override fun createDebugAdapterDescriptor(
        options: RunConfigurationOptions,
        environment: ExecutionEnvironment,
    ): DebugAdapterDescriptor =
        SemaDebugAdapterDescriptor(options, environment, serverDefinition)

    override fun isDebuggableFile(file: VirtualFile, project: Project): Boolean =
        file.fileType == SemaFileType

    override fun canRun(debugMode: String): Boolean =
        DebugMode.get(debugMode) == DebugMode.LAUNCH

    override fun prepareConfiguration(
        configuration: RunConfiguration,
        file: VirtualFile,
        project: Project,
    ): Boolean {
        val prepared = super.prepareConfiguration(configuration, file, project)
        if (configuration is DAPRunConfiguration && file.fileType == SemaFileType) {
            // Pre-populate language mapping so breakpoints resolve for Sema files
            // without requiring the user to manually fill the Mappings tab.
            val mappings = configuration.serverMappings.toMutableList()
            if (mappings.none { it.language == "Sema" }) {
                mappings.add(
                    ServerMappingSettings.createLanguageMappingSettings("Sema", "sema")
                )
                configuration.serverMappings = mappings
            }
        }
        return prepared
    }

    override fun getLaunchConfigurations(): MutableList<LaunchConfiguration> =
        mutableListOf(
            LaunchConfiguration(
                "sema-launch",
                "Debug Sema file",
                """
                {
                  "type": "sema-dap",
                  "request": "launch",
                  "name": "Debug Sema file",
                  "program": "${'$'}{file}",
                  "cwd": "${'$'}{workspaceFolder}",
                  "stopOnEntry": true
                }
                """.trimIndent(),
                DebugMode.LAUNCH,
            ),
        )

    private class SemaDebugAdapterDescriptor(
        options: RunConfigurationOptions,
        environment: ExecutionEnvironment,
        serverDefinition: DebugAdapterServerDefinition,
    ) : DebugAdapterDescriptor(options, environment, serverDefinition) {
        @Throws(ExecutionException::class)
        override fun startServer(): ProcessHandler =
            startServer(SemaCommandLine.dap(workingDirectory = environment.project.basePath))

        override fun getDapParameters(): MutableMap<String, Any> {
            val dapOptions = options as? DAPRunConfigurationOptions
            return launchParameters(
                program = dapOptions?.file ?: "",
                cwd = dapOptions?.workingDirectory ?: environment.project.basePath ?: "",
            )
        }

        override fun getDebugMode(): DebugMode = DebugMode.LAUNCH

        override fun getFileType(): FileType = SemaFileType

        override fun getServerName(): String = "Sema Debug Adapter"

        override fun isDebuggableFile(file: VirtualFile, project: Project): Boolean =
            file.fileType == SemaFileType
    }

    companion object {
        /**
         * Builds the DAP "launch" request parameters. `type` becomes the adapterID and must match
         * the `getLaunchConfigurations()` template, so both stay defined here in one place.
         */
        fun launchParameters(program: String, cwd: String): MutableMap<String, Any> =
            mutableMapOf(
                "type" to "sema-dap",
                "request" to "launch",
                "program" to program,
                "cwd" to cwd,
                "stopOnEntry" to true,
            )
    }
}
