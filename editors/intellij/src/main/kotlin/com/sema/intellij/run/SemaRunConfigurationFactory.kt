package com.sema.intellij.run

import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.execution.configurations.ConfigurationType
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.openapi.project.Project

class SemaRunConfigurationFactory(type: ConfigurationType) : ConfigurationFactory(type) {
    override fun getId(): String = "SemaRunConfiguration"

    override fun getName(): String = "Sema"

    override fun createTemplateConfiguration(project: Project): RunConfiguration {
        return SemaRunConfiguration(project, this, "Sema")
    }
}
