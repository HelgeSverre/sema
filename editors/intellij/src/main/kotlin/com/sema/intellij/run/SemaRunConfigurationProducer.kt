package com.sema.intellij.run

import com.intellij.execution.actions.ConfigurationContext
import com.intellij.execution.actions.LazyRunConfigurationProducer
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.sema.intellij.SemaFileType

class SemaRunConfigurationProducer : LazyRunConfigurationProducer<SemaRunConfiguration>() {

    override fun getConfigurationFactory(): ConfigurationFactory {
        return SemaRunConfigurationType.getInstance().configurationFactories[0]
    }

    override fun isConfigurationFromContext(
        configuration: SemaRunConfiguration,
        context: ConfigurationContext,
    ): Boolean {
        val file = context.location?.virtualFile ?: return false
        return file.fileType == SemaFileType && configuration.scriptPath == file.path
    }

    override fun setupConfigurationFromContext(
        configuration: SemaRunConfiguration,
        context: ConfigurationContext,
        sourceElement: Ref<PsiElement>,
    ): Boolean {
        val file = context.location?.virtualFile ?: return false
        if (file.fileType != SemaFileType) return false

        configuration.scriptPath = file.path
        configuration.name = file.nameWithoutExtension
        configuration.workingDirectory = context.project.basePath ?: ""
        return true
    }
}
