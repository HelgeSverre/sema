package com.sema.intellij.run

import com.intellij.execution.configurations.ConfigurationType
import com.intellij.execution.configurations.ConfigurationTypeBase
import com.sema.intellij.SemaIcons

class SemaRunConfigurationType : ConfigurationTypeBase(
    "SemaRunConfiguration",
    "Sema",
    "Run a Sema script",
    SemaIcons.FILE
) {
    init {
        addFactory(SemaRunConfigurationFactory(this))
    }

    companion object {
        fun getInstance(): SemaRunConfigurationType {
            return ConfigurationType.CONFIGURATION_TYPE_EP.findExtensionOrFail(SemaRunConfigurationType::class.java)
        }
    }
}
