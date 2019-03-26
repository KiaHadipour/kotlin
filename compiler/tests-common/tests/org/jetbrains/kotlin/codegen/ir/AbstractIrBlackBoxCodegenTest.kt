/*
 * Copyright 2010-2016 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.kotlin.codegen.ir

import org.jetbrains.kotlin.codegen.AbstractBlackBoxCodegenTest
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.test.ConfigurationKind
import org.jetbrains.kotlin.test.TargetBackend

abstract class AbstractIrBlackBoxCodegenTest : AbstractBlackBoxCodegenTest() {
    override fun updateConfiguration(configuration: CompilerConfiguration) {
        configuration.put(JVMConfigurationKeys.IR, true)
    }

/*
    //symbols are constructed with stdlib descriptors so stdlib should be presented
    // TODO rewrite symbols building
    override fun extractConfigurationKind(files: MutableList<TestFile>): ConfigurationKind {
        val result = super.extractConfigurationKind(files)
        return when (result) {
            ConfigurationKind.JDK_NO_RUNTIME, ConfigurationKind.JDK_ONLY -> ConfigurationKind.NO_KOTLIN_REFLECT
            else -> result
        }
    }
*/

    override fun getBackend() = TargetBackend.JVM_IR
}
