/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.backend.jvm

import org.jetbrains.kotlin.backend.common.CommonBackendContext
import org.jetbrains.kotlin.backend.common.ir.Ir
import org.jetbrains.kotlin.backend.common.phaser.PhaseConfig
import org.jetbrains.kotlin.backend.jvm.descriptors.JvmDeclarationFactory
import org.jetbrains.kotlin.backend.jvm.descriptors.JvmSharedVariablesManager
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrFile
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.descriptors.IrBuiltIns
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi2ir.PsiSourceManager

class JvmBackendContext(
    val state: GenerationState,
    val psiSourceManager: PsiSourceManager,
    override val irBuiltIns: IrBuiltIns,
    irModuleFragment: IrModuleFragment,
    symbolTable: SymbolTable,
    val phaseConfig: PhaseConfig
) : CommonBackendContext {
    override val builtIns = state.module.builtIns
    override val declarationFactory: JvmDeclarationFactory = JvmDeclarationFactory(state)
    override val sharedVariablesManager = JvmSharedVariablesManager(state.module, builtIns, irBuiltIns)

    override val ir = JvmIr(irModuleFragment, symbolTable)

    override var inVerbosePhase: Boolean = false

    override val configuration get() = state.configuration

    internal fun getJvmInternalClass(name: String): ClassDescriptor {
        return getClass(FqName("kotlin.jvm.internal").child(Name.identifier(name)))
    }

    internal fun getClass(fqName: FqName): ClassDescriptor {
        return state.module.getPackage(fqName.parent()).memberScope.getContributedClassifier(
            fqName.shortName(), NoLookupLocation.FROM_BACKEND
        ) as ClassDescriptor? ?: error("Class is not found: $fqName")
    }

    fun getIrClass(fqName: FqName): IrClassSymbol {
        return ir.symbols.externalSymbolTable.referenceClass(getClass(fqName))
    }

    override fun log(message: () -> String) {
        /*TODO*/
        if (inVerbosePhase) {
            print(message())
        }
    }

    override fun report(element: IrElement?, irFile: IrFile?, message: String, isError: Boolean) {
        /*TODO*/
        print(message)
    }

    inner class JvmIr(irModuleFragment: IrModuleFragment, symbolTable: SymbolTable) : Ir<JvmBackendContext>(this, irModuleFragment) {
        override val symbols = JvmSymbols(this@JvmBackendContext, symbolTable.lazyWrapper)

        override fun shouldGenerateHandlerParameterForDefaultBodyFun() = true
    }
}
