/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.backend

import org.jetbrains.kotlin.backend.common.descriptors.*
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.declarations.*
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.fir.descriptors.FirPackageFragmentDescriptor
import org.jetbrains.kotlin.fir.expressions.FirVariable
import org.jetbrains.kotlin.fir.resolve.FirSymbolProvider
import org.jetbrains.kotlin.fir.service
import org.jetbrains.kotlin.fir.symbols.ConeCallableSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirFunctionSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirVariableSymbol
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.*
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrPropertySymbol
import org.jetbrains.kotlin.ir.symbols.IrValueSymbol
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.name.FqName

class Fir2IrDeclarationStorage(
    private val session: FirSession,
    private val irSymbolTable: SymbolTable,
    private val moduleDescriptor: FirModuleDescriptor
) {
    private val firSymbolProvider = session.service<FirSymbolProvider>()

    private val fragmentCache = mutableMapOf<FqName, IrExternalPackageFragment>()

    private val classCache = mutableMapOf<FirRegularClass, IrClass>()

    private val functionCache = mutableMapOf<FirNamedFunction, IrSimpleFunction>()

    private val constructorCache = mutableMapOf<FirConstructor, IrConstructor>()

    private val parameterCache = mutableMapOf<FirValueParameter, IrValueParameter>()

    private val variableCache = mutableMapOf<FirVariable, IrVariable>()

    private fun getIrExternalPackageFragment(fqName: FqName): IrExternalPackageFragment {
        return fragmentCache.getOrPut(fqName) {
            // TODO: module descriptor is wrong here
            return irSymbolTable.declareExternalPackageFragment(FirPackageFragmentDescriptor(fqName, moduleDescriptor))
        }
    }

    fun getIrClass(regularClass: FirRegularClass, setParent: Boolean = true): IrClass {
        return classCache.getOrPut(regularClass) {
            val descriptor = WrappedClassDescriptor()
            val origin = IrDeclarationOrigin.DEFINED
            val modality = regularClass.modality!!
            regularClass.convertWithOffsets { startOffset, endOffset ->
                irSymbolTable.declareClass(startOffset, endOffset, origin, descriptor, modality) { symbol ->
                    IrClassImpl(
                        startOffset, endOffset, origin, symbol,
                        regularClass.name, regularClass.classKind,
                        regularClass.visibility, modality,
                        regularClass.isCompanion, regularClass.isInner,
                        regularClass.isData, false, regularClass.isInline
                    ).apply {
                        if (setParent) {
                            val classId = regularClass.classId
                            val parentId = classId.outerClassId
                            if (parentId != null) {
                                val parentFirSymbol = firSymbolProvider.getClassLikeSymbolByFqName(parentId)
                                if (parentFirSymbol is FirClassSymbol) {
                                    val parentIrSymbol = getIrClassSymbol(parentFirSymbol)
                                    parent = parentIrSymbol.owner
                                }
                            } else {
                                val packageFqName = classId.packageFqName
                                parent = getIrExternalPackageFragment(packageFqName)
                            }
                        }
                    }
                }
            }
        }
    }

    private fun IrDeclaration.setParentByOwnFir(firMember: FirCallableMemberDeclaration) {
        val firBasedSymbol = firMember.symbol
        if (firBasedSymbol is ConeCallableSymbol) {
            val callableId = firBasedSymbol.callableId
            val parentClassId = callableId.classId
            if (parentClassId != null) {
                val parentFirSymbol = firSymbolProvider.getClassLikeSymbolByFqName(parentClassId)
                if (parentFirSymbol is FirClassSymbol) {
                    val parentIrSymbol = getIrClassSymbol(parentFirSymbol)
                    val parentIrClass = parentIrSymbol.owner
                    parent = parentIrClass
                    parentIrClass.declarations += this
                }
            } else {
                val packageFqName = callableId.packageName
                val parentIrPackageFragment = getIrExternalPackageFragment(packageFqName)
                parent = parentIrPackageFragment
                parentIrPackageFragment.declarations += this
            }
        }
    }

    fun getIrFunction(function: FirNamedFunction, setParent: Boolean = true): IrSimpleFunction {
        return functionCache.getOrPut(function) {
            val descriptor = WrappedSimpleFunctionDescriptor()
            val origin = IrDeclarationOrigin.DEFINED
            function.convertWithOffsets { startOffset, endOffset ->
                irSymbolTable.declareSimpleFunction(startOffset, endOffset, origin, descriptor) { symbol ->
                    IrFunctionImpl(
                        startOffset, endOffset, origin, symbol,
                        function.name, function.visibility, function.modality!!,
                        function.returnTypeRef.toIrType(session, this),
                        function.isInline, function.isExternal,
                        function.isTailRec, function.isSuspend
                    )
                }
            }.apply {
                descriptor.bind(this)
                if (setParent) {
                    setParentByOwnFir(function)
                }
            }
        }
    }

    fun getIrConstructor(constructor: FirConstructor, setParent: Boolean = true): IrConstructor {
        return constructorCache.getOrPut(constructor) {
            val descriptor = WrappedClassConstructorDescriptor()
            val origin = IrDeclarationOrigin.DEFINED
            val isPrimary = constructor.isPrimary
            return constructor.convertWithOffsets { startOffset, endOffset ->
                irSymbolTable.declareConstructor(startOffset, endOffset, origin, descriptor) { symbol ->
                    IrConstructorImpl(
                        startOffset, endOffset, origin, symbol,
                        constructor.name, constructor.visibility,
                        constructor.returnTypeRef.toIrType(session, this),
                        false, false, isPrimary
                    ).apply {
                        descriptor.bind(this)
                        if (setParent) {
                            setParentByOwnFir(constructor)
                        }
                    }
                }
            }

        }
    }

    fun getIrProperty(property: FirProperty, setParent: Boolean = true): IrProperty {
        val descriptor = WrappedPropertyDescriptor()
        val origin = IrDeclarationOrigin.DEFINED
        return property.convertWithOffsets { startOffset, endOffset ->
            irSymbolTable.declareProperty(
                startOffset, endOffset,
                origin, descriptor, property.delegate != null
            ) { symbol ->
                IrPropertyImpl(
                    startOffset, endOffset, origin, symbol,
                    property.name, property.visibility, property.modality!!,
                    property.isVar, property.isConst, property.isLateInit,
                    property.delegate != null,
                    // TODO
                    isExternal = false
                ).apply {
                    descriptor.bind(this)
                    if (setParent) {
                        setParentByOwnFir(property)
                    }
                }
            }
        }
    }

    fun getIrValueParameter(valueParameter: FirValueParameter, index: Int = -1): IrValueParameter {
        return parameterCache.getOrPut(valueParameter) {
            val descriptor = WrappedValueParameterDescriptor()
            val origin = IrDeclarationOrigin.DEFINED
            val type = valueParameter.returnTypeRef.toIrType(session, this)
            valueParameter.convertWithOffsets { startOffset, endOffset ->
                irSymbolTable.declareValueParameter(
                    startOffset, endOffset, origin, descriptor, type
                ) { symbol ->
                    IrValueParameterImpl(
                        startOffset, endOffset, origin, symbol,
                        valueParameter.name, index, type,
                        null, valueParameter.isCrossinline, valueParameter.isNoinline
                    ).apply {
                        descriptor.bind(this)
                    }
                }
            }
        }
    }

    fun getIrVariable(variable: FirVariable): IrVariable {
        return variableCache.getOrPut(variable) {
            val descriptor = WrappedVariableDescriptor()
            val origin = IrDeclarationOrigin.DEFINED
            val type = variable.returnTypeRef.toIrType(session, this)
            variable.convertWithOffsets { startOffset, endOffset ->
                irSymbolTable.declareVariable(startOffset, endOffset, origin, descriptor, type) { symbol ->
                    IrVariableImpl(
                        startOffset, endOffset, origin, symbol, variable.name, type,
                        variable.isVar, isConst = false, isLateinit = false
                    ).apply {
                        descriptor.bind(this)
                    }
                }
            }
        }
    }

    fun getIrClassSymbol(firClassSymbol: FirClassSymbol): IrClassSymbol {
        val irClass = getIrClass(firClassSymbol.fir)
        return irSymbolTable.referenceClass(irClass.descriptor)
    }

    fun getIrFunctionSymbol(firFunctionSymbol: FirFunctionSymbol): IrFunctionSymbol {
        return when (val firDeclaration = firFunctionSymbol.fir) {
            is FirNamedFunction -> {
                val irDeclaration = getIrFunction(firDeclaration)
                irSymbolTable.referenceSimpleFunction(irDeclaration.descriptor)
            }
            is FirConstructor -> {
                val irDeclaration = getIrConstructor(firDeclaration)
                irSymbolTable.referenceConstructor(irDeclaration.descriptor)
            }
            else -> throw AssertionError("Should not be here")
        }
    }

    fun getIrPropertySymbol(firPropertySymbol: FirPropertySymbol): IrPropertySymbol {
        val irProperty = getIrProperty(firPropertySymbol.fir as FirProperty)
        return irSymbolTable.referenceProperty(irProperty.descriptor)
    }

    fun getIrValueSymbol(firVariableSymbol: FirVariableSymbol): IrValueSymbol {
        return when (val firDeclaration = firVariableSymbol.fir) {
            is FirValueParameter -> {
                val irDeclaration = getIrValueParameter(firDeclaration)
                irSymbolTable.referenceValueParameter(irDeclaration.descriptor)
            }
            is FirVariable -> {
                val irDeclaration = getIrVariable(firDeclaration)
                irSymbolTable.referenceVariable(irDeclaration.descriptor)
            }
            else -> throw AssertionError("Should not be here")
        }
    }
}