/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.backend.jvm

import org.jetbrains.kotlin.backend.common.ir.Symbols
import org.jetbrains.kotlin.backend.common.ir.copyTo
import org.jetbrains.kotlin.backend.common.ir.createImplicitParameterDeclarationWithWrappedDescriptor
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.descriptors.impl.EmptyPackageFragmentDescriptor
import org.jetbrains.kotlin.ir.builders.declarations.*
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrPackageFragment
import org.jetbrains.kotlin.ir.declarations.impl.IrExternalPackageFragmentImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrExternalPackageFragmentSymbolImpl
import org.jetbrains.kotlin.ir.types.defaultType
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.ReferenceSymbolTable
import org.jetbrains.kotlin.ir.util.functions
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.storage.LockBasedStorageManager
import org.jetbrains.kotlin.types.Variance

class JvmSymbols(
    context: JvmBackendContext,
    private val symbolTable: ReferenceSymbolTable
) : Symbols<JvmBackendContext>(context, symbolTable) {
    private val storageManager = LockBasedStorageManager(this::class.java.simpleName)

    private val irBuiltIns = context.irBuiltIns

    override val ThrowNullPointerException: IrSimpleFunctionSymbol
        get() = error("Unused in JVM IR")

    override val ThrowNoWhenBranchMatchedException: IrSimpleFunctionSymbol
        get() = error("Unused in JVM IR")

    override val ThrowTypeCastException: IrSimpleFunctionSymbol
        get() = error("Unused in JVM IR")

    private fun createPackage(fqName: FqName): IrPackageFragment =
        IrExternalPackageFragmentImpl(IrExternalPackageFragmentSymbolImpl(EmptyPackageFragmentDescriptor(context.state.module, fqName)))

    private val kotlinJvmInternalPackage: IrPackageFragment = createPackage(FqName("kotlin.jvm.internal"))
    private val kotlinJvmFunctionsPackage: IrPackageFragment = createPackage(FqName("kotlin.jvm.functions"))

    private fun createClass(fqName: FqName, builder: IrClassBuilder.() -> Unit = {}, block: (IrClass) -> Unit): IrClass =
        buildClass {
            name = fqName.shortName()
            builder()
        }.apply {
            parent = when (fqName.parent().asString()) {
                "kotlin.jvm.internal" -> kotlinJvmInternalPackage
                "kotlin.jvm.functions" -> kotlinJvmFunctionsPackage
                else -> error("Other packages are not supported yet: $fqName")
            }
            createImplicitParameterDeclarationWithWrappedDescriptor()
            block(this)
        }

    private val intrinsicsClass: IrClassSymbol = createClass(FqName("kotlin.jvm.internal.Intrinsics")) { klass ->
        klass.addFunction(Name.identifier("throwUninitializedPropertyAccessException"), irBuiltIns.unitType).apply {
            addValueParameter(Name.identifier("propertyName"), irBuiltIns.stringType)
        }
    }.symbol

    override val ThrowUninitializedPropertyAccessException: IrSimpleFunctionSymbol =
        intrinsicsClass.owner.functions.filter { it.name.asString() == "throwUninitializedPropertyAccessException" }.single().symbol

    override val stringBuilder: IrClassSymbol
        get() = symbolTable.referenceClass(context.getClass(FqName("java.lang.StringBuilder")))

    override val defaultConstructorMarker: IrClassSymbol =
        createClass(FqName("kotlin.jvm.internal.DefaultConstructorMarker")) { }.symbol

    override val copyRangeTo: Map<ClassDescriptor, IrSimpleFunctionSymbol>
        get() = error("Unused in JVM IR")

    override val coroutineImpl: IrClassSymbol
        get() = TODO("not implemented")

    override val coroutineSuspendedGetter: IrSimpleFunctionSymbol
        get() = TODO("not implemented")

    val javaLangClass: IrClassSymbol =
        symbolTable.referenceClass(context.getClass(FqName("java.lang.Class")))

    val lambdaClass: IrClassSymbol = createClass(FqName("kotlin.jvm.internal.Lambda")) { klass ->
        klass.addConstructor().apply {
            addValueParameter(Name.identifier("arity"), irBuiltIns.intType)
        }
    }.symbol

    private fun generateCallableReferenceMethods(klass: IrClass) {
        klass.addFunction(Name.identifier("getSignature"), irBuiltIns.stringType, Modality.OPEN).apply {
            dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
        }
        klass.addFunction(Name.identifier("getName"), irBuiltIns.stringType, Modality.OPEN).apply {
            dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
        }
        klass.addFunction(Name.identifier("getOwner"), irBuiltIns.kDeclarationContainerClass.typeWith(), Modality.OPEN).apply {
            dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
        }
    }

    val functionReference: IrClassSymbol = createClass(FqName("kotlin.jvm.internal.FunctionReference")) { klass ->
        klass.addConstructor().apply {
            addValueParameter(Name.identifier("arity"), irBuiltIns.intType)
            addValueParameter(Name.identifier("receiver"), irBuiltIns.anyNType)
        }

        generateCallableReferenceMethods(klass)
    }.symbol

    fun getFunction(parameterCount: Int): IrClassSymbol =
        symbolTable.referenceClass(builtIns.getFunction(parameterCount))

    private val jvmFunctionClasses = storageManager.createMemoizedFunction { n: Int ->
        createClass(FqName("kotlin.jvm.functions.Function$n"), {
            kind = ClassKind.INTERFACE
        }) { klass ->
            for (i in 1..n) {
                klass.addTypeParameter(Name.identifier("P$i"), irBuiltIns.anyNType, Variance.IN_VARIANCE)
            }
            val returnType = klass.addTypeParameter(Name.identifier("R"), irBuiltIns.anyNType, Variance.OUT_VARIANCE)

            klass.addFunction(Name.identifier("invoke"), returnType.defaultType, Modality.ABSTRACT).apply {
                dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
                for (i in 1..n) {
                    addValueParameter(Name.identifier("p$i"), klass.typeParameters[i - 1].defaultType)
                }
            }
        }.symbol
    }

    fun getJvmFunctionClass(parameterCount: Int): IrClassSymbol =
        jvmFunctionClasses(parameterCount)

    val functionN: IrClassSymbol = createClass(FqName("kotlin.jvm.functions.FunctionN"), {
        kind = ClassKind.INTERFACE
    }) { klass ->
        val returnType = klass.addTypeParameter(Name.identifier("R"), irBuiltIns.anyNType, Variance.OUT_VARIANCE)

        klass.addFunction(Name.identifier("invoke"), returnType.defaultType, Modality.ABSTRACT).apply {
            dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
            addValueParameter {
                name = Name.identifier("args")
                type = irBuiltIns.arrayClass.typeWith(irBuiltIns.anyNType)
                origin = IrDeclarationOrigin.DEFINED
                varargElementType = irBuiltIns.anyNType
            }
        }
    }.symbol

    private data class PropertyReferenceKey(
        val mutable: Boolean,
        val parameterCount: Int,
        val impl: Boolean
    )

    private val propertyReferenceClasses = storageManager.createMemoizedFunction { key: PropertyReferenceKey ->
        val (mutable, n, impl) = key
        val className = buildString {
            if (mutable) append("Mutable")
            append("PropertyReference")
            append(n)
            if (impl) append("Impl")
        }
        createClass(FqName("kotlin.jvm.internal.$className")) { klass ->
            if (impl) {
                klass.addConstructor().apply {
                    addValueParameter(Name.identifier("owner"), irBuiltIns.kDeclarationContainerClass.typeWith())
                    addValueParameter(Name.identifier("name"), irBuiltIns.stringType)
                    addValueParameter(Name.identifier("string"), irBuiltIns.stringType)
                }
            } else {
                klass.addConstructor()

                klass.addConstructor().apply {
                    addValueParameter(Name.identifier("receiver"), irBuiltIns.anyNType)
                }
            }

            val receiverFieldName = Name.identifier("receiver")
            klass.addProperty {
                name = receiverFieldName
            }.apply {
                backingField = buildField {
                    name = receiverFieldName
                    type = irBuiltIns.anyNType
                    visibility = Visibilities.PROTECTED
                }.also { field ->
                    field.parent = klass
                }
            }

            generateCallableReferenceMethods(klass)

            klass.addFunction(Name.identifier("get"), irBuiltIns.anyNType, Modality.ABSTRACT).apply {
                dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
                for (i in 0 until n) {
                    addValueParameter(Name.identifier("receiver$i"), irBuiltIns.anyNType)
                }
            }

            if (mutable) {
                klass.addFunction(Name.identifier("set"), irBuiltIns.unitType, Modality.ABSTRACT).apply {
                    dispatchReceiverParameter = klass.thisReceiver!!.copyTo(this)
                    for (i in 0 until n) {
                        addValueParameter(Name.identifier("receiver$i"), irBuiltIns.anyNType)
                    }
                    addValueParameter(Name.identifier("value"), irBuiltIns.anyNType)
                }
            }
        }.symbol
    }

    fun getPropertyReferenceClass(mutable: Boolean, parameterCount: Int, impl: Boolean): IrClassSymbol =
        propertyReferenceClasses(PropertyReferenceKey(mutable, parameterCount, impl))

    val reflection: IrClassSymbol = createClass(FqName("kotlin.jvm.internal.Reflection")) { klass ->
        klass.addFunction(Name.identifier("getOrCreateKotlinPackage"), irBuiltIns.kDeclarationContainerClass.typeWith()).apply {
            addValueParameter(Name.identifier("javaClass"), javaLangClass.typeWith())
            addValueParameter(Name.identifier("moduleName"), irBuiltIns.stringType)
        }

        klass.addFunction(Name.identifier("getOrCreateKotlinClass"), irBuiltIns.kClassClass.typeWith()).apply {
            addValueParameter(Name.identifier("javaClass"), javaLangClass.typeWith())
        }

        for (mutable in listOf(false, true)) {
            for (n in 0..2) {
                val functionName = Name.identifier((if (mutable) "mutableProperty" else "property") + n)
                klass.addFunction(functionName, irBuiltIns.getKPropertyClass(mutable, n).typeWith()).apply {
                    addValueParameter(Name.identifier("p"), getPropertyReferenceClass(mutable, n, impl = false).typeWith())
                }
            }
        }
    }.symbol
}
