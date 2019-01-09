/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.backend.js.lower

import org.jetbrains.kotlin.backend.common.FileLoweringPass
import org.jetbrains.kotlin.backend.common.ir.isOverridable
import org.jetbrains.kotlin.backend.common.lower.AbstractValueUsageTransformer
import org.jetbrains.kotlin.backend.common.utils.isPrimitiveArray
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.ir.backend.js.JsIrBackendContext
import org.jetbrains.kotlin.ir.backend.js.ir.JsIrBuilder
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.isNothing
import org.jetbrains.kotlin.ir.types.makeNotNull
import org.jetbrains.kotlin.ir.util.*


// Copied and adapted from Kotlin/Native

class AutoboxingTransformer(val context: JsIrBackendContext) : AbstractValueUsageTransformer(context.irBuiltIns), FileLoweringPass {
    override fun lower(irFile: IrFile) {
        irFile.transformChildrenVoid()

        // TODO: Track & insert parents for temporary variables
        irFile.patchDeclarationParents()
    }

    override fun IrExpression.useAs(type: IrType): IrExpression {

        val actualType = when (this) {
            is IrCall -> {
                if (this.symbol.owner.let { it is IrSimpleFunction && it.isSuspend }) {
                    irBuiltIns.anyNType
                } else {
                    this.symbol.owner.realOverride.returnType
                }
            }
            is IrGetField -> this.symbol.owner.type

            is IrTypeOperatorCall -> when (this.operator) {
                IrTypeOperator.IMPLICIT_INTEGER_COERCION ->
                    // TODO: is it a workaround for inconsistent IR?
                    this.typeOperand

                IrTypeOperator.CAST, IrTypeOperator.IMPLICIT_CAST -> context.irBuiltIns.anyNType

                else -> this.type
            }

            is IrGetValue -> {
                val value = this.symbol.owner
                if (value is IrValueParameter && value.isDispatchReceiver) {
                    irBuiltIns.anyNType
                } else {
                    this.type
                }
            }

            else -> this.type
        }

        // // TODO: Default parameters are passed as nulls and they need not to be unboxed. Fix this
        if (actualType.makeNotNull().isNothing())
            return this

        val expectedType = type

        val actualInlinedClass = actualType.getInlinedClass()
        val expectedInlinedClass = expectedType.getInlinedClass()

        val function = when {
            actualInlinedClass == null && expectedInlinedClass == null -> return this
            actualInlinedClass != null && expectedInlinedClass == null -> context.intrinsics.jsBoxIntrinsic
            actualInlinedClass == null && expectedInlinedClass != null -> context.intrinsics.jsUnboxIntrinsic
            else -> return this
        }

        return buildSafeCall(this, actualType, expectedType) { arg ->
            JsIrBuilder.buildCall(
                function,
                expectedType,
                typeArguments = listOf(actualType, expectedType)
            ).also {
                it.putValueArgument(0, arg)
            }
        }
    }

    private fun buildSafeCall(
        arg: IrExpression,
        actualType: IrType,
        resultType: IrType,
        call: (IrExpression) -> IrExpression
    ): IrExpression {
        if (!actualType.isNullable())
            return call(arg)
        return JsIrBuilder.run {
            // TODO: Set parent of local variables
            val tmp = buildVar(actualType, parent = null, initializer = arg)
            val nullCheck = buildIfElse(
                type = resultType,
                cond = buildCall(irBuiltIns.eqeqSymbol).apply {
                    putValueArgument(0, buildGetValue(tmp.symbol))
                    putValueArgument(1, buildNull(irBuiltIns.nothingNType))
                },
                thenBranch = buildNull(irBuiltIns.nothingNType),
                elseBranch = call(buildGetValue(tmp.symbol))
            )
            buildBlock(
                type = resultType,
                statements = listOf(
                    tmp,
                    nullCheck
                )
            )
        }
    }

    private val IrFunctionAccessExpression.target: IrFunction
        get() = when (this) {
            is IrCall -> this.callTarget
            is IrDelegatingConstructorCall -> this.symbol.owner
            else -> TODO(this.render())
        }

    private val IrCall.callTarget: IrFunction
        get() = if (superQualifier == null && symbol.owner.isOverridable) {
            // A virtual call.
            symbol.owner
        } else {
            symbol.owner.realOverride
        }


    override fun IrExpression.useAsDispatchReceiver(expression: IrFunctionAccessExpression): IrExpression {
        return this.useAsArgument(expression.target.dispatchReceiverParameter!!)
    }

    override fun IrExpression.useAsExtensionReceiver(expression: IrFunctionAccessExpression): IrExpression {
        return this.useAsArgument(expression.target.extensionReceiverParameter!!)
    }

    override fun IrExpression.useAsValueArgument(
        expression: IrFunctionAccessExpression,
        parameter: IrValueParameter
    ): IrExpression {

        return this.useAsArgument(expression.target.valueParameters[parameter.index])
    }


    override fun IrExpression.useAsVarargElement(expression: IrVararg): IrExpression {
        return this.useAs(
            // Do not box primitive inline classes
            if (this.type.isInlined() && !expression.type.isInlined() && !expression.type.isPrimitiveArray())
                irBuiltIns.anyNType
            else
                expression.varargElementType
        )
    }

    private val IrValueParameter.isDispatchReceiver: Boolean
        get() {
            val parent = this.parent
            if (parent is IrClass)
                return true
            if (parent is IrFunction && parent.dispatchReceiverParameter == this)
                return true
            return false
        }

    private val IrFunction.realOverride: IrFunction
        get() = when (this) {
            is IrSimpleFunction -> this.realOverride
            is IrConstructor -> this
            else -> error(this)
        }

    private val IrSimpleFunction.realOverride: IrSimpleFunction
        get() = if (modality == Modality.ABSTRACT) this else resolveFakeOverride()!!

}

