/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.backend

import org.jetbrains.kotlin.backend.common.descriptors.WrappedPropertyDescriptor
import org.jetbrains.kotlin.backend.common.descriptors.WrappedSimpleFunctionDescriptor
import org.jetbrains.kotlin.fir.*
import org.jetbrains.kotlin.fir.resolve.toSymbol
import org.jetbrains.kotlin.fir.symbols.ConeClassifierSymbol
import org.jetbrains.kotlin.fir.symbols.impl.*
import org.jetbrains.kotlin.fir.types.*
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.symbols.*
import org.jetbrains.kotlin.ir.types.IrErrorType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.impl.IrErrorTypeImpl
import org.jetbrains.kotlin.ir.types.impl.IrSimpleTypeImpl
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.psi.psiUtil.endOffset
import org.jetbrains.kotlin.psi.psiUtil.startOffsetSkippingComments
import org.jetbrains.kotlin.types.Variance

internal fun <T : IrElement> FirElement.convertWithOffsets(
    f: (startOffset: Int, endOffset: Int) -> T
): T {
    val startOffset = psi?.startOffsetSkippingComments ?: -1
    val endOffset = psi?.endOffset ?: -1
    return f(startOffset, endOffset)
}

private fun createErrorType(): IrErrorType = IrErrorTypeImpl(null, emptyList(), Variance.INVARIANT)

fun FirTypeRef.toIrType(session: FirSession, declarationStorage: Fir2IrDeclarationStorage): IrType {
    if (this !is FirResolvedTypeRef) {
        return createErrorType()
    }
    return type.toIrType(session, declarationStorage)
}

fun ConeKotlinType.toIrType(session: FirSession, declarationStorage: Fir2IrDeclarationStorage): IrType {
    return when (this) {
        is ConeKotlinErrorType -> createErrorType()
        is ConeLookupTagBasedType -> {
            val firSymbol = this.lookupTag.toSymbol(session) ?: return createErrorType()
            val irSymbol = firSymbol.toIrSymbol(declarationStorage)
            // TODO: arguments, annotations
            IrSimpleTypeImpl(irSymbol, this.isMarkedNullable, emptyList(), emptyList())
        }
        is ConeFlexibleType -> TODO()
        is ConeCapturedType -> TODO()
    }
}

fun ConeClassifierSymbol.toIrSymbol(declarationStorage: Fir2IrDeclarationStorage): IrClassifierSymbol {
    when (this) {
        is FirTypeParameterSymbol -> TODO()
        is FirTypeAliasSymbol -> TODO()
        is FirClassSymbol -> {
            // TODO: at some later stage we should bind symbol to its IR
            return toClassSymbol(declarationStorage)
        }
        else -> throw AssertionError("Should not be here: $this")
    }
}

fun FirReference.toSymbol(symbolTable: SymbolTable): IrSymbol? {
    if (this is FirNamedReference) {
        return toSymbol(symbolTable)
    }
    return null
}

fun FirNamedReference.toSymbol(symbolTable: SymbolTable): IrSymbol? {
    if (this is FirResolvedCallableReference) {
        when (val callableSymbol = this.callableSymbol) {
            is FirFunctionSymbol -> return callableSymbol.toFunctionSymbol(symbolTable)
            is FirPropertySymbol -> return callableSymbol.toPropertySymbol(symbolTable)
        }
    }
    return null
}

fun FirClassSymbol.toClassSymbol(declarationStorage: Fir2IrDeclarationStorage): IrClassSymbol {
    return declarationStorage.getIrClassSymbol(this)
}

fun FirFunctionSymbol.toFunctionSymbol(symbolTable: SymbolTable): IrFunctionSymbol {
    return symbolTable.referenceDeclaredFunction(WrappedSimpleFunctionDescriptor())
}

fun FirPropertySymbol.toPropertySymbol(symbolTable: SymbolTable): IrPropertySymbol {
    return symbolTable.referenceProperty(WrappedPropertyDescriptor())
}
