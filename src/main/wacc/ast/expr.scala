package wacc.ast

import parsley.generic.ParserBridge1

enum Expr {
    case Int_Liter(n: BigInt)
}

object Expr {
    object Int_Liter extends ParserBridge1[BigInt, Expr]
}