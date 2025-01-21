package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum RValue {
    case RExpr(e: Expr)
    case ArrayLit(x: List[Expr])
    case NewPair(e1: Expr, e2: Expr)
    case PairElem(v: LValue)
    case Call(v: String, x: List[Expr])
}

object RValue {
    object RExpr extends ParserBridge1[Expr, RValue]
    object ArrayLit extends ParserBridge1[List[Expr], RValue]
    object NewPair extends ParserBridge2[Expr, Expr, RValue]
    object PairElem extends ParserBridge1[LValue, RValue]
    object Call extends ParserBridge2[String, List[Expr], RValue]
}
