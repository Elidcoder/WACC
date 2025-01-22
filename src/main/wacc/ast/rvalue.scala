package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum RValue {
    case RExpr(e: Expr)
    case ArrayLit(x: List[Expr])
    case NewPair(e1: Expr, e2: Expr)
    case PElem(p: PairElem)
    case Call(v: String, x: List[Expr])
}

enum PairElem {
    case First(v: LValue)
    case Second(v: LValue)
}



object RValue {
    object RExpr extends ParserBridge1[Expr, RValue]
    object ArrayLit extends ParserBridge1[List[Expr], RValue]
    object NewPair extends ParserBridge2[Expr, Expr, RValue]
    object PElem extends ParserBridge1[PairElem, RValue]
    object Call extends ParserBridge2[String, List[Expr], RValue]
}

object PairElem {
    object First extends ParserBridge1[LValue, PairElem]
    object Second extends ParserBridge1[LValue, PairElem]
}
