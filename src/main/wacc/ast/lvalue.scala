package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum LValue {
    case Ident(v: String)
    case ArrayElem(v: String, x: List[Expr])
    case PairElem(v: LValue)
}

object  LValue {
    object Ident extends ParserBridge1[String, LValue]
    object ArrayElem extends ParserBridge2[String, List[Expr], LValue]
    object PairElem extends ParserBridge1[LValue, LValue]
}