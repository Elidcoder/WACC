package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum LValue {
    case Ident(v: String)
    case ArrayElem(v: String, x: List[Expr])
    case PElem(v: PairElem)
}

object  LValue {
    object Ident extends ParserBridge1[String, LValue]
    object ArrayElem extends ParserBridge2[String, List[Expr], LValue]
    object PElem extends ParserBridge1[PairElem, LValue]
}