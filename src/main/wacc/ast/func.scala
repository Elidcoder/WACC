package wacc.ast

import parsley.generic.ParserBridge4

case class Func(t: Type, v: String, l: List[Param], s: Stmt)

object Func extends ParserBridge4[Type, String, List[Param], Stmt, Func]

case class Param(t: Type, v: String)