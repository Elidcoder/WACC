package wacc.ast

import parsley.generic.{ParserBridge4, ParserBridge2}

case class Func(t: Type, v: String, l: List[Param], s: List[Stmt])

object Func extends ParserBridge4[Type, String, List[Param], List[Stmt], Func]

case class Param(t: Type, v: String)

object Param extends ParserBridge2[Type, String, Param]