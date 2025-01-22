package wacc.ast

import parsley.generic.{ParserBridge2, ParserBridge4}

case class Func(t: Type, v: String, l: List[Param], s: List[Stmt])
case object Func extends ParserBridge4[Type, String, List[Param], List[Stmt], Func]

case class Param(t: Type, v: String)
case object Param extends ParserBridge2[Type, String, Param]