package wacc.ast

import parsley.generic.{ParserBridge2, ParserBridge4}

case class Func(t: Type, v: Ident, l: List[Param], s: List[Stmt])
case object Func extends ParserBridge4[Type, Ident, List[Param], List[Stmt], Func]

case class Param(t: Type, v: Ident)
case object Param extends ParserBridge2[Type, Ident, Param]