package wacc.ast

case class Func(t: Type, v: Ident, l: List[Param], s: List[Stmt])(val pos: (Int, Int))
case object Func extends ParserBridgePos4[Type, Ident, List[Param], List[Stmt], Func] {
    override def labels: List[String] = List("function declaration")
}

case class Param(t: Type, v: Ident)(val pos: (Int, Int))
case object Param extends ParserBridgePos2[Type, Ident, Param]