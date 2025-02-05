package wacc.ast

case class Func[N, T](t: Type, v: Ident[N, T], l: List[Param[N, T]], s: List[Stmt[N, T]])(val pos: Pos)
case object Func extends FuncBridge {
    override def labels: List[String] = List("function declaration")
}

case class Param[N, T](t: Type, v: Ident[N, T])(val pos: Pos)
case object Param extends ParserBridgePos2[Const[Type], Ident, Param]