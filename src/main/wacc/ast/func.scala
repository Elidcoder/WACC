package wacc.ast

case class Func[N, T](t: Type, v: Ident[N, T], l: List[Param[N, T]], s: List[Stmt[N, T]])(val pos: (Int, Int))
case object Func extends ParserBridgePos4[Const[Type], Ident, ListWrap[Param], ListWrap[Stmt], Func] {
    override def labels: List[String] = List("function declaration")
}

case class Param[N, T](t: Type, v: Ident[N, T])(val pos: (Int, Int))
case object Param extends ParserBridgePos2[Const[Type], Ident, Param]