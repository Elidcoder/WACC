package wacc.ast

case class Func[N, T](retType: SemType, id: Ident[N, T], params: List[Param[N, T]], stmts: List[Stmt[N, T]])(val pos: Pos)
case object Func extends FuncBridge {
    override def labels: List[String] = List("function declaration")
}

case class Param[N, T](paramType: KnownType, paramId: Ident[N, T])(val pos: Pos)
case object Param extends ParserBridgePos2[Const[KnownType], Ident, Param]