package wacc.ast

case class Func[N, T](
    retType: SemType, 
    id: Ident[N, T], 
    params: List[Param[N, T]], 
    stmts: List[Stmt[N, T]]
)(val pos: Pos) {
    override def toString(): String = 
        val b = new StringBuilder
        b ++= s"$retType $id("
        b ++= params.headOption.getOrElse("").toString()
        params.drop(1)foreach{ p =>
            b ++= s", "
            b ++= p.toString()
        }
        b ++= ") is\n"
        b ++= formatStmtList(stmts, "    ")
        b.result()
}
case object Func extends FuncBridge {
    override def labels: List[String] = List("function declaration")
}

case class Param[N, T](paramType: KnownType, paramId: Ident[N, T])(val pos: Pos) {
    override def toString(): String = s"$paramType $paramId"
}
case object Param extends ParserBridgePos2[Const[KnownType], Ident, Param]
