package wacc.ast


case class Program[N, T](funcs: List[Func[N, T]], stmts: List[Stmt[N, T]])(val pos: Pos)
object Program extends ParserBridgePos2[ListWrap[Func], ListWrap[Stmt], Program] {
    /* Error message taken from the WACC Reference Compiler. */
    override def reason: Option[String] = Option("all program body and function declarations must be within `begin` and `end`")
}