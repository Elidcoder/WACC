package wacc.ast


case class Program[N, T](fs: List[Func[N, T]], x: List[Stmt[N, T]])(val pos: (Int, Int))
object Program extends ParserBridgePos2[ListWrap[Func], ListWrap[Stmt], Program] {
    /* Error message taken from the WACC Reference Compiler. */
    override def reason: Option[String] = Option("all program body and function declarations must be within `begin` and `end`")
}