package wacc.ast

case class Program[N, T](funcs: List[Func[N, T]], stmts: List[Stmt[N, T]])(val pos: Pos) {
    override def toString(): String = 
        val b = new StringBuilder()
        b ++= "begin\n"
        funcs.foreach{ f =>
            b ++= " "
            b ++= f.toString()
            b ++= "\n"}
        b ++= formatStmtList(stmts, "  ")
        b ++= "\nend"
        b.result()
}
object Program extends ParserBridgePos2[ListWrap[Func], ListWrap[Stmt], Program]
