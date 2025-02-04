package wacc.ast

type FuncList[N, T] = List[Func[N, T]]

case class Program[N, T](fs: List[Func[N, T]], x: List[Stmt[N, T]])(val pos: (Int, Int))
object Program extends ParserBridgePos2[FuncList, StmtList, Program]