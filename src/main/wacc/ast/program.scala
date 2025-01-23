package wacc.ast

case class Program(fs: List[Func], x: List[Stmt])(val pos: (Int, Int))
object Program extends ParserBridgePos2[List[Func], List[Stmt], Program]