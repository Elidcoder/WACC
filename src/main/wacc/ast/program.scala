package wacc.ast

case class Program(fs: List[Func], x: List[Stmt])(val pos: (Int, Int))
object Program extends ParserBridgePos2[List[Func], List[Stmt], Program] {
    /* Error message taken from the WACC Reference Compiler. */
    override def reason: Option[String] = Option("all program body and function declarations must be within `begin` and `end`")
}