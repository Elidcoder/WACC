package wacc.ast

import parsley.generic.ParserBridge2

case class Program(fs: List[Func], x: List[Stmt])
object Program extends ParserBridge2[List[Func], List[Stmt], Program]