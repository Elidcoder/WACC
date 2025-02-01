package wacc.ast

sealed trait Stmt

case class Skip() extends Stmt
case class NewAss(t: Type, v: Ident, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Assign(l: LValue, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Read(l: LValue)(val pos: (Int, Int)) extends Stmt
case class Free(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Return(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Exit(e: Expr)(val pos: (Int, Int)) extends Stmt
case class Print(e: Expr)(val pos: (Int, Int)) extends Stmt
case class PrintLn(e: Expr)(val pos: (Int, Int)) extends Stmt
case class If(e: Expr, s1: List[Stmt], s2: List[Stmt])(val pos: (Int, Int)) extends Stmt
case class While(e: Expr, s: List[Stmt])(val pos: (Int, Int)) extends Stmt
case class Nest(s: List[Stmt])(val pos: (Int, Int)) extends Stmt

case object NewAss extends ParserBridgePos3[Type, Ident, RValue, NewAss]
case object Assign extends ParserBridgePos2[LValue, RValue, Assign]
case object Read extends ParserBridgePos1[LValue, Read]
case object Free extends ParserBridgePos1[Expr, Free]
case object Return extends ParserBridgePos1[Expr, Return]
case object Exit extends ParserBridgePos1[Expr, Exit]
case object Print extends ParserBridgePos1[Expr, Print]
case object PrintLn extends ParserBridgePos1[Expr, PrintLn]
case object If extends ParserBridgePos3[Expr, List[Stmt], List[Stmt], If]
case object While extends ParserBridgePos2[Expr, List[Stmt], While]
case object Nest extends ParserBridgePos1[List[Stmt], Nest]