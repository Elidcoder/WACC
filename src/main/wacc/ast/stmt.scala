package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2, ParserBridge3}

sealed trait Stmt

case class Skip() extends Stmt
case class NewAss(t: Type, v: String, r: RValue) extends Stmt
case class Assign(l: LValue, r: RValue) extends Stmt
case class Read(l: LValue) extends Stmt
case class Free(e: Expr) extends Stmt
case class Return(e: Expr) extends Stmt
case class Exit(e: Expr) extends Stmt
case class Print(e: Expr) extends Stmt
case class PrintLn(e: Expr) extends Stmt
case class If(e: Expr, s1: List[Stmt], s2: List[Stmt]) extends Stmt
case class While(e: Expr, s: List[Stmt]) extends Stmt
case class Nest(s: List[Stmt]) extends Stmt

case object NewAss extends ParserBridge3[Type, String, RValue, NewAss]
case object Assign extends ParserBridge2[LValue, RValue, Assign]
case object Read extends ParserBridge1[LValue, Read]
case object Free extends ParserBridge1[Expr, Free]
case object Return extends ParserBridge1[Expr, Return]
case object Exit extends ParserBridge1[Expr, Exit]
case object Print extends ParserBridge1[Expr, Print]
case object PrintLn extends ParserBridge1[Expr, PrintLn]
case object If extends ParserBridge3[Expr, List[Stmt], List[Stmt], If]
case object While extends ParserBridge2[Expr, List[Stmt], While]
case object Nest extends ParserBridge1[List[Stmt], Nest]