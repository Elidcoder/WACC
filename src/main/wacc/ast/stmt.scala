package wacc.ast

import parsley.Parsley
import parsley.position._

sealed trait Stmt[N, T] {
    val pos: Pos
}

case class Skip[N, T]()(val pos: Pos) extends Stmt[N, T]
case class NewAss[N, T](assType: Type, id: Ident[N, T], rVal: RValue[N, T])(using val pos: Pos) extends Stmt[N, T]
case class Assign[N, T](lVal: LValue[N, T], rVal: RValue[N, T])(using val pos: Pos) extends Stmt[N, T]
case class Read[N, T](lVal: LValue[N, T])(using val pos: Pos) extends Stmt[N, T]
case class Free[N, T](expr: Expr[N, T])(using val pos: Pos) extends Stmt[N, T]
case class Return[N, T](expr: Expr[N, T])(using val pos: Pos, val t: T) extends Stmt[N, T]
case class Exit[N, T](expr: Expr[N, T])(using val pos: Pos) extends Stmt[N, T]
case class Print[N, T](expr: Expr[N, T])(using val pos: Pos) extends Stmt[N, T]
case class PrintLn[N, T](expr: Expr[N, T])(using val pos: Pos) extends Stmt[N, T]
case class If[N, T](expr: Expr[N, T], ifStmts: List[Stmt[N, T]], elseStmts: List[Stmt[N, T]])(using val pos: Pos) extends Stmt[N, T]
case class While[N, T](expr: Expr[N, T], stmts: List[Stmt[N, T]])(using val pos: Pos) extends Stmt[N, T]
case class Nest[N, T](stmts: List[Stmt[N, T]])(using val pos: Pos) extends Stmt[N, T]

case object Skip {
    def apply(): Parsley[Skip[String, Typeless]] = pos.map((x: (Int, Int)) => Skip[String, Typeless]()(Pos(x)))
}

case object NewAss extends ParserBridgePos3[Const[Type], Ident, RValue, NewAss]{
    override def labels: List[String] = List("assignment")
}
case object Assign extends ParserBridgePos2[LValue, RValue, Assign]{
    override def labels: List[String] = List("assignment")
}

case object Read extends ParserBridgePos1[LValue, Read]
case object Free extends ParserBridgePos1[Expr, Free]
case object Return extends ParserBridgePosType1[Expr, Return]
case object Exit extends ParserBridgePos1[Expr, Exit]
case object Print extends ParserBridgePos1[Expr, Print]
case object PrintLn extends ParserBridgePos1[Expr, PrintLn]
case object If extends ParserBridgePos3[Expr, ListWrap[Stmt], ListWrap[Stmt], If]
case object While extends ParserBridgePos2[Expr, ListWrap[Stmt], While]
case object Nest extends ParserBridgePos1[ListWrap[Stmt], Nest]
