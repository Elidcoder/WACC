package wacc.ast

import parsley.Parsley
import parsley.position._

sealed trait Stmt[N, T] {
    val pos: (Int, Int)
}

case class Skip[N, T]()(val pos: (Int, Int)) extends Stmt[N, T]
case class NewAss[N, T](t: Type, v: Ident[N, T], r: RValue[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class Assign[N, T](l: LValue[N, T], r: RValue[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class Read[N, T](l: LValue[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class Free[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class Return[N, T](e: Expr[N, T])(using val pos: (Int, Int), val t: T) extends Stmt[N, T]
case class Exit[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class Print[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class PrintLn[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Stmt[N, T]
case class If[N, T](e: Expr[N, T], s1: List[Stmt[N, T]], s2: List[Stmt[N, T]])(using val pos: (Int, Int)) extends Stmt[N, T]
case class While[N, T](e: Expr[N, T], s: List[Stmt[N, T]])(using val pos: (Int, Int)) extends Stmt[N, T]
case class Nest[N, T](s: List[Stmt[N, T]])(using val pos: (Int, Int)) extends Stmt[N, T]

case object Skip {
    def apply(): Parsley[Skip[String, Unit]] = pos.map(Skip[String, Unit]()(_))
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
