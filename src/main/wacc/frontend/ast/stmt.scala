package wacc.ast

import parsley.Parsley
import parsley.position.pos

sealed trait Stmt[N, T] {
    val pos: Pos
}

def formatStmtList[N, T](stmts: List[Stmt[N, T]], indent: String) =
    val b = new StringBuilder()
    b ++= indent
    b ++= stmts.headOption.getOrElse("").toString()
    stmts.drop(1)foreach{ s =>
        b ++= s";\n$indent"
        b ++= s.toString()
    }
    b.result()

case class Skip[N, T]()(val pos: Pos) extends Stmt[N, T] {
    override def toString(): String = "skip"
}
case class NewAss[N, T](assType: SemType, id: Ident[N, T], rVal: RValue[N, T])(using val pos: Pos) extends Stmt[N, T] {
    override def toString(): String = s"$assType $id = $rVal"
}
case class Assign[N, T](lVal: LValue[N, T], rVal: RValue[N, T])(using val pos: Pos, val ty: T) extends Stmt[N, T] {
    override def toString(): String = s"$lVal = $rVal"
}
case class Read[N, T](lVal: LValue[N, T])(using val pos: Pos, val ty: T) extends Stmt[N, T] {
    override def toString(): String = s"read $lVal"
}
case class Free[N, T](expr: Expr[N, T])(using val pos: Pos, val ty: T) extends Stmt[N, T] {
    override def toString(): String = s"free $expr"
}
case class Return[N, T](expr: Expr[N, T])(using val pos: Pos, val ty: T) extends Stmt[N, T] {
    override def toString(): String = s"return $expr"
}
case class Exit[N, T](expr: Expr[N, T])(using val pos: Pos) extends Stmt[N, T] {
    override def toString(): String = s"exit $expr"
}
case class Print[N, T](expr: Expr[N, T])(using val pos: Pos, val ty: T) extends Stmt[N, T] {
    override def toString(): String = s"print $expr"
}
case class PrintLn[N, T](expr: Expr[N, T])(using val pos: Pos, val ty: T) extends Stmt[N, T] {
    override def toString(): String = s"println $expr"
}
case class If[N, T](expr: Expr[N, T], ifStmts: List[Stmt[N, T]], elseStmts: List[Stmt[N, T]])(using val pos: Pos) extends Stmt[N, T] {
    override def toString(): String = s"if $expr then\n${formatStmtList(ifStmts, "    ")}\n  else\n${formatStmtList(elseStmts, "    ")}\n  fi"
}
case class While[N, T](expr: Expr[N, T], stmts: List[Stmt[N, T]])(using val pos: Pos) extends Stmt[N, T] {
    override def toString(): String = s"while $expr do\n${formatStmtList(stmts, "    ")}\n  done"
}
case class Nest[N, T](stmts: List[Stmt[N, T]])(using val pos: Pos) extends Stmt[N, T] {
    override def toString(): String = s"begin\n${formatStmtList(stmts, "    ")}\nend"
}

case object Skip {
    def apply(): Parsley[Skip[String, Typeless]] = pos.map((x: (Int, Int)) => Skip[String, Typeless]()(Pos(x)))
}

case object NewAss extends ParserBridgePos3[Const[SemType], Ident, RValue, NewAss]{
    override def labels: List[String] = List("assignment")
}
case object Assign extends ParserBridgePosType2[LValue, RValue, Assign]{
    override def labels: List[String] = List("assignment")
}

case object Read extends ParserBridgePosType1[LValue, Read]
case object Free extends ParserBridgePosType1[Expr, Free]
case object Return extends ParserBridgePosType1[Expr, Return]
case object Exit extends ParserBridgePos1[Expr, Exit]
case object Print extends ParserBridgePosType1[Expr, Print]
case object PrintLn extends ParserBridgePosType1[Expr, PrintLn]
case object If extends ParserBridgePos3[Expr, ListWrap[Stmt], ListWrap[Stmt], If]
case object While extends ParserBridgePos2[Expr, ListWrap[Stmt], While]
case object Nest extends ParserBridgePos1[ListWrap[Stmt], Nest]
