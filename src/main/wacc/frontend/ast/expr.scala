package wacc.ast

import parsley.Parsley
import parsley.position.pos

sealed trait LValue[N, T] {
    val pos: Pos
}
sealed trait RValue[N, T] {
    val pos: Pos
}
sealed trait Expr[N, T] extends RValue[N, T]
sealed trait PairElem[N, T] extends LValue[N, T], RValue[N, T]
sealed trait ArrayOrIdent[N, T] extends LValue[N, T], Expr[N, T]

case class Ident[N, T](name: N)(using val pos: Pos, val t: T) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T] {
    override def toString(): String = s"$name"
}
case class ArrayElem[N, T](id: Ident[N, T], exprs: List[Expr[N, T]])(using val pos: Pos) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T]{
    override def toString(): String = 
        val b = new StringBuilder()
        b ++= id.toString()
        exprs.foreach(e => b ++= s"[$e]")
        b.result()
}

case class ArrayLit[N, T](exprs: List[Expr[N, T]])(using val pos: Pos) extends RValue[N, T] {
    override def toString(): String = 
        val b = new StringBuilder()
        b ++= "["
        b ++= exprs.headOption.getOrElse("").toString()
        exprs.drop(1).foreach(e => b ++= s", $e")
        b ++= "]"
        b.result()
}
case class NewPair[N, T](fst: Expr[N, T], snd: Expr[N, T])(using val pos: Pos) extends RValue[N, T] {
    override def toString(): String = s"newpair($fst, $snd)"
}
case class Call[N, T](id: Ident[N, T], exprs: List[Expr[N, T]])(using val pos: Pos) extends RValue[N, T] {
    override def toString(): String = 
        val b = new StringBuilder()
        b ++= s"call $id("
        b ++= exprs.headOption.getOrElse("").toString()
        exprs.drop(1).foreach(e => b ++= s", $e")
        b ++= ")"
        b.result()
}

case class First[N, T](value: LValue[N, T])(using val pos: Pos) extends PairElem[N, T] {
    override def toString(): String = s"fst $value"
}
case class Second[N, T](value: LValue[N, T])(using val pos: Pos) extends PairElem[N, T] {
    override def toString(): String = s"snd $value"
}

case class Not[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"!$lhsExpr"
}
case class Neg[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"-$lhsExpr"
}
case class Len[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"len $lhsExpr"
}
case class Ord[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"ord $lhsExpr"
}
case class Chr[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"chr $lhsExpr"
}

case class Mul[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr * $rhsExpr"
}
case class Div[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr / $rhsExpr"
}
case class Mod[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr % $rhsExpr"
}
case class Add[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr + $rhsExpr"
}
case class Sub[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr - $rhsExpr"
}
case class Greater[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr > $rhsExpr"
}
case class GreaterEq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr >= $rhsExpr"
}
case class Less[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr < $rhsExpr"
}
case class LessEq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr <= $rhsExpr"
}
case class Eq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr == $rhsExpr"
}
case class NotEq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr != $rhsExpr"
}
case class And[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr && $rhsExpr"
}
case class Or[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"$lhsExpr || $rhsExpr"
}

case class IntLit[N, T](numb: Int)(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = numb.toString()
}
case class BoolLit[N, T](bool: Boolean)(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = bool.toString()
}
case class CharLit[N, T](char: Char)(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"\'$char\'"
}
case class StrLit[N, T](str: String)(using val pos: Pos) extends Expr[N, T] {
    override def toString(): String = s"\"$str\""
}
case class PairLit[N, T]()(val pos: Pos) extends Expr[N, T] {
    override def toString(): String = "null"
}

case object Ident extends IdentBridge {
    override def labels: List[String] = List("identifier")
}
case object ArrayElem extends ParserBridgePos2[Ident, ListWrap[Expr], ArrayElem]{
    override def labels: List[String] = List("array")
}

case object ArrayOrIdent extends ParserBridgePos2[Ident, OptionWrap[ListWrap[Expr]], ArrayOrIdent] {
    override def apply[String, Typeless](id: Ident[String, Typeless], exprs: Option[List[Expr[String, Typeless]]])(using pos: Pos): ArrayOrIdent[String, Typeless] = 
        exprs.fold(id)(ArrayElem(id, _))
}

case object ArrayLit extends ParserBridgePos1[ListWrap[Expr], ArrayLit] {
    override def labels: List[String] = List("array literal")
}
case object NewPair extends ParserBridgePos2[Expr, Expr, NewPair] {
    override def labels: List[String] = List("pair literal")
}
case object Call extends ParserBridgePos2[Ident, ListWrap[Expr], Call] {
    override def labels: List[String] = List("function call")
}

case object First extends ParserBridgePos1[LValue, PairElem] 
case object Second extends ParserBridgePos1[LValue, PairElem]

case object Not extends UnaryOperator[Expr, Expr]
case object Neg extends UnaryOperator[Expr, Expr]
case object Len extends UnaryOperator[Expr, Expr]
case object Ord extends UnaryOperator[Expr, Expr]
case object Chr extends UnaryOperator[Expr, Expr]

case object Mul extends Operator[Expr, Expr, Expr]
case object Div extends Operator[Expr, Expr, Expr]
case object Mod extends Operator[Expr, Expr, Expr]
case object Add extends Operator[Expr, Expr, Expr]
case object Sub extends Operator[Expr, Expr, Expr]
case object And extends Operator[Expr, Expr, Expr]
case object Or  extends Operator[Expr, Expr, Expr]

case object Greater   extends Operator[Expr, Expr, Expr]
case object GreaterEq extends Operator[Expr, Expr, Expr]
case object Less      extends Operator[Expr, Expr, Expr]
case object LessEq    extends Operator[Expr, Expr, Expr]
case object Eq        extends Operator[Expr, Expr, Expr]
case object NotEq     extends Operator[Expr, Expr, Expr]

case object IntLit extends ParserBridgePos1[Const[Int], IntLit] {
    override def labels = List("integer literal")
}
case object BoolLit extends ParserBridgePos1[Const[Boolean], BoolLit] {
    override def labels = List("boolean literal")
}
case object CharLit extends ParserBridgePos1[Const[Char], CharLit] {
    override def labels = List("character literal")
}
case object StrLit extends ParserBridgePos1[Const[String], StrLit] {
    override def labels = List("string literal")
}
case object PairLit {
    def apply(): Parsley[PairLit[String, Typeless]] = pos.map((position: (Int, Int)) => PairLit[String, Typeless]()(Pos(position)))
}
