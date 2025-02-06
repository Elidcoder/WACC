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

case class Ident[N, T](name: N)(using val pos: Pos, val t: T) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T]
case class ArrayElem[N, T](id: Ident[N, T], exprs: List[Expr[N, T]])(using val pos: Pos) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T]

case class ArrayLit[N, T](exprs: List[Expr[N, T]])(using val pos: Pos) extends RValue[N, T]
case class NewPair[N, T](fst: Expr[N, T], snd: Expr[N, T])(using val pos: Pos) extends RValue[N, T]
case class Call[N, T](id: Ident[N, T], exprs: List[Expr[N, T]])(using val pos: Pos) extends RValue[N, T]

case class First[N, T](value: LValue[N, T])(using val pos: Pos) extends PairElem[N, T]
case class Second[N, T](value: LValue[N, T])(using val pos: Pos) extends PairElem[N, T]

case class Not[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Neg[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Len[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Ord[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Chr[N, T](lhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]

case class Mul[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Div[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Mod[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Add[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Sub[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Greater[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class GreaterEq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Less[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class LessEq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Eq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class NotEq[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class And[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]
case class Or[N, T](lhsExpr: Expr[N, T], rhsExpr: Expr[N, T])(using val pos: Pos) extends Expr[N, T]

case class IntLit[N, T](numb: Int)(using val pos: Pos) extends Expr[N, T]
case class BoolLit[N, T](bool: Boolean)(using val pos: Pos) extends Expr[N, T]
case class CharLit[N, T](char: Char)(using val pos: Pos) extends Expr[N, T]
case class StrLit[N, T](str: String)(using val pos: Pos) extends Expr[N, T]
case class PairLit[N, T]()(val pos: Pos) extends Expr[N, T]

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

case object First extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("fst")
}
case object Second extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("snd")
}

case object Not extends UnaryOperator[Expr, Expr]
case object Neg extends UnaryOperator[Expr, Expr]
case object Len extends UnaryOperator[Expr, Expr]
case object Ord extends UnaryOperator[Expr, Expr]
case object Chr extends UnaryOperator[Expr, Expr]

case object Mul extends MathematicalOperator[Expr, Expr, Expr]
case object Div extends MathematicalOperator[Expr, Expr, Expr]
case object Mod extends MathematicalOperator[Expr, Expr, Expr]
case object Add extends MathematicalOperator[Expr, Expr, Expr]
case object Sub extends MathematicalOperator[Expr, Expr, Expr]

case object Greater   extends ComparisonOperator[Expr, Expr, Expr]
case object GreaterEq extends ComparisonOperator[Expr, Expr, Expr]
case object Less      extends ComparisonOperator[Expr, Expr, Expr]
case object LessEq    extends ComparisonOperator[Expr, Expr, Expr]
case object Eq        extends ComparisonOperator[Expr, Expr, Expr]
case object NotEq     extends ComparisonOperator[Expr, Expr, Expr]
case object And extends ComparisonOperator[Expr, Expr, Expr]
case object Or  extends ComparisonOperator[Expr, Expr, Expr]

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
