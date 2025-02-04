package wacc.ast

import parsley.Parsley
import parsley.position.pos

type ExprList[N, T] = List[Expr[N, T]]
type OptionalExprList[N, T] = Option[ExprList[N, T]]

sealed trait LValue[N, T] {
    val pos: (Int, Int)
}
sealed trait RValue[N, T] {
    val pos: (Int, Int)
}
sealed trait Expr[N, T] extends RValue[N, T]
sealed trait PairElem[N, T] extends RValue[N, T]
sealed trait ArrayOrIdent[N, T] extends LValue[N, T], Expr[N, T]

case class Ident[N, T](v: N)(using val pos: (Int, Int), t: T) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T] {
    def getType(): T = t
}
case class ArrayElem[N, T](i: Ident[N, T], x: List[Expr[N, T]])(using val pos: (Int, Int)) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T]

case class PElem[N, T](v: PairElem[N, T])(using val pos: (Int, Int)) extends LValue[N, T], RValue[N, T]

case class ArrayLit[N, T](x: List[Expr[N, T]])(using val pos: (Int, Int)) extends RValue[N, T]
case class NewPair[N, T](e1: Expr[N, T], e2: Expr[N, T])(using val pos: (Int, Int)) extends RValue[N, T]
case class Call[N, T](i: Ident[N, T], x: List[Expr[N, T]])(using val pos: (Int, Int)) extends RValue[N, T]

case class First[N, T](v: LValue[N, T])(using val pos: (Int, Int)) extends PairElem[N, T]
case class Second[N, T](v: LValue[N, T])(using val pos: (Int, Int)) extends PairElem[N, T]

case object Ident extends IdentBridge {
    override def labels: List[String] = List("identifier")
}
case object ArrayElem extends ParserBridgePos2[Ident, ExprList, ArrayElem]{
    override def labels: List[String] = List("array")
}

case object ArrayOrIdent extends ParserBridgePos2[Ident, OptionalExprList, ArrayOrIdent] {
    override def apply[String, Unit](i: Ident[String, Unit], exprs: OptionalExprList[String, Unit])(pos: (Int, Int)): ArrayOrIdent[String, Unit] = 
        given (Int, Int) = pos
        exprs match {
        case Some(es)   => ArrayElem(i, es)
        case None       => i
    } 
}

case object PElem extends ParserBridgePos1[PairElem, PElem]

case object ArrayLit extends ParserBridgePos1[ExprList, ArrayLit] {
    override def labels: List[String] = List("array literal")
}
case object NewPair extends ParserBridgePos2[Expr, Expr, NewPair] {
    override def labels: List[String] = List("pair literal")
}
case object Call extends ParserBridgePos2[Ident, ExprList, Call] {
    override def labels: List[String] = List("function call")
}

case object First extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("fst")
}
case object Second extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("snd")
}

// case class UnaryOp[N, T](op: UnOp)(x: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
// case class BinaryOp[N, T](op: BinOp)(x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]

// enum UnOp {
//     case Not, Neg, Len, Ord, Chr
// }
// enum BinOp {
//     case Mul, Div, Mod, Add, Sub, Greater, GreaterEq, Less, LessEq, Eq, NotEq, And, Or
// }

// type UnOpTest[N, T] = UnOp
// type BinOpTest[N, T] = BinOp

case class Not[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Neg[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Len[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Ord[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Chr[N, T](e: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]

case class Mul[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Div[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Mod[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Add[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Sub[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Greater[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class GreaterEq[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Less[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class LessEq[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Eq[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class NotEq[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class And[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class Or[N, T](x: Expr[N, T], y: Expr[N, T])(using val pos: (Int, Int)) extends Expr[N, T]

type IntWrap[N, T] = Int
type BoolWrap[N, T] = Boolean
type CharWrap[N, T] = Char
type StrWrap[N, T] = String

case class IntLit[N, T](n: IntWrap[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class BoolLit[N, T](b: BoolWrap[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class CharLit[N, T](c: CharWrap[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class StrLit[N, T](s: StrWrap[N, T])(using val pos: (Int, Int)) extends Expr[N, T]
case class PairLit[N, T]()(val pos: (Int, Int)) extends Expr[N, T]


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
case object And extends ComparisonOperator[Expr, Expr, Expr]
case object Or  extends ComparisonOperator[Expr, Expr, Expr]

case object Greater   extends ComparisonOperator[Expr, Expr, Expr]
case object GreaterEq extends ComparisonOperator[Expr, Expr, Expr]
case object Less      extends ComparisonOperator[Expr, Expr, Expr]
case object LessEq    extends ComparisonOperator[Expr, Expr, Expr]
case object Eq        extends ComparisonOperator[Expr, Expr, Expr]
case object NotEq     extends ComparisonOperator[Expr, Expr, Expr]

case object IntLit extends ParserBridgePos1[IntWrap, IntLit] {
    override def labels = List("integer literal")
}
case object BoolLit extends ParserBridgePos1[BoolWrap, BoolLit] {
    override def labels = List("boolean literal")
}
case object CharLit extends ParserBridgePos1[CharWrap, CharLit] {
    override def labels = List("character literal")
}
case object StrLit extends ParserBridgePos1[StrWrap, StrLit] {
    override def labels = List("string literal")
}
case object PairLit {
    def apply(): Parsley[PairLit[String, Unit]] = pos.map(PairLit[String, Unit]()(_))
}
