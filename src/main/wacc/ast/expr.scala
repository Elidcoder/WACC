package wacc.ast

sealed trait LValue
sealed trait RValue
sealed trait Expr extends RValue
sealed trait PairElem extends RValue
sealed trait ArrayOrIdent extends LValue, Expr

case class Ident(v: String)(val pos: (Int, Int)) extends LValue, Expr, ArrayOrIdent
case class ArrayElem(i: Ident, x: List[Expr])(val pos: (Int, Int)) extends LValue, Expr, ArrayOrIdent

case class PElem(v: PairElem)(val pos: (Int, Int)) extends LValue, RValue

case class ArrayLit(x: List[Expr])(val pos: (Int, Int)) extends RValue
case class NewPair(e1: Expr, e2: Expr)(val pos: (Int, Int)) extends RValue
case class Call(i: Ident, x: List[Expr])(val pos: (Int, Int)) extends RValue

case class First(v: LValue)(val pos: (Int, Int)) extends PairElem
case class Second(v: LValue)(val pos: (Int, Int)) extends PairElem

case object Ident extends ParserBridgePos1[String, Ident] {
    override def labels: List[String] = List("identifier")
}
case object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]{
    override def labels: List[String] = List("array")
}

case object ArrayOrIdent extends ParserBridgePos2[Ident, Option[List[Expr]], ArrayOrIdent] {
    override def apply(i: Ident, exprs: Option[List[Expr]])(pos: (Int, Int)): ArrayOrIdent = exprs match {
        case Some(es)   => ArrayElem(i, es)(pos)
        case None       => i
    }
    override def labels: List[String] = List("array type or identifier")
    
}

case object PElem extends ParserBridgePos1[PairElem, PElem]

case object ArrayLit extends ParserBridgePos1[List[Expr], ArrayLit] {
    override def labels: List[String] = List("array literal")
}
case object NewPair extends ParserBridgePos2[Expr, Expr, NewPair] {
    override def labels: List[String] = List("pair literal")
}
case object Call extends ParserBridgePos2[Ident, List[Expr], Call] {
    override def labels: List[String] = List("function call")
}

case object First extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("fst")
}
case object Second extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("snd")
}

case class Not(e: Expr)(val pos: (Int, Int)) extends Expr
case class Neg(e: Expr)(val pos: (Int, Int)) extends Expr
case class Len(e: Expr)(val pos: (Int, Int)) extends Expr
case class Ord(e: Expr)(val pos: (Int, Int)) extends Expr
case class Chr(e: Expr)(val pos: (Int, Int)) extends Expr

case class Mul(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Div(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Mod(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Add(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Sub(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Greater(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class GreaterEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Less(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class LessEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Eq(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class NotEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class And(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
case class Or(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr

case class IntLit(n: Int)(val pos: (Int, Int)) extends Expr
case class BoolLit(b: Boolean)(val pos: (Int, Int)) extends Expr
case class CharLit(c: Char)(val pos: (Int, Int)) extends Expr
case class StrLit(s: String)(val pos: (Int, Int)) extends Expr
case class PairLit() extends Expr


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

case object IntLit extends ParserBridgePos1[Int, IntLit] {
    override def labels = List("integer literal")
}
case object BoolLit extends ParserBridgePos1[Boolean, BoolLit] {
    override def labels = List("boolean literal")
}
case object CharLit extends ParserBridgePos1[Char, CharLit] {
    override def labels = List("character literal")
}
case object StrLit extends ParserBridgePos1[String, StrLit] {
    override def labels = List("string literal")
}
