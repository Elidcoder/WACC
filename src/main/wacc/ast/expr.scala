package wacc.ast

type ExprList[N, T] = List[Expr[N, T]]
type OptionalExprList[N, T] = Option[ExprList[N, T]]

sealed trait LValue[N, T]
sealed trait RValue[N, T]
sealed trait Expr[N, T] extends RValue[N, T]
sealed trait PairElem[N, T] extends RValue[N, T]
sealed trait ArrayOrIdent[N, T] extends LValue[N, T], Expr[N, T]

case class Ident[N, T](v: N)(val pos: (Int, Int), t: T) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T]
case class ArrayElem[N, T](i: Ident[N, T], x: List[Expr[N, T]])(val pos: (Int, Int)) extends LValue[N, T], Expr[N, T], ArrayOrIdent[N, T]

case class PElem[N, T](v: PairElem[N, T])(val pos: (Int, Int), t: T) extends LValue[N, T], RValue[N, T]

case class ArrayLit[N, T](x: List[Expr[N, T]])(val pos: (Int, Int), t: T) extends RValue[N, T]
case class NewPair[N, T](e1: Expr[N, T], e2: Expr[N, T])(val pos: (Int, Int), t: T) extends RValue[N, T]
case class Call[N, T](i: Ident[N, T], x: List[Expr[N, T]])(val pos: (Int, Int), t: T) extends RValue[N, T]

case class First[N, T](v: LValue[N, T])(val pos: (Int, Int), t: T) extends PairElem[N, T]
case class Second[N, T](v: LValue[N, T])(val pos: (Int, Int), t: T) extends PairElem[N, T]

case object Ident extends IdentBridge
case object ArrayElem extends ParserBridgePos2[Ident, ExprList, ArrayElem]

case object ArrayOrIdent extends ParserBridgePos2[Ident, OptionalExprList, ArrayOrIdent] {
    override def apply[String, Unit](i: Ident[String, Unit], exprs: OptionalExprList[String, Unit])(pos: (Int, Int)): ArrayOrIdent[String, Unit] = exprs match {
        case Some(es)   => ArrayElem(i, es)(pos)
        case None       => i
    }
    
}

case object PElem extends ParserBridgePosType1[PairElem, PElem]

case object ArrayLit extends ParserBridgePosType1[ExprList, ArrayLit]
case object NewPair extends ParserBridgePosType2[Expr, Expr, NewPair]
case object Call extends ParserBridgePosType2[Ident, ExprList, Call]

case object First extends ParserBridgePosType1[LValue, PairElem]
case object Second extends ParserBridgePosType1[LValue, PairElem]

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

case class Not[N, T](e: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Neg[N, T](e: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Len[N, T](e: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Ord[N, T](e: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Chr[N, T](e: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]

case class Mul[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Div[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Mod[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Add[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Sub[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Greater[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class GreaterEq[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Less[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class LessEq[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Eq[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class NotEq[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class And[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class Or[N, T](x: Expr[N, T], y: Expr[N, T])(val pos: (Int, Int)) extends Expr[N, T]

type IntWrap[N, T] = Int
type BoolWrap[N, T] = Boolean
type CharWrap[N, T] = Char
type StrWrap[N, T] = String

case class IntLit[N, T](n: IntWrap[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class BoolLit[N, T](b: BoolWrap[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class CharLit[N, T](c: CharWrap[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class StrLit[N, T](s: StrWrap[N, T])(val pos: (Int, Int)) extends Expr[N, T]
case class PairLit[N, T]() extends Expr[N, T]

// case object UnaryOp extends ParserBridgePos1[Expr, UnaryOp]
// case object BinaryOp extends ParserBridgePos2[Expr, Expr, BinaryOp]

case object Not extends ParserBridgePos1[Expr, Expr]
case object Neg extends ParserBridgePos1[Expr, Expr]
case object Len extends ParserBridgePos1[Expr, Expr]
case object Ord extends ParserBridgePos1[Expr, Expr]
case object Chr extends ParserBridgePos1[Expr, Expr]

case object Mul extends ParserBridgePos2[Expr, Expr, Expr]
case object Div extends ParserBridgePos2[Expr, Expr, Expr]
case object Mod extends ParserBridgePos2[Expr, Expr, Expr]
case object Add extends ParserBridgePos2[Expr, Expr, Expr]
case object Sub extends ParserBridgePos2[Expr, Expr, Expr]
case object Greater extends ParserBridgePos2[Expr, Expr, Expr]
case object GreaterEq extends ParserBridgePos2[Expr, Expr, Expr]
case object Less extends ParserBridgePos2[Expr, Expr, Expr]
case object LessEq extends ParserBridgePos2[Expr, Expr, Expr]
case object Eq extends ParserBridgePos2[Expr, Expr, Expr]
case object NotEq extends ParserBridgePos2[Expr, Expr, Expr]
case object And extends ParserBridgePos2[Expr, Expr, Expr]
case object Or extends ParserBridgePos2[Expr, Expr, Expr]

case object IntLit extends ParserBridgePos1[IntWrap, IntLit]
case object BoolLit extends ParserBridgePos1[BoolWrap, BoolLit]
case object CharLit extends ParserBridgePos1[CharWrap, CharLit]
case object StrLit extends ParserBridgePos1[StrWrap, StrLit]
