package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

sealed trait LValue
sealed trait RValue
sealed trait Expr extends RValue
sealed trait PairElem extends RValue

case class Ident(v: String) extends LValue, Expr
case class ArrayElem(i: Ident, x: List[Expr]) extends LValue, Expr

case class PElem(v: PairElem) extends LValue, RValue

case class ArrayLit(x: List[Expr]) extends RValue
case class NewPair(e1: Expr, e2: Expr) extends RValue
case class Call(i: Ident, x: List[Expr]) extends RValue

case class First(v: LValue) extends PairElem
case class Second(v: LValue) extends PairElem

case object Ident extends ParserBridge1[String, Ident]
case object ArrayElem extends ParserBridge2[Ident, List[Expr], ArrayElem]

case object PElem extends ParserBridge1[PairElem, PElem]

case object ArrayLit extends ParserBridge1[List[Expr], ArrayLit]
case object NewPair extends ParserBridge2[Expr, Expr, NewPair]
case object Call extends ParserBridge2[Ident, List[Expr], Call]

case object First extends ParserBridge1[LValue, PairElem]
case object Second extends ParserBridge1[LValue, PairElem]

case class Not(e: Expr) extends Expr
case class Neg(e: Expr) extends Expr
case class Len(e: Expr) extends Expr
case class Ord(e: Expr) extends Expr
case class Chr(e: Expr) extends Expr

case class Mul(x: Expr, y: Expr) extends Expr
case class Div(x: Expr, y: Expr) extends Expr
case class Mod(x: Expr, y: Expr) extends Expr
case class Add(x: Expr, y: Expr) extends Expr
case class Sub(x: Expr, y: Expr) extends Expr
case class Greater(x: Expr, y: Expr) extends Expr
case class GreaterEq(x: Expr, y: Expr) extends Expr
case class Less(x: Expr, y: Expr) extends Expr
case class LessEq(x: Expr, y: Expr) extends Expr
case class Eq(x: Expr, y: Expr) extends Expr
case class NotEq(x: Expr, y: Expr) extends Expr
case class And(x: Expr, y: Expr) extends Expr
case class Or(x: Expr, y: Expr) extends Expr

case class IntLit(n: BigInt) extends Expr
case class BoolLit(b: Boolean) extends Expr
case class CharLit(c: Char) extends Expr
case class StrLit(s: String) extends Expr
case class PairLit() extends Expr


case object Not extends ParserBridge1[Expr, Expr]
case object Neg extends ParserBridge1[Expr, Expr]
case object Len extends ParserBridge1[Expr, Expr]
case object Ord extends ParserBridge1[Expr, Expr]
case object Chr extends ParserBridge1[Expr, Expr]

case object Mul extends ParserBridge2[Expr, Expr, Expr]
case object Div extends ParserBridge2[Expr, Expr, Expr]
case object Mod extends ParserBridge2[Expr, Expr, Expr]
case object Add extends ParserBridge2[Expr, Expr, Expr]
case object Sub extends ParserBridge2[Expr, Expr, Expr]
case object Greater extends ParserBridge2[Expr, Expr, Expr]
case object GreaterEq extends ParserBridge2[Expr, Expr, Expr]
case object Less extends ParserBridge2[Expr, Expr, Expr]
case object LessEq extends ParserBridge2[Expr, Expr, Expr]
case object Eq extends ParserBridge2[Expr, Expr, Expr]
case object NotEq extends ParserBridge2[Expr, Expr, Expr]
case object And extends ParserBridge2[Expr, Expr, Expr]
case object Or extends ParserBridge2[Expr, Expr, Expr]

case object IntLit extends ParserBridge1[BigInt, IntLit]
case object BoolLit extends ParserBridge1[Boolean, BoolLit]
case object CharLit extends ParserBridge1[Char, CharLit]
case object StrLit extends ParserBridge1[String, StrLit]
