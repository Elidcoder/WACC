package wacc.semantic.typedAst

sealed trait LValue
sealed trait RValue
sealed trait Expr extends RValue
sealed trait PairElem extends RValue

case class Ident(oldName: String, uid: Int, t: Type) extends LValue, Expr 
object Ident {
    def apply(v: wacc.semantic.renamedAst.Ident) =
        new Ident(v.oldName, v.uid, wacc.semantic.typecheck.check(v.t))
}
case class ArrayElem(i: Ident, x: List[Expr]) extends LValue, Expr

case class PElem(v: PairElem, t: Type) extends LValue, RValue

case class ArrayLit(x: List[Expr], t: Type) extends RValue
case class NewPair(e1: Expr, e2: Expr, t: Type) extends RValue
case class Call(i: Ident, x: List[Expr], t: Type) extends RValue

case class First(v: LValue, t: Type) extends PairElem
case class Second(v: LValue, t: Type) extends PairElem

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

case class IntLit(n: Int) extends Expr
case class BoolLit(b: Boolean) extends Expr
case class CharLit(c: Char) extends Expr
case class StrLit(s: String) extends Expr
case class PairLit() extends Expr
