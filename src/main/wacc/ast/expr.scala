package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum Expr {
    case Unary(u: UnaryOp)
    case Binary(b: BinaryOp)
    case Lit(a: Atom)
}

enum UnaryOp {
    case Not(e: Expr)
    case Neg(e: Expr)
    case Len(e: Expr)
    case Ord(e: Expr)
    case Chr(e: Expr)
}

enum BinaryOp {
    case Mul(x: Expr, y: Expr)
    case Div(x: Expr, y: Expr)
    case Mod(x: Expr, y: Expr)
    case Add(x: Expr, y: Expr)
    case Sub(x: Expr, y: Expr)
    case Greater(x: Expr, y: Expr)
    case GreaterEq(x: Expr, y: Expr)
    case Less(x: Expr, y: Expr)
    case LessEq(x: Expr, y: Expr)
    case Eq(x: Expr, y: Expr)
    case NotEq(x: Expr, y: Expr)
    case And(x: Expr, y: Expr)
    case Or(x: Expr, y: Expr)
}

enum Atom {
    case IntLit(n: BigInt)
    case BoolLit(b: Boolean)
    case CharLit(c: Char)
    case StrLit(s: String)
    case PairLit
    case Ident(v: String)
    case ArrayLit(v: String, x: List[Expr])
}

object Expr {
    object Unary extends ParserBridge1[UnaryOp, Expr]
    object Binary extends ParserBridge1[BinaryOp, Expr]
    object Lit extends ParserBridge1[Atom, Expr]
}

object Atom {
    object IntLit extends ParserBridge1[BigInt, Atom]
    object BoolLit extends ParserBridge1[Boolean, Atom]
    object CharLit extends ParserBridge1[Char, Atom]
    object StrLit extends ParserBridge1[String, Atom]
    object Ident extends ParserBridge1[String, Atom]
    object ArrayLit extends ParserBridge2[String, List[Expr], Atom]
}


