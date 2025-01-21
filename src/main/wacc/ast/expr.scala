package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum Expr {
    case Not(e: Expr)
    case Neg(e: Expr)
    case Len(e: Expr)
    case Ord(e: Expr)
    case Chr(e: Expr)

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

    case IntLit(n: BigInt)
    case BoolLit(b: Boolean)
    case CharLit(c: Char)
    case StrLit(s: String)
    case PairLit
    case Ident(v: String)
    case ArrayLit(v: String, x: List[Expr])
}

object Expr {
    object Not extends ParserBridge1[Expr, Expr]
    object Neg extends ParserBridge1[Expr, Expr]
    object Len extends ParserBridge1[Expr, Expr]
    object Ord extends ParserBridge1[Expr, Expr]
    object Chr extends ParserBridge1[Expr, Expr]

    object Mul extends ParserBridge2[Expr, Expr, Expr]
    object Div extends ParserBridge2[Expr, Expr, Expr]
    object Mod extends ParserBridge2[Expr, Expr, Expr]
    object Add extends ParserBridge2[Expr, Expr, Expr]
    object Sub extends ParserBridge2[Expr, Expr, Expr]
    object Greater extends ParserBridge2[Expr, Expr, Expr]
    object GreaterEq extends ParserBridge2[Expr, Expr, Expr]
    object Less extends ParserBridge2[Expr, Expr, Expr]
    object LessEq extends ParserBridge2[Expr, Expr, Expr]
    object Eq extends ParserBridge2[Expr, Expr, Expr]
    object NotEq extends ParserBridge2[Expr, Expr, Expr]
    object And extends ParserBridge2[Expr, Expr, Expr]
    object Or extends ParserBridge2[Expr, Expr, Expr]

    object IntLit extends ParserBridge1[BigInt, Expr]
    object BoolLit extends ParserBridge1[Boolean, Expr]
    object CharLit extends ParserBridge1[Char, Expr]
    object StrLit extends ParserBridge1[String, Expr]
    object Ident extends ParserBridge1[String, Expr]
    object ArrayLit extends ParserBridge2[String, List[Expr], Expr]
}


