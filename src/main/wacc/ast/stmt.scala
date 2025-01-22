package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2, ParserBridge3}

enum Stmt {
    case Skip
    case NewAss(t: Type, v: String, r: RValue)
    case Assign(l: LValue, r: RValue)
    case Read(l: LValue)
    case Free(e: Expr)
    case Return(e: Expr)
    case Exit(e: Expr)
    case Print(e: Expr)
    case PrintLn(e: Expr)
    case If(e: Expr, s1: List[Stmt], s2: List[Stmt])
    case While(e: Expr, s: List[Stmt])
    case Nest(s: List[Stmt])
}

object Stmt {
    object NewAss extends ParserBridge3[Type, String, RValue, Stmt]
    object Assign extends ParserBridge2[LValue, RValue, Stmt]
    object Read extends ParserBridge1[LValue, Stmt]
    object Free extends ParserBridge1[Expr, Stmt]
    object Return extends ParserBridge1[Expr, Stmt]
    object Exit extends ParserBridge1[Expr, Stmt]
    object Print extends ParserBridge1[Expr, Stmt]
    object PrintLn extends ParserBridge1[Expr, Stmt]
    object If extends ParserBridge3[Expr, List[Stmt], List[Stmt], Stmt]
    object While extends ParserBridge2[Expr, List[Stmt], Stmt]
    object Nest extends ParserBridge1[List[Stmt], Stmt]
}