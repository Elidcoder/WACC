package wacc.ast

import parsley.generic.ParserBridge1

enum Stmt {
    case Skip
    case Exit(e: Expr)
}

object Stmt {
    object Exit extends ParserBridge1[Expr, Stmt]
}