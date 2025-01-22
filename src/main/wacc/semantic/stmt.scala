package wacc.semantic

import wacc.ast.*

def checkSemanticsStmts(xs: List[Stmt], env: Environment): (Option[String], Environment) = xs match {
    case Nil   => (None, env)
    case x::xs => checkSemanticsStmt(x, env) match {
        case (Some(err), env) => checkSemanticsStmts(xs, env) match {
            case (Some(err2), env) => (Some(err + err2), env)
            case (None,    env) => (Some(err),        env)
        }
        case (None,   env) => checkSemanticsStmts(xs, env) match {
            case (Some(err),  env) => (Some(err), env)
            case (None,    env) => (None,   env)
        }
    }
}

def checkSemanticsStmt(s: Stmt, env: Environment): (Option[String], Environment) = s match {
    case Skip()                                      => (None, env)
    case NewAss(t: Type, Ident(v), r: RValue)        => checkSemanticsNewAss(t, v, r, env)
    case Assign(l: LValue, r: RValue)                => checkSemanticsAss(l, r, env)
    case Free(e: Expr)                               => checkSemanticsFree(e, env)
    case Return(e: Expr)                             => checkSemanticsReturn(e, env)
    case Exit(e: Expr)                               => checkSemanticsExit(e, env)
    case Print(e: Expr)                              => checkSemanticsPrint(e, env)
    case PrintLn(e: Expr)                            => checkSemanticsPrint(e, env)
    case If(e: Expr, s1: List[Stmt], s2: List[Stmt]) => checkSemanticsIf(e, s1, s2, env)
    case While(e: Expr, s: List[Stmt])               => checkSemanticsWhile(e, s, env)
    case Nest(s: List[Stmt])                         => checkSemanticsNest(s, env)
    case Read(l: LValue)                             => checkSemanticsRead(l, env)
}
