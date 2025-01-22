package wacc.semantic

import wacc.ast.* 

def checkSemanticsWhile(e: Expr, s: List[Stmt], env: Environment): (Option[String], Environment) = checkSemanticsExpr(e, env) match {
    case (Left(err), env)      => checkSemanticsStmts(s, env) match {
        case (Some(err2), env) => (Some(err + err2), env)
        case (None,       env) => (Some(err), env)
    }
    case (Right(BoolT()), env) => checkSemanticsStmts(s, env) match {
        case (Some(err2), env) => (Some(err2), env)
        case (None,      env)  => (None, env)
    }
    case (_, env)              => checkSemanticsStmts(s, env) match {
        case (Some(err2), env) => (Some("CONDITION MUST BE A BOOLEAN EXPRESSION" + err2), env)
        case (None,      env)  => (Some("CONDITION MUST BE A BOOLEAN EXPRESSION"), env)
    }
}
