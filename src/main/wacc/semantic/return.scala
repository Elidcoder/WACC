package wacc.semantic

import wacc.ast.*

def checkSemanticsReturn(e: Expr, env: Environment):(Option[String], Environment) = checkSemanticsExpr(e, env) match {
    case (Left(err), env) => (Some(err), env)
    case (Right(t), env)  => ???
}
