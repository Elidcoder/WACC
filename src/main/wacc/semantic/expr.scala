package wacc.semantic

import wacc.ast.*

// there's probably some idiomatic way to do this using predefined functions
def checkSemanticsExprs(xs: List[Expr], env: Environment): (Option[String], Environment) = xs match {
    case Nil   => (None, env)
    case x::xs => checkSemanticsExpr(x, env) match {
        case (Left(err), env) => checkSemanticsExprs(xs, env) match {
            case (Some(err2), env) => (Some(err + err2), env)
            case (None,    env) => (Some(err),        env)
        }
        case (Right(t),   env) => checkSemanticsExprs(xs, env) match {
            case (Some(err),  env) => (Some(err), env)
            case (None,    env)    => (None,   env)
        }
    }
}

def checkSemanticsExpr(x: Expr, env: Environment): (Either[String, Type], Environment) = ???