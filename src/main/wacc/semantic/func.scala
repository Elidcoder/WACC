package wacc.semantic

import wacc.ast.*

def checkFuncsSemantics(fs: List[Func], env: Environment): (Option[String], Environment) = fs match {
    case Nil => (None, env)
    case f::fs => checkSemantics(f, env) match {
        case (Some(err), env) => checkFuncsSemantics(fs, env) match {
            case (Some(err2), env) => (Some(err + err2), env)
            case (None,    env) => (Some(err),        env)
        }
        case (None,   env) => checkFuncsSemantics(fs, env) match {
            case (Some(err),  env) => (Some(err), env)
            case (None,    env) => (None,   env)
        }
    }
}

def checkSemantics(f: Func, env: Environment): (Option[String], Environment) = ???