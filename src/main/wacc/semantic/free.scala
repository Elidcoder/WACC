package wacc.semantic

import wacc.ast.*

def checkSemanticsFree(e: Expr, env: Environment): (Option[String], Environment) = e match {
    case Ident(v: String) => getType(env, v) match {
        case None             => (Some("VARIABLE IS NOT DECLARED"), env)
        case Some(ArrayT(_))  => (None, env)
        case Some(PairT(_,_)) => (None, env)
        case _                => (Some("INVALID TYPE FOR FREE: MUST BE ARRAY OR PAIR"), env)
    }
    case ArrayElem(Ident(v: String), x: List[Expr]) => getType(env, v) match {
        case None                     => checkSemanticsExprs(x, env) match { 
            case (None, env)      => (Some("ARRAY IS NOT DECLARED"), env)
            case (Some(err), env) => (Some("ARRAY IS NOT DECLARED" + err), env)
        }
        case Some(ArrayT(ArrayT(_)))  => checkSemanticsExprs(x, env)
        case Some(ArrayT(PairT(_,_))) => checkSemanticsExprs(x, env)
        case _                        => checkSemanticsExprs(x, env) match {
            case (None, env)      => (Some("INVALID TYPE FOR FREE: MUST BE ARRAY OR PAIR"), env)
            case (Some(err), env) => (Some("INVALID TYPE FOR FREE: MUST BE ARRAY OR PAIR" + err), env)
        }
    }
    case _                            => (Some("INVALID ARGUMENT FOR FREE"), env)
}
