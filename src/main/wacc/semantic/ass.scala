package wacc.semantic

import wacc.ast.* 

def checkSemanticsAss(l: LValue, r: RValue, env: Environment): (Option[String], Environment) = checkSemanticsLValue(l, env) match {
    case (Left(err), env) => checkSemanticsRValue(r, env) match {
        case (Right(_),  env) => (Some(err), env)
        case (Left(err2),env) => (Some(err + err2), env)
    }
    case (Right(t), env) => checkSemanticsRValue(r, env) match {
        case (Left(err), env) => (Some(err), env)
        case (Right(rt), env) => if t.getClass == rt.getClass
                    then (None, env)
                    else (Some("TYPES DON'T MATCH"), env)
    }
}

