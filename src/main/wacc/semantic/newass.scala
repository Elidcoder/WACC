package wacc.semantic

import wacc.ast.* 

def checkSemanticsNewAss(t: Type, v: String, r: RValue, env: Environment): (Option[String], Environment) = checkSemanticsRValue(r, env) match {
    case (Left(err), env)    => (Some(err), env)
    case (Right(rtype), scope::envs) => if rtype.getClass == t.getClass then
        (None, (((v -> rtype) :: scope) :: envs)) else
        (Some("TYPES ARE INCOMPATIBLE"), env)
    case (_, Nil) => throw new IllegalStateException("WHAT THE FUCK THIS IS NOT MEANT TO BE EMPTY HOLLY SHIT")
}
