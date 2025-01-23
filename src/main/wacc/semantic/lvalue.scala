package wacc.semantic

import wacc.ast.* 

def checkSemanticsLValue(l: LValue, env: Environment): (Either[String, Type], Environment) = l match {
    case Ident(v)               => getType(env, v) match {
        case None    => (Left("Variable Referenced before declaration"), env)
        case Some(t) => (Right(t), env)
    }
    case ArrayElem(Ident(v), x) => getArrayElemType(env, v, x.length) match {
        case None    => (Left("ARRAY REFERENCED BEFORE DECLARATION"), env)
        case Some(t) => (Right(t), env)
    }
    case PElem(First(l)) => checkSemanticsLValue(l, env) match {
        case (Left(err), env) => (Left(err), env)
        case (Right(t),  env) => (Right(t), env)
    }
    case PElem(Second(l)) => checkSemanticsLValue(l, env) match {
        case (Left(err), env) => (Left(err), env)
        case (Right(t),  env) => (Right(t), env)
    }
}
