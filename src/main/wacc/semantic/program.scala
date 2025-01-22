package wacc.semantic

import wacc.ast.*

def checkSemanticsProgram(p: Program): Option[String] = checkFuncsSemantics(p.fs, List.empty) match {
    case (Some(err), env) => checkSemanticsStmts(p.x, env) match {
        case (Some(err2), env) => Some(err + err2)
        case _ => Some(err)
    }
    case (None, env) => checkSemanticsStmts(p.x, env) match {
        case (Some(err), env) => Some(err)
        case _ => None
    }
}