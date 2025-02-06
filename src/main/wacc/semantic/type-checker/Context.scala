package wacc.semantic.typecheck

import wacc.ast.{Type, ?, Pos}
import wacc.semantic.Environment
import wacc.error.SemanticWaccErr.{OutOfScope, AlreadyDeclared}
import wacc.error.WaccErr
import java.io.File

enum Body {
    case Function(returnType: Type)
    case Main
}

class Context(var body: Body, val env: Environment, val file: File) {
    def getType(uid: Int)(using pos: Pos): Type = 
        given Context = this
        uid match
        case -1 => 
            // TODO(Scope error: Undeclared)
            error(OutOfScope(pos))
            ?
        case -2 => 
            // TODO(Scope error: Already declared)
            error(AlreadyDeclared(pos))
            ?
        case n => env.get(n)
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
