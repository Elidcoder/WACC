package wacc.semantic.typecheck

import wacc.ast.{Type, ?, Pos}
import wacc.semantic.{Environment, QualifiedName}
import wacc.error.TypeErr.{OutOfScope, AlreadyDeclared}
import wacc.error.WaccErr
import java.io.File

enum Body {
    case Function(returnType: Type)
    case Main
}

class Context(var body: Body, val env: Environment, val file: File) {
    def getType(n: QualifiedName)(using pos: Pos): Type = 
        given Context = this
        n.uid match
        case -1 => 
            error(OutOfScope(n.oldName, pos))
            ?
        case -2 => 
            error(AlreadyDeclared(n.oldName, pos))
            ?
        case n => env.get(n)
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
