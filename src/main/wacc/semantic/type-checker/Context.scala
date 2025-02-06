package wacc.semantic.typecheck

import wacc.ast.{SemType, ?, Pos}
import wacc.error.WaccErr
import wacc.error.TypeErr.{OutOfScope, AlreadyDeclared, FuncAlreadyDeclared}
import wacc.semantic.{Environment, QualifiedName, UID_UNDECLARED, UID_ALREADY_IN_SCOPE, UID_FUNC_ALREADY_IN_SCOPE}

import java.io.File

enum Body {
    case Function(returnType: SemType)
    case Main
}

class Context(var body: Body, val env: Environment, val file: File) {
    def getType(n: QualifiedName)(using pos: Pos): SemType = 
        given Context = this
        n.uid match
        case UID_UNDECLARED => 
            error(OutOfScope(n.oldName))
            ?
        case UID_ALREADY_IN_SCOPE => 
            error(AlreadyDeclared(n.oldName))
            ?
        case UID_FUNC_ALREADY_IN_SCOPE =>
            error(FuncAlreadyDeclared(n.oldName))
            ?
        case n => env.get(n)
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
