package wacc.semantic.typecheck

import wacc.ast.{Type, ?, Pos}
import wacc.error.WaccErr
import wacc.error.TypeErr.{OutOfScope, AlreadyDeclared, FuncAlreadyDeclared}
import wacc.semantic.{Environment, QualifiedName, Undeclared, AlreadyDeclaredInScope, FuncAlreadyDeclaredInScope}

import java.io.File

enum Body {
    case Function(returnType: Type)
    case Main
}

class Context(var body: Body, val env: Environment, val file: File) {
    def getType(n: QualifiedName)(using pos: Pos): Type = 
        given Context = this
        n.uid match
        case Undeclared => 
            error(OutOfScope(n.oldName))
            ?
        case AlreadyDeclaredInScope => 
            error(AlreadyDeclared(n.oldName))
            ?
        case FuncAlreadyDeclaredInScope =>
            error(FuncAlreadyDeclared(n.oldName))
            ?
        case n => env.get(n)
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
