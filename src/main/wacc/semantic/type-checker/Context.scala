package wacc.semantic.typecheck

import wacc.ast.{Type, ?}
import wacc.semantic.Environment
import wacc.error.TypeErr.{OutOfScope, AlreadyDeclared}
import wacc.error.{WaccErr, R2}
import java.io.File

enum Body {
    case Function(returnType: Type)
    case Main
}

class Context(var body: Body, val env: Environment, val file: File) {
    def getType(uid: Int): Type = 
        given Context = this
        uid match
        case -1 => 
            // TODO(Add the ID parameter to the OutOfScope Error when it can be extracted from uid)
            //error(OutOfScope(new R2(0, 0)))
            ?
        case -2 => 
            // TODO(Add the ID parameter to the AlreadyDeclared Error when it can be extracted from uid)
            //error(AlreadyDeclared(new R2(0,0)))
            ?
        case n => env.get(n)
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
