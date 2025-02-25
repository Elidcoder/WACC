package wacc.backend.generator

import wacc.ast._
import wacc.semantic.QualifiedName
import wacc.backend.ir.Instr

object generator {
    // TODO()
    def generate(prog: Program[QualifiedName, KnownType]):List[Instr] = {
        List.empty
    }
    
    // TODO()
    def generate(func: Func[QualifiedName, KnownType]):List[Instr] = {
        List.empty
    }

    // TODO()
    def generate(stmt: Stmt[QualifiedName, KnownType]):List[Instr] = {
        List.empty
    }

    // TODO()
    def generate(expr: Expr[QualifiedName, KnownType]):List[Instr] = {
        List.empty
    }
}
