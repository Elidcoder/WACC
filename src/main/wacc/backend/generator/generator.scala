package wacc.backend.generator

import wacc.ast._
import wacc.semantic.QualifiedName
import wacc.backend.ir.Instr
import wacc.backend.ir.Block

object generator {
    // TODO()
    def generate(prog: Program[QualifiedName, KnownType]): List[Block] = {
        ???
    }
    
    // TODO()
    def generate(func: Func[QualifiedName, KnownType]): Block = {
        ???
    }

    // TODO()
    def generate(stmts: List[Stmt[QualifiedName, KnownType]]):List[Instr] = {
        ???
    }

    // TODO()
    def generate(stmt: Stmt[QualifiedName, KnownType]):List[Instr] = {
        ???
    }
}
