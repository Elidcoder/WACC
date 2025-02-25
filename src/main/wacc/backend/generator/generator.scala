package wacc.backend.generator

import wacc.ast._
import wacc.semantic.QualifiedName
import wacc.backend.ir.{Instr, Block}

object generator {
    def generate(prog: Program[QualifiedName, KnownType]): List[Block] = {
        ???
    }
    
    def generate(func: Func[QualifiedName, KnownType]): Block = {
        ???
    }

    def generate(stmts: List[Stmt[QualifiedName, KnownType]]):List[Instr] = {
        ???
    }

    def generate(stmt: Stmt[QualifiedName, KnownType]):List[Instr] = {
        ???
    }
}
