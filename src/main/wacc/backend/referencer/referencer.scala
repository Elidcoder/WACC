package wacc.backend.referencer

import wacc.ast._
import wacc.backend.Context
import wacc.semantic.QualifiedName
import wacc.backend.ir.{DataSize, Reg, Stack, nonOutputRegisters}

sealed trait Prebuilt 

case class PbMalloc() extends Prebuilt
case class PbExit() extends Prebuilt
case class PbErrOverflow() extends Prebuilt
case class PbPrint(varType: KnownType) extends Prebuilt
case class PbFree(varType: KnownType) extends Prebuilt
case class PbRead(varType: KnownType) extends Prebuilt

object referencer {
    private def getTypeSize(usedType: KnownType): DataSize = usedType match {
        case IntT() => DataSize.DWORD
        case CharT() => DataSize.BYTE
        case BoolT() => DataSize.BYTE

        // TOOD(Figure out better)
        case StringT() => DataSize.BYTE //*n
        case PairT(a, b) => DataSize.QWORD //+ getTypeSize(a) + getTypeSize(b)
        case ArrayT(t) => DataSize.DWORD //+ getTypeSize(t)//*n
        case _ => ???
    }

    private def getTypeSizeBytes(usedType: KnownType): Int = {
        getTypeSize(usedType)// convert to int
        return 10
    }

    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Unit = {
        prog.funcs.foreach(reference)

        // go through the statements as main prog.stmts
    }
    
    private def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        given funcName:QualifiedName = func.id.name

        /* Parameter made into registers */
        func.params.zip(nonOutputRegisters).foreach(
            (param, reg) => 
                ctx.addVar(param.paramId.name, Reg(reg, getTypeSize(param.paramType)))
        )

        /* Paramters exceeding numb registers */
        func.params.reverse.drop(nonOutputRegisters.size).foreach(
            (param) => {
                ctx.addVar(param.paramId.name, Stack(ctx.getFuncOff(funcName)))
                ctx.incFuncOff(funcName, getTypeSizeBytes(param.paramType))
            }
        )
        
        /* Handle statements*/
        reference(func.stmts)
    }

    private def reference(funcStmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, funcName: QualifiedName): Unit
        = funcStmts.foreach(reference)

    private def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = stmt match {
        case NewAss(_, id, _) => {
            ctx.addVar(id.name, Stack(ctx.getFuncOff(funcName)))
            ctx.incFuncOff(funcName, getTypeSizeBytes(id.t))
        }
        case If(cond, ifStmts, elseStmts) => {
            reference(ifStmts)
            reference(elseStmts)
        }
        case While(cond, subStmts) => reference(subStmts)
        case Nest(stmts) => reference(stmts)
        case Return(expr) => ???
        case _ => {}
    }
}
