package wacc.backend.referencer

import wacc.ast._
import wacc.backend.Context
import wacc.semantic.QualifiedName
import wacc.backend.ir._

sealed trait Prebuilt 

case class PbMalloc() extends Prebuilt
case class PbExit() extends Prebuilt
case class PbErrOverflow() extends Prebuilt
case class PbPrint(varType: KnownType) extends Prebuilt
case class PbFree(varType: KnownType) extends Prebuilt
case class PbRead(varType: KnownType) extends Prebuilt

object referencer {
    private def getTypeSize(usedType: KnownType): DataSize = usedType match {
        case IntT() => DWORD()
        case CharT() => BYTE()
        case BoolT() => BYTE()
        case StringT() => QWORD()
        case PairT(a, b) => QWORD()
        case ArrayT(t) => QWORD()
        case _ => ???
    }

    private def buildReg(reg: Register, ty: Type): Reg[DataSize] = ty match {
        case IntT() => Reg[DWORD](reg)
        case CharT() => Reg[BYTE](reg)
        case BoolT() => Reg[BYTE](reg)
        case StringT() => Reg[QWORD](reg)
        case PairT(a, b) => Reg[QWORD](reg)
        case ArrayT(t) => Reg[QWORD](reg)
        case _ => ???
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
                ctx.addVar(param.paramId.name, buildReg(reg, param.paramType))
        )

        /* Paramters exceeding numb registers */
        func.params.reverse.drop(nonOutputRegisters.size).foreach(
            (param) => {
                ctx.addVar(param.paramId.name, Stack(ctx.getFuncOff(funcName)))
                ctx.incFuncOff(funcName, getTypeSize(param.paramType).bytes)
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
            ctx.incFuncOff(funcName,  getTypeSize(id.t).bytes)
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
