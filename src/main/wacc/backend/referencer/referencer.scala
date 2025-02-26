package wacc.backend.referencer

import wacc.backend.Context
import wacc.semantic.QualifiedName

import wacc.ast._
import wacc.backend.ir._

sealed trait Prebuilt 

case class PbMalloc()      extends Prebuilt
case class PbExit()        extends Prebuilt
case class PbErrOverflow() extends Prebuilt
case class PbPrint(varType: KnownType) extends Prebuilt
case class PbFree( varType: KnownType) extends Prebuilt
case class PbRead( varType: KnownType) extends Prebuilt

object referencer {
    private val parameterRegisters: List[Register] = List(rdi, rsi, rdx, rcx, r8, r9) 

    private def getTypeSize(usedType: Type): DataSize = usedType match {
        case IntT()          => DWORD()
        case CharT()         => BYTE()
        case BoolT()         => BYTE()
        case StringT()       => QWORD()
        case PairT(a, b)     => QWORD()
        case ArrayT(t)       => QWORD()
        case FuncT(retTy, _) => getTypeSize(retTy)
        case _               => QWORD()
    }

    private def buildReg(reg: Register, ty: Type): Reg[DataSize] = ty match {
        case IntT()          => Reg[DWORD](reg)
        case CharT()         => Reg[BYTE](reg)
        case BoolT()         => Reg[BYTE](reg)
        case StringT()       => Reg[QWORD](reg)
        case PairT(a, b)     => Reg[QWORD](reg)
        case ArrayT(t)       => Reg[QWORD](reg)
        case FuncT(retTy, _) => buildReg(reg, retTy)
        case _               => ???
    }

    /* Creates a stack reference for a new variable and increases the function offset */
    private def addVarToContext(id: Ident[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = {
        ctx.addVar(id.name, Stack(ctx.getFuncOff(funcName)))
        ctx.incFuncOff(funcName, getTypeSize(id.t).bytes)
    }

    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Program[QualifiedName, KnownType] = {
        given funcName:QualifiedName = QualifiedName("main", -1)

        /* Reference each function defined at the top. */
        prog.funcs.foreach(reference)
        
        /* Reference the main function. */
        reference(prog.stmts)

        /* Return the given program to allow chaining in main. */
        prog
    }
    
    private def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        given funcName:QualifiedName = func.id.name

        /* Parameter made into registers */
        func.params.zip(parameterRegisters).foreach(
            (param, reg) => 
                ctx.addVar(param.paramId.name, buildReg(reg, param.paramType))
        )

        /* Paramters exceeding numb registers */
        func.params.reverse.drop(parameterRegisters.size).foreach(
            (param) => addVarToContext(param.paramId)
        )
        
        /* Handle statements*/
        reference(func.stmts)
    }

    private def reference(funcStmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, funcName: QualifiedName): Unit
        = funcStmts.foreach(reference)

    private def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = stmt match {
        case NewAss(_, id, _) => addVarToContext(id)

        /* Check nested statements. */
        case If(cond, ifStmts, elseStmts) => {reference(ifStmts); reference(elseStmts)}
        case While(cond, subStmts) => reference(subStmts)
        case Nest(stmts) => reference(stmts)

        case _ => {}
    }
}
