package wacc.backend.referencing

import wacc.backend.Context
import wacc.semantic.QualifiedName

import wacc.ast._
import wacc.backend.ir._
import wacc.backend.generator.STACK_PTR_REG
import wacc.backend.generator.BASE_PTR_REG

object referencer {
    /* An ordered list of registers used for parameters. */
    val parameterRegisters: List[Register] = List(RDI, RSI, RDX, RCX, R8, R9) 

    /* Stores the initial offset for any function due to the initial operations. */
    private val INITIAL_PARAM_OFF = 16

    /* Returns the dataSize matching a given type, 
     * we have a static guarantee from frontend _ never occurs. */
    def getTypeSize(usedType: Type): DataSize = usedType match {
        case IntT()          => DWORD
        case CharT()         => BYTE
        case BoolT()         => BYTE
        case StringT()       => QWORD
        case PairT(a, b)     => QWORD
        case ArrayT(t)       => QWORD
        case FuncT(retTy, _) => getTypeSize(retTy)
        case _               => QWORD
    }

    /* Creates a stack reference for a new variable and increases the function offset */
    protected [referencing] def addVarToContext(id: Ident[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = {
        ctx.incFuncOff(funcName, getTypeSize(id.t).bytes)
        ctx.addVar(id.name, MemOff(BASE_PTR_REG, -ctx.getFuncOff(funcName)))
    }

    /* Reference the variables used in a program. */
    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Program[QualifiedName, KnownType] = {
        given mainName:QualifiedName = QualifiedName("main", -1)

        /* Reference each function defined at the top. */
        prog.funcs.foreach(reference)
        
        /* Reference the main function. */
        reference(prog.stmts)
        ctx.mainOffset = ctx.getFuncOff(mainName)

        /* Return the given program to allow chaining in main. */
        prog
    }
    
    /* Reference the variables used in a function as well as its parameters. */
    protected [referencing] def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        given funcName:QualifiedName = func.id.name

        /* Parameter made into registers */
        func.params.zip(parameterRegisters).foreach(
            (param, reg) => 
                ctx.addVar(param.paramId.name, Reg(reg))
        )

        /* Paramters exceeding numb registers */
        var offset = INITIAL_PARAM_OFF
        func.params.reverse.drop(parameterRegisters.size).foreach( (param) => 
            ctx.addVar(param.paramId.name, MemOff(STACK_PTR_REG, offset))
            offset += getTypeSize(param.paramId.t).bytes
        )
        
        /* Handle statements*/
        reference(func.stmts)
    }

    /* Call reference on all stmts in a list of stmts. */
    protected [referencing] def reference(stmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, funcName: QualifiedName): Unit
        = stmts.foreach(reference)

    /* Reference the variables used in a stmt. */
    protected [referencing] def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = stmt match {
        case NewAss(_, id, rval) => {
            addVarToContext(id)
        }

        /* Check nested statements. */
        case If(cond, ifStmts, elseStmts) => {reference(ifStmts); reference(elseStmts)}
        case While(cond, subStmts) => reference(subStmts)
        case Nest(stmts) => reference(stmts)

        case _ => {}
    }
}
