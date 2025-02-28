package wacc.backend.referencing

import wacc.backend.Context
import wacc.semantic.QualifiedName

import wacc.ast._
import wacc.backend.ir._

object referencer {


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

        /* Paramters exceeding numb registers */
        var offset = 0
        func.params.reverse.foreach(
            (param) => 
                ctx.addVar(param.paramId.name, MemOff(BASE_PTR_REG, offset + INITIAL_PARAM_OFF))
                offset += getTypeSize(param.paramId.t).bytes
        )
        ctx.addFuncParamOff(funcName, offset)
        
        /* Handle statements*/
        reference(func.stmts)
    }

    /* Call reference on all stmts in a list of stmts. */
    protected [referencing] def reference(stmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, funcName: QualifiedName): Unit
        = stmts.foreach(reference)

    /* Reference the variables used in a stmt. */
    protected [referencing] def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = stmt match {
        case NewAss(_, id, rval) => addVarToContext(id)

        /* Check nested statements. */
        case If(cond, ifStmts, elseStmts) => {reference(ifStmts); reference(elseStmts)}
        case While(cond, subStmts) => reference(subStmts)
        case Nest(stmts) => reference(stmts)

        case _ => {}
    }
}
