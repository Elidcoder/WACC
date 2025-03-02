package wacc.backend.referencing

import wacc.backend.Context
import wacc.semantic.QualifiedName

import wacc.ast._
import wacc.backend.ir._

object referencer {
    /* Stores the initial offset for any function due to the initial operations. */
    private val INITIAL_PARAM_OFF = 2 * QWORD.bytes

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
        ctx.addVar(id.name, Mem(BASE_PTR_REG, -ctx.getFuncOff(funcName)))
    }

    /* Reference the variables used in a program. */
    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Program[QualifiedName, KnownType] = {
        given mainName:QualifiedName = ctx.mainName

        /* Reference each function defined at the top. */
        prog.funcs.foreach(reference)
        
        /* Reference the main function. */
        reference(prog.stmts)

        /* Return the given program to allow chaining in main. */
        prog
    }
    
    /* Reference the variables used in a function as well as its parameters. */
    protected [referencing] def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        given funcName:QualifiedName = func.id.name

        /* Paramters exceeding numb registers */
        val offset = func.params.foldRight(0)( (param, acc) => 
            ctx.addVar(param.paramId.name, Mem(BASE_PTR_REG, acc + INITIAL_PARAM_OFF))
            acc + getTypeSize(param.paramId.t).bytes
        )
        ctx.addFuncParamOff(funcName, offset)
        
        /* Handle function body */
        reference(func.stmts)
    }

    /* Call reference on all stmts in a list of stmts. */
    protected [referencing] def reference(stmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, funcName: QualifiedName): Unit
        = stmts.foreach(reference)

    /* Reference the variables used in a stmt. */
    protected [referencing] def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = stmt match {
        case NewAss(_, id, rval) => addVarToContext(id)

        /* Check nested statements. */
        case If(_, ifStmts, elseStmts) => {
            reference(ifStmts)
            reference(elseStmts)
        }
        case While(_, subStmts) => reference(subStmts)
        case Nest(subStmts)     => reference(subStmts)

        case _ => {}
    }
}
