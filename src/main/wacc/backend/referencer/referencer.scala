package wacc.backend.referencer

import wacc.backend.Context
import wacc.semantic.QualifiedName

import wacc.ast._
import wacc.backend.ir._
import wacc.backend.generator.STACK_PTR_REG

object referencer {
    /* An ordered list of registers used for parameters. */
    private val parameterRegisters: List[Register] = List(RDI, RSI, RDX, RCX, R8, R9) 

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
    private def addVarToContext(id: Ident[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = {
        ctx.addVar(id.name, MemOff(STACK_PTR_REG, ctx.getFuncOff(funcName)))
        ctx.incFuncOff(funcName, getTypeSize(id.t).bytes)
    }

    /* Reference the variables used in a program as well as storing any strings. */
    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Program[QualifiedName, KnownType] = {
        given funcName:QualifiedName = QualifiedName("main", -1)

        /* Reference each function defined at the top. */
        prog.funcs.foreach(reference)
        
        /* Reference the main function. */
        reference(prog.stmts)

        /* Return the given program to allow chaining in main. */
        prog
    }
    
    /* Reference the variables used in a function as well as its parameters. */
    private def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        given funcName:QualifiedName = func.id.name

        /* Parameter made into registers */
        func.params.zip(parameterRegisters).foreach(
            (param, reg) => 
                ctx.addVar(param.paramId.name, Reg(reg))
        )

        /* Paramters exceeding numb registers */
        func.params.reverse.drop(parameterRegisters.size).foreach(
            (param) => addVarToContext(param.paramId)
        )
        
        /* Handle statements*/
        reference(func.stmts)
    }

    /* Call reference on all stmts in a list of stmts. */
    private def reference(stmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, funcName: QualifiedName): Unit
        = stmts.foreach(reference)

    /* Reference the variables used in a stmt as well as storing any strings. */
    private def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = stmt match {
        case NewAss(_, id, rval) => {
            addVarToContext(id)
            searchForStrings(rval)
        }
        case Assign(_, rVal) => searchForStrings(rVal) 

        /* Check nested statements. */
        case If(cond, ifStmts, elseStmts) => {reference(ifStmts); reference(elseStmts)}
        case While(cond, subStmts) => reference(subStmts)
        case Nest(stmts) => reference(stmts)

        case _ => {}
    }

    /* Search through a rValue. recursively looking for strings. */
    private def searchForStrings(rval: RValue[QualifiedName, KnownType])(using ctx: Context, funcName: QualifiedName): Unit = rval match {
        case Eq(lhsExpr, rhsExpr) => {
            searchForStrings(lhsExpr)
            searchForStrings(rhsExpr)
        }
        case NotEq(lhsExpr, rhsExpr) => {
            searchForStrings(lhsExpr)
            searchForStrings(rhsExpr)
        }
        case StrLit(str) => {
            ctx.addRoData(str, ctx.nextStringLabel())
        }
        case _ => {}
    }
}
