package wacc.backend.referencer

import wacc.ast._
import wacc.backend.Context
import wacc.semantic.QualifiedName
import wacc.backend.ir.{DataSize, Reg, nonOutputRegisters}

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
    }


    /*private def getExprType(expr: Expr[QualifiedName, KnownType]): KnownType = expr match {
        case And(_, _) | Eq(_, _)| Greater(_, _) | GreaterEq(_, _) 
            | NotEq(_, _) | BoolLit(_) | Or(_, _) | Not(_) => BoolT()
        case CharLit(_) |  Chr(_) => CharT()
        case StrLit(str) => StringT()
        case id@Ident(_) => id.t

        case ArrayElem(id, exprs) => {
            val arrElemType = if (exprs.isEmpty) IntT() else getExprType(exprs.head)
            ArrayT(arrElemType)
        }
        case _: ArrayOrIdent[?, ?] => ???
        case PairLit() => ???

        // may not always be int cos of add strings
        case _ => IntT()
    }*/

    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Unit = {
        prog.funcs.foreach(reference)

        // go through the statements as main prog.stmts
    }
    
    def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        /* Initialise offset. */
        ctx.addFunc(func.id.name, 0)
        
        // Calculate initial offset through register usage of inputs
        // Tag the inputs correctly

        /* Parameter made into registers */
        func.params.zip(nonOutputRegisters).foreach(
            (param, reg) => 
                ctx.addVar(param.paramId.name, Reg(reg, getTypeSize(param.paramType)))
        )

        /* Paramters exceeding numb registers */
        func.params.drop(nonOutputRegisters.size).foreach(
            //make stack ref
            (param) => ???
        )
        
        /* Handle statements*/
        reference(func.stmts, func.id.name)
    }

    def reference(funcStmts: List[Stmt[QualifiedName, KnownType]], funcName: QualifiedName)(using ctx: Context): Unit  = {
        funcStmts.foreach((stmt) => reference(stmt, funcName))
    }

    def reference(stmt: Stmt[QualifiedName, KnownType], funcName: QualifiedName)(using ctx: Context): Unit  = {
        stmt match {
            case NewAss(assType, id, rVal) => {
                //getTypeSize(assType)
            }
            case If(cond, ifStmts, elseStmts) => {
                reference(ifStmts, funcName)
                reference(elseStmts, funcName)
            }
            case While(cond, subStmts) => reference(subStmts, funcName)
            case Nest(stmts) => reference(stmts, funcName)  
            case Return(expr) => ???
            

            /* Add prebuilts */
            /*
            case Print(expr) => {
                ctx.addPrebuilt(PbPrint(getExprType(expr)))
            }
            case PrintLn(expr) => {
                ctx.addPrebuilt(PbPrint(getExprType(expr)))
            }
            case Read  (expr) => ctx.addPrebuilt(PbRead(getExprType(expr)))
            case Exit  (_) => ctx.addPrebuilt(PbExit())
            case Free  (expr) => ctx.addPrebuilt(PbFree(getExprType(expr)))*/
            case _ => {}
        }
    }
}

/*
RoData
labelling + Reference

Variables
into references

Prebuilt Widgets
set flags to add prebuilt functions

refencer

program

func:
    args = 0
    varoffset = 0
    referencer - list stmt
    store varoffset

args++
argsoffset
offset -- ++size


map - QName to reference
map - string to roData
map - func Qname to offset
list of flags for widgets
*/