package wacc.backend.generator

import wacc.ast._
import wacc.semantic.QualifiedName
import wacc.backend.ir._
import wacc.backend.ir
import wacc.backend.Context
import scala.collection.mutable.Builder

final val RETURN_REG       = rax
final val TEMP_REG         = r10
final val FIRST_PARAM_REG  = rdi
final val SECOND_PARAM_REG = rsi
final val THIRD_PARAM_REG  = rdx
final val FOURTH_PARAM_REG = rcx
final val FIFTH_PARAM_REG  = r8
final val SIXTH_PARAM_REG  = r9

object generator {
    def generate(prog: Program[QualifiedName, KnownType])(using ctx: Context): List[Block] = {
        val blockBuilder = List.newBuilder[Block]
        // TODO: change roData for main
        val mainBuilder = List.newBuilder[Instr]
        generateStmts(prog.stmts, mainBuilder)
        blockBuilder += Block(Label ("main"), None, mainBuilder.result())
        prog.funcs.foreach { func => blockBuilder += generate(func) }
        // ctx.getPrebuilts().foreach { prebuilt => builder += generatePrebuiltBlock(prebuilt) }
        blockBuilder.result()
    }
    
    def generate(func: Func[QualifiedName, KnownType])(using ctx: Context): Block = {
        // TODO: roData for function?
        val builder = List.newBuilder[Instr]
        generateStmts(func.stmts, builder)
        Block(Label (func.id.name.oldName), None, builder.result())
    }

    def generate(
        rVal: RValue[QualifiedName, Type], 
        builder: Builder[Instr, List[Instr]]
    ): Unit = {
        ???
    }

    def generateAddSubMul(
        left: Expr[QualifiedName, KnownType], 
        right: Expr[QualifiedName, KnownType], 
        build: (Reg[DWORD], Reg[DWORD]) => Instr,
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        generate(left, builder) 
        builder 
            += IPush (Reg[DWORD] (RETURN_REG)) 
        generate(right, builder)
        builder
            += IPush (Reg[DWORD] (RETURN_REG)) 
            += IPop (Reg[DWORD] (TEMP_REG))
            += IPop (Reg[DWORD] (RETURN_REG))
            += build((Reg[DWORD] (RETURN_REG)), (Reg[DWORD] (TEMP_REG)))
            // TODO: check for overflow
    }

    def generateDiv(
        left: Expr[QualifiedName, KnownType], 
        right: Expr[QualifiedName, KnownType],
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        generate(right, builder)
        builder
            += IPush (Reg[DWORD] (RETURN_REG))
        generate(left, builder) 
        builder
            += IPush (Reg[DWORD] (RETURN_REG)) 
            += IPop (Reg[DWORD] (RETURN_REG))
            += IPop (Reg[DWORD] (TEMP_REG))
            += ICmp (Reg[DWORD] (TEMP_REG), Imm[DWORD] (0))
            // TODO: call cdq
            // TODO: check div by zero
            += IDiv (Reg[DWORD] (TEMP_REG))
    }

    def generate(
        expr: Expr[QualifiedName, KnownType],
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        expr match {
            case Ident(_)  => ??? // TODO
            case Add(x, y) => generateAddSubMul(x, y, IAdd.apply, builder)
            case Sub(x, y) => generateAddSubMul(x, y, ISub.apply, builder)
            case Mul(x, y) => generateAddSubMul(x, y, IMul.apply, builder)
            case Div(x, y) => generateDiv(x, y, builder)
            case _         => ??? // TODO
        }
        builder.result()
    }
    
    def generateExprs(
        exprs: List[Expr[QualifiedName, KnownType]], 
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        exprs.foreach { expr => generate(expr, builder) }
    }

    def generate(
        stmt: Stmt[QualifiedName, KnownType], 
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        stmt match {
            case Skip() => ()
            case Exit(expr) => 
                generate(expr, builder)
                builder
                    += IMov (Reg[DWORD] (FIRST_PARAM_REG), (Reg[DWORD] (RETURN_REG)))
                    += ICall ("_exit") // TODO: replace _exit string with prebuilt attrib
            case NewAss(assType, id, rVal) => ??? // TODO
            case _                         => ??? // TODO
        }
    }

    def generateStmts(
        stmts: List[Stmt[QualifiedName, KnownType]],
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        stmts.foreach { stmt => generate(stmt, builder) }
    }
}
