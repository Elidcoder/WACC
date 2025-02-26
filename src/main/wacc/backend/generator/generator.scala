package wacc.backend.generator

import wacc.ast._
import wacc.semantic.QualifiedName
import wacc.backend.ir._
import wacc.backend.ir
import wacc.backend.Context
import scala.collection.mutable.Builder

final val RETURN_REG       = rax
final val TEMP_REG         = r10
final val BASE_PTR_REG     = rbp
final val STACK_PTR_REG    = rsp
final val FIRST_PARAM_REG  = rdi
final val SECOND_PARAM_REG = rsi
final val THIRD_PARAM_REG  = rdx
final val FOURTH_PARAM_REG = rcx
final val FIFTH_PARAM_REG  = r8
final val SIXTH_PARAM_REG  = r9

final val REMAINDER_REG = rdx

object generator {
    def generate(prog: Program[QualifiedName, KnownType])(using ctx: Context): List[Block] = {
        val blockBuilder = List.newBuilder[Block]
        // TODO: change roData for main
        val mainBuilder = List.newBuilder[Instr]
        mainBuilder
            += IPush (Reg[QWORD] (BASE_PTR_REG))
            += IMov (Reg[QWORD] (BASE_PTR_REG), Reg[QWORD] (STACK_PTR_REG))
        generateStmts(prog.stmts, mainBuilder)
        mainBuilder
            += IMov (Reg[DWORD] (RETURN_REG), Imm[DWORD] (0))
            += IPush (Reg[QWORD] (BASE_PTR_REG))
            += IRet
        blockBuilder += Block(Label ("main"), None, mainBuilder.result())
        prog.funcs.foreach { func => blockBuilder += generate(func) }
        // ctx.getPrebuilts().foreach { prebuilt => builder += generatePrebuiltBlock(prebuilt) }
        blockBuilder.result()
    }
    
    def generate(func: Func[QualifiedName, KnownType])(using ctx: Context): Block = {
        // TODO: roData for function?
        val builder = List.newBuilder[Instr]
        builder
            += IPush (Reg[QWORD] (BASE_PTR_REG))
            += IMov (Reg[QWORD] (BASE_PTR_REG), Reg[QWORD] (STACK_PTR_REG))
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
            += build((Reg[DWORD] (RETURN_REG)), (Reg[DWORD] (REMAINDER_REG)))
            // TODO: check for overflow
    }

    def generateDivMod(
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

    def generateBinCond(
        left: Expr[QualifiedName, KnownType],
        right: Expr[QualifiedName, KnownType],
        cond: JumpCond,
        builder: Builder[Instr, List[Instr]],
    )(using ctx: Context): Unit = {
        generate(left, builder) 
        builder 
            += IPush (Reg[QWORD] (RETURN_REG)) 
        generate(right, builder)
        builder 
            += IPush (Reg[QWORD] (RETURN_REG))
            += IPop (Reg[QWORD] (TEMP_REG))
            += IPop (Reg[QWORD] (RETURN_REG))
            += ICmp (Reg[QWORD] (RETURN_REG), Reg[QWORD] (TEMP_REG))
            += ISet (Reg[BYTE] (RETURN_REG), cond)
    }

    def generate(
        expr: Expr[QualifiedName, KnownType],
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        expr match {
            case Not(expr) => 
                generate(expr, builder)
                builder += 
                    ISet (Reg[BYTE] (RETURN_REG), JumpCond.NE) 
            case Ident(_)  => ??? // TODO
            case Add(x, y) => generateAddSubMul(x, y, IAdd.apply, builder)
            case Sub(x, y) => generateAddSubMul(x, y, ISub.apply, builder)
            case Mul(x, y) => generateAddSubMul(x, y, IMul.apply, builder)
            case Div(x, y) => generateDivMod(x, y, builder)
            case Mod(x, y) => generateDivMod(x, y, builder)
                builder
                    += IMov (Reg[QWORD] (RETURN_REG), Reg[QWORD] (TEMP_REG))
            case Eq(left, right) => 
                generateBinCond(left, right, JumpCond.E, builder)
            case NotEq(left, right) => 
                generateBinCond(left, right, JumpCond.NE, builder)
            case Greater(left, right) => 
                generateBinCond(left, right, JumpCond.G, builder)
            case GreaterEq(left, right) =>
                generateBinCond(left, right, JumpCond.GE, builder)
            case Less(left, right) =>
                generateBinCond(left, right, JumpCond.L, builder)
            case LessEq(left, right) =>
                generateBinCond(left, right, JumpCond.LE, builder) 
            case And(left, right) => 
                val afterLabel = ctx.nextLabel()
                generate(left, builder)
                builder 
                    += Jmp (afterLabel, JumpCond.NE)
                generate(right, builder)
                builder 
                    += afterLabel
                    += ISet (Reg[BYTE] (RETURN_REG), JumpCond.E)
            case Or(left, right) =>
                val afterLabel = ctx.nextLabel()
                generate(left, builder)
                builder 
                    += Jmp (afterLabel, JumpCond.E)
                generate(right, builder)
                builder 
                    += afterLabel
                    += ISet (Reg[BYTE] (RETURN_REG), JumpCond.E)
            case BoolLit(bool) =>
                builder += 
                    IMov (Reg[BYTE] (RETURN_REG), (Imm (if (bool) 1 else 0)))
                    ICmp (Reg[BYTE] (RETURN_REG), Imm (1))
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
            case NewAss(assType, id, rVal) => ??? // TODO
            case Assign(lVal, rVal) => ???
            case Read(lVal) => ???
            case Free(expr) => ???
            case Return(expr) =>
                generate(expr, builder)
                builder
                    += IMov (Reg[QWORD] (STACK_PTR_REG), Reg[QWORD] (BASE_PTR_REG))
                    += IPop (Reg[QWORD] (BASE_PTR_REG))
                    += IRet
            case Exit(expr) => 
                generate(expr, builder)
                builder
                    += IMov (Reg[DWORD] (FIRST_PARAM_REG), (Reg[DWORD] (RETURN_REG)))
                    += ICall ("_exit") // TODO: replace _exit string with prebuilt attrib
            case Print(expr) => ???
            case PrintLn(expr) => ???
            case If(cond, ifStmts, elseStmts) => 
                val (ifLabel, endLabel) = (ctx.nextLabel(), ctx.nextLabel())
                generate(cond, builder)
                builder
                    += Jmp (ifLabel, JumpCond.E)
                generateStmts(elseStmts, builder)
                builder
                    += Jmp (endLabel, JumpCond.E)
                    += ifLabel
                generateStmts(ifStmts, builder)
                builder
                    += endLabel
            case While(cond, stmts) =>
                val (condLabel, bodyLabel) = (ctx.nextLabel(), ctx.nextLabel())
                builder
                    += Jmp (condLabel, JumpCond.UnCond)
                    += bodyLabel
                generateStmts(stmts, builder)
                builder
                    += condLabel
                generate(cond, builder)
                builder
                    += Jmp (bodyLabel, JumpCond.E)
            case Nest(stmts) => 
                generateStmts(stmts, builder)
        }
    }

    def generateStmts(
        stmts: List[Stmt[QualifiedName, KnownType]],
        builder: Builder[Instr, List[Instr]]
    )(using ctx: Context): Unit = {
        stmts.foreach { stmt => generate(stmt, builder) }
    }
}
