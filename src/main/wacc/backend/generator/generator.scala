package wacc.backend.generator

import wacc.ast._
import wacc.semantic.QualifiedName
import wacc.backend.ir._
import wacc.backend.ir
import wacc.backend.Context
import scala.collection.mutable.Builder

import wacc.backend.referencing.referencer.{getTypeSize, parameterRegisters}
import wacc.backend.generator.prebuilts._

type InstrBuilder = Builder[Instr, List[Instr]]

final val RETURN_REG       = RAX
final val TEMP_REG         = R10
final val PAIR_REG         = R11
final val BASE_PTR_REG     = RBP
final val STACK_PTR_REG    = RSP
final val FIRST_PARAM_REG  = RDI
final val SECOND_PARAM_REG = RSI
final val THIRD_PARAM_REG  = RDX
final val FOURTH_PARAM_REG = RCX
final val FIFTH_PARAM_REG  = R8
final val SIXTH_PARAM_REG  = R9

final val PAIR_ELEM_REG = RBX //TODO(change and check)

final val REMAINDER_REG = RDX

object generator {
    def generate(prog: Program[QualifiedName, KnownType])(using ctx: Context): List[Block] = {
        given DataSize = QWORD
        val blockBuilder = List.newBuilder[Block]
        given mainBuilder: InstrBuilder = List.newBuilder[Instr]
        mainBuilder
            += IPush (Reg (BASE_PTR_REG))
            += IMov (Reg (BASE_PTR_REG), Reg (STACK_PTR_REG))
        if ctx.mainOffset != 0 then
            mainBuilder += ISub (Reg (STACK_PTR_REG), Imm (ctx.mainOffset))
        generateStmts(prog.stmts)
        if ctx.mainOffset != 0 then
            mainBuilder += IAdd (Reg (STACK_PTR_REG), Imm (ctx.mainOffset))
        mainBuilder
            += IMov (Reg (RETURN_REG), Imm (0))
            += IPop (Reg (BASE_PTR_REG))
            += IRet
        blockBuilder += Block(Label ("main"), Some(ctx.getAllRodata()), mainBuilder.result())
        prog.funcs.foreach { func => blockBuilder += generate(func) }
        ctx.getPrebuilts().foreach { prebuilt => blockBuilder ++= prebuiltGenerator.generatePrebuiltBlock(prebuilt) }
        blockBuilder.result()
    }
    
    def generate(func: Func[QualifiedName, KnownType])(using ctx: Context): Block = {
        given DataSize = QWORD
        given builder: InstrBuilder = List.newBuilder[Instr]
        builder
            += IPush (Reg (BASE_PTR_REG))
            += IMov (Reg (BASE_PTR_REG), Reg (STACK_PTR_REG))
        if ctx.getFuncOff(func.id.name) != 0 then
            builder += ISub (Reg (STACK_PTR_REG), Imm (ctx.getFuncOff(func.id.name)))
        generateStmts(func.stmts)
        Block(Label (func.id.name.oldName), None, builder.result())
    }

    def generate(
        rVal: RValue[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = 
        given DataSize = QWORD
        rVal match {
            case expr: Expr[QualifiedName, KnownType] => generate(expr)
            case ArrayLit(exprs) => ???
            case NewPair(fst, snd) =>
                ctx.addPrebuilt(PbMalloc)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Imm (QWORD.bytes * 2))(using DWORD)
                    += ICall ("_malloc")
                    += IMov (Reg (PAIR_REG), Reg (RETURN_REG))
                generate(fst)
                builder += IMov (MemInd (PAIR_REG), Reg (RETURN_REG))
                generate(snd)
                builder 
                    += IMov (MemOff (PAIR_REG, QWORD.bytes), Reg (RETURN_REG))
                    += IMov (Reg (RETURN_REG), Reg (PAIR_REG))
            
            case First(value) => 
                loadPairElem(value)
                builder += IMov (Reg (RETURN_REG), MemInd (PAIR_ELEM_REG))
            case Second(value) => 
                loadPairElem(value)
                builder += IMov (Reg (RETURN_REG), MemOff (PAIR_ELEM_REG, QWORD.bytes))

            case Call(id, exprs) => 
                exprs.zip(parameterRegisters).foreach { (expr, reg) =>
                    generate(expr)
                    builder += IMov (Reg (reg), Reg (RETURN_REG))(using QWORD) //TODO(get types)
                }
                //TODO(stack parameters)
                builder += ICall (s"wacc_${id.name.oldName}")(using QWORD) //TODO(generic label)
        }

    def loadPairElem(
        lVal: LValue[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = QWORD
        builder
            += IMov (Reg(PAIR_ELEM_REG), findOp(lVal))
            += ICmp(Reg(PAIR_ELEM_REG), Imm(0))
            += Jmp(Label("_errNull"), JumpCond.E)
    }

    def findOp(
        lVal: LValue[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): ValDest = {
        lVal match {
            case Ident(name) => ctx.getVarRef(name)
            case ArrayElem(id, expr) => id.t match {
                case ArrayT(ty) => 
                    ???
                case _ => ???
            }
            case First(lVal) => 
                loadPairElem(lVal)
                MemInd(PAIR_ELEM_REG)
            case Second(lVal) => 
                loadPairElem(lVal)
                MemOff(PAIR_ELEM_REG, QWORD.bytes)
        }
    }

    def generateAddSubMul(
        left: Expr[QualifiedName, KnownType], 
        right: Expr[QualifiedName, KnownType], 
        apply: (Reg, Reg) => Instr
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = DWORD
        generate(left) 
        builder 
            += IPush (Reg (RETURN_REG)) 
        generate(right)
        builder
            += IPush (Reg (RETURN_REG)) 
            += IPop (Reg (TEMP_REG))
            += IPop (Reg (RETURN_REG))
            += apply((Reg (RETURN_REG)), (Reg (REMAINDER_REG)))
            // TODO: check for overflow
    }

    def generateDivMod(
        left: Expr[QualifiedName, KnownType], 
        right: Expr[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = DWORD
        generate(right)
        builder
            += IPush (Reg (RETURN_REG))
        generate(left) 
        builder
            += IPush (Reg (RETURN_REG)) 
            += IPop (Reg (RETURN_REG))
            += IPop (Reg (TEMP_REG))
            += ICmp (Reg (TEMP_REG), Imm (0))
            // TODO: call cdq
            // TODO: check div by zero
            += IDiv (Reg (TEMP_REG))
    }

    def generateBinCond(
        left: Expr[QualifiedName, KnownType],
        right: Expr[QualifiedName, KnownType],
        cond: JumpCond
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = QWORD
        generate(left) 
        builder 
            += IPush (Reg (RETURN_REG)) 
        generate(right)
        builder 
            += IPush (Reg (RETURN_REG))
            += IPop (Reg (TEMP_REG))
            += IPop (Reg (RETURN_REG))
            += ICmp (Reg (RETURN_REG), Reg (TEMP_REG))
            += ISet (Reg (RETURN_REG), cond)
    }

    def generate(
        expr: Expr[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = DWORD
        expr match {
            case Not(expr) => generate(expr)
                builder += ISet (Reg (RETURN_REG), JumpCond.NE)
            case Neg(expr) => generate(expr)
                builder += INeg (Reg (RETURN_REG))
            case Len(expr) => generate(expr)
                builder += IMov (Reg (RETURN_REG), MemOff (RETURN_REG, -4))
            case Ord(expr) => generate(expr)
                builder += IMovzx (Reg (RETURN_REG), Reg (RETURN_REG), BYTE)
            case Chr(expr) => generate(expr)
                builder
                    += ITest (Reg (RETURN_REG), Imm (-128))
                    += IMov (Reg (SECOND_PARAM_REG), Reg (RETURN_REG), JumpCond.NE)
                    += Jmp (Label ("_errBadChar"), JumpCond.NE)
            case i@Ident(name) => 
                builder += IMov (Reg (RETURN_REG), ctx.getVarRef(name))(using getTypeSize(i.t))
            case Add(x, y) => generateAddSubMul(x, y, IAdd.apply)
            case Sub(x, y) => generateAddSubMul(x, y, ISub.apply)
            case Mul(x, y) => generateAddSubMul(x, y, IMul.apply)
            case Div(x, y) => generateDivMod(x, y)
            case Mod(x, y) => generateDivMod(x, y)
                builder += IMov (Reg (RETURN_REG), Reg (TEMP_REG))(using QWORD)
            case Eq(left, right) => 
                generateBinCond(left, right, JumpCond.E)
            case NotEq(left, right) => 
                generateBinCond(left, right, JumpCond.NE)
            case Greater(left, right) => 
                generateBinCond(left, right, JumpCond.G)
            case GreaterEq(left, right) =>
                generateBinCond(left, right, JumpCond.GE)
            case Less(left, right) =>
                generateBinCond(left, right, JumpCond.L)
            case LessEq(left, right) =>
                generateBinCond(left, right, JumpCond.LE) 
            case And(left, right) => 
                val afterLabel = ctx.nextLabel()
                generate(left)
                builder 
                    += Jmp (afterLabel, JumpCond.NE)
                generate(right)
                builder 
                    += afterLabel
                    += ISet (Reg (RETURN_REG), JumpCond.E)
            case Or(left, right) =>
                val afterLabel = ctx.nextLabel()
                generate(left)
                builder 
                    += Jmp (afterLabel, JumpCond.E)
                generate(right)
                builder 
                    += afterLabel
                    += ISet (Reg (RETURN_REG), JumpCond.E)
            case BoolLit(bool) =>
                builder 
                    += IMov (Reg (RETURN_REG), (Imm (if (bool) 1 else 0)))(using BYTE)
                    += ICmp (Reg (RETURN_REG), Imm (1))(using BYTE)
            case IntLit(numb) =>
                builder 
                    += IMov (Reg (RETURN_REG), Imm (numb))
            case StrLit(str) => 
                val label = ctx.nextStringLabel()
                ctx.addRoData(str, RoData(str.size, str, label))
                builder 
                    += ILea (Reg (RETURN_REG), Rip (label))
            case wacc.ast.CharLit(char) => 
                builder += IMov (Reg (RETURN_REG), Imm (char.toInt))
            case wacc.ast.PairLit() => 
                builder += IMov (Reg (RETURN_REG), Imm (0))
            case ArrayElem(id, exprs) => ???
        }
        builder.result()
    }

    def generate(
        stmt: Stmt[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = QWORD
        stmt match {
            case Skip() => ()
            case NewAss(assType, id, rVal) => 
                generate(rVal)
                builder
                    += IMov (ctx.getVarRef(id.name), Reg (RETURN_REG))(using getTypeSize(assType))
            case a@Assign(lVal, rVal) =>
                generate(rVal)
                builder
                    += IMov (findOp(lVal), Reg (RETURN_REG))(using getTypeSize(a.ty))
            case r@Read(lVal) => 
                ctx.addPrebuilt(PbRead(r.ty))
                val lValOp = findOp(lVal)
                builder 
                    += IMov (Reg (FIRST_PARAM_REG), lValOp)(using getTypeSize(r.ty))
                    += ICall ("_readi") // TODO(generic)
                    += IMov (lValOp, Reg (RETURN_REG))(using getTypeSize(r.ty))
            case Free(expr) =>
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Reg (RETURN_REG))
                    += ICall ("_free")
            case Return(expr) =>
                generate(expr)
                builder
                    += IMov (Reg (STACK_PTR_REG), Reg (BASE_PTR_REG))
                    += IPop (Reg (BASE_PTR_REG))
                    += IRet
            case Exit(expr) => 
                ctx.addPrebuilt(PbExit)
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))(using DWORD)
                    += ICall ("_exit")
            case p@Print(expr) => 
                ctx.addPrebuilt(PbPrint(p.ty))
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))
                    += ICall ("_prints")
            case p@PrintLn(expr) => 
                ctx.addPrebuilt(PbPrintln(p.ty))
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))
                    += ICall ("_prints")
                    += ICall ("_println")
            case If(cond, ifStmts, elseStmts) => 
                val (ifLabel, endLabel) = (ctx.nextLabel(), ctx.nextLabel())
                generate(cond)
                builder
                    += Jmp (ifLabel, JumpCond.E)
                generateStmts(elseStmts)
                builder
                    += Jmp (endLabel, JumpCond.E)
                    += ifLabel
                generateStmts(ifStmts)
                builder
                    += endLabel
            case While(cond, stmts) =>
                val (condLabel, bodyLabel) = (ctx.nextLabel(), ctx.nextLabel())
                builder
                    += Jmp (condLabel, JumpCond.UnCond)
                    += bodyLabel
                generateStmts(stmts)
                builder
                    += condLabel
                generate(cond)
                builder
                    += Jmp (bodyLabel, JumpCond.E)
            case Nest(stmts) => 
                generateStmts(stmts)
        }
    }

    def generateStmts(
        stmts: List[Stmt[QualifiedName, KnownType]]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        stmts.foreach(generate)
    }
}
