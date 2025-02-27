package wacc.backend.generator

import wacc.ast._
import wacc.backend.ir._
import wacc.backend.Context
import wacc.semantic.QualifiedName
import scala.collection.mutable.{Builder, Set}

import wacc.backend.generator.prebuilts._
import wacc.backend.referencing.referencer.getTypeSize

type InstrBuilder = Builder[Instr, List[Instr]]

final val RETURN_REG = RAX
final val TEMP_REG   = R10
final val MALLOC_REG   = R11

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
        prog.funcs.foreach { func => blockBuilder += generate(func) }
        val prebuilts: Set[Block] = Set()
        ctx.getPrebuilts().foreach { prebuilt => prebuilts ++= prebuiltGenerator.generatePrebuiltBlock(prebuilt) }
        blockBuilder ++= prebuilts
        val mainBlock = Block(Label ("main"), Some(ctx.getAllRodata()), mainBuilder.result())
        mainBlock :: blockBuilder.result()
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
        Block(Label (s"wacc_${func.id.name.oldName}"), None, builder.result())
    }

    def generate(
        rVal: RValue[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = 
        given DataSize = QWORD
        rVal match {
            case expr: Expr[QualifiedName, KnownType] => generate(expr)
            case a@ArrayLit(exprs) => 
                val elemSize = getTypeSize(a.t.asInstanceOf[ArrayT[QualifiedName, KnownType]].t)
                val label = ctx.addPrebuilt(PbMalloc)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Imm (elemSize.bytes * exprs.size + DWORD.bytes))(using DWORD)
                    += ICall (label)
                    += IMov (Reg (MALLOC_REG), Reg (RETURN_REG))
                    += IAdd (Reg (MALLOC_REG), Imm (DWORD.bytes))
                    += IMov (MemOff (MALLOC_REG, -4), Imm (exprs.size))(using DWORD)
                exprs.zipWithIndex.foreach { (expr, i) =>
                    generate(expr)
                    builder
                        += IMov (MemOff (MALLOC_REG, i * elemSize.bytes), Reg (RETURN_REG))
                }
            case NewPair(fst, snd) =>
                val label = ctx.addPrebuilt(PbMalloc)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Imm (QWORD.bytes * 2))(using DWORD)
                    += ICall (label)
                    += IMov (Reg (MALLOC_REG), Reg (RETURN_REG))
                generate(fst)
                builder += IMov (MemInd (MALLOC_REG), Reg (RETURN_REG))
                generate(snd)
                builder 
                    += IMov (MemOff (MALLOC_REG, QWORD.bytes), Reg (RETURN_REG))
                    += IMov (Reg (RETURN_REG), Reg (MALLOC_REG))
            
            case First(value) => 
                loadPairElem(value)
                builder += IMov (Reg (RETURN_REG), MemInd (PAIR_ELEM_REG))
            case Second(value) => 
                loadPairElem(value)
                builder += IMov (Reg (RETURN_REG), MemOff (PAIR_ELEM_REG, QWORD.bytes))

            case Call(id, exprs) => 
                val ty = id.t.asInstanceOf[FuncT]
                exprs.zip(parameterRegisters).foreach { (_, reg) =>
                    builder 
                        += IPush (Reg (reg))
                }
                exprs.zip(parameterRegisters).zip(ty.paramTs).foreach { (exprReg, t) =>
                    generate(exprReg._1)
                    builder 
                        += IMov (Reg (exprReg._2), Reg (RETURN_REG))(using getTypeSize(t))
                }
                //TODO(stack parameters)
                builder += ICall (s"wacc_${id.name.oldName}")(using QWORD)

                exprs.zip(parameterRegisters).reverse.foreach { (_, reg) =>
                    builder 
                        += IPop (Reg (reg))
                }
        }

    def loadPairElem(
        lVal: LValue[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = QWORD
        val label = ctx.addPrebuilt(PbErrNull)
        builder
            += IMov (Reg(PAIR_ELEM_REG), findOp(lVal))
            += ICmp(Reg(PAIR_ELEM_REG), Imm(0))
            += Jmp(Label(label), JumpCond.E)
    }

    def findOp(
        lVal: LValue[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): ValDest = {
        lVal match {
            case Ident(name) => ctx.getVarRef(name)
            case ArrayElem(id, expr) => ???
                // val size = getTypeSize(id.t.asInstanceOf[ArrayT[QualifiedName, KnownType]].t)
                // ctx.addPrebuilt(PbArrStore(size))

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
        generate(left) 
        builder 
            += IPush (Reg (RETURN_REG)) 
        generate(right)
        builder
            += IPush (Reg (RETURN_REG)) 
            += IPop (Reg (TEMP_REG))
            += IPop (Reg (RETURN_REG))
            += apply((Reg (RETURN_REG)), (Reg (TEMP_REG)))
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
        given DataSize = DWORD
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
            += ICmp (Reg (RETURN_REG), Imm (1))(using BYTE)
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
            case Chr(expr) => 
                val label = ctx.addPrebuilt(PbErrBadChar)
                generate(expr)
                builder
                    += ITest (Reg (RETURN_REG), Imm (-128))
                    += IMov (Reg (SECOND_PARAM_REG), Reg (RETURN_REG), JumpCond.NE)
                    += Jmp (Label (label), JumpCond.NE)
            case i@Ident(name) => 
                builder += IMov (Reg (RETURN_REG), ctx.getVarRef(name))(using getTypeSize(i.t))
            case Add(x, y) => generateAddSubMul(x, y, IAdd.apply)
            case Sub(x, y) => generateAddSubMul(x, y, ISub.apply)
            case Mul(x, y) => generateAddSubMul(x, y, IMul.apply)
            case Div(x, y) => generateDivMod(x, y)
            case Mod(x, y) => generateDivMod(x, y)
                builder += IMov (Reg (RETURN_REG), Reg (REMAINDER_REG))(using QWORD)
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
                val newStr = str.flatMap {
                    case '\n' => "\\n"
                    case '\t' => "\\t"
                    case '\b' => "\\b"
                    case '\r' => "\\r"
                    case '\f' => "\\f"
                    case '\\' => "\\\\"
                    case '\"' => "\\\""
                    case '\'' => "\\\'"
                    case c => c.toString
                }
                val roData = ctx.addRoData(newStr)
                builder 
                    += ILea (Reg (RETURN_REG), Rip (roData.label))
            case wacc.ast.CharLit(char) => 
                builder += IMov (Reg (RETURN_REG), Imm (char.toInt))
            case wacc.ast.PairLit() => 
                builder += IMov (Reg (RETURN_REG), Imm (0))
            case ArrayElem(id, exprs) => 
                val size = getTypeSize(id.t.asInstanceOf[ArrayT[QualifiedName, KnownType]].t)
                ctx.addPrebuilt(PbArrLoad(size))
                builder += IPush(Reg(FIRST_PARAM_REG))
                generate(id)
                generateArrayElem(size, exprs)
                builder += IPop(Reg(FIRST_PARAM_REG))
        }
        builder.result()
    }

    def generateArrayElem(
        size: DataSize,
        exprs: List[Expr[QualifiedName, KnownType]]
    )(using ctx: Context, builder: InstrBuilder): Unit =  exprs match {
                case Nil => ()
                case ex::exs =>
                    given DataSize = QWORD
                    val label = ctx.addPrebuilt(PbArrLoad(size))
                    builder += IPush(Reg(RETURN_REG))
                    generate(ex)
                    builder 
                        += IMov(Reg(FIRST_PARAM_REG), Reg(RETURN_REG))(using size)
                        += IPop(Reg(RETURN_REG))
                    ICall(label)
                    generateArrayElem(size, exs)
    }

    def arrayAssign(
        exprs: List[Expr[QualifiedName, KnownType]],
        rVal: RValue[QualifiedName, KnownType],
        size: DataSize
    )(using ctx: Context, builder: InstrBuilder): Unit = exprs match {
        case Nil => ()
        case List(expr) =>
            given DataSize = QWORD
            val label = ctx.addPrebuilt(PbArrStore(size))
            builder += IPush(Reg(FIRST_PARAM_REG))
            builder += IPush(Reg(SECOND_PARAM_REG))
            builder += IMov(Reg(FIRST_PARAM_REG), Reg(RETURN_REG))
            generate(expr)
            builder += IMov(Reg(SECOND_PARAM_REG), Reg(RETURN_REG))
            generate(rVal)
            builder 
                += ICall(label) // TODO(generic)
                += IPop(Reg(FIRST_PARAM_REG))
                += IPop(Reg(SECOND_PARAM_REG))
        case ex::exs =>
            generateArrayElem(size, exs)
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
            case a@Assign(lVal, rVal) => lVal match {
                case ArrayElem(id, exprs) => 
                    val size = getTypeSize(id.t.asInstanceOf[ArrayT[QualifiedName, KnownType]].t)
                    builder += IPush(Reg(FIRST_PARAM_REG))
                    generate(id)
                    arrayAssign(exprs, rVal, size)
                case _ => 
                    generate(rVal)
                    val lValOp = findOp(lVal)
                    builder
                        += IMov (lValOp, Reg (RETURN_REG))(using getTypeSize(a.ty))
            }
            case r@Read(lVal) => 
                val label = ctx.addPrebuilt(PbRead(r.ty))
                val lValOp = findOp(lVal)
                builder 
                    += IPush(Reg(FIRST_PARAM_REG))
                    += IMov (Reg (FIRST_PARAM_REG), lValOp)(using getTypeSize(r.ty))
                    += ICall (label)
                    += IPop(Reg(FIRST_PARAM_REG))
                    += IMov (lValOp, Reg (RETURN_REG))(using getTypeSize(r.ty))
            case f@Free(expr) =>
                val label = ctx.addPrebuilt(PbFree(f.ty))
                generate(expr)
                builder
                    += IPush (Reg (FIRST_PARAM_REG))
                    += IMov (Reg (FIRST_PARAM_REG), Reg (RETURN_REG))
                    += ICall (label)
                    += IPop (Reg (FIRST_PARAM_REG))
            case Return(expr) =>
                generate(expr)
                builder
                    += IMov (Reg (STACK_PTR_REG), Reg (BASE_PTR_REG))
                    += IPop (Reg (BASE_PTR_REG))
                    += IRet
            case Exit(expr) => 
                val label = ctx.addPrebuilt(PbExit)
                generate(expr)
                builder
                    += IPush (Reg (FIRST_PARAM_REG))
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))(using DWORD)
                    += ICall (label)
                    += IPop (Reg (FIRST_PARAM_REG))
            case p@Print(expr) => 
                val label = ctx.addPrebuilt(PbPrint(p.ty))
                generate(expr)
                builder
                    += IPush (Reg (FIRST_PARAM_REG))
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))
                    += ICall (label)
                    += IPop (Reg (FIRST_PARAM_REG))
            case p@PrintLn(expr) => 
                val label = ctx.addPrebuilt(PbPrintln(p.ty))
                generate(expr)
                builder
                    += IPush (Reg (FIRST_PARAM_REG))
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))
                    += ICall (label)
                    += ICall ("_println")
                    += IPop (Reg (FIRST_PARAM_REG))
            case If(cond, ifStmts, elseStmts) => 
                val (ifLabel, endLabel) = (ctx.nextLabel(), ctx.nextLabel())
                generate(cond)
                builder
                    += Jmp (ifLabel, JumpCond.E)
                generateStmts(elseStmts)
                builder
                    += Jmp (endLabel, JumpCond.UnCond)
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
