package wacc.backend.generator

import wacc.ast._
import wacc.backend.ir._
import wacc.backend.Context
import wacc.semantic.QualifiedName
import wacc.backend.referencing.referencer.getTypeSize

import scala.collection.mutable.{Builder, Set}

type InstrBuilder = Builder[Instr, List[Instr]]

final val NULL         = 0
final val FALSE        = 0
final val EXIT_SUCCESS = 0
final val TRUE         = 1
final val ASCII_OVERFLOW_MASK = 0xFFFFFF80

object generator {
    /* Generate the code for the whole program (delegates to other generates). */
    def generate(prog: Program[QualifiedName, KnownType])(using ctx: Context): List[Block] = {
        given Context = ctx
        given DataSize = QWORD
        given mainBuilder: InstrBuilder = List.newBuilder[Instr]
        val blockBuilder: Set[Block]    = Set()
        val mainOffset                  = ctx.getFuncOff(ctx.mainName)

        /* Generate the main function. */
        generateFuncStart(mainOffset)
        generateStmts(prog.stmts)
        if (mainOffset != 0) then
            mainBuilder += IAdd(Reg(STACK_PTR_REG), Imm(mainOffset))
        mainBuilder
            += IMov(Reg(RETURN_REG), Imm(EXIT_SUCCESS))
            += IPop(Reg(BASE_PTR_REG))
            += IRet
        
        /* Generate all other functions' code. */
        prog.funcs.foreach(blockBuilder += generate(_))
        ctx.getPrebuilts().foreach(prebuiltGenerator.generatePrebuiltBlock(_)(using blockBuilder))
        val mainBlock = Block(Label(ctx.mainName.oldName), Some(ctx.getAllRodata()), mainBuilder.result())
        blockBuilder += mainBlock
        blockBuilder.toList
    }
    
    /* Generate the function label's string following the WACC convention. */
    private def scrambleFuncName(name: QualifiedName): String = s"wacc_${name.oldName}"
    
    /* Generate the code for a function. */
    private def generate(func: Func[QualifiedName, KnownType])(using ctx: Context): Block = {
        given builder: InstrBuilder = List.newBuilder[Instr]
        generateFuncStart(ctx.getFuncOff(func.id.name))
        generateStmts(func.stmts)
        Block(Label (scrambleFuncName(func.id.name)), None, builder.result())
    }

    /* Generate the starting part of a function's code (setup). */
    private def generateFuncStart(offset: Int)(using builder: InstrBuilder) = {
        given DataSize = QWORD
        builder
            += IPush (Reg (BASE_PTR_REG))
            += IMov (Reg (BASE_PTR_REG), Reg (STACK_PTR_REG))
        if (offset != 0) then
            builder += ISub (Reg (STACK_PTR_REG), Imm (offset))
    }

    /* Generate an rval's code. */
    private def generate(rVal: RValue[QualifiedName, KnownType])(using ctx: Context, builder: InstrBuilder): Unit = 
        given DataSize = QWORD
        rVal match {
            /* Delegate if it is an expr. */
            case expr: Expr[QualifiedName, KnownType] => generate(expr)

            /* Load and evaluate the value into the right register with correct size. */
            case First(value) => 
                loadPairElem(value)
                builder += IMov (Reg (RETURN_REG), Mem (PAIR_ELEM_REG))
            /* Load and evaluate the value into the right register with correct size. */
            case Second(value) => 
                loadPairElem(value)
                builder += IMov (Reg (RETURN_REG), Mem (PAIR_ELEM_REG, QWORD.bytes))

            /* Correctly size elements and then malloc each one as well as the initial pointer. */
            case a@ArrayLit(exprs) => 
                val elemSize = getTypeSize(a.t.asInstanceOf[ArrayT[QualifiedName, KnownType]].t)
                val label = ctx.addPrebuilt(PbMalloc)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Imm (elemSize.bytes * exprs.size + DWORD.bytes))(using DWORD)
                    += ICall (label)
                    += IMov (Reg (MALLOC_REG), Reg (RETURN_REG))
                    += IAdd (Reg (MALLOC_REG), Imm (DWORD.bytes))
                    += IMov (Mem (MALLOC_REG, -DWORD.bytes), Imm (exprs.size))(using DWORD)
                exprs.zipWithIndex.foreach { (expr, i) =>
                    generate(expr)
                    builder
                        += IMov (Mem (MALLOC_REG, i * elemSize.bytes), Reg (RETURN_REG))
                }
                builder += IMov (Reg (RETURN_REG), Reg (MALLOC_REG))
            
            /* Correctly size elements and then malloc each one as well as the pointer. */
            case NewPair(fst, snd) =>
                val label = ctx.addPrebuilt(PbMalloc)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Imm (QWORD.bytes * 2))(using DWORD)
                    += ICall (label)
                    += IMov (Reg (MALLOC_REG), Reg (RETURN_REG))
                generate(fst)
                builder += IMov (Mem (MALLOC_REG), Reg (RETURN_REG))
                generate(snd)
                builder 
                    += IMov (Mem (MALLOC_REG, QWORD.bytes), Reg (RETURN_REG))
                    += IMov (Reg (RETURN_REG), Reg (MALLOC_REG))

            /* Correctly push parameters, modify the sp and add the function call. */
            case Call(id, exprs) => 
                val paramTypes = id.t.asInstanceOf[FuncT].paramTs
                var offset = ctx.getFuncParamOff(id.name)
                builder 
                    += ISub (Reg (STACK_PTR_REG), Imm (offset))
                exprs.zip(paramTypes).foreach( (expr, ty) =>
                    val size = getTypeSize(ty)
                    offset -= size.bytes
                    generate(expr)
                    builder += IMov (Mem (STACK_PTR_REG, offset), Reg (RETURN_REG))(using size)
                )
                builder 
                    += ICall (scrambleFuncName(id.name))
                    += IAdd (Reg (STACK_PTR_REG), Imm (ctx.getFuncParamOff(id.name)))
        }
    
    /* Add instructions to load a pair element from an lval ensuring it is not null.*/
    private def loadPairElem(lVal: LValue[QualifiedName, KnownType])(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = QWORD
        val label = ctx.addPrebuilt(PbErrNull)
        val lValOp = findOp(lVal)
        builder
            += IMov (Reg (PAIR_ELEM_REG), lValOp)
            += ICmp (Reg (PAIR_ELEM_REG), Imm (NULL))
            += Jmp (Label (label), JumpCond.Eq)
    }

    /* Determines the DestOp (destination/location) of a given lvalue. */
    private def findOp(lVal: LValue[QualifiedName, KnownType])(using ctx: Context, builder: InstrBuilder): DestOp = {
        lVal match {
            case Ident(name) => ctx.getVarRef(name)
            case ArrayElem(id, exprs) => {
                val size = getArraySize(id.t, exprs.size)
                val label = ctx.addPrebuilt(PbArrRef(size))
                exprs.reverse match
                    case ex :: exs => {
                        generateNestedArrayElem(id, exs)
                        builder += IPush(Reg(RETURN_REG))
                        generate(ex)
                        builder
                            += IMov (Reg (ARR_REF_PARAM_REG), Reg (RETURN_REG))(using DWORD)
                            += IPop (Reg (ARR_REF_RETURN_REG))
                            += ICall (label)
                        Mem(ARR_REF_RETURN_REG)
                    }
                    case Nil => findOp(id)
            }
            case First(lVal) => {
                loadPairElem(lVal)
                Mem(PAIR_ELEM_REG)
            }
            case Second(lVal) => {
                loadPairElem(lVal)
                Mem(PAIR_ELEM_REG, QWORD.bytes)
            }
        }
    }

    /* Generates code for a binary arithmetic operation by evaluating both sides and then applying the 'instruction'. */
    private def generateAddSubMul(
        left: Expr[QualifiedName, KnownType], 
        right: Expr[QualifiedName, KnownType], 
        apply: (Reg, Reg) => Instr
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        generate(left) 
        builder += IPush (Reg (RETURN_REG)) 
        generate(right)
        builder
            += IPush (Reg (RETURN_REG)) 
            += IPop (Reg (TEMP_REG))
            += IPop (Reg (RETURN_REG))
            += apply((Reg (RETURN_REG)), (Reg (TEMP_REG)))
            += Jmp (Label(ctx.addPrebuilt(PbErrOverflow)), JumpCond.Overflow)
    }

    /* Generate code for division or modulo operation, including a zero check. */
    private def generateDivMod(
        left: Expr[QualifiedName, KnownType], 
        right: Expr[QualifiedName, KnownType]
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = DWORD
        generate(right)
        builder += IPush (Reg (RETURN_REG))
        generate(left) 
        builder
            += IPop (Reg (TEMP_REG))
            += ICmp (Reg (TEMP_REG), Imm (FALSE))
            += Jmp (Label (ctx.addPrebuilt(PbDivZero)), JumpCond.Eq)
            += ICdq
            += IDiv (Reg (TEMP_REG))
    }

    /* Generate the code for a condition as well as both sides, correctly placing the jump. */
    private def generateBinCond(
        left: Expr[QualifiedName, KnownType],
        right: Expr[QualifiedName, KnownType],
        cond: JumpCond
    )(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = DWORD
        generate(left) 
        builder += IPush (Reg (RETURN_REG)) 
        generate(right)
        builder 
            += IPush (Reg (RETURN_REG))
            += IPop (Reg (TEMP_REG))
            += IPop (Reg (RETURN_REG))
            += ICmp (Reg (RETURN_REG), Reg (TEMP_REG))
            += ISet (Reg (RETURN_REG), cond)
            += ICmp (Reg (RETURN_REG), Imm (TRUE))(using BYTE)
    }

    /* Generate the code for an expression. */
    private def generate(expr: Expr[QualifiedName, KnownType])(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = DWORD
        expr match {
            /* Generate code for a binary arithmetic operator. */
            case Add(x, y) => generateAddSubMul(x, y, IAdd.apply)
            case Sub(x, y) => generateAddSubMul(x, y, ISub.apply)
            case Mul(x, y) => generateAddSubMul(x, y, IMul.apply)
            case Div(x, y) => generateDivMod(x, y)
            case Mod(x, y) => {
                generateDivMod(x, y)
                builder += IMov (Reg (RETURN_REG), Reg (REMAINDER_REG))(using QWORD)
            }

            /* Generate code for a binary comparison operator. */
            case Eq(left, right)        => generateBinCond(left, right, JumpCond.Eq)
            case NotEq(left, right)     => generateBinCond(left, right, JumpCond.NotEq)
            case Greater(left, right)   => generateBinCond(left, right, JumpCond.Gr)
            case GreaterEq(left, right) => generateBinCond(left, right, JumpCond.GrEq)
            case Less(left, right)      => generateBinCond(left, right, JumpCond.Less)
            case LessEq(left, right)    => generateBinCond(left, right, JumpCond.LessEq)
            case And(left, right) => {
                val afterLabel = ctx.nextLabel()
                generate(left)
                builder 
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += Jmp (afterLabel, JumpCond.NotEq)
                generate(right)
                builder 
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += afterLabel
                    += ISet (Reg (RETURN_REG), JumpCond.Eq)
            }
            case Or(left, right) => {
                val afterLabel = ctx.nextLabel()
                generate(left)
                builder 
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += Jmp (afterLabel, JumpCond.Eq)
                generate(right)
                builder 
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += afterLabel
                    += ISet (Reg (RETURN_REG), JumpCond.Eq)
            }

            /* Generate code for a unary operator. */
            case Len(expr) => {
                generate(expr)
                builder += IMov (Reg (RETURN_REG), Mem (RETURN_REG, -DWORD.bytes))
            }
            case Ord(expr) => {
                generate(expr)
                builder += IMovzx (Reg (RETURN_REG), Reg (RETURN_REG), BYTE)
            }
            case Not(expr) => {
                generate(expr)
                builder
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += ISet (Reg (RETURN_REG), JumpCond.NotEq)
            }
            case Neg(expr) => {
                generate(expr)
                builder 
                += INeg (Reg (RETURN_REG))
                += Jmp (Label(ctx.addPrebuilt(PbErrOverflow)), JumpCond.Overflow)
            }
            case Chr(expr) => {
                val label = ctx.addPrebuilt(PbErrBadChar)
                generate(expr)
                builder
                    += ITest (Reg (RETURN_REG), Imm (ASCII_OVERFLOW_MASK))
                    += IMov (Reg (SECOND_PARAM_REG), Reg (RETURN_REG), JumpCond.NotEq)
                    += Jmp (Label (label), JumpCond.NotEq)
            }

            /* Generate code for an identifier. */
            case i@Ident(name) => 
                builder += IMov (Reg (RETURN_REG), ctx.getVarRef(name))(using getTypeSize(i.t))
            
            /* Generate code for a literal. */
            case wacc.ast.CharLit(char) => 
                builder += IMov (Reg (RETURN_REG), Imm (char.toInt))
            case wacc.ast.PairLit() => 
                builder += IMov (Reg (RETURN_REG), Imm (NULL))
            case BoolLit(bool) =>
                builder += IMov (Reg (RETURN_REG), (Imm (if (bool) TRUE else FALSE)))(using BYTE)
            case IntLit(numb) =>
                builder += IMov (Reg (RETURN_REG), Imm (numb))
            case StrLit(str) => {
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
                builder += ILea (Reg (RETURN_REG), Mem (roData.label))(using QWORD)
            }
            case ArrayElem(id, exprs) => {
                val size = getArraySize(id.t, exprs.size)
                val label = ctx.addPrebuilt(PbArrRef(size))
                exprs.reverse match
                    case ex :: exs => 
                        generateNestedArrayElem(id, exs)
                        builder += IPush(Reg(RETURN_REG))
                        generate(ex)
                        builder
                            += IMov (Reg (ARR_REF_PARAM_REG), Reg (RETURN_REG))(using DWORD)
                            += IPop (Reg (ARR_REF_RETURN_REG))
                            += ICall (label)
                            += IMov (Reg (RETURN_REG), Mem (ARR_REF_RETURN_REG))(using size)
                    case Nil => generate(id)
            }
        }
        builder.result()
    }

    /* Calculates and returns the datasize of an array */
    private def getArraySize(t: Type, n: Int): DataSize = (t, n) match {
        case (t, 0)         => getTypeSize(t)
        case (ArrayT(t), n) => getArraySize(t, n - 1)
        case (t, n)         => getTypeSize(t)
    }

    /* Generate the code for each element in a nested array as well as some joining code. */
    private def generateNestedArrayElem(
        id: Ident[QualifiedName, KnownType],
        exprs: List[Expr[QualifiedName, KnownType]]
    )(using ctx: Context, builder: InstrBuilder): Unit = exprs match {
        case Nil     => generate(id)
        case ex::exs => {
            given DataSize = QWORD
            val label = ctx.addPrebuilt(PbArrRef(QWORD))
            generateNestedArrayElem(id, exs)
            builder += IPush(Reg(RETURN_REG))
            generate(ex)
            builder
                += IMov (Reg (ARR_REF_PARAM_REG), Reg (RETURN_REG))(using DWORD)
                += IPop (Reg (ARR_REF_RETURN_REG))
                += ICall (label)
                += IMov (Reg (RETURN_REG), Mem (ARR_REF_RETURN_REG))
        }
    }

    /* Generate the code for a statment. */
    private def generate(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context, builder: InstrBuilder): Unit = {
        given DataSize = QWORD
        stmt match {
            case Skip()      => ()
            case Nest(stmts) => generateStmts(stmts)

            /* Move the value into the right place based on the reference calculated in the referencer. */
            case NewAss(assType, id, rVal) => {
                generate(rVal)
                builder += IMov (ctx.getVarRef(id.name), Reg (RETURN_REG))(using getTypeSize(assType))
            }

            /* Generate the rvalue, save the result to find the lval's location and then save the result there. */
            case a@Assign(lVal, rVal) => {
                generate(rVal)
                builder += IPush(Reg(RETURN_REG))
                val lValOp = findOp(lVal)
                builder
                    += IPop(Reg(RETURN_REG))
                    += IMov (lValOp, Reg (RETURN_REG))(using getTypeSize(a.ty))
            }

            /* Add the correct read prebuilt using the lval and then add the correct call. */
            case r@Read(lVal) => {
                val label  = ctx.addPrebuilt(PbRead(r.ty))
                val lValOp = findOp(lVal)
                builder 
                    += IMov (Reg (FIRST_PARAM_REG), lValOp)(using getTypeSize(r.ty))
                    += ICall (label)
                    += IMov (lValOp, Reg (RETURN_REG))(using getTypeSize(r.ty))
            }

            /* Add the free prebuilt, generate the expr, modify the return if it was an array and correctly calling free. */
            case f@Free(expr) => {
                val label = ctx.addPrebuilt(PbFree(f.ty))
                generate(expr)
                f.ty match
                    case ArrayT(t) => builder += ISub (Reg (RETURN_REG), Imm (DWORD.bytes))
                    case _ => ()
                builder
                    += IMov (Reg (FIRST_PARAM_REG), Reg (RETURN_REG))
                    += ICall (label)
            }

            /* Generate the expr, then reset the base and stack pointer and then return. */
            case Return(expr) => {
                generate(expr)
                builder
                    += IMov (Reg (STACK_PTR_REG), Reg (BASE_PTR_REG))
                    += IPop (Reg (BASE_PTR_REG))
                    += IRet
            }
            
            /* Generate the expr, then add the exit prebuilt and call it correctly. */
            case Exit(expr) => {
                val label = ctx.addPrebuilt(PbExit)
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))(using DWORD)
                    += ICall (label)
            }

            /* Generate the expr, then add the print prebuilt and call it correctly. */
            case p@Print(expr) => {
                val label = ctx.addPrebuilt(PbPrint(p.ty))
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))(using getTypeSize(p.ty))
                    += ICall (label)
            }

             /* Generate the expr, then add the println prebuilt and call it correctly. */
            case p@PrintLn(expr) => {
                val label = ctx.addPrebuilt(PbPrintln(p.ty))
                generate(expr)
                builder
                    += IMov (Reg (FIRST_PARAM_REG), (Reg (RETURN_REG)))(using getTypeSize(p.ty))
                    += ICall (label)
                    += ICall ("_println")
            }
            
            /* Generate the condition, its jumps and the 2 bodies. */
            case If(cond, ifStmts, elseStmts) => {
                val (ifLabel, endLabel) = (ctx.nextLabel(), ctx.nextLabel())
                generate(cond)
                builder
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += Jmp (ifLabel, JumpCond.Eq)
                generateStmts(elseStmts)
                builder
                    += Jmp (endLabel)
                    += ifLabel
                generateStmts(ifStmts)
                builder += endLabel
            }

            /* Generate the cond, its jumps and the body. */
            case While(cond, stmts) =>{
                val (condLabel, bodyLabel) = (ctx.nextLabel(), ctx.nextLabel())
                builder
                    += Jmp (condLabel)
                    += bodyLabel
                generateStmts(stmts)
                builder += condLabel
                generate(cond)
                builder
                    += ICmp (Reg(RETURN_REG), Imm(TRUE))(using BYTE)
                    += Jmp (bodyLabel, JumpCond.Eq)
            }
        }
    }

    /* Generate every statement in the list. */
    private def generateStmts(stmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context, builder: InstrBuilder): Unit 
        = stmts.foreach(generate)
}
