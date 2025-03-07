package wacc.backend.generator

import scala.collection.mutable.Set

import wacc.backend.ir._
import wacc.ast.KnownType
import wacc.ast.{CharT, StringT, IntT, BoolT, ArrayT, PairT}

final val FIRST_LABEL = 0
final val SECOND_LABEL = 1
final val THIRD_LABEL = 2
final val READ_SYSTEM_CALL = 0
final val PRINT_SYSTEM_CALL = 0
final val ERROR_EXIT_CODE = -1
final val FLUSH_ALL = 0
final val ARRAY_LENGTH_SIZE = 4
final val PRINT_CALL = "printf@plt"
final val EXIT_CALL = "exit@plt"
final val FLUSH_CALL = "fflush@plt"
final val MALLOC_CALL = "malloc@plt"
final val PUTS_CALL = "puts@plt"
final val FREE_CALL = "free@plt"

/* A trait representing a prebuilt section of code inserted 
 * whenever inbuilt function calls are found in the program. */
sealed trait Prebuilt {
    def labelString: String
    val block: Block
    def generateRoData(strs: List[String]): List[RoData] = 
        strs.zipWithIndex.map ( (str, index) => 
            RoData(str, Label(s".L.${labelString}_str${index}"))
        )
}

/* Prebuilt section for out of bounds errors in array indexing. */
case object PbOutOfBounds extends Prebuilt {
    def labelString = "_errOutOfBounds"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: array index %d out of bounds\\n"))
    val block = Block (
        Label(labelString),
        Some(roData),
        prebuiltGenerator.errorExitInstrs(roData(FIRST_LABEL).label)
    )
}

/* Prebuilt section for a invalid char. */
case object PbErrBadChar extends Prebuilt {
    def labelString = "_errBadChar"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: int %d is not ascii character 0-127 \\n"))
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.functionStart ++ List(
                ILea(Reg(FIRST_PARAM_REG), Mem(roData(FIRST_LABEL).label)),
                IMov(Reg(SYSTEM_CALL_REG), Imm(PRINT_SYSTEM_CALL))(using BYTE),
                ICall(PRINT_CALL),
                IMov(Reg(FIRST_PARAM_REG), Imm(FLUSH_ALL)),
                ICall(FLUSH_CALL),
                IMov(Reg(FIRST_PARAM_REG), Imm(ERROR_EXIT_CODE))(using BYTE),
                ICall(EXIT_CALL)
            ) ++ prebuiltGenerator.functionEnd
        )
}

/* Prebuilt section for attempts to dereference or free a null pair. */
case object PbErrNull extends Prebuilt {
    def labelString = "_errNull"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: null pair dereferenced or freed\\n"))
    val block = Block (
        Label(labelString),
        Some(roData),
        prebuiltGenerator.errorExitInstrs(roData(FIRST_LABEL).label)
    )
}

/* Prebuilt section for malloc failing due to insufficient memory. */
case object PbErrOutOfMemory extends Prebuilt {
    def labelString = "_errOutOfMemory"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: out of memory\\n"))
    val block = Block (
        Label(labelString),
        Some(roData),
        prebuiltGenerator.errorExitInstrs(roData(FIRST_LABEL).label)
    )
}

/* Prebuilt section for malloc calls. */
case object PbMalloc extends Prebuilt {
    def labelString = "_malloc"
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            None,
            prebuiltGenerator.functionStart ++ List(
                ICall(MALLOC_CALL),
                ICmp(Reg(RETURN_REG), Imm(NULL)),
                Jmp(Label(PbErrOutOfMemory.labelString), JumpCond.Eq)
            ) ++ prebuiltGenerator.functionEnd
        )
}

/* Prebuilt section for exit calls. */
case object PbExit extends Prebuilt{
    def labelString = "_exit"
    val block = Block (
        Label(labelString),
        None,
        prebuiltGenerator.functionStart ++ List(
            ICall(EXIT_CALL)
        ) ++ prebuiltGenerator.functionEnd
    )
}

/* Prebuilt section for over or under flowing in integer calculations. */
case object PbErrOverflow extends Prebuilt{
    def labelString = "_errOverflow"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: integer overflow or underflow occurred\\n"))
    val block = Block (
        Label(labelString),
        Some(roData),
        prebuiltGenerator.errorExitInstrs(roData(FIRST_LABEL).label)
    )
}

/* Prebuilt section for a 0 denominator in division. */
case object PbDivZero extends Prebuilt{
    def labelString = "_errDivZero"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: division or modulo by zero\\n"))
    val block = Block (
        Label(labelString),
        Some(roData),
        prebuiltGenerator.errorExitInstrs(roData(FIRST_LABEL).label)
    )
}

/* Prebuilt section for a print call. */
case class PbPrint(varType: KnownType) extends Prebuilt{
    def labelString = varType match {
        case ArrayT(CharT()) => "_prints"
        case ArrayT(_)       => "_printp"
        case PairT(_, _)     => "_printp"
        case IntT()          => "_printi"
        case BoolT()         => "_printb"
        case CharT()         => "_printc"
        case StringT()       => "_prints"
        case _               => ""
    }
    private val roData: List[RoData] = varType match {
        case CharT() => generateRoData(List("%c"))
        case IntT() => generateRoData(List("%d"))
        case ArrayT(CharT()) => generateRoData(List("%.*s"))
        case ArrayT(_) => generateRoData(List("%p"))
        case PairT(_,_) => generateRoData(List("%p"))
        case StringT() => generateRoData(List("%.*s"))
        case BoolT() => generateRoData(List("false", "true", "%.*s"))
        case _ => List()
    }
    given DataSize = QWORD
    val block: Block = varType match {
        case CharT() => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(BYTE, roData(FIRST_LABEL).label.name))
        case IntT() => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(DWORD, roData(FIRST_LABEL).label.name))
        case ArrayT(CharT()) => Block(Label(labelString), Some(roData), prebuiltGenerator.printsBlock(roData(FIRST_LABEL)))
        case ArrayT(_) => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(QWORD, roData(FIRST_LABEL).label.name))
        case PairT(_,_) => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(QWORD, roData(FIRST_LABEL).label.name))
        case StringT() => Block(Label(labelString), Some(roData), prebuiltGenerator.printsBlock(roData(FIRST_LABEL)))
        case BoolT() => 
            val jmpLabels = List(".L_printb0", ".L_printb1")
            Block (
                Label(labelString),
                Some(roData),
                prebuiltGenerator.functionStart ++ List(
                    ICmp(Reg(FIRST_PARAM_REG), Imm(FALSE))(using BYTE),
                    Jmp(Label(jmpLabels(FIRST_LABEL)), JumpCond.NotEq),
                    ILea(Reg(THIRD_PARAM_REG), Mem(roData(FIRST_LABEL).label)),
                    Jmp(Label(jmpLabels(SECOND_LABEL))),
                    Label(jmpLabels(FIRST_LABEL)),
                    ILea(Reg(THIRD_PARAM_REG), Mem(roData(SECOND_LABEL).label)),
                    Label(jmpLabels(SECOND_LABEL)),
                    IMov(Reg(SECOND_PARAM_REG), Mem(THIRD_PARAM_REG, -(roData(THIRD_LABEL).str.length))),
                    ILea(Reg(FIRST_PARAM_REG), Mem(roData(THIRD_LABEL).label)),
                    IMov(Reg(SYSTEM_CALL_REG), Imm(PRINT_SYSTEM_CALL))(using BYTE),
                    ICall(PRINT_CALL),
                    IMov(Reg(FIRST_PARAM_REG), Imm(FLUSH_ALL)),
                    ICall(FLUSH_CALL)
                ) ++ prebuiltGenerator.functionEnd
            )
        case _ => Block(Label(labelString), None, List())
    }
}

/* Prebuilt section for a println call. */
case class PbPrintln(varType: KnownType) extends Prebuilt{
    def labelString = PbPrint(varType).labelString
    val label: String = "_println"
    private val roData: List[RoData] = 
        List(RoData("", Label(s".L.${label}_str0")))
    val block = 
        given DataSize = QWORD
        Block (
            Label(label),
            Some(roData),
            prebuiltGenerator.functionStart ++ List(
                ILea(Reg(FIRST_PARAM_REG), Mem(roData(FIRST_LABEL).label)),
                ICall(PUTS_CALL),
                IMov(Reg(FIRST_PARAM_REG), Imm(FLUSH_ALL)),
                ICall(FLUSH_CALL)
            ) ++ prebuiltGenerator.functionEnd
        )
}

/* Prebuilt section for a free or freepair call. */
case class PbFree(varType: KnownType) extends Prebuilt{
    def labelString = varType match {
        case ArrayT(_) => "_free"
        case PairT(_,_) => "_freepair"
        case _ => ""
    }
    given DataSize = QWORD
    val instrs: List[Instr] = varType match {
        case ArrayT(_) => 
                List(
                    ICall(FREE_CALL)
                )
        case PairT(_,_) => 
                List(
                    ICmp(Reg(FIRST_PARAM_REG), Imm(NULL)),
                    Jmp(Label(PbErrNull.labelString), JumpCond.Eq),
                    ICall(FREE_CALL)
                )
        case _ => List()
    }
    val block = Block (Label(labelString), None, 
        prebuiltGenerator.functionStart ++ instrs ++ prebuiltGenerator.functionEnd)
}

/* Prebuilt section for a read call. */
case class PbRead(arType: KnownType) extends Prebuilt{
    def labelString = arType match{
        case CharT() => "_readc"
        case IntT()  => "_readi"
        case _ => ""
    }
    private val roData: List[RoData] = arType match {
        case CharT() => generateRoData(List(" %c"))
        case IntT() => generateRoData(List("%d"))
        case _ => List()
    }
    val block: Block = arType match {
        case CharT() => Block(Label(labelString), Some(roData), prebuiltGenerator.readBlock(BYTE, roData(FIRST_LABEL).label.name))
        case IntT() => Block(Label(labelString), Some(roData), prebuiltGenerator.readBlock(DWORD, roData(FIRST_LABEL).label.name))
        case _ => Block(Label(labelString), None, List())
    }
}

/* Prebuilt section for an array reference. */
case class PbArrRef(size: DataSize) extends Prebuilt {
    def labelString = s"_arrRef${size.bytes}"
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            None,
            List(
                IPush(Reg(ARR_REF_TEMP_REG)),
                ITest(Reg(ARR_REF_PARAM_REG), Reg(ARR_REF_PARAM_REG))(using DWORD),
                IMov(Reg(SECOND_PARAM_REG), Reg(ARR_REF_PARAM_REG), JumpCond.Less),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.Less),
                IMov(Reg(ARR_REF_TEMP_REG), Mem(ARR_REF_RETURN_REG, -ARRAY_LENGTH_SIZE)),
                ICmp(Reg(ARR_REF_PARAM_REG), Reg(ARR_REF_TEMP_REG))(using DWORD),
                IMov(Reg(SECOND_PARAM_REG), Reg(ARR_REF_PARAM_REG), JumpCond.GrEq),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.GrEq),
                ILea(Reg(ARR_REF_RETURN_REG), Mem(ARR_REF_RETURN_REG, ARR_REF_PARAM_REG, size)),
                IPop(Reg(ARR_REF_TEMP_REG)),
                IRet
            )
        )
}

/* Prebuilt generator. */
object prebuiltGenerator {
    /* Ensures prebuilt dependencies for prebuilts are included. */
    def generatePrebuiltBlock(prebuilt: Prebuilt)(using builder: Set[Block]): Unit = 
        given Set[Block] = builder
        prebuilt match {
            case (PbErrNull|PbErrOutOfMemory|PbErrOverflow|PbDivZero) => generatePrebuiltBlock(PbPrint(StringT()))
            case PbMalloc           => generatePrebuiltBlock(PbErrOutOfMemory)
            case PbPrintln(varType) => generatePrebuiltBlock(PbPrint(varType))
            case PbFree(PairT(_,_)) => generatePrebuiltBlock(PbErrNull)
            case PbArrRef(size)     => generatePrebuiltBlock(PbOutOfBounds)
            case _ => List.empty
        }
        builder += prebuilt.block
    
    /* Instruction to align the stack after a function call. */
    val STACK_ALIGNMENT = 16
    val alignStack: Instr = IAnd(Reg(STACK_PTR_REG), Imm(-STACK_ALIGNMENT))(using QWORD)

    /* Instruction set that appears at the start of each function. */
    val functionStart: List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(BASE_PTR_REG)),
            IMov(Reg(BASE_PTR_REG), Reg(STACK_PTR_REG)),
            alignStack
        )

    /* Instruction set that appears at the end of each function. */
    val functionEnd: List[Instr] =
        given DataSize = QWORD
        List(
            IMov(Reg(STACK_PTR_REG), Reg(BASE_PTR_REG)),
            IPop(Reg(BASE_PTR_REG)),
            IRet
        )

    /* Instruction set that appears at the end of each print call. */
    val printEnd: List[Instr] =
        given DataSize = QWORD
        List(
            IMov(Reg(SYSTEM_CALL_REG), Imm(PRINT_SYSTEM_CALL))(using BYTE),
            ICall(PRINT_CALL),
            IMov(Reg(FIRST_PARAM_REG), Imm(FLUSH_ALL)),
            ICall(FLUSH_CALL)
        )
    
    /* Instruction set that appears before an exit call. */
    def errorExitInstrs(label: Label): List[Instr] = 
        given DataSize = QWORD
        List(
            alignStack,
            ILea(Reg(FIRST_PARAM_REG), Mem(label)),
            ICall(PbPrint(StringT()).labelString),
            IMov(Reg(FIRST_PARAM_REG), Imm(ERROR_EXIT_CODE))(using BYTE),
            ICall("exit@plt")
        )
    
    /* Instruction set that appears around a read call. */
    def readBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            ISub(Reg(STACK_PTR_REG), Imm(STACK_ALIGNMENT)),
            IMov(Mem(STACK_PTR_REG), Reg(FIRST_PARAM_REG))(using size),
            ILea(Reg(SECOND_PARAM_REG), Mem(STACK_PTR_REG)),
            ILea(Reg(FIRST_PARAM_REG), Mem(Label(label))),
            IMov(Reg(SYSTEM_CALL_REG), Imm(READ_SYSTEM_CALL))(using BYTE),
            ICall("scanf@plt"),
            IMov(Reg(RETURN_REG), Mem(STACK_PTR_REG))(using size),
            IAdd(Reg(STACK_PTR_REG), Imm(STACK_ALIGNMENT))
        ) ++ functionEnd
    
    /* Instruction set for any print block. */
    def genericPrintBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(SECOND_PARAM_REG), Reg(FIRST_PARAM_REG))(using size),
            ILea(Reg(FIRST_PARAM_REG), Mem(Label(label))),
        ) ++ printEnd ++ functionEnd
    
    /* Instruction set for a prints. */
    def printsBlock(roData: RoData): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(THIRD_PARAM_REG), Reg(FIRST_PARAM_REG)),
            IMov(Reg(SECOND_PARAM_REG), Mem(FIRST_PARAM_REG, -(roData.str.length)))(using DWORD),
            ILea(Reg(FIRST_PARAM_REG), Mem(roData.label)),    
        ) ++ printEnd ++ functionEnd
}
