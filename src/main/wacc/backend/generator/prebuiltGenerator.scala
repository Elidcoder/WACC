package wacc.backend.generator.prebuilts

import wacc.backend.ir._
import wacc.ast.KnownType
import wacc.ast.{CharT, StringT, IntT, BoolT, ArrayT, PairT}
import wacc.backend.generator.{RETURN_REG, TEMP_REG}

sealed trait Prebuilt {
    def labelString: String
    val block: Block
    def generateRoData(strs: List[String]): List[RoData] = 
        strs.zipWithIndex.map { case (str, index) => 
            RoData(str.length, str, Label(s".L.${labelString}_str${index}"))
        }
}

case object PbOutOfBounds extends Prebuilt {
    def labelString = "_errOutOfBounds"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: array index %d out of bounds\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
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
                ILea(Reg(FIRST_PARAM_REG), Mem(roData(0).label)),
                IMov(Reg(SYSTEM_CALL_REG), Imm(0))(using BYTE),
                ICall("printf@plt"),
                IMov(Reg(FIRST_PARAM_REG), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(FIRST_PARAM_REG), Imm(-1))(using BYTE),
                ICall("exit@plt")
            ) ++ prebuiltGenerator.functionEnd
        )
}
case object PbErrNull extends Prebuilt {
    def labelString = "_errNull"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: null pair dereferenced or freed\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbErrOutOfMemory extends Prebuilt {
    def labelString = "_errOutOfMemory"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: out of memory\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbMalloc extends Prebuilt {
    def labelString = "_malloc"
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            None,
            prebuiltGenerator.functionStart ++ List(
                ICall("malloc@plt"),
                ICmp(Reg(RETURN_REG), Imm(0)),
                Jmp(Label(PbErrOutOfMemory.labelString), JumpCond.E)
            ) ++ prebuiltGenerator.functionEnd
        )
}
case object PbExit extends Prebuilt{
    def labelString = "_exit"
    val block = 
        Block (
            Label(labelString),
            None,
            prebuiltGenerator.functionStart ++ List(
                ICall("exit@plt")
            ) ++ prebuiltGenerator.functionEnd
        )
}
case object PbErrOverflow extends Prebuilt{
    def labelString = "_errOverflow"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: integer overflow or underflow occurred\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbDivZero extends Prebuilt{
    def labelString = "_errDivZero"
    private val roData: List[RoData] = 
        generateRoData(List("fatal error: division or modulo by zero\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
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
        case CharT() => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(BYTE, roData(0).label.name))
        case IntT() => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(DWORD, roData(0).label.name))
        case ArrayT(CharT()) => Block(Label(labelString), Some(roData), prebuiltGenerator.printsBlock)
        case ArrayT(_) => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(QWORD, roData(0).label.name))
        case PairT(_,_) => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(QWORD, roData(0).label.name))
        case StringT() => Block(Label(labelString), Some(roData), prebuiltGenerator.printsBlock)
        case BoolT() => 
            Block (
                Label(labelString),
                Some(roData),
                prebuiltGenerator.functionStart ++ List(
                    ICmp(Reg(FIRST_PARAM_REG), Imm(0))(using BYTE),
                    Jmp(Label(".L_printb0"), JumpCond.NE),
                    ILea(Reg(THIRD_PARAM_REG), Mem(roData(0).label)),
                    Jmp(Label(".L_printb1")),
                    Label(".L_printb0"),
                    ILea(Reg(THIRD_PARAM_REG), Mem(roData(1).label)),
                    Label(".L_printb1"),
                    IMov(Reg(SECOND_PARAM_REG), Mem(THIRD_PARAM_REG, -4)),
                    ILea(Reg(FIRST_PARAM_REG), Mem(roData(2).label)),
                    IMov(Reg(SYSTEM_CALL_REG), Imm(0))(using BYTE),
                    ICall("printf@plt"),
                    IMov(Reg(FIRST_PARAM_REG), Imm(0)),
                    ICall("fflush@plt")
                ) ++ prebuiltGenerator.functionEnd
            )
        case _ => Block(Label(labelString), None, List())
    }
}
case class PbPrintln(varType: KnownType) extends Prebuilt{
    def labelString = PbPrint(varType).labelString
    val label: String = "_println"
    private val roData: List[RoData] = 
        List(RoData(0, "", Label(s".L.${label}_str0")))
    val block = 
        given DataSize = QWORD
        Block (
            Label(label),
            Some(roData),
            prebuiltGenerator.functionStart ++ List(
                ILea(Reg(FIRST_PARAM_REG), Mem(roData(0).label)),
                ICall("puts@plt"),
                IMov(Reg(FIRST_PARAM_REG), Imm(0)),
                ICall("fflush@plt")
            ) ++ prebuiltGenerator.functionEnd
        )
}
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
                    ICall("free@plt")
                )
        case PairT(_,_) => 
                List(
                    ICmp(Reg(FIRST_PARAM_REG), Imm(0)),
                    Jmp(Label(PbErrNull.labelString), JumpCond.E),
                    ICall("free@plt")
                )
        case _ => List()
    }
    val block = Block (Label(labelString), None, 
        prebuiltGenerator.functionStart ++ instrs ++ prebuiltGenerator.functionEnd)
}
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
        case CharT() => Block(Label(labelString), Some(roData), prebuiltGenerator.readBlock(BYTE, roData(0).label.name))
        case IntT() => Block(Label(labelString), Some(roData), prebuiltGenerator.readBlock(DWORD, roData(0).label.name))
        case _ => Block(Label(labelString), None, List())
    }
}
case class PbArrRef(size: DataSize) extends Prebuilt {
    def labelString = s"_arrRef${size.bytes}"
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            None,
            List(
                IPush(Reg(BASE_MEMORY_REG)),
                ITest(Reg(TEMP_REG), Reg(TEMP_REG))(using DWORD),
                IMov(Reg(SECOND_PARAM_REG), Reg(TEMP_REG), JumpCond.L),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.L),
                IMov(Reg(BASE_MEMORY_REG), Mem(SIXTH_PARAM_REG, -4)),
                ICmp(Reg(TEMP_REG), Reg(BASE_MEMORY_REG))(using DWORD),
                IMov(Reg(SECOND_PARAM_REG), Reg(TEMP_REG), JumpCond.GE),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.GE),
                ILea(Reg(SIXTH_PARAM_REG), Mem(SIXTH_PARAM_REG, TEMP_REG, size)),
                IPop(Reg(BASE_MEMORY_REG)),
                IRet
            )
        )
}

object prebuiltGenerator {
    def generatePrebuiltBlock(prebuilt: Prebuilt): List[Block] = 
        val next: List[Block] = prebuilt match {
            case (PbErrNull|PbErrOutOfMemory|PbErrOverflow|PbDivZero) => generatePrebuiltBlock(PbPrint(StringT()))
            case PbMalloc           => generatePrebuiltBlock(PbErrOutOfMemory)
            case PbPrintln(varType) => generatePrebuiltBlock(PbPrint(varType))
            case PbFree(PairT(_,_)) => generatePrebuiltBlock(PbErrNull)
            case PbArrRef(size)     => generatePrebuiltBlock(PbOutOfBounds)
            case _ => List.empty
        }
        prebuilt.block :: next
    val alignStack: Instr = IAnd(Reg(STACK_PTR_REG), Imm(-16))(using QWORD)
    val functionStart: List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(BASE_PTR_REG)),
            IMov(Reg(BASE_PTR_REG), Reg(STACK_PTR_REG)),
            alignStack
        )
    val functionEnd: List[Instr] =
        given DataSize = QWORD
        List(
            IMov(Reg(STACK_PTR_REG), Reg(BASE_PTR_REG)),
            IPop(Reg(BASE_PTR_REG)),
            IRet
        )
    def errorExitInstrs(label: Label): List[Instr] = 
        given DataSize = QWORD
        List(
            alignStack,
            ILea(Reg(FIRST_PARAM_REG), Mem(label)),
            ICall(PbPrint(StringT()).labelString),
            IMov(Reg(FIRST_PARAM_REG), Imm(-1))(using BYTE),
            ICall("exit@plt")
        )
    def readBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            ISub(Reg(STACK_PTR_REG), Imm(16)),
            IMov(Mem(STACK_PTR_REG), Reg(FIRST_PARAM_REG))(using size),
            ILea(Reg(SECOND_PARAM_REG), Mem(STACK_PTR_REG, 0)),
            ILea(Reg(FIRST_PARAM_REG), Mem(Label(label))),
            IMov(Reg(SYSTEM_CALL_REG), Imm(0))(using BYTE),
            ICall("scanf@plt"),
            IMov(Reg(RETURN_REG), Mem(STACK_PTR_REG))(using size),
            IAdd(Reg(STACK_PTR_REG), Imm(16))
        ) ++ functionEnd
    def genericPrintBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(SECOND_PARAM_REG), Reg(FIRST_PARAM_REG))(using size),
            ILea(Reg(FIRST_PARAM_REG), Mem(Label(label))),
            IMov(Reg(SYSTEM_CALL_REG), Imm(0))(using BYTE),
            ICall("printf@plt"),
            IMov(Reg(FIRST_PARAM_REG), Imm(0)),
            ICall("fflush@plt")
        ) ++ functionEnd
    val printsBlock: List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(THIRD_PARAM_REG), Reg(FIRST_PARAM_REG)),
            IMov(Reg(SECOND_PARAM_REG), Mem(FIRST_PARAM_REG, -4))(using DWORD),
            ILea(Reg(FIRST_PARAM_REG), Mem(Label(".L._prints_str0"))),
            IMov(Reg(SYSTEM_CALL_REG), Imm(0))(using BYTE),
            ICall("printf@plt"),
            IMov(Reg(FIRST_PARAM_REG), Imm(0)),
            ICall("fflush@plt")
        ) ++ functionEnd
}
