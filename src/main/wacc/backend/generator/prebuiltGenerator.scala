package wacc.backend.generator.prebuilts

import wacc.backend.ir._
import wacc.ast.KnownType
import wacc.ast.{CharT, StringT, IntT, BoolT, ArrayT, PairT}

sealed trait Prebuilt {
    def labelString: String
}

case object PbOutOfBounds extends Prebuilt {
    def labelString = "_errOutOfBounds"
}
case object PbErrBadChar extends Prebuilt {
    def labelString = "_errBadChar"
}
case object PbErrNull extends Prebuilt {
    def labelString = "_errNull"
}
case object PbErrOutOfMemory extends Prebuilt {
    def labelString = "_errOutOfMemory"
}
case object PbMalloc extends Prebuilt {
    def labelString = "_malloc"
}
case object PbExit extends Prebuilt{
    def labelString = "_exit"
}
case object PbErrOverflow extends Prebuilt{
    def labelString = "_errOverflow"
}
case object PbDivZero extends Prebuilt{
    def labelString = "_errDivZero"
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
}
case class PbPrintln(varType: KnownType) extends Prebuilt{
    def labelString = PbPrint(varType).labelString
}
case class PbFree(varType: KnownType) extends Prebuilt{
    def labelString = varType match {
        case ArrayT(_) => "_free"
        case PairT(_,_) => "_freepair"
        case _ => ""
    }
}
case class PbRead(arType: KnownType) extends Prebuilt{
    def labelString = arType match{
        case CharT() => "_readc"
        case IntT()  => "_readi"
        case _ => ""
    }
}
case class PbArrRef(size: DataSize) extends Prebuilt {
    def labelString = s"_arrRef${size.bytes}"
}

object prebuiltGenerator {
    def generatePrebuiltBlock(prebuilt: Prebuilt): List[Block] = prebuilt match {
        case PbMalloc => mallocBlock :: generatePrebuiltBlock(PbErrOutOfMemory) ++ generatePrebuiltBlock(PbPrint(StringT()))
        case PbExit => List(exitBlock)
        case PbDivZero => divZeroBlock :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbErrOverflow => errOverflowBlock :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbPrint(varType) => varType match {
            case ArrayT(CharT()) => List(printsBlock)
            case CharT() => List(printcBlock)
            case IntT() => List(printiBlock)
            case BoolT() => List(printbBlock)
            case ArrayT(_) => List(printpBlock)
            case StringT() => List(printsBlock)
            case PairT(_,_) => List(printpBlock)
            case _          => List()
        }
        case PbPrintln(varType) => printlnBlock :: generatePrebuiltBlock(PbPrint(varType))
        case PbFree(varType) => varType match {
            case ArrayT(_) => List(freeBlock)
            case PairT(_,_) => freePairBlock :: generatePrebuiltBlock(PbErrNull) 
            case _ => List()
        }
        case PbRead(varType) => varType match {
            case CharT() => List(readcBlock)
            case IntT() => List(readiBlock)
            case _      => List()
        }
        case PbErrNull => errNullBlock :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbArrRef(size) => genericArrRefBlock(size) :: generatePrebuiltBlock(PbOutOfBounds)
        case PbOutOfBounds => List(outOfBoundsBlock)
        case PbErrOutOfMemory => List(errOutOfMemoryBlock)
        case PbErrBadChar => List(errBadCharBlock)
    }
    def generateRoData(labelString: String, strs: List[String]): List[RoData] = 
        strs.zipWithIndex.map { case (str, index) => 
            RoData(str.length, str, Label(s".L.${labelString}_str${index}"))
        }
    val alignStack: Instr = IAnd(Reg(RSP), Imm(-16))(using QWORD)
    private val functionStart: List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(RBP)),
            IMov(Reg(RBP), Reg(RSP)),
            alignStack
        )
    private val functionEnd: List[Instr] =
        given DataSize = QWORD
        List(
            IMov(Reg(RSP), Reg(RBP)),
            IPop(Reg(RBP)),
            IRet
        )
    private def errorExitInstrs(label: Label): List[Instr] = 
        given DataSize = QWORD
        List(
            alignStack,
            ILea(Reg(RDI), Mem(label)),
            ICall(PbPrint(StringT()).labelString),
            IMov(Reg(RDI), Imm(-1))(using BYTE),
            ICall("exit@plt")
        )
    private val outOfBoundsRoData: List[RoData] = 
        generateRoData(PbOutOfBounds.labelString, List("fatal error: array index %d out of bounds\\n"))
    val outOfBoundsBlock = 
        Block (
            Label(PbOutOfBounds.labelString),
            Some(outOfBoundsRoData),
            errorExitInstrs(outOfBoundsRoData(0).label)
        )
    private val errBadCharRoData: List[RoData] = 
        generateRoData(PbErrBadChar.labelString, List("fatal error: int %d is not ascii character 0-127 \\n"))
    private val errBadCharBlock = 
        given DataSize = QWORD
        Block (
            Label(PbErrBadChar.labelString),
            Some(errBadCharRoData),
            functionStart ++ List(
                ILea(Reg(RDI), Mem(errBadCharRoData(0).label)),
                IMov(Reg(RAX), Imm(0))(using BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RDI), Imm(-1))(using BYTE),
                ICall("exit@plt")
            ) ++ functionEnd
        )
    private val errNullRoData: List[RoData] = 
        generateRoData(PbErrNull.labelString, List("fatal error: null pair dereferenced or freed\\n"))
    val errNullBlock = 
        Block (
            Label(PbErrNull.labelString),
            Some(errNullRoData),
            errorExitInstrs(errNullRoData(0).label)
        )
    private val errOutOfMemoryRoData: List[RoData] = 
        generateRoData(PbErrOutOfMemory.labelString, List("fatal error: out of memory\\n"))
    val errOutOfMemoryBlock = 
        Block (
            Label(PbErrOutOfMemory.labelString),
            Some(errOutOfMemoryRoData),
            errorExitInstrs(errOutOfMemoryRoData(0).label)
        )
    val mallocBlock = 
        given DataSize = QWORD
        Block (
            Label(PbMalloc.labelString),
            None,
            functionStart ++ List(
                ICall("malloc@plt"),
                ICmp(Reg(RAX), Imm(0)),
                Jmp(Label(PbErrOutOfMemory.labelString), JumpCond.E)
            ) ++ functionEnd
        )
    private val exitBlock = 
        Block (
            Label(PbExit.labelString),
            None,
            functionStart ++ List(
                ICall("exit@plt")
            ) ++ functionEnd
        )
    private val errOverflowRoData: List[RoData] = 
        generateRoData(PbErrOverflow.labelString, List("fatal error: integer overflow or underflow occurred\\n"))
    val errOverflowBlock = 
        Block (
            Label(PbErrOverflow.labelString),
            Some(errOverflowRoData),
            errorExitInstrs(errOverflowRoData(0).label)
        )
    private val divZeroRoData: List[RoData] = 
        generateRoData(PbDivZero.labelString, List("fatal error: division or modulo by zero\\n"))
    val divZeroBlock = 
        Block (
            Label(PbDivZero.labelString),
            Some(divZeroRoData),
            errorExitInstrs(divZeroRoData(0).label)
        )
    private def readBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            ISub(Reg(RSP), Imm(16)),
            IMov(Mem(RSP), Reg(RDI))(using size),
            ILea(Reg(RSI), Mem(RSP, 0)),
            ILea(Reg(RDI), Mem(Label(label))),
            IMov(Reg(RAX), Imm(0))(using BYTE),
            ICall("scanf@plt"),
            IMov(Reg(RAX), Mem(RSP))(using size),
            IAdd(Reg(RSP), Imm(16))
        ) ++ functionEnd
    val readcBlock = 
        Block (
            Label("_readc"),
            Some(List(RoData(3, " %c", Label(".L._readc_str0")))),
            readBlock(BYTE, ".L._readc_str0")
        )
    val readiBlock =
        Block (
            Label("_readi"),
            Some(List(RoData(2, "%d", Label(".L._readi_str0")))),
            readBlock(DWORD, ".L._readi_str0")
        )
    private def genericPrintBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(RSI), Reg(RDI))(using size),
            ILea(Reg(RDI), Mem(Label(label))),
            IMov(Reg(RAX), Imm(0))(using BYTE),
            ICall("printf@plt"),
            IMov(Reg(RDI), Imm(0)),
            ICall("fflush@plt")
        ) ++ functionEnd
    private val printcRoData: List[RoData] = 
        generateRoData(PbPrint(CharT()).labelString, List("%c"))
    val printcBlock: Block = Block (
        Label(PbPrint(CharT()).labelString),
        Some(printcRoData),
        genericPrintBlock(BYTE, printcRoData(0).label.name)
    )
    private val printiRoData: List[RoData] = 
        generateRoData(PbPrint(IntT()).labelString, List("%d"))
    val printiBlock: Block = Block (
        Label(PbPrint(IntT()).labelString),
        Some(printiRoData),
        genericPrintBlock(DWORD, printiRoData(0).label.name)
    )
    private val printpRoData: List[RoData] = 
        generateRoData(PbPrint(ArrayT(BoolT())).labelString, List("%p"))
    val printpBlock: Block = Block (
        Label(PbPrint(ArrayT(BoolT())).labelString),
        Some(printpRoData),
        genericPrintBlock(QWORD, printpRoData(0).label.name)
    )
    private val printbRoData: List[RoData] = 
        generateRoData(PbPrint(BoolT()).labelString, List("false", "true", "%.*s"))
    val printbBlock: Block =  
        given DataSize = QWORD
        Block (
            Label("_printb"),
            Some(printbRoData),
            functionStart ++ List(
                ICmp(Reg(RDI), Imm(0))(using BYTE),
                Jmp(Label(".L_printb0"), JumpCond.NE),
                ILea(Reg(RDX), Mem(printbRoData(0).label)),
                Jmp(Label(".L_printb1")),
                Label(".L_printb0"),
                ILea(Reg(RDX), Mem(printbRoData(1).label)),
                Label(".L_printb1"),
                IMov(Reg(RSI), Mem(RDX, -4)),
                ILea(Reg(RDI), Mem(printbRoData(2).label)),
                IMov(Reg(RAX), Imm(0))(using BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt")
            ) ++ functionEnd
        )
    private val printsRoData: List[RoData] = 
        generateRoData(PbPrint(StringT()).labelString, List("%.*s"))
    val printsBlock: Block = 
        given DataSize = QWORD
        Block (
            Label(PbPrint(StringT()).labelString),
            Some(printsRoData),
            functionStart ++ List(
                IMov(Reg(RDX), Reg(RDI)),
                IMov(Reg(RSI), Mem(RDI, -4))(using DWORD),
                ILea(Reg(RDI), Mem(printsRoData(0).label)),
                IMov(Reg(RAX), Imm(0))(using BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt")
            ) ++ functionEnd
        )
    private val printlnRoData: List[RoData] = 
        generateRoData("_println", List(""))
    val printlnBlock = 
        given DataSize = QWORD
        Block (
            Label("_println"),
            Some(printlnRoData),
            functionStart ++ List(
                ILea(Reg(RDI), Mem(printlnRoData(0).label)),
                ICall("puts@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt")
            ) ++ functionEnd
        )
    val freePairBlock = 
        given DataSize = QWORD
        Block (
            Label("_freepair"),
            None,
            functionStart ++ List(
                ICmp(Reg(RDI), Imm(0)),
                Jmp(Label(PbErrNull.labelString), JumpCond.E),
                ICall("free@plt")
            ) ++ functionEnd
        )
    val freeBlock = 
        Block (
            Label("_free"),
            None,
            functionStart ++ List(
                ICall("free@plt")
            ) ++ functionEnd
        )
    def genericArrRefBlock(size: DataSize): Block = 
        given DataSize = QWORD
        Block (
            Label(s"_arrRef${size.bytes}"),
            None,
            List(
                IPush(Reg(RBX)),
                ITest(Reg(R10), Reg(R10))(using DWORD),
                IMov(Reg(RSI), Reg(R10), JumpCond.L),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.L),
                IMov(Reg(RBX), Mem(R9, -4)),
                ICmp(Reg(R10), Reg(RBX))(using DWORD),
                IMov(Reg(RSI), Reg(R10), JumpCond.GE),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.GE),
                ILea(Reg(R9), Mem(R9, R10, size)),
                IPop(Reg(RBX)),
                IRet
            )
        )
}
