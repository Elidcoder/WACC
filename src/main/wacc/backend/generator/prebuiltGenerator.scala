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
        case _ => ???
    }
}
case class PbRead(arType: KnownType) extends Prebuilt{
    def labelString = arType match{
        case CharT() => "_readc"
        case IntT()  => "_readi"
        case _ => ""
    }
}
case class PbArrLoad(size: DataSize) extends Prebuilt {
    def labelString = s"_arrLoad${size.bytes}"
}
case class PbArrStore(size: DataSize) extends Prebuilt {
    def labelString = s"_arrStore${size.bytes}"
}

object prebuiltGenerator {
    def generatePrebuiltBlock(prebuilt: Prebuilt): List[Block] = prebuilt match {
        case PbMalloc => mallocBlock :: errOutOfMemory :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbExit => List(exitBlock)
        case PbDivZero => divZeroBlock :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbErrOverflow => overflowBlock :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbPrint(varType) => varType match {
            case ArrayT(CharT()) => List(printsBlock)
            case CharT() => List(printcBlock)
            case IntT() => List(printiBlock)
            case BoolT() => List(printbBlock)
            case ArrayT(_) => List(printpBlock)
            case StringT() => List(printsBlock)
            case PairT(_,_) => List(printpBlock)
            case _          => ???
        }
        case PbPrintln(varType) => printlnBlock :: generatePrebuiltBlock(PbPrint(varType))
        case PbFree(varType) => varType match {
            case ArrayT(_) => List(freeBlock)
            case PairT(_,_) => freePairBlock :: generatePrebuiltBlock(PbErrNull) 
            case _ => ???
        }
        case PbRead(varType) => varType match {
            case CharT() => List(readcBlock)
            case IntT() => List(readiBlock)
            case _      => List()
        }
        case PbErrNull => errNull :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbArrLoad(size) => genericArrLoadBlock(size) :: generatePrebuiltBlock(PbOutOfBounds)
        case PbArrStore(size) => genericArrStoreBlock(size) :: generatePrebuiltBlock(PbOutOfBounds)
        case PbOutOfBounds => List(outOfBoundsBlock)
        case PbErrBadChar => List(badCharBlock)
    }
    private def readBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(RBP)),
            IMov(Reg(RBP), Reg(RSP)),
            IAnd(Reg(RSP), Imm(-16)),
            ISub(Reg(RSP), Imm(16)),
            IMov(MemInd(RSP), Reg(RDI))(using size),
            ILea(Reg(RSI), MemOff(RSP, 0)),
            ILea(Reg(RDI), Rip(Label(label))),
            IMov(Reg(RAX), Imm(0))(using BYTE),
            ICall("scanf@plt"),
            IMov(Reg(RAX), MemInd(RSP))(using size = size),
            IAdd(Reg(RSP), Imm(16)),
            IMov(Reg(RSP), Reg(RBP)),
            IPop(Reg(RBP)),
            IRet
        )
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
    val mallocBlock: Block = 
        given DataSize = QWORD
        Block (
            Label("_malloc"),
            None,
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                ICall("malloc@plt"),
                ICmp(Reg(RAX), Imm(0)),
                Jmp(Label("_errOutOfMemory"), JumpCond.E),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    // private val printEnd = 
    //     given DataSize = QWORD
    //     List(
    //         IMov(Reg(RAX), Imm(0))(using size = BYTE),
    //         ICall("puts@plt"),
    //         IMov(Reg(RDI), Imm(0)),
    //         ICall("fflush@plt"),
    //         IMov(Reg(RSP), Reg(RBP)),
    //         IPop(Reg(RBP)),
    //         IRet
    //     )
    private def genericPrintBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(RBP)),
            IMov(Reg(RBP), Reg(RSP)),
            IAnd(Reg(RSP), Imm(-16)),
            IMov(Reg(RSI), Reg(RDI))(using size = size),
            ILea(Reg(RDI), Rip(Label(label))),
            IMov(Reg(RAX), Imm(0))(using size = BYTE),
            ICall("printf@plt"),
            IMov(Reg(RDI), Imm(0)),
            ICall("fflush@plt"),
            IMov(Reg(RSP), Reg(RBP)),
            IPop(Reg(RBP)),
            IRet
        )
    val printcBlock: Block = Block (
        Label("_printc"),
        Some(List(RoData(2, "%c", Label(".L._printc_str0")))),
        genericPrintBlock(BYTE, ".L._printc_str0")
    )
    val printiBlock: Block = Block (
        Label("_printi"),
        Some(List(RoData(2, "%d", Label(".L._printi_str0")))),
        genericPrintBlock(DWORD, ".L._printi_str0")
    )
    val printpBlock: Block = Block (
        Label("_printp"),
        Some(List(RoData(2, "%p", Label(".L._printp_str0")))),
        genericPrintBlock(QWORD, ".L._printp_str0")
    )
    val printbBlock: Block =  
        given DataSize = QWORD
        Block (
            Label("_printb"),
            Some(List(
                RoData(5, "false", Label(".L._printb_str0")),
                RoData(4, "true", Label(".L._printb_str1")),
                RoData(4, "%.*s", Label(".L._printb_str2"))
            )),
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                ICmp(Reg(RDI), Imm(0))(using size = BYTE),
                Jmp(Label(".L_printb0"), JumpCond.NE),
                ILea(Reg(RDX), Rip(Label(".L._printb_str0"))),
                Jmp(Label(".L_printb1"), JumpCond.UnCond),
                Label(".L_printb0"),
                ILea(Reg(RDX), Rip(Label(".L._printb_str1"))),
                Label(".L_printb1"),
                IMov(Reg(RSI), MemOff(RDX, -4)),
                ILea(Reg(RDI), Rip(Label(".L._printb_str2"))),
                IMov(Reg(RAX), Imm(0))(using size = BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    val printsBlock: Block = 
        given DataSize = QWORD
        Block (
            Label("_prints"),
            Some(List(RoData(4, "%.*s", Label(".L._prints_str0")))),
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                IMov(Reg(RDX), Reg(RDI)),
                IMov(Reg(RSI), MemOff(RDI, -4)),
                ILea(Reg(RDI), Rip(Label(".L._prints_str0"))),
                IMov(Reg(RAX), Imm(0))(using size = BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    val printlnBlock = 
        given DataSize = QWORD
        Block (
            Label("_println"),
            Some(List(RoData(0, "", Label(".L._println_str0")))),
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._println_str0"))),
                ICall("puts@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    val overflowBlock = 
        given DataSize = QWORD
        Block (
            Label("_errOverflow"),
            Some(List(RoData(52, "fatal error: integer overflow or underflow occurred\\n", Label(".L._errOverflow_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errOverflow_str0"))),
                ICall("_prints"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )
    val divZeroBlock = 
        given DataSize = QWORD
        Block (
            Label("_errDivZero"),
            Some(List(RoData(40, "fatal error: division or modulo by zero\\n", Label(".L._errDivZero_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errDivZero_str0"))),
                ICall("_prints"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )
    val errOutOfMemory =
        given DataSize = QWORD 
        Block (
            Label("_errOutOfMemory"),
            Some(List(RoData(27, "fatal error: out of memory\\n", Label(".L._errOutOfMemory_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errOutOfMemory_str0"))),
                ICall("_prints"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )
    val exitBlock = 
        given DataSize = QWORD
        Block (
            Label("_exit"),
            None,
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                ICall("exit@plt"),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    val freePairBlock = 
        given DataSize = QWORD
        Block (
            Label("_freepair"),
            None,
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                ICmp(Reg(RDI), Imm(0)),
                Jmp(Label("_errNull"), JumpCond.E),
                ICall("free@plt"),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    val errNull = 
        given DataSize = QWORD
        Block (
            Label("_errNull"),
            Some(List(RoData(45, "fatal error: null pair dereferenced or freed\\n", Label(".L._errNull_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errNull_str0"))),
                ICall("_prints"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )
    val freeBlock = 
        given DataSize = QWORD
        Block (
            Label("_free"),
            None,
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                ICall("free@plt"),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    val badCharBlock = 
        given DataSize = QWORD
        Block (
            Label("_errBadChar"),
            Some(List(RoData(50, "fatal error: int %d is not ascii character 0-127 \\n", Label(".L._errBadChar_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errBadChar_str0"))),
                IMov(Reg(RAX), Imm(0))(using size = BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )

    val outOfBoundsBlock = 
        given DataSize = QWORD
        Block (
            Label("_errOutOfBounds"),
            Some(List(RoData(42, "fatal error: array index %d out of bounds\\n", Label(".L._errOutOfBounds_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errOutOfBounds_str0"))),
                IMov(Reg(RAX), Imm(0))(using size = BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )

    def genericArrLoadBlock(size: DataSize): Block = 
        given DataSize = QWORD
        Block (
            Label(s"_arrLoad${size.bytes}"),
            None,
            List(
                IPush(Reg(RBX)),
                ITest(Reg(R10), Reg(R10))(using size),
                IMov(Reg(RSI), Reg(R10), JumpCond.L),
                Jmp(Label("_errOutOfBounds"), JumpCond.L),
                IMov(Reg(RBX), MemOff(R9, -4)),
                ICmp(Reg(R10), Reg(RBX))(using size),
                IMov(Reg(RSI), Reg(R10), JumpCond.GE),
                Jmp(Label("_errOutOfBounds"), JumpCond.GE),
                IMov(Reg(R9), MemOff(R9, 4))(using size), // MemOff wrong
                IPop(Reg(RBX)),
                IRet
            )
        )
    
    def genericArrStoreBlock(size: DataSize): Block = 
        given DataSize = QWORD
        Block (
            Label(s"_arrStore${size.bytes}"),
            None,
            List(
                IPush(Reg(RBX)),
                ITest(Reg(R10), Reg(R10))(using size),
                IMov(Reg(RSI), Reg(R10), JumpCond.L),
                Jmp(Label("_errOutOfBounds"), JumpCond.L),
                IMov(Reg(RBX), MemOff(R9, -4)),
                ICmp(Reg(R10), Reg(RBX))(using size),
                IMov(Reg(RSI), Reg(R10), JumpCond.GE),
                Jmp(Label("_errOutOfBounds"), JumpCond.GE),
                IMov(MemOff(R9, 4), Reg(RAX))(using size), // MemOff wrong
                IPop(Reg(RBX)),
                IRet
            )
        )
}
