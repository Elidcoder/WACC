package wacc.backend.generator.prebuilts

import wacc.backend.ir._
import wacc.ast.KnownType
import wacc.ast.{CharT, StringT, IntT, BoolT, ArrayT, PairT}

sealed trait Prebuilt 

case object PbMalloc      extends Prebuilt
case object PbExit        extends Prebuilt
case object PbErrOverflow extends Prebuilt
case object DivZero       extends Prebuilt
case class PbPrint(varType: KnownType)   extends Prebuilt
case class PbPrintln(varType: KnownType) extends Prebuilt
case class PbFree(varType: KnownType)   extends Prebuilt
case class PbRead(arType: KnownType)   extends Prebuilt

object prebuiltGenerator {

    def generatePrebuiltBlock(prebuilt: Prebuilt): List[Block] = prebuilt match {
        case PbMalloc => List(mallocBlock)
        case PbExit => List(exitBlock)
        case DivZero => List(divZeroBlock)
        case PbErrOverflow => List(overflowBlock)
        case PbPrint(varType) => varType match {
            case CharT() => List(printcBlock)
            case IntT() => List(printiBlock)
            case BoolT() => List(printbBlock)
            case ArrayT(_) => List(printpBlock)
            case StringT() => List(printsBlock)
            case PairT(_,_) => List(printpBlock)
            case _          => List()
        }
        case PbPrintln(varType) => printlnBlock :: generatePrebuiltBlock(PbPrint(varType))
        case PbFree(varType) => ???
        case PbRead(varType) => ???
    }
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
                Jmp(Label("_errOverflow"), JumpCond.E),
                IMov(Reg(RSP), Reg(RBP)),
                IPop(Reg(RBP)),
                IRet
            )
        )
    private val printEnd = 
        given DataSize = QWORD
        List(
            IMov(Reg(RAX), Imm(0))(using size = BYTE),
            ICall("puts@plt"),
            IMov(Reg(RDI), Imm(0)),
            ICall("fflush@plt"),
            IMov(Reg(RSP), Reg(RBP)),
            IPop(Reg(RBP)),
            IRet
        )
    private def genericPrintBlock(size: DataSize): List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(RBP)),
            IMov(Reg(RBP), Reg(RSP)),
            IAnd(Reg(RSP), Imm(-16)),
            IMov(Reg(RSI), Reg(RDI))(using size = size),
            ILea(Reg(RDI), Rip(Label(".L._printi_str0"))),
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
        genericPrintBlock(BYTE)
    )
    val printiBlock: Block = Block (
        Label("_printi"),
        Some(List(RoData(2, "%d", Label(".L._printi_str0")))),
        genericPrintBlock(DWORD)
    )
    val printpBlock: Block = Block (
        Label("_printp"),
        Some(List(RoData(2, "%p", Label(".L._printp_str0")))),
        genericPrintBlock(QWORD)
    )
    val printbBlock: Block =  
        given DataSize = QWORD
        Block (
            Label("_printp"),
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
            Some(List(RoData(52, "fatal error: integer overflow or underflow occurred\n", Label(".L._errOverflow_str0")))),
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
            Label("_erRDIvZero"),
            Some(List(RoData(40, "fatal error: division or modulo by zero\n", Label(".L._println_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._erRDIvZero_str0"))),
                ICall("_prints"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )
    val errOutOfMemory =
        given DataSize = QWORD 
        Block (
            Label("_errOutOfMemory"),
            Some(List(RoData(27, "fatal error: out of memory\n", Label(".L._errOutOfMemory_str0")))),
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
}
