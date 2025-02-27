package wacc.backend.generator.prebuilts

import wacc.backend.ir._
import wacc.ast.KnownType

sealed trait Prebuilt 

case object PbMalloc      extends Prebuilt
case object PbExit        extends Prebuilt
case object PbErrOverflow extends Prebuilt
case object DivZero         extends Prebuilt
case class PbPrint(varType: KnownType)   extends Prebuilt
case class PbPrintln(varType: KnownType) extends Prebuilt
case class PbFree( varType: KnownType)   extends Prebuilt
case class PbRead( varType: KnownType)   extends Prebuilt

object prebuiltGenerator {

    def generatePrebuiltBlock(prebuilt: Prebuilt): List[Block] = prebuilt match {
        case PbMalloc => List(mallocBlock)
        case PbExit => List(exitBlock)
        case DivZero => List(divZeroBlock)
        case PbErrOverflow => List(overflowBlock)
        case PbPrint(varType) => List(printBlock)
        case PbPrintln(varType) => List(printBlock, printlnBlock)
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
    val exitBlock: Block = 
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
    val printBlock: Block = 
        given DataSize = QWORD
        Block (
            Label("_prints"),
            Some(List(RoData(4, "%.*s", Label(".L._prints_str0")))),
            List(
                IPush(Reg(RBP)),
                IMov(Reg(RBP), Reg(RSP)),
                IAnd(Reg(RSP), Imm(-16)),
                IMov(Reg(RDX), Reg(RDI)),
                IMov(Reg(RSI), MemOff(RDI, -4))(using size = DWORD),
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
            Label("_errDivZero"),
            Some(List(RoData(40, "fatal error: division or modulo by zero\n", Label(".L._println_str0")))),
            List(
                IAnd(Reg(RSP), Imm(-16)),
                ILea(Reg(RDI), Rip(Label(".L._errDivZero_str0"))),
                ICall("_prints"),
                IMov(Reg(RDI), Imm(-1))(using size = BYTE),
                ICall("exit@plt")
            )
        )
}
