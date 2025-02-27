package wacc.backend.generator.prebuilts

import wacc.backend.ir._

object prebuiltGenerator {
    val exitBlock: Block = Block (
        Label("_exit"),
        None,
        List(
            IMov(Reg[QWORD](rbp), Reg[QWORD](rsp)),
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ICall("exit@plt"),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IRet
        )
    )
    private val printEnd = 
        List(
            IMov(Reg[BYTE](rax), Imm[BYTE](0)),
            ICall("puts@plt"),
            IMov(Reg[QWORD](rdi), Imm[QWORD](0)),
            ICall("fflush@plt"),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IPop(Reg[QWORD](rbp)),
            IRet
        )
    private def genericPrintBlock(size: DataSize): List[Instr] = 
        List(
            IPush(Reg[QWORD](rbp)),
            IMov(Reg[QWORD](rbp), Reg[QWORD](rsp)),
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            IMov(Reg[size.type](rsi), Reg[size.type](rdi)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._printi_str0"))),
            IMov(Reg[BYTE](rax), Imm[BYTE](0)),
            ICall("printf@plt"),
            IMov(Reg[QWORD](rdi), Imm[QWORD](0)),
            ICall("fflush@plt"),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IPop(Reg[QWORD](rbp)),
            IRet
        )
    val printcBlock: Block = Block (
        Label("_printc"),
        Some(List(RoData(2, "%c", Label(".L._printc_str0")))),
        genericPrintBlock(BYTE())
    )
    val printiBlock: Block = Block (
        Label("_printi"),
        Some(List(RoData(2, "%d", Label(".L._printi_str0")))),
        genericPrintBlock(DWORD())
    )
    val printpBlock: Block = Block (
        Label("_printp"),
        Some(List(RoData(2, "%p", Label(".L._printp_str0")))),
        genericPrintBlock(QWORD())
    )
    val printbBlock: Block =  Block (
        Label("_printp"),
        Some(List(
            RoData(5, "false", Label(".L._printb_str0")),
            RoData(4, "true", Label(".L._printb_str1")),
            RoData(4, "%.*s", Label(".L._printb_str2"))
        )),
        List(
            IPush(Reg[QWORD](rbp)),
            IMov(Reg[QWORD](rbp), Reg[QWORD](rsp)),
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ICmp(Reg[BYTE](rdi), Imm[BYTE](0)),
            Jmp(Label(".L_printb0"), JumpCond.NE),
            ILea(Reg[QWORD](rdx), Rip[QWORD](Label(".L._printb_str0"))),
            Jmp(Label(".L_printb1"), JumpCond.UnCond),
            Label(".L_printb0"),
            ILea(Reg[QWORD](rdx), Rip[QWORD](Label(".L._printb_str1"))),
            Label(".L_printb1"),
            IMov(Reg[DWORD](rsi), MemOff[DWORD](rdx, -4)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._printb_str2"))),
            IMov(Reg[BYTE](rax), Imm[BYTE](0)),
            ICall("printf@plt"),
            IMov(Reg[QWORD](rdi), Imm[QWORD](0)),
            ICall("fflush@plt"),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IPop(Reg[QWORD](rbp)),
            IRet
        )
    )
    val printsBlock: Block = Block (
        Label("_prints"),
        Some(List(RoData(4, "%.*s", Label(".L._prints_str0")))),
        List(
            IPush(Reg[QWORD](rbp)),
            IMov(Reg[QWORD](rbp), Reg[QWORD](rsp)),
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            IMov(Reg[QWORD](rdx), Reg[QWORD](rdi)),
            IMov(Reg[DWORD](rsi), MemOff[DWORD](rdi, -4)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._prints_str0"))),
            IMov(Reg[BYTE](rax), Imm[BYTE](0)),
            ICall("printf@plt"),
            IMov(Reg[QWORD](rdi), Imm[QWORD](0)),
            ICall("fflush@plt"),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IPop(Reg[QWORD](rbp)),
            IRet
        )
    )
    val printlnBlock = Block (
        Label("_println:"),
        Some(List(RoData(0, "", Label(".L._println_str0")))),
        List(
            IPush(Reg[QWORD](rbp)),
            IMov(Reg[QWORD](rbp), Reg[QWORD](rsp)),
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._println_str0"))),
            ICall("puts@plt"),
            IMov(Reg[QWORD](rdi), Imm[QWORD](0)),
            ICall("fflush@plt"),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IPop(Reg[QWORD](rbp)),
            IRet
        )
    )
    val overflowBlock = Block (
        Label("_errOverflow"),
        Some(List(RoData(52, "fatal error: integer overflow or underflow occurred\n", Label(".L._errOverflow_str0")))),
        List(
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._errOverflow_str0"))),
            ICall("_prints"),
            IMov(Reg[BYTE](rdi), Imm[BYTE](-1)),
            ICall("exit@plt")
        )
    )
    val divZeroBlock = Block (
        Label("_errDivZero"),
        Some(List(RoData(40, "fatal error: division or modulo by zero\n", Label(".L._println_str0")))),
        List(
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._errDivZero_str0"))),
            ICall("_prints"),
            IMov(Reg[BYTE](rdi), Imm[BYTE](-1)),
            ICall("exit@plt")
        )
    )
    val malloc = Block (
        Label("_malloc"),
        None,
        List(
            IPush(Reg[QWORD](rbp)),
            IMov(Reg[QWORD](rbp), Reg[QWORD](rsp)),
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ICall("malloc@plt"),
            ICmp(Reg[QWORD](rax), Imm[QWORD](0)),
            Jmp(Label("_errOutOfMemory"), JumpCond.E),
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp)),
            IPop(Reg[QWORD](rbp)),
            IRet
        )
    )
    val errOutOfMemory = Block (
        Label("_errOutOfMemory"),
        Some(List(RoData(27, "fatal error: out of memory\n", Label(".L._errOutOfMemory_str0")))),
        List(
            IAnd(Reg[QWORD](rsp), Imm[QWORD](-16)),
            ILea(Reg[QWORD](rdi), Rip[QWORD](Label(".L._errOutOfMemory_str0"))),
            ICall("_prints"),
            IMov(Reg[BYTE](rdi), Imm[BYTE](-1)),
            ICall("exit@plt")
        )
    )
}
