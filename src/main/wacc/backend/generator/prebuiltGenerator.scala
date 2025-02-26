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
    val printBlock: Block = Block (
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
}
