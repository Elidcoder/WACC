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
            IMov(Reg[QWORD](rsp), Reg[QWORD](rbp))
        )
    )
}
