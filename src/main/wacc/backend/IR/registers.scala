package wacc.backend.ir

sealed trait Register

/* Caller */
case object RAX extends Register

case object RCX extends Register
case object RDX extends Register
case object RSI extends Register
case object RDI extends Register

case object R8 extends Register
case object R9 extends Register
case object R10 extends Register
case object R11 extends Register

/* Callee */
case object RBX extends Register

case object RSP extends Register
case object RBP extends Register

case object R12 extends Register
case object R13 extends Register
case object R14 extends Register
case object R15 extends Register

/* Instruction pointer */
case object RIP extends Register


/* Name registers according to their usecases. */
final val BASE_PTR_REG     = RBP
final val STACK_PTR_REG    = RSP
final val FIRST_PARAM_REG  = RDI
final val SECOND_PARAM_REG = RSI
final val THIRD_PARAM_REG  = RDX
final val FOURTH_PARAM_REG = RCX
final val FIFTH_PARAM_REG  = R8
final val SIXTH_PARAM_REG  = R9

sealed trait DataSize {
    val bytes: Int
}
sealed trait DataSizeSmall extends DataSize

case object BYTE extends DataSizeSmall {
    override val bytes = 1
}
case object WORD extends DataSizeSmall {
    override val bytes = 2
}
case object DWORD extends DataSize {
    override val bytes = 4
}
case object QWORD extends DataSize {
    override val bytes = 8
}

/* An ordered list of registers used for parameters. */
val parameterRegisters: List[Register] = List(
    FIRST_PARAM_REG, 
    SECOND_PARAM_REG, 
    THIRD_PARAM_REG, 
    FOURTH_PARAM_REG, 
    FIFTH_PARAM_REG,
    SIXTH_PARAM_REG
) 
