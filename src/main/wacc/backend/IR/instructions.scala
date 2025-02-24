package wacc.backend.ir

sealed trait Operand

case class Reg(reg: Register, size: RegisterSize) extends Operand

