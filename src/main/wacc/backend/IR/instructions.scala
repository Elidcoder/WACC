package wacc.backend.ir

sealed trait Operand
sealed trait DestOp extends Operand
sealed trait SourceOp extends Operand

sealed trait Instr {
    def size: DataSize
}

enum JumpCond {
    case E, NE, G, GE, L, LE, O
}

case class Reg(reg: Register) extends DestOp, SourceOp
case class Imm(value: Int)    extends SourceOp

case class Mem private (
    reg: Register, 
    index: Option[(Register, DataSize)], 
    offset: Option[Either[Int, Label]]
) extends DestOp

case object Mem {
    def apply(reg: Register): Mem = new Mem(reg, None, None)
    def apply (reg: Register, offset: Int): Mem = new Mem(reg, None, Some(Left(offset)))
    def apply(reg: Register, index: Register, scale: DataSize): Mem = new Mem(reg, Some((index, scale)), None)
    def apply(reg: Register, index: Register, scale: DataSize, offset: Int): Mem = new Mem(reg, Some((index, scale)), Some(Left(offset)))
    def apply(label: Label): Mem = new Mem(RIP, None, Some(Right(label)))
}

case object IRet extends Instr {
    val size = QWORD
}
case object ICdq extends Instr {
    val size = QWORD
}
case class IPush(source: Operand) extends Instr {
    val size = QWORD
}
case class IPop(dest: DestOp) extends Instr {
    val size = QWORD
}
case class Label(name: String) extends Instr {
    val size = QWORD
}
case class ICall(funcName: String) extends Instr {
    val size = QWORD
}
case class Jmp private (label: Label, cond: Option[JumpCond]) extends Instr {
    val size = QWORD
}
case class ISet private (dest: Reg, cond: Option[JumpCond]) extends Instr {
    val size = BYTE
}

case class INeg(dest: DestOp)(using val size: DataSize) extends Instr
case class IDiv(dest: Reg)(using val size: DataSize) extends Instr
case class ILea(dest: Reg, target: Mem)(using val size: DataSize) extends Instr
case class ITest(dest: DestOp, opR: SourceOp)(using val size: DataSize) extends Instr
case class IMul(dest: Reg, opR: DestOp)(using val size: DataSize) extends Instr
case class IMovzx(dest: Reg, source: SourceOp, smallSize: DataSizeSmall)(using val size: DataSize) extends Instr
case class IAnd private (dest: DestOp, opR: Operand)(using val size: DataSize) extends Instr
case class IAdd private (dest: DestOp, opR: Operand)(using val size: DataSize) extends Instr
case class ISub private (dest: DestOp, opR: Operand)(using val size: DataSize) extends Instr
case class ICmp private (dest: DestOp, opR: Operand)(using val size: DataSize) extends Instr
case class IMov private (dest: DestOp, source: Operand, cond: Option[JumpCond])(using val size: DataSize) extends Instr


case object Jmp {
    def apply(label: Label): Jmp = new Jmp(label, None)
    def apply(label: Label, cond: JumpCond): Jmp = new Jmp(label, Some(cond))
}
case object ISet {
    def apply(dest: Reg, cond: JumpCond): ISet = new ISet(dest, Some(cond))
    def apply(dest: Reg): ISet = new ISet(dest, None)
}
case object IAnd {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): IAnd = new IAnd(dest, opR)
    def apply(dest: Mem, opR: SourceOp)(using size: DataSize): IAnd = new IAnd(dest, opR)
}
case object IAdd {
    def apply(dest: Reg, opR: Operand)(using size: DataSize): IAdd = new IAdd(dest, opR)
    def apply(dest: Mem, opR: SourceOp)(using size: DataSize): IAdd = new IAdd(dest, opR)
}
case object ISub {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): ISub = new ISub(dest, opR)
    def apply(dest: Mem, opR: SourceOp)(using size: DataSize): ISub = new ISub(dest, opR)
}
case object ICmp {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): ICmp = new ICmp(dest, opR)
    def apply(dest: Mem, opR: SourceOp)(using size: DataSize): ICmp = new ICmp(dest, opR)
}
case object IMov {    
    def apply(dest: Reg, opR: Reg)(using size: DataSize): IMov = new IMov(dest, opR, None)
    def apply(dest: Reg, opR: Imm)(using size: DataSize): IMov = new IMov(dest, opR, None)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): IMov = new IMov(dest, opR, None)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): IMov = new IMov(dest, opR, None)(using size = to32Bit(size))
    def apply(dest: DestOp, opR: Reg)(using size: DataSize): IMov = new IMov(dest, opR, None)
    def apply(dest: Reg, opR: DestOp)(using size: DataSize): IMov = new IMov(dest, opR, None)
    def apply(dest: Reg, opR: DestOp, cond: JumpCond)(using size: DataSize): IMov = new IMov(dest, opR, Some(cond))
}


private def to32Bit(size: DataSize): DataSize = size match
    case QWORD => DWORD
    case _ => size
