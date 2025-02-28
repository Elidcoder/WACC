package wacc.backend.ir

sealed trait Operand
sealed trait DestOp extends Operand

sealed trait Instr {
    def size: DataSize
}

case class Reg(reg: Register) extends DestOp
case class Imm(value: Int)    extends Operand

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

case class IPush(source: Operand) extends Instr {
    val size = QWORD
}
case class IPop (dest: DestOp) extends Instr {
    val size = QWORD
}
case class Label(name: String) extends Instr {
    val size = QWORD
}
case class ICall(funcName: String) extends Instr {
    val size = QWORD
}
case object IRet extends Instr {
    val size = QWORD
}
case object ICdq extends Instr {
    val size = QWORD
}

case class IAnd private (dest: Operand, opR: Operand)(using val size: DataSize) extends Instr
case class INeg(dest: DestOp)(using val size: DataSize) extends Instr
case class ITest private (dest: Operand, opR: Operand)(using val size: DataSize) extends Instr

case class IAdd private (dest: Operand, opR: Operand)(using val size: DataSize) extends Instr
case class ISub private (dest: Operand, opR: Operand)(using val size: DataSize) extends Instr
case class IMul private (dest: Operand, opR: Operand)(using val size: DataSize) extends Instr
case class IDiv private (dest: Operand)(using val size: DataSize) extends Instr
case class ICmp private (dest: Operand, opR: Operand)(using val size: DataSize) extends Instr

case class IMov private (dest: Operand, source: Operand, cond: JumpCond)(using val size: DataSize) extends Instr
case class IMovzx private (dest: Operand, source: Operand, smallSize: DataSize)(using val size: DataSize) extends Instr
case class ILea private (dest: Operand, target: Operand)(using val size: DataSize) extends Instr

enum JumpCond {
    case UnCond, E, NE, G, GE, L, LE, O
}

case class Jmp(label: Label, cond: JumpCond) extends Instr {
    val size = QWORD
}
case class ISet private (dest: Operand, cond: JumpCond)(using val size: DataSize) extends Instr

case object ITest {
    def apply(dest: Reg, opR: Operand)(using size: DataSize): ITest = new ITest(dest, opR)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): ITest = new ITest(dest, opR)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): ITest = new ITest(dest, opR)
}
case object IAdd {
    def apply(dest: Reg, opR: Operand)(using size: DataSize): IAdd = new IAdd(dest, opR)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): IAdd = new IAdd(dest, opR)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): IAdd = new IAdd(dest, opR)
}
case object ISub {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): ISub = new ISub(dest, opR)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): ISub = new ISub(dest, opR)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): ISub = new ISub(dest, opR)
}
case object IMul {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): IMul = new IMul(dest, opR)
}
case object IDiv {    
    def apply(dest: Reg)(using size: DataSize): IDiv = new IDiv(dest)
}
case object ICmp {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): ICmp = new ICmp(dest, opR)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): ICmp = new ICmp(dest, opR)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): ICmp = new ICmp(dest, opR)
}
case object IMov {    
    def apply(dest: DestOp, opR: Reg, cond: JumpCond)(using size: DataSize): IMov = new IMov(dest, opR, cond)
    def apply(dest: DestOp, opR: Imm, cond: JumpCond)(using size: DataSize): IMov = new IMov(dest, opR, cond)
    def apply(dest: Reg, opR: Mem, cond: JumpCond)(using size: DataSize): IMov = new IMov(dest, opR, cond)
    def apply(dest: Reg, opR: Reg)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
    def apply(dest: Reg, opR: Mem)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
    def apply(dest: Reg, opR: Imm)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
    def apply(dest: DestOp, opR: Reg)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
    def apply(dest: Reg, opR: DestOp)(using size: DataSize): IMov = new IMov(dest, opR, JumpCond.UnCond)
}
case object IAnd {    
    def apply(dest: Reg, opR: Operand)(using size: DataSize): IAnd = new IAnd(dest, opR)
    def apply(dest: Mem, opR: Reg)(using size: DataSize): IAnd = new IAnd(dest, opR)
    def apply(dest: Mem, opR: Imm)(using size: DataSize): IAnd = new IAnd(dest, opR)
}
case object ILea {    
    def apply(dest: Reg, opR: Mem): ILea = new ILea(dest, opR)(using QWORD)
}
case object ISet {
    def apply(dest: Reg, cond: JumpCond) = new ISet(dest, cond)(using BYTE)
}

case object IMovzx {
    def apply(dest: Reg, source: Operand, smallSize: DataSize)(using size: DataSize): IMovzx = new IMovzx(dest, source, smallSize)
    def apply(dest: Mem, source: Reg, smallSize: DataSize)(using size: DataSize): IMovzx = new IMovzx(dest, source, smallSize)
    def apply(dest: Mem, source: Imm, smallSize: DataSize)(using size: DataSize): IMovzx = new IMovzx(dest, source, smallSize)
}
