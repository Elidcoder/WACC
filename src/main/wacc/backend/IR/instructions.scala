package wacc.backend.ir

sealed trait Operand[S <: DataSize]
sealed trait Mem[S <: DataSize] extends Operand[S]

sealed trait Instr
sealed trait Reference

case class Stack(offset: Int) extends Reference
case class Heap(pointer: Int) extends Reference

case class Reg[S <: DataSize](reg: Register) extends Reference, Operand[S]

case class Rip[S <: DataSize](label: Label)                   extends Mem[S]
case class MemInd[S <: DataSize](reg: Register)               extends Mem[S]
case class Imm[S <: DataSize](value: Int)                     extends Operand[S]
case class MemOff[S <: DataSize](reg: Register, offset: Int)  extends Mem[S]

case class IPush[S <: DataSize](source: Operand[S])  extends Instr
case class IPop[S <: DataSize] private (dest: Operand[S])     extends Instr
case class Label(name: String)                      extends Instr
case class ICall(funcName: String)                   extends Instr

case class IAdd[S <: DataSize] private (dest: Operand[S], opR: Operand[S]) extends Instr
case class ISub[S <: DataSize] private (dest: Operand[S], opR: Operand[S]) extends Instr
case class ICmp[S <: DataSize] private (dest: Operand[S], opR: Operand[S]) extends Instr

case class IMov[S <: DataSize] private (source: Operand[S], dest: Operand[S]) extends Instr
case class ILea[S <: DataSize] private (dest: Operand[S], target: Operand[S]) extends Instr

enum JumpCond {
    case UnCond, Eq, NotEq, Gr, GrEq, Le, LeEq 
}

case class Jmp(label: Label, cond: JumpCond) extends Instr

case object Reg {
    def apply[S <: DataSize](reg: Register) = new Reg(reg)
}

case object IAdd {
    def apply[S <: DataSize](dest: Reg[S], opR: Operand[S]): IAdd[S] = new IAdd(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Reg[S]): IAdd[S] = new IAdd(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Imm[S]): IAdd[S] = new IAdd(dest, opR)
}
case object ISub {    
    def apply[S <: DataSize](dest: Reg[S], opR: Operand[S]): ISub[S] = new ISub(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Reg[S]): ISub[S] = new ISub(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Imm[S]): ISub[S] = new ISub(dest, opR)
}
case object ICmp {    
    def apply[S <: DataSize](dest: Reg[S], opR: Operand[S]): ICmp[S] = new ICmp(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Reg[S]): ICmp[S] = new ICmp(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Imm[S]): ICmp[S] = new ICmp(dest, opR)
}
case object IMov {    
    def apply[S <: DataSize](dest: Reg[S], opR: Operand[S]): IMov[S] = new IMov(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Reg[S]): IMov[S] = new IMov(dest, opR)
    def apply[S <: DataSize](dest: Mem[S], opR: Imm[S]): IMov[S] = new IMov(dest, opR)
}
case object ILea {    
    def apply[S <: DataSize](dest: Reg[S], opR: Mem[S]): ILea[S] = new ILea(dest, opR)
}
case object IPop {    
    def apply[S <: DataSize](source: Reg[S]): IPush[S] = new IPush(source)
    def apply[S <: DataSize](source: Mem[S]): IPush[S] = new IPush(source)
}
