package wacc.backend.ir

sealed trait Operand

sealed trait Instr

sealed trait Reference

case class Stack(offset: Int) extends Reference
case class Heap(pointer: Int) extends Reference

case class Reg(reg: Register, size: DataSize) extends Reference, Operand

case class Rip(label: Label, size: DataSize)                  extends Operand 
case class MemInd(reg: Register, size: DataSize)              extends Operand 
case class Imm(value: Int, size: DataSize = DataSize.DWORD)   extends Operand 
case class MemOff(reg: Register, offset: Int, size: DataSize) extends Operand 

case class Push(source: Operand)  extends Instr
case class Pop(dest: Operand)     extends Instr
case class Label(name: String)    extends Instr
case class Call(funcName: String) extends Instr

case class Add(dest: Operand, opR: Operand) extends Instr
case class Sub(dest: Operand, opR: Operand) extends Instr
case class Cmp(dest: Operand, opR: Operand) extends Instr

case class Mov(source: Operand, dest: Operand) extends Instr
case class Lea(dest: Operand, target: Operand) extends Instr

enum JumpCond {
    case UnCond, Eq, NotEq, Gr, GrE, Le, LeE  
}

case class Jmp(label: Label, cond: JumpCond) extends Instr
