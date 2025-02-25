package wacc.backend.ir

sealed trait Operand

sealed trait Instr

sealed trait Reference

case class Stack(offset: Int) extends Reference
case class Heap(pointer: Int) extends Reference
case class Reg(reg: Register, size: DataSize) extends Operand with Reference

case class Push(source: Operand) extends Instr
case class Pop(dest: Operand) extends Instr
case class Label(name: String) extends Instr
case class Call(funcName: String) extends Instr

case class Add(dest: Operand, opR: Operand) extends Instr
case class Sub(dest: Operand, opR: Operand) extends Instr
case class Cmp(dest: Operand, opR: Operand) extends Instr

case class Mov(source: Operand, dest: Operand) extends Instr
case class Lea(dest: Operand, target: Operand) extends Instr
