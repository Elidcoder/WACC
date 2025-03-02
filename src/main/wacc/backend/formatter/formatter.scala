package wacc.backend.formatter

import java.io.{Writer, BufferedWriter}
import wacc.backend.ir._

/* Output the standard headings for x86 64, control the writer and format the blocks. */
def formatBlocks(blocks: List[Block], writer: Writer) = {
    given  wr: BufferedWriter = new BufferedWriter( writer)

    writeLine(".intel_syntax noprefix")
    writeLine(".globl main")

    blocks.foreach(format)

    wr.close()
}

/* Format a block adding rodata if necessary. */
private def format(block: Block)(using wr: BufferedWriter): Unit = {
    block.roData.foreach { (roData) => 
        writeLine(".section .rodata")
        roData.foreach(format)
        writeLine(".text")
    }
    format(block.name)
    block.instrs.foreach(format)
    wr.newLine()
}

/* Format a single rodata. */
private def format(roData: RoData)(using wr: BufferedWriter): Unit = {
    writeIndentedLine(s".int ${roData.str.length}")
    format(roData.label)
    writeIndentedLine(s".asciz \"${roData.str}\"")
}

/* Format each sub instruction. */
private def format(instrs: List[Instr])(using wr: BufferedWriter): Unit 
    = instrs.foreach(format)

/* Format an instruction. */
private def format(instr: Instr)(using wr: BufferedWriter): Unit = 
    given DataSize = instr.size
    instr match {
        case IRet             => writeIndentedLine("ret")
        case ICdq             => writeIndentedLine("cdq")
        case l@Label(_)       => formatLabel(l)
        case ICall(funcName)  => writeIndentedLine(s"call $funcName")
        case IPush(source)    => formatUnInstr(source, "push")
        case IPop(dest)       => formatUnInstr(dest, "pop")
        case INeg(dest)       => formatUnInstr(dest, "neg")
        case IDiv(dest)       => formatUnInstr(dest, "idiv")
        case ISet(dest, con)  => formatUnInstr(dest, s"set${con.getOrElse("").toString.toLowerCase}")
        case IAdd(dest, opR)  => formatBinInstr(dest, opR, "add")
        case ISub(dest, opR)  => formatBinInstr(dest, opR, "sub")
        case IMul(dest, opR)  => formatBinInstr(dest, opR, "imul")
        case ICmp(dest, opR)  => formatBinInstr(dest, opR, "cmp")
        case ILea(dest, opR)  => formatBinInstr(dest, opR, "lea")
        case IAnd(dest, opR)  => formatBinInstr(dest, opR, "and")
        case ITest(dest, opR) => formatBinInstr(dest, opR, "test")
        case IMov(dest, source, con) => 
            val instr = con.fold("mov")(cond => s"cmov${cond.toString.toLowerCase}")
            formatBinInstr(dest, source, instr)
        case Jmp(label, con) => 
            val instr = con.fold("jmp")(cond => s"j${cond.toString.toLowerCase}")
            writeIndentedLine(s"$instr ${label.name}")
        case IMovzx(dest, source, size) => writeIndentedLine(s"movzx ${format(dest)}, ${format(source)(using size = size)}")
    }

/* Format a unary instruction. */
private def formatUnInstr(op: Operand, instr: String)(using wr: BufferedWriter, size: DataSize): Unit 
    = writeIndentedLine(s"$instr ${format(op)}")

/* Formats a binary instruction. */
private def formatBinInstr(opL: Operand, opR: Operand, instr: String)(using wr: BufferedWriter, size: DataSize): Unit 
    = writeIndentedLine(s"$instr ${format(opL)}, ${format(opR)}")

/* Write a label to the writer */
private def formatLabel(label: Label)(using wr: BufferedWriter): Unit 
    = writeLine(s"${label.name}:")

/* Format an offset. */
private def formatOff(optOff: Option[Either[Int, Label]]) = optOff.fold("") { 
    case Left(offset) => 
        if (offset < 0) then
            s" - ${-offset}"
        else
            s" + $offset"
    case Right(label) =>
        s" + ${label.name}"
}

/* Format a scaled memory access. */
private def formatScale(optScl: Option[(Register, DataSize)]) = optScl.fold("")( (reg, scl) =>
    s" + ${format(reg)(using QWORD)} * ${scl.bytes}"
)

/* Format a memory, immediate value or register access. */
private def format(op: Operand)(using wr: BufferedWriter, size: DataSize): String = 
    op match
        case Reg(reg)   => format(reg)
        case Imm(value) => value.toString
        case Mem(reg, scale, offset) => 
            memSize(size) + s"[${format(reg)(using QWORD)}${formatScale(scale)}${formatOff(offset)}]"

/* Format a datasize. */
private def memSize(size: DataSize): String = s"${size.name} ptr "

/* Add the size suffix to the registers string representation. */
private def formatNumbReg(prefix: String)(using size: DataSize) = s"$prefix${
    size match 
        case BYTE  => "b"
        case WORD  => "w"
        case DWORD => "d"
        case QWORD => ""
}"

/* Format R'regChar'X registers: RAX, RBX, RCX, RDX. */
private def formatRKXReg(regChar: Char)(using size: DataSize) 
    = size match 
        case BYTE  => s"${regChar}l"
        case WORD  => s"${regChar}x"
        case DWORD => s"e${regChar}x"
        case QWORD => s"r${regChar}x"

/* Format R'regChar''I or P' registers: RSI, RDI, RSP, RBP. */
private def formatRKTReg(regChar: Char, iOrP: Char)(using size: DataSize) 
    = size match 
        case BYTE  => s"$regChar${iOrP}l"
        case WORD  => s"$regChar$iOrP"
        case DWORD => s"e$regChar$iOrP"
        case QWORD => s"r$regChar$iOrP"

/* Format a register. */
private def format(reg: Register)(using size: DataSize): String = reg match {
    case RAX => formatRKXReg('a')
    case RBX => formatRKXReg('b')
    case RCX => formatRKXReg('c')
    case RDX => formatRKXReg('d')
    case RSI => formatRKTReg('s', 'i')
    case RDI => formatRKTReg('d', 'i')
    case R8  => formatNumbReg("r8") 
    case R9  => formatNumbReg("r9") 
    case R10 => formatNumbReg("r10") 
    case R11 => formatNumbReg("r11") 
    case RSP => formatRKTReg('s', 'p')
    case RBP => formatRKTReg('b', 'p')
    case R12 => formatNumbReg("r12") 
    case R13 => formatNumbReg("r13") 
    case R14 => formatNumbReg("r14") 
    case R15 => formatNumbReg("r15") 
    case RIP => "rip"
}

/* Write a line to the writer (automatically adds the newline). */
private def writeLine(line: String)(using wr: BufferedWriter): Unit = {
    wr.write(line)
    wr.newLine()
}

/* Write an indented line to the writer. */
private def writeIndentedLine(line: String)(using wr: BufferedWriter): Unit = {
    wr.write("  ")
    writeLine(line)
}
