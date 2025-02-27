package wacc.backend.formatter

import java.io.{Writer, BufferedWriter}
import wacc.backend.ir._

def formatBlocks(blocks: List[Block], writer: Writer) = {
    given  wr: BufferedWriter = new BufferedWriter( writer)

    writeLine(".intel_syntax noprefix")
    writeLine(".globl main")

    blocks.foreach(format)

    wr.close()
}

private def format(block: Block)(using wr: BufferedWriter): Unit = {
    for (roData <- block.roData) {
        writeLine(".section .rodata")
        roData.foreach(format)
        writeLine(".text")
    }
    format(block.name)
    block.instrs.foreach(format)
    wr.newLine()
}

private def format(roData: RoData)(using wr: BufferedWriter): Unit = {
    writeIndentedLine(s".int ${roData.size}")
    format(roData.label)
    writeIndentedLine(s".asciz \"${roData.str}\"")
}


private def format(instrs: List[Instr])(using wr: BufferedWriter): Unit = {
    instrs.foreach(format)
}

private def format(instr: Instr)(using wr: BufferedWriter): Unit = 
    given DataSize = instr.size
    instr match {
        case IPush(source) => writeIndentedLine(s"push ${format(source)}")
        case IRet => writeIndentedLine("ret")
        case IPop(dest) => writeIndentedLine(s"pop ${format(dest)}")
        case l@Label(name) => formatLabel(l)
        case ICall(funcName) => writeIndentedLine(s"call $funcName")
        case IAdd(dest, opR) => writeIndentedLine(s"add ${format(dest)}, ${format(opR)}")
        case ISub(dest, opR) => writeIndentedLine(s"sub ${format(dest)}, ${format(opR)}")
        case IMul(dest, opR) => writeIndentedLine(s"imul ${format(dest)}, ${format(opR)}")
        case IDiv(dest) =>
            writeLine("cdq")
            writeIndentedLine(s"idiv ${format(dest)}")
        case ICmp(dest, opR) => writeIndentedLine(s"cmp ${format(dest)}, ${format(opR)}")
        case IMov(dest, source, cond) => cond match
            case JumpCond.UnCond => writeIndentedLine(s"mov ${format(dest)}, ${format(source)}")
            case _ => writeIndentedLine(s"cmov${cond.toString.toLowerCase} ${format(dest)}, ${format(source)}")
        case ILea(dest, target) => writeIndentedLine(s"lea ${format(dest)}, ${format(target)}")
        case Jmp(label, cond) => 
            if cond == JumpCond.UnCond then
                writeIndentedLine(s"jmp ${label.name}")
            else
                writeIndentedLine(s"j${cond.toString.toLowerCase} ${label.name}")
        case IAnd(dest, source) => writeIndentedLine(s"and ${format(dest)}, ${format(source)}")
        case INeg(dest) => writeIndentedLine(s"neg ${format(dest)}")
        case ITest(dest, source) => writeIndentedLine(s"test ${format(dest)}, ${format(source)}")
        case IMovzx(dest, source, _) => writeIndentedLine(s"movzx ${format(dest)}, ${format(source)}")
        case ISet(dest, cond) => 
            if cond == JumpCond.UnCond then
                writeIndentedLine(s"set ${format(dest)}")
            else
                writeIndentedLine(s"set${cond.toString.toLowerCase} ${format(dest)}")
    }

private def formatLabel(label: Label)(using wr: BufferedWriter): Unit = {
    writeLine(s"${label.name}:")
}

private def format[S <: DataSize](op: Operand)(using wr: BufferedWriter, size: DataSize): String = 
    op match
        case Reg(reg) => format(reg)
        case Rip(label) => s"[rip + ${label.name}]"
        case MemInd(reg) => s"[${format(reg)}]"
        case Imm(value) => value.toString
        case MemOff(reg, offset) => 
            if offset < 0 then
                s"[${format(reg)} - ${-offset}]"
            else
                s"[${format(reg)} + $offset]"

private def format(reg: Register)(using size: DataSize): String = reg match {
    case RAX => size match {
        case BYTE => "al"
        case WORD => "ax"
        case DWORD => "eax"
        case QWORD => "rax"
    }
    case RBX => size match {
        case BYTE => "bl"
        case WORD => "bx"
        case DWORD => "ebx"
        case QWORD => "rbx"
    }
    case RCX => size match {
        case BYTE => "cl"
        case WORD => "cx"
        case DWORD => "ecx"
        case QWORD => "rcx"
    }
    case RDX => size match {
        case BYTE => "dl"
        case WORD => "dx"
        case DWORD => "edx"
        case QWORD => "rdx"
    }
    case RSI => size match {
        case BYTE => "sil"
        case WORD => "si"
        case DWORD => "esi"
        case QWORD => "rsi"
    }
    case RDI => size match {
        case BYTE => "dil"
        case WORD => "di"
        case DWORD => "edi"
        case QWORD => "rdi"
    }
    case R8 => size match {
        case BYTE => "r8b"
        case WORD => "r8w"
        case DWORD => "r8d"
        case QWORD => "r8"
    }
    case R9 => size match {
        case BYTE => "r9b"
        case WORD => "r9w"
        case DWORD => "r9d"
        case QWORD => "r9"
    }
    case R10 => size match {
        case BYTE => "r10b"
        case WORD => "r10w"
        case DWORD => "r10d"
        case QWORD => "r10"
    }
    case R11 => size match {
        case BYTE => "r11b"
        case WORD => "r11w"
        case DWORD => "r11d"
        case QWORD => "r11"
    }
    case RSP => size match {
        case BYTE => "spl"
        case WORD => "sp"
        case DWORD => "esp"
        case QWORD => "rsp"
    }
    case RBP => size match {
        case BYTE => "bpl"
        case WORD => "bp"
        case DWORD => "ebp"
        case QWORD => "rbp"
    }
    case R12 => size match {
        case BYTE => "r12b"
        case WORD => "r12w"
        case DWORD => "r12d"
        case QWORD => "r12"
    }
    case R13 => size match {
        case BYTE => "r13b"
        case WORD => "r13w"
        case DWORD => "r13d"
        case QWORD => "r13"
    }
    case R14 => size match {
        case BYTE => "r14b"
        case WORD => "r14w"
        case DWORD => "r14d"
        case QWORD => "r14"
    }
    case R15 => size match {
        case BYTE => "r15b"
        case WORD => "r15w"
        case DWORD => "r15d"
        case QWORD => "r15"
    }
}




private def writeLine(line: String)(using wr: BufferedWriter): Unit = {
    wr.write(line)
    wr.newLine()
}

private def writeIndentedLine(line: String)(using wr: BufferedWriter): Unit = {
    wr.write("  ")
    wr.write(line)
    wr.newLine()
}
