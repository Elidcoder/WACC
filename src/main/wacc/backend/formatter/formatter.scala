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

private def format(instr: Instr)(using wr: BufferedWriter): Unit = instr match
    case IPush(source) => writeIndentedLine(s"push $source")
    case IRet => writeIndentedLine("ret")
    case IPop(dest) => writeIndentedLine(s"pop $dest")
    case Label(name) => format(instr)
    case ICall(funcName) => writeIndentedLine(s"call $funcName")
    case IAdd(dest, opR) => writeIndentedLine(s"add ${format(dest)}, ${format(opR)}")
    case ISub(dest, opR) => writeIndentedLine(s"sub ${format(dest)}, ${format(opR)}")
    case IMul(dest, opR) => writeIndentedLine(s"imul ${format(dest)}, ${format(opR)}")
    case IDiv(dest) =>
        writeLine("cdq")
        writeIndentedLine(s"idiv ${format(dest)}")
    case ICmp(dest, opR) => writeIndentedLine(s"cmp ${format(dest)}, ${format(opR)}")
    case IMov(source, dest) => writeIndentedLine(s"mov ${format(source)}, ${format(dest)}")
    case ILea(dest, target) => writeIndentedLine(s"lea ${format(dest)}, ${format(target)}")
    case Jmp(label, cond) => 
        if cond == JumpCond.UnCond then
            writeIndentedLine(s"jmp ${label.name}")
        else
            writeIndentedLine(s"j${cond.toString.toLowerCase} ${label.name}")

private def format(label: Label)(using wr: BufferedWriter): Unit = {
    writeLine(s"${label.name}:")
}

private def format[S <: DataSize](op: Operand[S])(using wr: BufferedWriter): Unit = op match
    case Reg(reg) => wr.write(reg.toString)
    case Rip(label) => wr.write(s"[rip + ${label.name}]")
    case MemInd(reg) => wr.write(s"[$reg]")
    case Imm(value) => wr.write(value.toString)
    case MemOff(reg, offset) => 
        if offset < 0 then
            wr.write(s"[$reg - ${-offset}]")
        else
            wr.write(s"[$reg + $offset]")



private def writeLine(line: String)(using wr: BufferedWriter): Unit = {
    wr.write(line)
    wr.newLine()
}

private def writeIndentedLine(line: String)(using wr: BufferedWriter): Unit = {
    wr.write("\t")
    wr.write(line)
    wr.newLine()
}
