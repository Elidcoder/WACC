package wacc.backend.generator.prebuilts

import wacc.backend.ir._
import wacc.ast.KnownType
import wacc.ast.{CharT, StringT, IntT, BoolT, ArrayT, PairT}

sealed trait Prebuilt {
    def labelString: String
    val block: Block
}

case object PbOutOfBounds extends Prebuilt {
    def labelString = "_errOutOfBounds"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(labelString, List("fatal error: array index %d out of bounds\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbErrBadChar extends Prebuilt {
    def labelString = "_errBadChar"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(labelString, List("fatal error: int %d is not ascii character 0-127 \\n"))
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.functionStart ++ List(
                ILea(Reg(RDI), Mem(roData(0).label)),
                IMov(Reg(RAX), Imm(0))(using BYTE),
                ICall("printf@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt"),
                IMov(Reg(RDI), Imm(-1))(using BYTE),
                ICall("exit@plt")
            ) ++ prebuiltGenerator.functionEnd
        )
}
case object PbErrNull extends Prebuilt {
    def labelString = "_errNull"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(labelString, List("fatal error: null pair dereferenced or freed\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbErrOutOfMemory extends Prebuilt {
    def labelString = "_errOutOfMemory"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(labelString, List("fatal error: out of memory\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbMalloc extends Prebuilt {
    def labelString = "_malloc"
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            None,
            prebuiltGenerator.functionStart ++ List(
                ICall("malloc@plt"),
                ICmp(Reg(RAX), Imm(0)),
                Jmp(Label(PbErrOutOfMemory.labelString), JumpCond.E)
            ) ++ prebuiltGenerator.functionEnd
        )
}
case object PbExit extends Prebuilt{
    def labelString = "_exit"
    val block = 
        Block (
            Label(labelString),
            None,
            prebuiltGenerator.functionStart ++ List(
                ICall("exit@plt")
            ) ++ prebuiltGenerator.functionEnd
        )
}
case object PbErrOverflow extends Prebuilt{
    def labelString = "_errOverflow"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(labelString, List("fatal error: integer overflow or underflow occurred\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case object PbDivZero extends Prebuilt{
    def labelString = "_errDivZero"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(labelString, List("fatal error: division or modulo by zero\\n"))
    val block = 
        Block (
            Label(labelString),
            Some(roData),
            prebuiltGenerator.errorExitInstrs(roData(0).label)
        )
}
case class PbPrint(varType: KnownType) extends Prebuilt{
    def labelString = varType match {
        case ArrayT(CharT()) => "_prints"
        case ArrayT(_)       => "_printp"
        case PairT(_, _)     => "_printp"
        case IntT()          => "_printi"
        case BoolT()         => "_printb"
        case CharT()         => "_printc"
        case StringT()       => "_prints"
        case _               => ""
    }
    private val roData: List[RoData] = varType match {
        case CharT() => prebuiltGenerator.generateRoData(labelString, List("%c"))
        case IntT() => prebuiltGenerator.generateRoData(labelString, List("%d"))
        case ArrayT(CharT()) => prebuiltGenerator.generateRoData(labelString, List("%.*s"))
        case ArrayT(_) => prebuiltGenerator.generateRoData(labelString, List("%p"))
        case PairT(_,_) => prebuiltGenerator.generateRoData(labelString, List("%p"))
        case StringT() => prebuiltGenerator.generateRoData(labelString, List("%.*s"))
        case BoolT() => prebuiltGenerator.generateRoData(labelString, List("false", "true", "%.*s"))
        case _ => List()
    }
    given DataSize = QWORD
    val block: Block = varType match {
        case CharT() => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(BYTE, roData(0).label.name))
        case IntT() => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(DWORD, roData(0).label.name))
        case ArrayT(CharT()) => Block(Label(labelString), Some(roData), prebuiltGenerator.printsBlock)
        case ArrayT(_) => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(QWORD, roData(0).label.name))
        case PairT(_,_) => Block(Label(labelString), Some(roData), prebuiltGenerator.genericPrintBlock(QWORD, roData(0).label.name))
        case StringT() => Block(Label(labelString), Some(roData), prebuiltGenerator.printsBlock)
        case BoolT() => 
            Block (
                Label(labelString),
                Some(roData),
                prebuiltGenerator.functionStart ++ List(
                    ICmp(Reg(RDI), Imm(0))(using BYTE),
                    Jmp(Label(".L_printb0"), JumpCond.NE),
                    ILea(Reg(RDX), Mem(roData(0).label)),
                    Jmp(Label(".L_printb1")),
                    Label(".L_printb0"),
                    ILea(Reg(RDX), Mem(roData(1).label)),
                    Label(".L_printb1"),
                    IMov(Reg(RSI), Mem(RDX, -4)),
                    ILea(Reg(RDI), Mem(roData(2).label)),
                    IMov(Reg(RAX), Imm(0))(using BYTE),
                    ICall("printf@plt"),
                    IMov(Reg(RDI), Imm(0)),
                    ICall("fflush@plt")
                ) ++ prebuiltGenerator.functionEnd
            )
        case _ => Block(Label(labelString), None, List())
    }
}
case class PbPrintln(varType: KnownType) extends Prebuilt{
    def labelString = PbPrint(varType).labelString
    val label: String = "_println"
    private val roData: List[RoData] = 
        prebuiltGenerator.generateRoData(label, List(""))
    val block = 
        given DataSize = QWORD
        Block (
            Label(label),
            Some(roData),
            prebuiltGenerator.functionStart ++ List(
                ILea(Reg(RDI), Mem(roData(0).label)),
                ICall("puts@plt"),
                IMov(Reg(RDI), Imm(0)),
                ICall("fflush@plt")
            ) ++ prebuiltGenerator.functionEnd
        )
}
case class PbFree(varType: KnownType) extends Prebuilt{
    def labelString = varType match {
        case ArrayT(_) => "_free"
        case PairT(_,_) => "_freepair"
        case _ => ""
    }
    given DataSize = QWORD
    val instrs: List[Instr] = varType match {
        case ArrayT(_) => 
                List(
                    ICall("free@plt")
                )
        case PairT(_,_) => 
                List(
                    ICmp(Reg(RDI), Imm(0)),
                    Jmp(Label(PbErrNull.labelString), JumpCond.E),
                    ICall("free@plt")
                )
        case _ => List()
    }
    val block = Block (Label(labelString), None, 
        prebuiltGenerator.functionStart ++ instrs ++ prebuiltGenerator.functionEnd)
}
case class PbRead(arType: KnownType) extends Prebuilt{
    def labelString = arType match{
        case CharT() => "_readc"
        case IntT()  => "_readi"
        case _ => ""
    }
    private val roData: List[RoData] = arType match {
        case CharT() => prebuiltGenerator.generateRoData(labelString, List(" %c"))
        case IntT() => prebuiltGenerator.generateRoData(labelString, List("%d"))
        case _ => List()
    }
    val block: Block = arType match {
        case CharT() => Block(Label(labelString), Some(roData), prebuiltGenerator.readBlock(BYTE, roData(0).label.name))
        case IntT() => Block(Label(labelString), Some(roData), prebuiltGenerator.readBlock(DWORD, roData(0).label.name))
        case _ => Block(Label(labelString), None, List())
    }
}
case class PbArrRef(size: DataSize) extends Prebuilt {
    def labelString = s"_arrRef${size.bytes}"
    val block = 
        given DataSize = QWORD
        Block (
            Label(labelString),
            None,
            List(
                IPush(Reg(RBX)),
                ITest(Reg(R10), Reg(R10))(using DWORD),
                IMov(Reg(RSI), Reg(R10), JumpCond.L),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.L),
                IMov(Reg(RBX), Mem(R9, -4)),
                ICmp(Reg(R10), Reg(RBX))(using DWORD),
                IMov(Reg(RSI), Reg(R10), JumpCond.GE),
                Jmp(Label(PbOutOfBounds.labelString), JumpCond.GE),
                ILea(Reg(R9), Mem(R9, R10, size)),
                IPop(Reg(RBX)),
                IRet
            )
        )
}

object prebuiltGenerator {
    def generatePrebuiltBlock(prebuilt: Prebuilt): List[Block] = prebuilt match {
        case PbMalloc => PbMalloc.block :: generatePrebuiltBlock(PbErrOutOfMemory) ++ generatePrebuiltBlock(PbPrint(StringT()))
        case PbDivZero => PbDivZero.block :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbErrOverflow => PbErrOverflow.block :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbPrintln(varType) => PbPrintln(varType).block :: generatePrebuiltBlock(PbPrint(varType))
        case PbFree(ty@PairT(_,_)) => PbFree(ty).block :: generatePrebuiltBlock(PbErrNull)
        case PbErrNull => PbErrNull.block :: generatePrebuiltBlock(PbPrint(StringT()))
        case PbArrRef(size) => PbArrRef(size).block :: generatePrebuiltBlock(PbOutOfBounds)
        case _ => List(prebuilt.block)
    }
    def generateRoData(labelString: String, strs: List[String]): List[RoData] = 
        strs.zipWithIndex.map { case (str, index) => 
            RoData(str.length, str, Label(s".L.${labelString}_str${index}"))
        }
    val alignStack: Instr = IAnd(Reg(RSP), Imm(-16))(using QWORD)
    val functionStart: List[Instr] = 
        given DataSize = QWORD
        List(
            IPush(Reg(RBP)),
            IMov(Reg(RBP), Reg(RSP)),
            alignStack
        )
    val functionEnd: List[Instr] =
        given DataSize = QWORD
        List(
            IMov(Reg(RSP), Reg(RBP)),
            IPop(Reg(RBP)),
            IRet
        )
    def errorExitInstrs(label: Label): List[Instr] = 
        given DataSize = QWORD
        List(
            alignStack,
            ILea(Reg(RDI), Mem(label)),
            ICall(PbPrint(StringT()).labelString),
            IMov(Reg(RDI), Imm(-1))(using BYTE),
            ICall("exit@plt")
        )
    def readBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            ISub(Reg(RSP), Imm(16)),
            IMov(Mem(RSP), Reg(RDI))(using size),
            ILea(Reg(RSI), Mem(RSP, 0)),
            ILea(Reg(RDI), Mem(Label(label))),
            IMov(Reg(RAX), Imm(0))(using BYTE),
            ICall("scanf@plt"),
            IMov(Reg(RAX), Mem(RSP))(using size),
            IAdd(Reg(RSP), Imm(16))
        ) ++ functionEnd
    def genericPrintBlock(size: DataSize, label: String): List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(RSI), Reg(RDI))(using size),
            ILea(Reg(RDI), Mem(Label(label))),
            IMov(Reg(RAX), Imm(0))(using BYTE),
            ICall("printf@plt"),
            IMov(Reg(RDI), Imm(0)),
            ICall("fflush@plt")
        ) ++ functionEnd
    val printsBlock: List[Instr] = 
        given DataSize = QWORD
        functionStart ++ List(
            IMov(Reg(RDX), Reg(RDI)),
            IMov(Reg(RSI), Mem(RDI, -4))(using DWORD),
            ILea(Reg(RDI), Mem(Label(".L._prints_str0"))),
            IMov(Reg(RAX), Imm(0))(using BYTE),
            ICall("printf@plt"),
            IMov(Reg(RDI), Imm(0)),
            ICall("fflush@plt")
        ) ++ functionEnd
}
