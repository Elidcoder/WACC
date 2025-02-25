package wacc.backend.formatter

import wacc.backend.ir.{Block, RoData, Instr}

// TODO(USE IO STREAM INSTEAD)
def formatBlocks(blocks: List[Block]):String = {
    given  sb: StringBuilder = new StringBuilder()

    blocks.foreach(format)

    sb.result()
}

def format(block: Block)(using sb:StringBuilder): Unit = {
    ???
}

def format(roData: RoData)(using sb:StringBuilder): Unit = {
    ???
}

def format(instrs: List[Instr])(using sb:StringBuilder): Unit = {
    instrs.foreach(format)
}

def format(instr: Instr)(using sb:StringBuilder): Unit = {
    ???
}
