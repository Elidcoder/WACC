package wacc.backend.ir

class RoData(val size: Int, val str: String, val label: Label) 

class Block(val name: Label, val roData: Option[List[RoData]], val instrs: List[Instr]) 
