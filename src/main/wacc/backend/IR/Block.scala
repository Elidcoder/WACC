package wacc.backend.ir

class RoData(val size: Int, val str: String, val label: Label) 

class Block(val name: Label, val roData: Option[List[RoData]], val instrs: List[Instr]) {
    override def equals(obj: Any): Boolean = obj match {
        case other: Block => this.name.name == other.name.name
        case _ => false
    }
    override def hashCode(): Int = name.name.hashCode
}
