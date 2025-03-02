package wacc.backend.ir

/* A class representing read only data (strings) in functions, 
 * pairing each with a label used to replace it in the compiled code. */
class RoData(val str: String, val label: Label) 

/* A class representing a function. Contains the function label, a list of instructions 
 * (body of the function) along with the functions read only data. */
class Block(val name: Label, val roData: Option[List[RoData]], val instrs: List[Instr]) {
    override def equals(obj: Any): Boolean = obj match {
        case other: Block => this.name.name == other.name.name
        case _ => false
    }
    override def hashCode(): Int = name.name.##
}
