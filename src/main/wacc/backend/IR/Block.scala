package wacc.backend.ir

/* A class representing read only data (strings) in functions, 
 * pairing each with a label used to replace it in the compiled code. */
case class RoData(val str: String, val label: Label)

/* A class representing a function. Contains the function label, a list of instructions 
 * (body of the function) along with the functions read only data. */
case class Block(val name: Label, val roData: Option[List[RoData]], val instrs: List[Instr])
