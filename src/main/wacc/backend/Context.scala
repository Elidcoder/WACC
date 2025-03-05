package wacc.backend

import scala.collection.mutable.{Map, Set}
import wacc.backend.ir.{Label, DestOp, RoData}
import wacc.backend.generator.Prebuilt
import wacc.semantic.QualifiedName

class Context() {
    private val MAIN_UID = -1

    /* Create a unique label for a string. */
    private var stringUID: Int = 0
    def nextStringLabel(): Label = {
        stringUID += 1
        Label(s".L.str$stringUID")
    }
    
    /* Create a unique label for a section of code. */
    private var labelUID: Int = 0
    def nextLabel(): Label = {
        labelUID += 1
        Label(s".L$labelUID")
    }

    /* A map along with helper functions storing the location of a given variable in memory. */
    private val nameReferences: Map[QualifiedName, DestOp] = Map.empty
    def addVar(varName: QualifiedName, ref: DestOp) = nameReferences.put(varName, ref)
    def getVarRef(varName: QualifiedName): DestOp = nameReferences(varName)

    /* A map along with helper functions storing the offset of each function in the program. */
    private val funcOffsets: Map[QualifiedName, Int] = Map.empty
    def addFunc(funcName: QualifiedName, offset: Int) = funcOffsets.put(funcName, offset)
    def incFuncOff(funcName: QualifiedName, offsetInc: Int) = funcOffsets.updateWith(funcName)(
        _.fold(Some(offsetInc)){curOff => Some(curOff + offsetInc)} 
    )
    def getFuncOff(funcName: QualifiedName): Int = funcOffsets.getOrElseUpdate(funcName, 0)
    val mainName:QualifiedName = QualifiedName("main", MAIN_UID)

    /* A map along with helper functions storing the offsets of the parameters of each function in the program */
    private val funcParamOffsets: Map[QualifiedName, Int] = Map.empty
    def addFuncParamOff(funcName: QualifiedName, offset: Int) = funcParamOffsets.put(funcName, offset)
    def getFuncParamOff(funcName: QualifiedName): Int = funcParamOffsets.getOrElse(funcName, 0)
    
    /* A map along with helper functions storing the read only data in the program */
    private val strRoData: Map[String, RoData] = Map.empty
    def addRoData(str: String): RoData = strRoData.getOrElseUpdate(str, {
        val ro = RoData(str, nextStringLabel())
        ro
    })
    def getRoData(str: String): RoData = strRoData(str)
    def getAllRodata(): List[RoData] = strRoData.values.toList

    /* A set along with helper functions containing the prebuilt sections needed in the program. */
    private val prebuiltsUsed: Set[Prebuilt] = Set.empty
    def addPrebuilt(prebuilt: Prebuilt): String = {
        prebuiltsUsed += prebuilt
        prebuilt.labelString
    }
    def getPrebuilts(): List[Prebuilt] = prebuiltsUsed.toList
}
