package wacc.backend

import scala.collection.mutable.{Map, Set}
import scala.collection.immutable
import wacc.backend.ir.{Label, Operand}
import wacc.semantic.QualifiedName
import wacc.backend.generator.prebuilts.Prebuilt
import wacc.backend.ir.RoData

class Context() {
    /* Stores the initial offset for any function due to the initial operations. */
    private val INITIAL_FUNC_OFF = 16

    private var stringUID: Int = 0
    def nextStringLabel(): Label = {
        stringUID += 1
        Label(s".L.str$stringUID")
    }
    
    private var labelUID: Int = 0
    def nextLabel(): Label = {
        labelUID += 1
        Label(s".L$labelUID")
    }

    private val nameReferences: Map[QualifiedName, Operand] = Map.empty
    def addVar(varName: QualifiedName, ref: Operand) = nameReferences.put(varName, ref)
    def getVarRef(varName: QualifiedName): Operand = nameReferences(varName)

    private val funcOffsets: Map[QualifiedName, Int] = Map.empty
    def addFunc(funcName: QualifiedName, offset: Int) = funcOffsets.put(funcName, offset)
    def incFuncOff(funcName: QualifiedName, offsetInc: Int) = funcOffsets.updateWith(funcName)(
        _.fold(Some(INITIAL_FUNC_OFF + offsetInc)){curOff => Some(curOff + offsetInc)} 
    )
    def getFuncOff(funcName: QualifiedName): Int = funcOffsets.getOrElse(funcName,
        (() => {funcOffsets.put(funcName, INITIAL_FUNC_OFF); INITIAL_FUNC_OFF})()
    )
    
    private val strRoData: Map[String, RoData] = Map.empty
    def addRoData(str: String, ro: RoData) = strRoData.put(str, ro)
    def getRoData(str: String): RoData = strRoData(str)
    def getAllRodata(): List[RoData] = strRoData.values.toList

    private val prebuiltsUsed: Set[Prebuilt] = Set.empty
    def addPrebuilt(prebuilt: Prebuilt) = prebuiltsUsed.add(prebuilt)
    def getPrebuilts(): List[Prebuilt] = prebuiltsUsed.toList

    var mainOffset: Int = 0;
}
