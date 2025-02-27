package wacc.backend

import scala.collection.mutable.{Map, Set}
import scala.collection.immutable
import wacc.backend.ir.{Label, ValDest}
import wacc.semantic.QualifiedName
import wacc.backend.generator.prebuilts.Prebuilt
import wacc.backend.ir.RoData

class Context() {

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

    private val nameReferences: Map[QualifiedName, ValDest] = Map.empty
    def addVar(varName: QualifiedName, ref: ValDest) = nameReferences.put(varName, ref)
    def getVarRef(varName: QualifiedName): ValDest = nameReferences(varName)

    private val funcOffsets: Map[QualifiedName, Int] = Map.empty
    def addFunc(funcName: QualifiedName, offset: Int) = funcOffsets.put(funcName, offset)
    def incFuncOff(funcName: QualifiedName, offsetInc: Int) = funcOffsets.updateWith(funcName)(
        _.fold(Some(offsetInc)){curOff => Some(curOff + offsetInc)} 
    )
    def getFuncOff(funcName: QualifiedName): Int = funcOffsets.getOrElse(funcName,
        (() => {funcOffsets.put(funcName, 0); 0})()
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
