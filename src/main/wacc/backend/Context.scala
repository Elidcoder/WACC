package wacc.backend

import scala.collection.mutable.{Map, Set}
import scala.collection.immutable
import wacc.backend.ir.{Reference, RoData, Label}
import wacc.semantic.QualifiedName
import wacc.backend.referencer.Prebuilt

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

    private val nameReferences: Map[QualifiedName, Reference] = Map.empty
    def addVar(name: QualifiedName, ref: Reference) = nameReferences.put(name, ref)
    def getVarRef(name: QualifiedName): Reference = nameReferences(name)

    private val funcOffsets: Map[QualifiedName, Int] = Map.empty
    def addFunc(name: QualifiedName, offset: Int) = funcOffsets.put(name, offset)
    def incFuncOff(name: QualifiedName, offsetInc: Int) = funcOffsets.updateWith(name)(
        _.fold(Some(16 + offsetInc)){curOff => Some(curOff + offsetInc)} 
    )
    def getFuncOff(name: QualifiedName): Int = funcOffsets(name)

    private val strRoData: Map[String, RoData] = Map.empty
    def addRoData(name: String, ro: RoData) = strRoData.put(name, ro)
    def getRoData(name: String): RoData = strRoData(name)

    private val prebuiltsUsed: Set[Prebuilt] = Set.empty
    def addPrebuilt(prebuilt: Prebuilt) = prebuiltsUsed.add(prebuilt)
    def getPrebuilts(): List[Prebuilt] = prebuiltsUsed.toList

    var mainOffset: Int = 0;
}
