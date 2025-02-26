package wacc.backend

import scala.collection.mutable.{Map, Set}
import scala.collection.immutable
import wacc.backend.ir.{Reference, Label}
import wacc.semantic.QualifiedName
import wacc.backend.referencer.Prebuilt

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

    private val nameReferences: Map[QualifiedName, Reference] = Map.empty
    def addVar(varName: QualifiedName, ref: Reference) = nameReferences.put(varName, ref)
    def getVarRef(varName: QualifiedName): Reference = nameReferences(varName)

    private val funcOffsets: Map[QualifiedName, Int] = Map.empty
    def addFunc(funcName: QualifiedName, offset: Int) = funcOffsets.put(funcName, offset)
    def incFuncOff(funcName: QualifiedName, offsetInc: Int) = funcOffsets.updateWith(funcName)(
        _.fold(Some(INITIAL_FUNC_OFF + offsetInc)){curOff => Some(curOff + offsetInc)} 
    )
    def getFuncOff(funcName: QualifiedName): Int = funcOffsets.getOrElse(funcName,
        (() => {funcOffsets.put(funcName, INITIAL_FUNC_OFF); INITIAL_FUNC_OFF})()
    )
    
    private val strRoData: Map[String, Label] = Map.empty
    def addRoData(str: String, ro: Label) = strRoData.put(str, ro)
    def getRoData(str: String): Label = strRoData(str)

    private val prebuiltsUsed: Set[Prebuilt] = Set.empty
    def addPrebuilt(prebuilt: Prebuilt) = prebuiltsUsed.add(prebuilt)
    def getPrebuilts(): List[Prebuilt] = prebuiltsUsed.toList

    var mainOffset: Int = 0;
}
