package wacc.backend

import scala.collection.mutable
import wacc.backend.Reference
import wacc.ast.QualifiedName

class Context() {
    private val map: mutable.Map[QualifiedName, Reference] = mutable.Map.empty
    def add(name: QualifiedName, ref: Reference) = map.put(name, ref)
    def get(name: QualifiedName): Reference = map(name)
    private val uid: Int = 0
    def nextLabel(): Int = uid++
}
