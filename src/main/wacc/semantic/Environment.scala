package wacc.semantic 

import scala.collection.mutable
import wacc.ast.*

class Environment() {
    private val map: mutable.Map[Int, Type] = mutable.Map.empty
    private var nextUID: Int = 0
    def uid(): Int = nextUID
    def add(v: String, t: Type): Int = 
        map.put(nextUID, t)
        nextUID += 1
        nextUID - 1
    def get(uid: Int): Type = map(uid)
    override def toString(): String = map.toString()
}
type MutScope = mutable.Map[String, Ident[QualifiedName, Unit]]
type Scope = Map[String, Ident[QualifiedName, Unit]]
final val Undeclared = -1
final val AlreadyDeclaredInScope = -2
