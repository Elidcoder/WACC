package wacc.semantic 

import scala.collection.mutable
import wacc.ast.*

class Environment() {
    private val map: mutable.Map[Int, Ident[QualifiedName, Unit]] = mutable.Map.empty
    private var nextUID: Int = 0
    def uid(): Int = nextUID
    def add(v: String, pos: (Int, Int)): Ident[QualifiedName, Unit] = 
        given (Int, Int) = pos
        given Unit = ()
        val i = Ident[QualifiedName, Unit](QualifiedName(v, nextUID))
        map.put(nextUID,i)
        nextUID = nextUID + 1
        i
    def get(uid: Int): Ident[QualifiedName, Unit] = map(uid)
    override def toString(): String = map.toString()
}
type MutScope = mutable.Map[String, Ident[QualifiedName, Unit]]
type Scope = Map[String, Ident[QualifiedName, Unit]]
final val Undeclared = -1
final val AlreadyDeclaredInScope = -2
