package wacc.semantic 

import scala.collection.mutable
import scala.collection.mutable.Map.empty
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
class FuncScope() {
    private val map: mutable.Map[String, QualifiedName] = empty
    def get = map(_)
    def getOption = map.get(_)
    def put = map.put(_, _)
    def contains = map.contains(_)
}
type MutScope = mutable.Map[String, QualifiedName]
type Scope = Map[String, QualifiedName]
final val Undeclared = -1
final val AlreadyDeclaredInScope = -2
final val FuncAlreadyDeclaredInScope = -3
