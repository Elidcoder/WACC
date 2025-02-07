package wacc.semantic 

import scala.collection.mutable
import wacc.ast.SemType

private final val UID_STARTING  = 0
private final val UID_INCREMENT = 1
protected [semantic] final val UID_UNDECLARED            = -1
protected [semantic] final val UID_ALREADY_IN_SCOPE      = -2
protected [semantic] final val UID_FUNC_ALREADY_IN_SCOPE = -3

class Environment() {
    private val map: mutable.Map[Int, SemType] = mutable.Map.empty
    private var nextUID: Int = UID_STARTING
    def uid(): Int = nextUID
    def add(v: String, t: SemType): Int = 
        map.put(nextUID, t)
        nextUID += UID_INCREMENT
        nextUID -  UID_INCREMENT
    def get(uid: Int): SemType = map(uid)
    override def toString(): String = map.toString()
}

class FuncScope() {
    private val map: mutable.Map[String, QualifiedName] = mutable.Map.empty
    def get = map(_)
    def getOption = map.get(_)
    def put = map.put(_, _)
    def contains = map.contains(_)
}

type MutScope = mutable.Map[String, QualifiedName]
object MutScope {
    def apply(): MutScope = mutable.Map.empty
    def apply(scope: MutScope): MutScope = mutable.Map.from(scope)
}

type Scope = Map[String, QualifiedName]
object Scope {
    def apply(): Scope = Map.empty
    def apply(scope: Scope): Scope = Map.from(scope)
}
