package wacc.semantic 

import wacc.ast.Type
import scala.collection.mutable

class Environment() {
    private val map: mutable.Map[Int, renamedAst.Ident] = mutable.Map.empty
    private var nextUID: Int = 0
    def uid(): Int = nextUID
    def add(v: String, t: renamedAst.Type): renamedAst.Ident = 
        val i = renamedAst.Ident(v, nextUID, t)
        map.put(nextUID,i)
        nextUID = nextUID + 1
        i
    def get(uid: Int): renamedAst.Ident = map(uid)
    override def toString(): String = map.toString()
}
type MutScope = mutable.Map[String, renamedAst.Ident]
type Scope = Map[String, renamedAst.Ident]
final val Undeclared = -1
