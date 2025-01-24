package wacc.semantic 

import wacc.ast.Type
import scala.collection.mutable

class environment() {
    private val map: mutable.Map[Int, renamedAst.Type] = mutable.Map.empty
    private var nextUID: Int = 0
    def uid(): Int = nextUID
    def add(t: renamedAst.Type): Unit = 
        nextUID = nextUID + 1;
        map.put(nextUID,t)
    def get(uid: Int): renamedAst.Type = map(uid)
    override def toString(): String = map.toString()
}
type MutScope = mutable.Map[String, Int]
type Scope = Map[String, Int]
final val Undeclared = -1

type Environment = List[List[(String, Type)]]

def contains(env: Environment, v: String): Boolean = ???
def add(env: Environment, v: String, t: Type): Unit = ???
def getType(env: Environment, v: String): Option[Type] = ???
def removeOption(env: Environment, v: String): Option[Environment] = ???
def remove(env: Environment, v: String): Environment = ???
def getArrayElemType(env: Environment, v: String, d: Int): Option[Type] = ???