package wacc.semantic 

import wacc.ast.Type
import scala.collection.mutable

type environment = mutable.Map[String, Type]
type Environment = List[List[(String, Type)]]

def contains(env: Environment, v: String): Boolean = ???
def add(env: Environment, v: String, t: Type): Unit = ???
def getType(env: Environment, v: String): Option[Type] = ???
def removeOption(env: Environment, v: String): Option[Environment] = ???
def remove(env: Environment, v: String): Environment = ???
def getArrayElemType(env: Environment, v: String, d: Int): Option[Type] = ???