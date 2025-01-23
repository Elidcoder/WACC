package wacc.semantic 

import wacc.ast.Type

type Environment = List[List[(String, Type)]]

def contains(env: Environment, v: String): Boolean = ???
def getType(env: Environment, v: String): Option[Type] = ???
def removeOption(env: Environment, v: String): Option[Environment] = ???
def remove(env: Environment, v: String): Environment = ???
def getScopeFunctionType(env: Environment) = ???
def getArrayElemType(env: Environment, v: String, n: Int): Option[Type] = ???
