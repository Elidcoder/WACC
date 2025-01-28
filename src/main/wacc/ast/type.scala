package wacc.ast

sealed trait Type

case class ArrayT(t: Type)(val pos: (Int, Int)) extends Type
case class PairT(x: Type, y: Type)(val pos: (Int, Int)) extends Type
case class RedPairT() extends Type

case class IntT() extends Type
case class BoolT() extends Type
case class CharT() extends Type
case class StringT() extends Type

case object ArrayT extends ParserBridgePos1[Type, Type]{
    override def labels: List[String] = List("array type")
}
case object PairT extends ParserBridgePos2[Type, Type, Type]{
    override def labels: List[String] = List("pair type")
}