package wacc.ast

sealed trait Type

case object ? extends Type
// case class FuncT[T](returnT: T, paramTs: List[T])(val pos: (Int, Int)) extends Type

case class ArrayT[N, T](t: Type)(using val pos: (Int, Int)) extends Type
case class PairT[N, T](x: Type, y: Type)(using val pos: (Int, Int)) extends Type
case class RedPairT() extends Type

case class IntT() extends Type
case class BoolT() extends Type
case class CharT() extends Type
case class StringT() extends Type

case object ArrayT extends ParserBridgePos1[Const[Type], Const[Type]]{
    override def labels: List[String] = List("array type")
}
case object PairT extends ParserBridgePos2[Const[Type], Const[Type], Const[Type]]{
    override def labels: List[String] = List("pair type")
}
