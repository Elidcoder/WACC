package wacc.ast

sealed trait Type
sealed trait SemType
class Typeless() extends Type

case object ? extends Type
case class FuncT(returnT: Type, paramTs: List[Type])(val pos: Pos) extends Type

case class ArrayT[N, T](t: Type)(using val pos: Pos) extends Type {
    override def toString(): String = t.toString() + "[]"
}
case class PairT[N, T](x: Type, y: Type)(using val pos: Pos) extends Type {
    override def toString(): String = "pair(" + x.toString() + ", " + y.toString() + ")"
}
case class RedPairT() extends Type {
    override def toString(): String = "pair"
}

case class IntT() extends Type {
    override def toString(): String = "int"
}
case class BoolT() extends Type {
    override def toString(): String = "boolean"
}
case class CharT() extends Type {
    override def toString(): String = "char"
}
case class StringT() extends Type {
    override def toString(): String = "string"
}

case object ArrayT extends ParserBridgePos1[Const[Type], Const[Type]]{
    override def labels: List[String] = List("array type")
}
case object PairT extends ParserBridgePos2[Const[Type], Const[Type], Const[Type]]{
    override def labels: List[String] = List("pair type")
}