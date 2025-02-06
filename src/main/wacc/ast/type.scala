package wacc.ast

sealed trait Type
sealed trait SemType
sealed trait KnownType extends SemType
class Typeless() extends Type

case object ? extends SemType

case class FuncT(returnT: SemType, paramTs: List[SemType])(val pos: Pos) extends KnownType {
    override def toString(): String = 
        val builder = new StringBuilder
        builder ++= "("
        builder ++= paramTs.headOption.getOrElse("").toString()
        paramTs.drop(1).foreach((t: SemType) => builder ++= s", ${t.toString}")
        builder ++= s") -> ${returnT.toString()}"
        builder.result()
}

case class ArrayT[N, T](t: SemType) extends KnownType {
    override def toString(): String = t match
        case ? => "Array Type"
        case x => s"$x[]"
}
case class PairT[N, T](x: SemType, y: SemType) extends KnownType {
    override def toString(): String = (x, y) match
        case (?, ?) => "pair"
        case (left, right) => s"pair($left, $right)"
}

case class IntT() extends KnownType {
    override def toString(): String = "int"
}
case class BoolT() extends KnownType {
    override def toString(): String = "boolean"
}
case class CharT() extends KnownType {
    override def toString(): String = "char"
}
case class StringT() extends KnownType {
    override def toString(): String = "string"
}

case object ArrayT extends ParserBridge1[Const[SemType], Const[KnownType]]{
    override def labels: List[String] = List("array type")
}
case object PairT extends ParserBridge2[Const[SemType], Const[SemType], Const[KnownType]]{
    override def labels: List[String] = List("pair type")
}