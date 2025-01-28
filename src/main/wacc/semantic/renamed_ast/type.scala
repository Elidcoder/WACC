package wacc.semantic.renamedAst

sealed trait Type

case object ? extends Type

case class ArrayT(t: Type) extends Type
case class PairT(x: Type, y: Type) extends Type

case class IntT() extends Type
case class BoolT() extends Type
case class CharT() extends Type
case class StringT() extends Type
