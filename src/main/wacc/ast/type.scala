package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

sealed trait Type

case class ArrayT(t: Type) extends Type
case class PairT(x: Type, y: Type) extends Type
case class RedPairT() extends Type

case class IntT() extends Type
case class BoolT() extends Type
case class CharT() extends Type
case class StringT() extends Type

case object ArrayT extends ParserBridge1[Type, Type]
case object PairT extends ParserBridge2[Type, Type, Type]