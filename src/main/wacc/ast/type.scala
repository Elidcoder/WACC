package wacc.ast

sealed trait Type[N, T]

case class unknown[N, T]() extends Type[N, T]
case class FuncT[N, T](returnT: Type[N, T], paramTs: List[Type[N, T]]) extends Type[N, T]

case class ArrayT[N, T](t: Type[N, T])(val pos: (Int, Int)) extends Type[N, T]
case class PairT[N, T](x: Type[N, T], y: Type[N, T])(val pos: (Int, Int)) extends Type[N, T]
case class RedPairT[N, T]() extends Type[N, T]

case class IntT[N, T]() extends Type[N, T]
case class BoolT[N, T]() extends Type[N, T]
case class CharT[N, T]() extends Type[N, T]
case class StringT[N, T]() extends Type[N, T]

case object ArrayT extends ParserBridgePos1[Type, Type]
case object PairT extends ParserBridgePos2[Type, Type, Type]