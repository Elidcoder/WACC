package wacc.ast

import parsley.generic.{ParserBridge1, ParserBridge2}

enum Type {
    case Base(t: BaseType)
    case ArrayT(t: Type)
    case PairT(x: PairType, y: PairType)
}

enum BaseType {
    case Int
    case Bool
    case Char
    case String
}

enum PairType {
    case PBase(t: BaseType)
    case PArrayT(t: Type)
    case PPairT
}

object Type {
    object Base extends ParserBridge1[BaseType, Type]
    object ArrayT extends ParserBridge1[Type, Type]
    object PairT extends ParserBridge2[PairType, PairType, Type]
}

object PairType {
    object PBase extends ParserBridge1[BaseType, PairType]
    object PArrayT extends ParserBridge1[Type, PairType]
}
