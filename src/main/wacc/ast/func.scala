package wacc.ast

type ParamList[N, T] = List[Param[N, T]]

case class Func[N, T](t: Type, v: Ident[N, T], l: ParamList[N, T], s: StmtList[N, T])(val pos: (Int, Int))
case object Func extends ParserBridgePos4[TypeWrap, Ident, ParamList, StmtList, Func]

case class Param[N, T](t: Type, v: Ident[N, T])(val pos: (Int, Int))
case object Param extends ParserBridgePos2[TypeWrap, Ident, Param]