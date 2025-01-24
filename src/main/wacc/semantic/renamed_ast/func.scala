package wacc.semantic.renamedAst

case class Func(t: Type, v: Ident, l: List[Param], s: List[Stmt])

case class Param(t: Type, v: Ident)
