package wacc.semantic.renamedAst

sealed trait Stmt

case class Skip() extends Stmt
case class Assign(l: LValue, r: RValue) extends Stmt
case class Read(l: LValue) extends Stmt
case class Free(e: Expr) extends Stmt
case class Return(e: Expr) extends Stmt
case class Exit(e: Expr) extends Stmt
case class Print(e: Expr) extends Stmt
case class PrintLn(e: Expr) extends Stmt
case class If(e: Expr, s1: List[Stmt], s2: List[Stmt]) extends Stmt
case class While(e: Expr, s: List[Stmt]) extends Stmt
case class Nest(s: List[Stmt]) extends Stmt
