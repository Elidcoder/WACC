package wacc.ast

import parsley.Parsley
import parsley.position._
import parsley.ap._
import parsley.generic.ErrorBridge

type Const[T] = [_, _] =>> T
type ListWrap[A[N, T]] = [N, T] =>> List[A[N, T]]

trait ParserSingletonBridgePos[+A] extends ErrorBridge {
    protected def con(pos: (Int, Int)): A
    def from[B](op: Parsley[B]): Parsley[A] = error(pos.map(this.con(_)) <* op)
    final def <#[B](op: Parsley[B]): Parsley[A] = this.from(op)
}

trait ParserSingletonBridgePosType[+A] extends ErrorBridge {
    protected def con(pos: (Int, Int), t: Typeless): A
    def from[B](op: Parsley[B]): Parsley[A] = error(pos.map(this.con(_, Typeless())) <* op)
    final def <#[B](op: Parsley[B]): Parsley[A] = this.from(op)
}

trait IdentBridge extends ParserSingletonBridgePosType[String => Ident[String, Typeless]] {
    def apply[String, Typeless](x: String)(pos: (Int, Int), t: Typeless): Ident[String, Typeless]
    def apply(x: Parsley[String]): Parsley[Ident[String, Typeless]] = error(ap1(pos.map(con(_, Typeless())), x))
    override final def con(pos: (Int, Int), t: Typeless): String => Ident[String, Typeless] = this.apply(_)(pos, t)
}

trait FuncBridge extends ParserSingletonBridgePos[(Type, Ident[String, Typeless], List[Param[String, Typeless]]) => (List[Stmt[String, Typeless]]) => Func[String, Typeless]] {
    def apply[String, Typeless](x: Type, y: Ident[String, Typeless], z: List[Param[String, Typeless]], zz: List[Stmt[String, Typeless]])(pos: (Int, Int)): Func[String, Typeless]
    def apply(x: Parsley[Type], y: Parsley[Ident[String, Typeless]], z: Parsley[List[Param[String, Typeless]]]): Parsley[(List[Stmt[String, Typeless]]) => Func[String, Typeless]] = 
        error(ap3(pos.map(con), x, y, z))
    override final def con(pos: (Int, Int)): (Type, Ident[String, Typeless], List[Param[String, Typeless]]) => (List[Stmt[String, Typeless]]) => Func[String, Typeless] = 
        (x: Type, y: Ident[String, Typeless], z: List[Param[String, Typeless]]) => this.apply(x, y, z, _)(pos)
}

trait ParserBridgePos1[-A[String, Typeless], +B[String, Typeless]] extends ParserSingletonBridgePos[A[String, Typeless] => B[String, Typeless]] {
    def apply[String, Typeless](x: A[String, Typeless])(pos: (Int, Int)): B[String, Typeless]
    def apply(x: Parsley[A[String, Typeless]]): Parsley[B[String, Typeless]] = error(ap1(pos.map(con), x))

    override final def con(pos: (Int, Int)): A[String, Typeless] => B[String, Typeless] = this.apply(_)(pos)
}

trait UnaryOperator[-A[String, Typeless], +B[String, Typeless]] extends ParserBridgePos1[A, B]  {
    override def labels: List[String] = List("unary operator")
}

trait ParserBridgePos2[-A[String, Typeless], -B[String, Typeless], +C[String, Typeless]] extends ParserSingletonBridgePos[(A[String, Typeless], B[String, Typeless]) => C[String, Typeless]] {
    def apply[String, Typeless](x: A[String, Typeless], y: B[String, Typeless])(pos: (Int, Int)): C[String, Typeless]
    def apply(x: Parsley[A[String, Typeless]], y: =>Parsley[B[String, Typeless]]): Parsley[C[String, Typeless]] = error(ap2(pos.map(con), x, y))

    override final def con(pos: (Int, Int)): (A[String, Typeless], B[String, Typeless]) => C[String, Typeless] = this.apply(_, _)(pos)
}

trait ComparisonOperator[-A[String, Typeless], -B[String, Typeless], +C[String, Typeless]] extends ParserBridgePos2[A, B, C]  {
    override def labels: List[String] = List("comparison operator")
}

trait MathematicalOperator[-A[String, Typeless], -B[String, Typeless], +C[String, Typeless]] extends ParserBridgePos2[A, B, C]  {
    override def labels: List[String] = List("mathematical operator")
}

trait ParserBridgePos3[-A[String, Typeless], -B[String, Typeless], -C[String, Typeless], +D[String, Typeless]] extends ParserSingletonBridgePos[(A[String, Typeless], B[String, Typeless], C[String, Typeless]) => D[String, Typeless]] {
    def apply[String, Typeless](x: A[String, Typeless], y: B[String, Typeless], z: C[String, Typeless])(pos: (Int, Int)): D[String, Typeless]
    def apply(x: Parsley[A[String, Typeless]], y: =>Parsley[B[String, Typeless]], z: =>Parsley[C[String, Typeless]]): Parsley[D[String, Typeless]] = error(ap3(pos.map(con), x, y, z))

    override final def con(pos: (Int, Int)): (A[String, Typeless], B[String, Typeless], C[String, Typeless]) => D[String, Typeless] = this.apply(_, _, _)(pos)
}

trait ParserBridgePos4[-A[String, Typeless], -B[String, Typeless], -C[String, Typeless], -D[String, Typeless], +E[String, Typeless]] extends ParserSingletonBridgePos[(A[String, Typeless], B[String, Typeless], C[String, Typeless], D[String, Typeless]) => E[String, Typeless]] {
    def apply[String, Typeless](x: A[String, Typeless], y: B[String, Typeless], z: C[String, Typeless], zz: D[String, Typeless])(pos: (Int, Int)): E[String, Typeless]
    def apply(x: Parsley[A[String, Typeless]], y: =>Parsley[B[String, Typeless]], z: =>Parsley[C[String, Typeless]], zz: =>Parsley[D[String, Typeless]]): Parsley[E[String, Typeless]] = error(ap4(pos.map(con), x, y, z, zz))

    override final def con(pos: (Int, Int)): (A[String, Typeless], B[String, Typeless], C[String, Typeless], D[String, Typeless]) => E[String, Typeless] = this.apply(_, _, _, _)(pos)
}
trait ParserBridgePosType1[-A[String, Typeless], +B[String, Typeless]] extends ParserSingletonBridgePosType[A[String, Typeless] => B[String, Typeless]] {
    def apply[String, Typeless](x: A[String, Typeless])(pos: (Int, Int), t: Typeless): B[String, Typeless]
    def apply(x: Parsley[A[String, Typeless]]): Parsley[B[String, Typeless]] = error(ap1(pos.map(con(_, Typeless())), x))

    override final def con(pos: (Int, Int), t: Typeless): A[String, Typeless] => B[String, Typeless] = this.apply(_)(pos, t)
}

trait ParserBridgePosType2[-A[String, Typeless], -B[String, Typeless], +C[String, Typeless]] extends ParserSingletonBridgePosType[(A[String, Typeless], B[String, Typeless]) => C[String, Typeless]] {
    def apply[String, Typeless](x: A[String, Typeless], y: B[String, Typeless])(pos: (Int, Int), t: Typeless): C[String, Typeless]
    def apply(x: Parsley[A[String, Typeless]], y: =>Parsley[B[String, Typeless]]): Parsley[C[String, Typeless]] = error(ap2(pos.map(con(_, Typeless())), x, y))

    override final def con(pos: (Int, Int), t: Typeless): (A[String, Typeless], B[String, Typeless]) => C[String, Typeless] = this.apply(_, _)(pos, t)
}
