package wacc.ast

import parsley.Parsley
import parsley.position._
import parsley.ap._
import parsley.generic.ErrorBridge

trait ParserSingletonBridgePos[+A] extends ErrorBridge {
    protected def con(pos: (Int, Int)): A
    def from[B](op: Parsley[B]): Parsley[A] = error(pos.map(this.con(_)) <* op)
    final def <#[B](op: Parsley[B]): Parsley[A] = this.from(op)
}

trait ParserSingletonBridgePosType[+A] extends ErrorBridge {
    protected def con(pos: (Int, Int), t: Unit): A
    def from[B](op: Parsley[B]): Parsley[A] = error(pos.map(this.con(_, ())) <* op)
    final def <#[B](op: Parsley[B]): Parsley[A] = this.from(op)
}

trait IdentBridge extends ParserSingletonBridgePosType[String => Ident[String, Unit]] {
    def apply[String, Unit](x: String)(pos: (Int, Int), t: Unit): Ident[String, Unit]
    def apply(x: Parsley[String]): Parsley[Ident[String, Unit]] = error(ap1(pos.map(con(_, ())), x))
    override final def con(pos: (Int, Int), t: Unit): String => Ident[String, Unit] = this.apply(_)(pos, t)
}

trait ParserBridgePos1[-A[String, Unit], +B[String, Unit]] extends ParserSingletonBridgePos[A[String, Unit] => B[String, Unit]] {
    def apply[String, Unit](x: A[String, Unit])(pos: (Int, Int)): B[String, Unit]
    def apply(x: Parsley[A[String, Unit]]): Parsley[B[String, Unit]] = error(ap1(pos.map(con), x))

    override final def con(pos: (Int, Int)): A[String, Unit] => B[String, Unit] = this.apply(_)(pos)
}

trait UnaryOperator[-A[String, Unit], +B[String, Unit]] extends ParserBridgePos1[A, B]  {
    override def labels: List[String] = List("unary operator")
}

trait ParserBridgePos2[-A[String, Unit], -B[String, Unit], +C[String, Unit]] extends ParserSingletonBridgePos[(A[String, Unit], B[String, Unit]) => C[String, Unit]] {
    def apply[String, Unit](x: A[String, Unit], y: B[String, Unit])(pos: (Int, Int)): C[String, Unit]
    def apply(x: Parsley[A[String, Unit]], y: =>Parsley[B[String, Unit]]): Parsley[C[String, Unit]] = error(ap2(pos.map(con), x, y))

    override final def con(pos: (Int, Int)): (A[String, Unit], B[String, Unit]) => C[String, Unit] = this.apply(_, _)(pos)
}

trait ComparisonOperator[-A[String, Unit], -B[String, Unit], +C[String, Unit]] extends ParserBridgePos2[A, B, C]  {
    override def labels: List[String] = List("comparison operator")
}

trait MathematicalOperator[-A[String, Unit], -B[String, Unit], +C[String, Unit]] extends ParserBridgePos2[A, B, C]  {
    override def labels: List[String] = List("mathematical operator")
}

trait ParserBridgePos3[-A[String, Unit], -B[String, Unit], -C[String, Unit], +D[String, Unit]] extends ParserSingletonBridgePos[(A[String, Unit], B[String, Unit], C[String, Unit]) => D[String, Unit]] {
    def apply[String, Unit](x: A[String, Unit], y: B[String, Unit], z: C[String, Unit])(pos: (Int, Int)): D[String, Unit]
    def apply(x: Parsley[A[String, Unit]], y: =>Parsley[B[String, Unit]], z: =>Parsley[C[String, Unit]]): Parsley[D[String, Unit]] = error(ap3(pos.map(con), x, y, z))

    override final def con(pos: (Int, Int)): (A[String, Unit], B[String, Unit], C[String, Unit]) => D[String, Unit] = this.apply(_, _, _)(pos)
}

trait ParserBridgePos4[-A[String, Unit], -B[String, Unit], -C[String, Unit], -D[String, Unit], +E[String, Unit]] extends ParserSingletonBridgePos[(A[String, Unit], B[String, Unit], C[String, Unit], D[String, Unit]) => E[String, Unit]] {
    def apply[String, Unit](x: A[String, Unit], y: B[String, Unit], z: C[String, Unit], zz: D[String, Unit])(pos: (Int, Int)): E[String, Unit]
    def apply(x: Parsley[A[String, Unit]], y: =>Parsley[B[String, Unit]], z: =>Parsley[C[String, Unit]], zz: =>Parsley[D[String, Unit]]): Parsley[E[String, Unit]] = error(ap4(pos.map(con), x, y, z, zz))

    override final def con(pos: (Int, Int)): (A[String, Unit], B[String, Unit], C[String, Unit], D[String, Unit]) => E[String, Unit] = this.apply(_, _, _, _)(pos)
}
trait ParserBridgePosType1[-A[String, Unit], +B[String, Unit]] extends ParserSingletonBridgePosType[A[String, Unit] => B[String, Unit]] {
    def apply[String, Unit](x: A[String, Unit])(pos: (Int, Int), t: Unit): B[String, Unit]
    def apply(x: Parsley[A[String, Unit]]): Parsley[B[String, Unit]] = error(ap1(pos.map(con(_, ())), x))

    override final def con(pos: (Int, Int), t: Unit): A[String, Unit] => B[String, Unit] = this.apply(_)(pos, t)
}

trait ParserBridgePosType2[-A[String, Unit], -B[String, Unit], +C[String, Unit]] extends ParserSingletonBridgePosType[(A[String, Unit], B[String, Unit]) => C[String, Unit]] {
    def apply[String, Unit](x: A[String, Unit], y: B[String, Unit])(pos: (Int, Int), t: Unit): C[String, Unit]
    def apply(x: Parsley[A[String, Unit]], y: =>Parsley[B[String, Unit]]): Parsley[C[String, Unit]] = error(ap2(pos.map(con(_, ())), x, y))

    override final def con(pos: (Int, Int), t: Unit): (A[String, Unit], B[String, Unit]) => C[String, Unit] = this.apply(_, _)(pos, t)
}
