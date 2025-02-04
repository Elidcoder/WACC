package wacc.semantic

import scala.collection.mutable.Map.{empty, from}

import wacc.ast.*

class QualifiedName(val oldName: String, val uid: Int)

def rename(prog: Program[String, Unit]): (Program[QualifiedName, Unit], Environment) = 
    given env: Environment = new Environment()
    given mainScope: MutScope = empty
    val Fs = prog.fs.map(rename(_))
    val Ss = rename(prog.x, mainScope.toMap)
    (Program(Fs, Ss)(prog.pos), env)

def rename(f: Func[String, Unit])(using env: Environment, mainScope: MutScope): Func[QualifiedName, Unit] = 
    val paramMap: MutScope = from((f.l.map { 
        param => 
            (param.v.v, env.add(param.v.v, param.pos))
        }))
    val F = env.add(f.v.v, f.pos)
    mainScope.put(f.v.v, F)
    paramMap.put(f.v.v, F)
    Func(
        f.t,
        paramMap(f.v.v),
        f.l.map(p => Param(p.t, paramMap(p.v.v))(p.pos)),
        rename(f.s, paramMap.toMap))(f.pos)

def rename(ss: List[Stmt[String, Unit]], parentScope: Scope)(using Environment): List[Stmt[QualifiedName, Unit]] = {
    given curScope: MutScope = from(parentScope)
    ss.map(rename(_))
}

def rename(s: Stmt[String, Unit])(using curScope: MutScope, env: Environment): Stmt[QualifiedName, Unit] = s match {
    case Skip() => Skip()(s.pos)
    case NewAss(t, Ident(v), r) => 
        val i = env.add(v, s.pos);
        curScope.put(v, i);
        Assign(i, rename(r))(s.pos)
    case Assign(l, r) => Assign(rename(l), rename(r))(s.pos)
    case Read(l) => Read(rename(l))(s.pos)
    case Free(e) => Free(rename(e))(s.pos)
    case Return(e) => Return(rename(e))(s.pos)
    case Exit(e) => Exit(rename(e))(s.pos)
    case Print(e) => Print(rename(e))(s.pos)
    case PrintLn(e) => PrintLn(rename(e))(s.pos)
    case If(e, s1s, s2s) => If(rename(e), rename(s1s, curScope.toMap), rename(s2s, curScope.toMap))(s.pos)
    case While(e, ss) => While(rename(e), ss.map(rename(_)))(s.pos)
    case Nest(ss) => Nest(ss.map(rename(_)))(s.pos)
}

def rename(l: LValue[String, Unit])(using env: Environment, curScope: MutScope): LValue[QualifiedName, Unit] = l match {
    case i: Ident[String, Unit] => curScope.rebuildWithIdent(i)(identity(_))
    case ArrayElem(i, x) => curScope.rebuildWithIdent(i)(ArrayElem(_, x.map(rename(_)))(l.pos))
    case PElem(First(l)) => rename(l)
    case PElem(Second(l)) => rename(l)
}

def rename(r: RValue[String, Unit])(using env: Environment, curScope: MutScope): RValue[QualifiedName, Unit] = r match {
    case e: Expr[String, Unit] => rename(e)
    case ArrayLit(es) => ArrayLit(es.map(rename(_)))(r.pos, ())
    case NewPair(e1, e2) => NewPair(rename(e1), rename(e2))(r.pos, ())
    case Call(i, es) => curScope.rebuildWithIdent(i)(Call(_, es.map(rename(_)))(r.pos, ()))
    case First(l) => First(rename(l))(r.pos, ())
    case Second(l) => Second(rename(l))(r.pos, ())
    case PElem(f: First[String, Unit]) => PElem(First(rename(f.v))(f.pos, ()))(r.pos, ())
    case PElem(s: Second[String, Unit]) => PElem(Second(rename(s.v))(s.pos, ()))(r.pos, ())
}

def rename(e: Expr[String, Unit])(using env: Environment, curScope: MutScope): Expr[QualifiedName, Unit] = e match {
    case Not(e) => Not(rename(e))(e.pos)
    case Neg(e) => Neg(rename(e))(e.pos)
    case Len(e) => Len(rename(e))(e.pos)
    case Ord(e) => Ord(rename(e))(e.pos)
    case Chr(e) => Chr(rename(e))(e.pos)
    case Mul(x, y) => Mul(rename(x), rename(y))(e.pos)
    case Div(x, y) => Div(rename(x), rename(y))(e.pos)
    case Mod(x, y) => Mod(rename(x), rename(y))(e.pos)
    case Add(x, y) => Add(rename(x), rename(y))(e.pos)
    case Sub(x, y) => Sub(rename(x), rename(y))(e.pos)
    case Greater(x, y) => Greater(rename(x), rename(y))(e.pos)
    case GreaterEq(x, y) => GreaterEq(rename(x), rename(y))(e.pos)
    case Less(x, y) => Less(rename(x), rename(y))(e.pos)
    case LessEq(x, y) => LessEq(rename(x), rename(y))(e.pos)
    case Eq(x, y) => Eq(rename(x), rename(y))(e.pos)
    case NotEq(x, y) => NotEq(rename(x), rename(y))(e.pos)
    case And(x, y) => And(rename(x), rename(y))(e.pos)
    case Or(x, y) => Or(rename(x), rename(y))(e.pos)
    case IntLit(n: Int) => IntLit(n)(e.pos)
    case BoolLit(b: Boolean) => BoolLit(b)(e.pos)
    case CharLit(c: Char) => CharLit(c)(e.pos)
    case StrLit(s: String) => StrLit(s)(e.pos)
    case PairLit() => PairLit()(e.pos)
    case i: Ident[String, Unit] => curScope.rebuildWithIdent(i)(identity(_))
    case ArrayElem(i, es) => curScope.rebuildWithIdent(i)(ArrayElem(_, es.map(rename(_)))(e.pos))
}

extension (scope: MutScope) 
    def rebuildWithIdent[A](id: Ident[String, Unit])
    (build: Ident[QualifiedName, Unit] => A)
    (using curScope: MutScope): A = curScope.get(id.v) match {
        case Some(i) => build(i)
        case None    => build(Ident(QualifiedName(id.v, Undeclared))(id.pos, ()))
    }