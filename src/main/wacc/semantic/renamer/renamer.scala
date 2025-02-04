package wacc.semantic

import scala.collection.mutable.Map.{empty, from}

import wacc.ast.*

class QualifiedName(val oldName: String, val uid: Int)

def rename(prog: Program[String, Unit]): (Program[QualifiedName, Unit], Environment) = 
    given env: Environment = new Environment()
    given MutScope = empty
    given Scope = Map.empty
    val Fs = renameFuncs(prog.fs)
    val Ss = rename(prog.x)
    (Program(Fs, Ss)(prog.pos), env)

def renameFuncs(fs: List[Func[String, Unit]])(using env: Environment, mainScope: MutScope, parentScope: Scope) = {
    fs.foreach { f =>
        mainScope.put(f.v.v, env.add(f.v.v, f.pos))
    }
    fs.map(rename(_))
}

def rename(f: Func[String, Unit])(using env: Environment, mainScope: MutScope, parentScope: Scope): Func[QualifiedName, Unit] = 
    given funcScope: MutScope = from(mainScope)
    f.l.foreach {param => 
        funcScope.put(param.v.v, env.add(param.v.v, param.pos))    
    }
    Func(
        f.t,
        funcScope(f.v.v),
        f.l.map(p => Param(p.t, funcScope(p.v.v))(p.pos)),
        rename(f.s))(f.pos)

def rename(ss: List[Stmt[String, Unit]])(using env: Environment, curScope: MutScope, parentScope: Scope): List[Stmt[QualifiedName, Unit]] = {
    given Scope    = parentScope ++ curScope.toMap
    given MutScope = empty
    ss.map(rename(_))
}

def rename(s: Stmt[String, Unit])(using curScope: MutScope, env: Environment, parentScope: Scope): Stmt[QualifiedName, Unit] = 
    given (Int, Int) = s.pos
    given Unit = ()
    s match {
        case Skip() => Skip()(s.pos)
        case NewAss(t, i, r) => 
            val newI = if (curScope.contains(i.v)) 
                then Ident[QualifiedName, Unit](QualifiedName(i.v, AlreadyDeclaredInScope))
                else env.add(i.v, i.pos)
            val rR = rename(r)
            curScope.put(i.v, newI)
            Assign(newI, rR)
        case Assign(l, r) => Assign(rename(l), rename(r))
        case Read(l) => Read(rename(l))
        case Free(e) => Free(rename(e))
        case Return(e) => Return(rename(e))
        case Exit(e) => Exit(rename(e))
        case Print(e) => Print(rename(e))
        case PrintLn(e) => PrintLn(rename(e))
        case If(e, s1s, s2s) => If(rename(e), rename(s1s), rename(s2s))
        case While(e, ss) => While(rename(e), rename(ss))
        case Nest(ss) => Nest(rename(ss))
    }

def rename(l: LValue[String, Unit])(using env: Environment, curScope: MutScope, parentScope: Scope): LValue[QualifiedName, Unit] = 
    given (Int, Int) = l.pos
    l match {
    case i: Ident[String, Unit] => curScope.rebuildWithIdent(i)(identity(_))
    case ArrayElem(i, x) => curScope.rebuildWithIdent(i)(ArrayElem(_, x.map(rename(_))))
    case PElem(First(l)) => rename(l)
    case PElem(Second(l)) => rename(l)
}

def rename(r: RValue[String, Unit])(using env: Environment, curScope: MutScope, parentScope: Scope): RValue[QualifiedName, Unit] = 
    given (Int, Int) = r.pos
    r match {
        case e: Expr[String, Unit] => rename(e)
        case ArrayLit(es) => ArrayLit(es.map(rename(_)))
        case NewPair(e1, e2) => NewPair(rename(e1), rename(e2))
        case Call(i, es) => curScope.rebuildWithIdent(i)(Call(_, es.map(rename(_))))
        case First(l) => First(rename(l))
        case Second(l) => Second(rename(l))
        case PElem(f: First[String, Unit]) => PElem(First(rename(f.v)))
        case PElem(s: Second[String, Unit]) => PElem(Second(rename(s.v)))
    }

def rename(e: Expr[String, Unit])(using env: Environment, curScope: MutScope, parentScope: Scope): Expr[QualifiedName, Unit] = 
    given (Int, Int) = e.pos
    e match {
        case Not(e) => Not(rename(e))
        case Neg(e) => Neg(rename(e))
        case Len(e) => Len(rename(e))
        case Ord(e) => Ord(rename(e))
        case Chr(e) => Chr(rename(e))
        case Mul(x, y) => Mul(rename(x), rename(y))
        case Div(x, y) => Div(rename(x), rename(y))
        case Mod(x, y) => Mod(rename(x), rename(y))
        case Add(x, y) => Add(rename(x), rename(y))
        case Sub(x, y) => Sub(rename(x), rename(y))
        case Greater(x, y) => Greater(rename(x), rename(y))
        case GreaterEq(x, y) => GreaterEq(rename(x), rename(y))
        case Less(x, y) => Less(rename(x), rename(y))
        case LessEq(x, y) => LessEq(rename(x), rename(y))
        case Eq(x, y) => Eq(rename(x), rename(y))
        case NotEq(x, y) => NotEq(rename(x), rename(y))
        case And(x, y) => And(rename(x), rename(y))
        case Or(x, y) => Or(rename(x), rename(y))
        case IntLit(n: Int) => IntLit(n)
        case BoolLit(b: Boolean) => BoolLit(b)
        case CharLit(c: Char) => CharLit(c)
        case StrLit(s: String) => StrLit(s)
        case PairLit() => PairLit()(e.pos)
        case i: Ident[String, Unit] => curScope.rebuildWithIdent(i)(identity(_))
        case ArrayElem(i, es) => curScope.rebuildWithIdent(i)(ArrayElem(_, es.map(rename(_))))
    }

extension (curScope: MutScope) 
    def rebuildWithIdent[A](id: Ident[String, Unit])
    (build: Ident[QualifiedName, Unit] => A)
    (using parentScope: Scope, pos: (Int, Int)): A = 
        given Unit = ()
        build(curScope.get(id.v)
        .getOrElse(parentScope.get(id.v)
        .getOrElse(Ident[QualifiedName, Unit](QualifiedName(id.v, Undeclared)))))