package wacc.semantic

import scala.collection.mutable.Map.{empty, from}

import wacc.ast.*

class QualifiedName(val oldName: String, val uid: Int)

def rename(prog: Program[String, Typeless]): (Program[QualifiedName, Typeless], Environment) = 
    given env: Environment = new Environment()
    given MutScope = empty
    given Scope = Map.empty
    val Fs = renameFuncs(prog.fs)
    val Ss = rename(prog.x)
    (Program(Fs, Ss)(prog.pos), env)

def renameFuncs(fs: List[Func[String, Typeless]])(using env: Environment, mainScope: MutScope, parentScope: Scope) = {
    fs.foreach { f =>
        given Pos = f.v.pos
        given Typeless = Typeless()
        mainScope.put(f.v.v, Ident[QualifiedName, Typeless](QualifiedName(f.v.v, env.add(f.v.v, FuncT(rename(f.t), f.l.map(p => rename(p.t)))(f.pos)))))
    }
    fs.map(rename(_))
}

def rename(f: Func[String, Typeless])(using env: Environment, mainScope: MutScope, parentScope: Scope): Func[QualifiedName, Typeless] = 
    given funcScope: MutScope = from(mainScope)
    given Typeless = Typeless()
    f.l.foreach {param => 
        given Pos = param.v.pos
        funcScope.put(param.v.v, Ident(QualifiedName(param.v.v, env.add(param.v.v, rename(param.t)))))  
    }
    given Pos = f.pos
    Func(
        rename(f.t),
        funcScope(f.v.v),
        f.l.map(p => 
            Param(rename(p.t), funcScope(p.v.v))(p.pos)
        ),
        rename(f.s))(f.pos)

def rename(ss: List[Stmt[String, Typeless]])(using env: Environment, curScope: MutScope, parentScope: Scope): List[Stmt[QualifiedName, Typeless]] = {
    given Scope    = parentScope ++ curScope.toMap
    given MutScope = empty
    ss.map(rename(_))
}

def rename(s: Stmt[String, Typeless])(using curScope: MutScope, env: Environment, parentScope: Scope): Stmt[QualifiedName, Typeless] = 
    given Pos = s.pos
    given Typeless = Typeless()
    s match {
        case Skip() => Skip()(s.pos)
        case NewAss(t, i, r) => 
            val newUID: Int = if (curScope.contains(i.v)) 
                then AlreadyDeclaredInScope
                else 
                    given Pos = s.pos
                    env.add(i.v, rename(t))
            val rR = rename(r)
            given Pos = i.pos
            curScope.put(i.v, Ident[QualifiedName, Typeless](QualifiedName(i.v, newUID)))
            Assign(curScope(i.v), rR)
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

def rename(l: LValue[String, Typeless])(using env: Environment, curScope: MutScope, parentScope: Scope): LValue[QualifiedName, Typeless] = 
    given Pos = l.pos
    l match {
    case i: Ident[String, Typeless] => curScope.rebuildWithIdent(i)(identity(_))
    case ArrayElem(i, x) => curScope.rebuildWithIdent(i)(ArrayElem(_, x.map(rename(_))))
    case First(l) => First(rename(l))
    case Second(l) => First(rename(l))
}

def rename(r: RValue[String, Typeless])(using env: Environment, curScope: MutScope, parentScope: Scope): RValue[QualifiedName, Typeless] = 
    given Pos = r.pos
    r match {
        case e: Expr[String, Typeless] => rename(e)
        case ArrayLit(es) => ArrayLit(es.map(rename(_)))
        case NewPair(e1, e2) => NewPair(rename(e1), rename(e2))
        case Call(i, es) => curScope.rebuildWithIdent(i)(Call(_, es.map(rename(_))))
        case First(l) => First(rename(l))
        case Second(l) => Second(rename(l))
    }

def rename(e: Expr[String, Typeless])(using env: Environment, curScope: MutScope, parentScope: Scope): Expr[QualifiedName, Typeless] = 
    given Pos = e.pos
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
        case i: Ident[String, Typeless] => curScope.rebuildWithIdent(i)(identity(_))
        case ArrayElem(i, es) => curScope.rebuildWithIdent(i)(ArrayElem(_, es.map(rename(_))))
    }

extension (curScope: MutScope) 
    def rebuildWithIdent[A](id: Ident[String, Typeless])
    (build: Ident[QualifiedName, Typeless] => A)
    (using parentScope: Scope, pos: Pos): A = 
        given Typeless = Typeless()
        build(curScope.get(id.v)
        .getOrElse(parentScope.get(id.v)
        .getOrElse(Ident[QualifiedName, Typeless](QualifiedName(id.v, Undeclared)))))

def rename(t: Type)(using pos: Pos): Type = t match {
    case PairT(t1, t2) => PairT(rename(t1), rename(t2))
    case ArrayT(t) => ArrayT(rename(t))
    case ft@FuncT(t, ps) => FuncT(rename(t), ps.map(rename(_)))(ft.pos)
    case RedPairT() => PairT(?, ?)
    case t => t
}