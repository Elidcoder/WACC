package wacc.semantic

import scala.collection.mutable.Map.{empty, from}

import wacc.ast

def rename(prog: ast.Program): (renamedAst.Program, Environment) = 
    given env: Environment = new Environment()
    given MutScope = empty
    given Scope = Map.empty
    val renamedFs = renameFuncs(prog.fs)
    val renamedSs = rename(prog.x)
    (renamedAst.Program(renamedFs, renamedSs), env)

def renameFuncs(fs: List[ast.Func])(using env: Environment, mainScope: MutScope, parentScope: Scope): List[renamedAst.Func] = {
    fs.foreach { f =>
        mainScope.put(f.v.v, env.add(f.v.v, renamedAst.FuncT(rename(f.t), f.l.map(p => rename(p.t)))))
    }
    fs.map(rename(_))
}

def rename(f: ast.Func)(using env: Environment, mainScope: MutScope, parentScope: Scope): renamedAst.Func = 
    given funcScope: MutScope = from(mainScope)
    f.l.foreach { param => 
            funcScope.put(param.v.v, env.add(param.v.v, rename(param.t)))
        }
    renamedAst.Func(funcScope(f.v.v), rename(f.s))

def rename(ss: List[ast.Stmt])(using curScope: MutScope, env: Environment, parentScope: Scope): List[renamedAst.Stmt] = {
    given Scope = parentScope ++ curScope.toMap
    given MutScope = empty
    ss.map(rename(_))
}

def rename(s: ast.Stmt)(using curScope: MutScope, env: Environment, parentScope: Scope): renamedAst.Stmt = s match {
    case ast.Skip() => renamedAst.Skip()
    case ast.NewAss(t, ast.Ident(v), r) => 
        val i = env.add(v, rename(t))
        val rR = rename(r)
        curScope.put(v, i)
        renamedAst.Assign(i, rR)
    case ast.Assign(l, r) => renamedAst.Assign(rename(l), rename(r))
    case ast.Read(l) => renamedAst.Read(rename(l))
    case ast.Free(e) => renamedAst.Free(rename(e))
    case ast.Return(e) => renamedAst.Return(rename(e))
    case ast.Exit(e) => renamedAst.Exit(rename(e))
    case ast.Print(e) => renamedAst.Print(rename(e))
    case ast.PrintLn(e) => renamedAst.PrintLn(rename(e))
    case ast.If(e, s1s, s2s) => renamedAst.If(rename(e), rename(s1s), rename(s2s))
    case ast.While(e, ss) => renamedAst.While(rename(e), rename(ss))
    case ast.Nest(ss) => renamedAst.Nest(rename(ss))
}

def rename(l: ast.LValue)(using env: Environment, curScope: MutScope, parentScope: Scope): renamedAst.LValue = l match {
    case i: ast.Ident => curScope.rebuildWithIdent(i)(identity(_))
    case ast.ArrayElem(i, x) => curScope.rebuildWithIdent(i)(renamedAst.ArrayElem(_, x.map(rename(_))))
    case ast.PElem(ast.First(l)) => rename(l)
    case ast.PElem(ast.Second(l)) => rename(l)
}

def rename(r: ast.RValue)(using env: Environment, curScope: MutScope, parentScope: Scope): renamedAst.RValue = r match {
    case e: ast.Expr => rename(e)
    case ast.ArrayLit(es) => renamedAst.ArrayLit(es.map(rename(_)))
    case ast.NewPair(e1, e2) => renamedAst.NewPair(rename(e1), rename(e2))
    case ast.Call(i, es) => curScope.rebuildWithIdent(i)(renamedAst.Call(_, es.map(rename(_))))
    case ast.First(l) => renamedAst.First(rename(l))
    case ast.Second(l) => renamedAst.Second(rename(l))
    case ast.PElem(ast.First(l)) => renamedAst.PElem(renamedAst.First(rename(l)))
    case ast.PElem(ast.Second(l)) => renamedAst.PElem(renamedAst.First(rename(l)))
}

def rename(e: ast.Expr)(using env: Environment, curScope: MutScope, parentScope: Scope): renamedAst.Expr = e match {
    case ast.Not(e) => renamedAst.Not(rename(e))
    case ast.Neg(e) => renamedAst.Neg(rename(e))
    case ast.Len(e) => renamedAst.Len(rename(e))
    case ast.Ord(e) => renamedAst.Ord(rename(e))
    case ast.Chr(e) => renamedAst.Chr(rename(e))
    case ast.Mul(x, y) => renamedAst.Mul(rename(x), rename(y))
    case ast.Div(x, y) => renamedAst.Div(rename(x), rename(y))
    case ast.Mod(x, y) => renamedAst.Mod(rename(x), rename(y))
    case ast.Add(x, y) => renamedAst.Add(rename(x), rename(y))
    case ast.Sub(x, y) => renamedAst.Sub(rename(x), rename(y))
    case ast.Greater(x, y) => renamedAst.Greater(rename(x), rename(y))
    case ast.GreaterEq(x, y) => renamedAst.GreaterEq(rename(x), rename(y))
    case ast.Less(x, y) => renamedAst.Less(rename(x), rename(y))
    case ast.LessEq(x, y) => renamedAst.LessEq(rename(x), rename(y))
    case ast.Eq(x, y) => renamedAst.Eq(rename(x), rename(y))
    case ast.NotEq(x, y) => renamedAst.NotEq(rename(x), rename(y))
    case ast.And(x, y) => renamedAst.And(rename(x), rename(y))
    case ast.Or(x, y) => renamedAst.Or(rename(x), rename(y))
    case ast.IntLit(n: Int) => renamedAst.IntLit(n)
    case ast.BoolLit(b: Boolean) => renamedAst.BoolLit(b)
    case ast.CharLit(c: Char) => renamedAst.CharLit(c)
    case ast.StrLit(s: String) => renamedAst.StrLit(s)
    case ast.PairLit() => renamedAst.PairLit()
    case i: ast.Ident => curScope.rebuildWithIdent(i)(identity(_))
    case ast.ArrayElem(i, es) => curScope.rebuildWithIdent(i)(renamedAst.ArrayElem(_, es.map(rename(_))))
}

extension (curScope: MutScope) 
    def rebuildWithIdent[A](id: ast.Ident)
    (build: renamedAst.Ident => A)
    (using parentScope: Scope): A = 
        build(curScope.get(id.v)
        .getOrElse(parentScope.get(id.v)
            .getOrElse(renamedAst.Ident(id.v, Undeclared, renamedAst.?))))

def rename(t: ast.Type): renamedAst.Type = t match {
    case ast.ArrayT(t) => renamedAst.ArrayT(rename(t))
    case ast.PairT(x, y) => renamedAst.PairT(rename(x), rename(y))
    case ast.RedPairT() => renamedAst.PairT(renamedAst.?, renamedAst.?)
    case ast.IntT() => renamedAst.IntT()
    case ast.BoolT() => renamedAst.BoolT()
    case ast.CharT() => renamedAst.CharT()
    case ast.StringT() => renamedAst.StringT()
}
