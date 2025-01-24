package wacc.semantic

import scala.collection.mutable.Map.{empty, from}

import wacc.ast

def rename(prog: ast.Program): (renamedAst.Program, environment) = 
    given env: environment = new environment()
    val renamedFs = prog.fs.map(rename(_))
    val renamedSs = rename(prog.x)
    (renamedAst.Program(renamedFs, renamedSs), env)

def rename(f: ast.Func)(using env: environment): renamedAst.Func = 
    env.add(rename(f.t)); 
    val paramMap: Map[String, Int] = ((f.v.v, env.uid()) :: f.l.map { param => env.add(rename(param.t)); (param.v.v, env.uid()) }).toMap
    renamedAst.Func(
        rename(f.t),
        renamedAst.Ident(f.v.v, paramMap(f.v.v)),
        f.l.map(p => renamedAst.Param(rename(p.t), renamedAst.Ident(p.v.v, paramMap(p.v.v)))),
        rename(f.s, paramMap.toMap))


def rename(ss: List[ast.Stmt])(using environment): List[renamedAst.Stmt] = {
    given curScope: MutScope = empty
    ss.map(rename(_))
}

def rename(ss: List[ast.Stmt], parentScope: Scope)(using environment): List[renamedAst.Stmt] = {
    given curScope: MutScope = from(parentScope)
    ss.map(rename(_))
}

def rename(s: ast.Stmt)(using curScope: MutScope, env: environment): renamedAst.Stmt = s match {
    case ast.Skip() => renamedAst.Skip()
    case ast.NewAss(t, ast.Ident(v), r) => 
        env.add(rename(t));
        curScope.put(v, env.uid());
        renamedAst.Assign(renamedAst.Ident(v, env.uid()), rename(r))
    case ast.Assign(l, r) => renamedAst.Assign(rename(l), rename(r))
    case ast.Read(l) => renamedAst.Read(rename(l))
    case ast.Free(e) => renamedAst.Free(rename(e))
    case ast.Return(e) => renamedAst.Return(rename(e))
    case ast.Exit(e) => renamedAst.Exit(rename(e))
    case ast.Print(e) => renamedAst.Print(rename(e))
    case ast.PrintLn(e) => renamedAst.PrintLn(rename(e))
    case ast.If(e, s1s, s2s) => renamedAst.If(rename(e), rename(s1s, curScope.toMap), rename(s2s, curScope.toMap))
    case ast.While(e, ss) => renamedAst.While(rename(e), ss.map(rename(_)))
    case ast.Nest(ss) => renamedAst.Nest(ss.map(rename(_)))
}

def rename(l: ast.LValue)(using env: environment, curScope: MutScope): renamedAst.LValue = l match {
    case ast.Ident(v) => curScope.get(v) match {
        case Some(uid) => renamedAst.Ident(v, uid)
        case None      => renamedAst.Ident(v, Undeclared)
    }
    case ast.ArrayElem(ast.Ident(v), x) => curScope.get(v) match {
        case Some(uid) => renamedAst.Ident(v, uid)
        case None      => renamedAst.Ident(v, Undeclared)
    }
    case ast.PElem(ast.First(l)) => rename(l)
    case ast.PElem(ast.Second(l)) => rename(l)
}

def rename(r: ast.RValue)(using env: environment, curScope: MutScope): renamedAst.RValue = r match {
    case e: ast.Expr => rename(e)
    case ast.ArrayLit(es) => renamedAst.ArrayLit(es.map(rename(_)))
    case ast.NewPair(e1, e2) => renamedAst.NewPair(rename(e1), rename(e2))
    case ast.Call(ast.Ident(v), es) => curScope.get(v) match {
        case Some(uid) => renamedAst.Call(renamedAst.Ident(v, uid), es.map(rename(_)))
        case None      => renamedAst.Call(renamedAst.Ident(v, Undeclared), es.map(rename(_)))
    }
    case ast.First(l) => renamedAst.First(rename(l))
    case ast.Second(l) => renamedAst.Second(rename(l))
    case ast.PElem(ast.First(l)) => renamedAst.PElem(renamedAst.First(rename(l)))
    case ast.PElem(ast.Second(l)) => renamedAst.PElem(renamedAst.First(rename(l)))
}

def rename(e: ast.Expr)(using env: environment, curScope: MutScope): renamedAst.Expr = e match {
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
    case ast.Ident(v) => curScope.get(v) match {
        case Some(uid) => renamedAst.Ident(v, uid)
        case None      => renamedAst.Ident(v, Undeclared)
    }
    case ast.ArrayElem(ast.Ident(v), es) => curScope.get(v) match {
        case Some(uid) => renamedAst.ArrayElem(renamedAst.Ident(v, uid), es.map(rename(_)))
        case None      => renamedAst.ArrayElem(renamedAst.Ident(v, Undeclared), es.map(rename(_)))
    }
}

def rename(t: ast.Type): renamedAst.Type = t match {
    case ast.ArrayT(t) => renamedAst.ArrayT(rename(t))
    case ast.PairT(x, y) => renamedAst.PairT(rename(x), rename(y))
    case ast.RedPairT() => renamedAst.RedPairT()
    case ast.IntT() => renamedAst.IntT()
    case ast.BoolT() => renamedAst.BoolT()
    case ast.CharT() => renamedAst.CharT()
    case ast.StringT() => renamedAst.StringT()
}
