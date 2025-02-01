// package wacc.semantic

// import scala.collection.mutable.Map.{empty, from}

// import wacc.ast.*

// class QualifiedName(val oldName: String, val uid: Int)

// def rename(prog: Program[String, Unit]): (Program[QualifiedName, Unit], Environment) = 
//     given env: Environment = new Environment()
//     given mainScope: MutScope = empty
//     val Fs = prog.fs.map(rename(_))
//     val Ss = rename(prog.x, mainScope.toMap)
//     (Program(Fs, Ss), env)

// def rename(f: Func[String, Unit])(using env: Environment, mainScope: MutScope): Func[QualifiedName, Unit] = 
//     val T = rename(f.t)
//     val paramMap: MutScope = from((f.l.map { 
//         param => 
//             (param.v.v, env.add(param.v.v, rename(param.t)))
//         }))
//     val F = env.add(f.v.v, FuncT(T, paramMap.map(_._2.t).toList))
//     mainScope.put(f.v.v, F)
//     paramMap.put(f.v.v, F)
//     Func(
//         T,
//         paramMap(f.v.v),
//         f.l.map(p => Param(rename(p.t), paramMap(p.v.v))),
//         rename(f.s, paramMap.toMap))

// def rename(ss: List[Stmt[String, Unit]], parentScope: Scope)(using Environment): List[Stmt] = {
//     given curScope: MutScope = from(parentScope)
//     ss.map(rename(_))
// }

// def rename(s: Stmt[String, Unit])(using curScope: MutScope, env: Environment): Stmt = s match {
//     case Skip() => Skip()
//     case NewAss(t, Ident(v), r) => 
//         val i = env.add(v, rename(t));
//         curScope.put(v, i);
//         Assign(i, rename(r))
//     case Assign(l, r) => Assign(rename(l), rename(r))
//     case Read(l) => Read(rename(l))
//     case Free(e) => Free(rename(e))
//     case Return(e) => Return(rename(e))
//     case Exit(e) => Exit(rename(e))
//     case Print(e) => Print(rename(e))
//     case PrintLn(e) => PrintLn(rename(e))
//     case If(e, s1s, s2s) => If(rename(e), rename(s1s, curScope.toMap), rename(s2s, curScope.toMap))
//     case While(e, ss) => While(rename(e), ss.map(rename(_)))
//     case Nest(ss) => Nest(ss.map(rename(_)))
// }

// def rename(l: LValue[String, Unit])(using env: Environment, curScope: MutScope): LValue = l match {
//     case i: Ident[String, Unit] => curScope.rebuildWithIdent(i)(identity(_))
//     case ArrayElem(i, x) => curScope.rebuildWithIdent(i)(ArrayElem(_, x.map(rename(_))))
//     case PElem(First(l)) => rename(l)
//     case PElem(Second(l)) => rename(l)
// }

// def rename(r: RValue[String, Unit])(using env: Environment, curScope: MutScope): RValue = r match {
//     case e: Expr[String, Unit] => rename(e)
//     case ArrayLit(es) => ArrayLit(es.map(rename(_)), ArrayT(?))
//     case NewPair(e1, e2) => NewPair(rename(e1), rename(e2), PairT(?, ?))
//     case Call(i, es) => curScope.rebuildWithIdent(i)(Call(_, es.map(rename(_)), ?))
//     case First(l) => First(rename(l), ?)
//     case Second(l) => Second(rename(l), ?)
//     case PElem(First(l)) => PElem(First(rename(l), ?), ?)
//     case PElem(Second(l)) => PElem(First(rename(l), ?), ?)
// }

// def rename(e: Expr[String, Unit])(using env: Environment, curScope: MutScope): Expr = e match {
//     case Not(e) => Not(rename(e))
//     case Neg(e) => Neg(rename(e))
//     case Len(e) => Len(rename(e))
//     case Ord(e) => Ord(rename(e))
//     case Chr(e) => Chr(rename(e))
//     case Mul(x, y) => Mul(rename(x), rename(y))
//     case Div(x, y) => Div(rename(x), rename(y))
//     case Mod(x, y) => Mod(rename(x), rename(y))
//     case Add(x, y) => Add(rename(x), rename(y))
//     case Sub(x, y) => Sub(rename(x), rename(y))
//     case Greater(x, y) => Greater(rename(x), rename(y))
//     case GreaterEq(x, y) => GreaterEq(rename(x), rename(y))
//     case Less(x, y) => Less(rename(x), rename(y))
//     case LessEq(x, y) => LessEq(rename(x), rename(y))
//     case Eq(x, y) => Eq(rename(x), rename(y))
//     case NotEq(x, y) => NotEq(rename(x), rename(y))
//     case And(x, y) => And(rename(x), rename(y))
//     case Or(x, y) => Or(rename(x), rename(y))
//     case IntLit(n: Int) => IntLit(n)
//     case BoolLit(b: Boolean) => BoolLit(b)
//     case CharLit(c: Char) => CharLit(c)
//     case StrLit(s: String) => StrLit(s)
//     case PairLit() => PairLit()
//     case i: Ident[String, Unit] => curScope.rebuildWithIdent(i)(identity(_))
//     case ArrayElem(i, es) => curScope.rebuildWithIdent(i)(ArrayElem(_, es.map(rename(_))))
// }

// extension (scope: MutScope) 
//     def rebuildWithIdent[A](id: Ident[String, Unit])
//     (build: Ident => A)
//     (using curScope: MutScope): A = curScope.get(id.v) match {
//         case Some(i) => build(i)
//         case None    => build(Ident(id.v, Undeclared, ?))
//     }

// def rename(t: Type[String, Unit]): Type[QualifiedName, Unit] = t match {
//     case ArrayT(t) => ArrayT(rename(t))
//     case PairT(x, y) => PairT(rename(x), rename(y))
//     case RedPairT() => PairT(?, ?)
//     case IntT() => IntT()
//     case BoolT() => BoolT()
//     case CharT() => CharT()
//     case StringT() => StringT()
// }
