package wacc.semantic

import wacc.ast._

class QualifiedName(val oldName: String, val uid: Int) {
    override def toString(): String = oldName
}

def rename(
    prog: Program[String, Typeless]
): (Program[QualifiedName, Typeless], Environment) = 
    given env: Environment = new Environment()
    given MutScope = MutScope()
    given Scope = Scope()
    given FuncScope = FuncScope()
    val Fs = renameFuncs(prog.funcs)
    val Ss = rename(prog.stmts)
    (Program(Fs, Ss)(prog.pos), env)

def renameFuncs(
    funcs: List[Func[String, Typeless]]
)(using env: Environment, 
    mainScope: MutScope, 
    parentScope: Scope, 
    funcNameScope: FuncScope
) = {
    funcs.foreach { func =>
        val newUID: Int = if (funcNameScope.contains(func.id.name)) 
            then UID_ALREADY_IN_SCOPE
            else env.add(func.id.name, FuncT(func.retType, func.params.map(p => p.paramType))(func.pos))
        funcNameScope.put(func.id.name, QualifiedName(func.id.name, newUID))
    }
    funcs.map(rename(_))
}

def rename(
    func: Func[String, Typeless]
)(using env: Environment, 
    mainScope: MutScope, 
    parentScope: Scope, 
    funcNameScope: FuncScope
): Func[QualifiedName, Typeless] = 
    given funcScope: MutScope = MutScope(mainScope)
    given Typeless = Typeless()
    func.params.foreach { param => 
        val newUID: Int = if (funcScope.contains(param.paramId.name)) 
            then UID_ALREADY_IN_SCOPE
            else env.add(param.paramId.name, param.paramType)
        funcScope.put(param.paramId.name, QualifiedName(param.paramId.name, newUID)) 
    }
    given Pos = func.pos
    Func(
        func.retType,
        Ident[QualifiedName, Typeless](funcNameScope.get(func.id.name)),
        func.params.map(p => 
            Param(p.paramType, Ident[QualifiedName, Typeless](funcScope(p.paramId.name)))(p.pos)
        ),
    rename(func.stmts))(func.pos)

def rename(
    stmts: List[Stmt[String, Typeless]]
)(using env: Environment, 
    curScope: MutScope, 
    parentScope: Scope, 
    funcNameScope: FuncScope
): List[Stmt[QualifiedName, Typeless]] = {
    given Scope    = Scope(parentScope ++ curScope)
    given MutScope = MutScope()
    stmts.map(rename(_))
}

def rename(
    stmt: Stmt[String, Typeless]
)(using curScope: MutScope, 
    env: Environment, 
    parentScope: Scope, 
    funcNameScope: FuncScope
): Stmt[QualifiedName, Typeless] = 
    given Pos = stmt.pos
    given Typeless = Typeless()
    stmt match {
        case Skip()                         => Skip()(stmt.pos)
        case NewAss(newType, id, rval)      => 
            val newUID: Int = if (curScope.contains(id.name)) 
                then UID_ALREADY_IN_SCOPE
                else 
                    env.add(id.name, newType)
            val rR = rename(rval)
            given Pos = id.pos
            curScope.put(id.name, QualifiedName(id.name, newUID))
            Assign(Ident[QualifiedName, Typeless](curScope(id.name)), rR)
        case Assign(lval, rval)             => Assign(rename(lval), rename(rval))
        case Read(lval)                     => Read(rename(lval))
        case Free(expr)                     => Free(rename(expr))
        case Return(expr)                   => Return(rename(expr))
        case Exit(expr)                     => Exit(rename(expr))
        case Print(expr)                    => Print(rename(expr))
        case PrintLn(expr)                  => PrintLn(rename(expr))
        case If(expr, stmtsThen, stmtsElse) => If(rename(expr), rename(stmtsThen), rename(stmtsElse))
        case While(expr, stmts)             => While(rename(expr), rename(stmts))
        case Nest(stmts)                    => Nest(rename(stmts))
    }

def rename(
    lval: LValue[String, Typeless]
)(using env: Environment, curScope: MutScope, parentScope: Scope): LValue[QualifiedName, Typeless] = 
    given Pos = lval.pos
    lval match {
    case id: Ident[String, Typeless] => curScope.rebuildWithIdent(id)(identity(_))
    case ArrayElem(id, exprs)        => curScope.rebuildWithIdent(id)(ArrayElem(_, exprs.map(rename(_))))
    case First(lval)                 => First(rename(lval))
    case Second(lval)                => Second(rename(lval))
}

def rename(
    rval: RValue[String, Typeless]
)(using env: Environment, 
    curScope: MutScope, 
    parentScope: Scope, 
    funcNameScope: FuncScope
): RValue[QualifiedName, Typeless] = 
    given Pos = rval.pos
    rval match {
        case expr: Expr[String, Typeless] => rename(expr)
        case ArrayLit(exprs)              => ArrayLit(exprs.map(rename(_)))
        case NewPair(lexpr, rexpr)        => NewPair(rename(lexpr), rename(rexpr))
        case Call(id, exprs)              => funcNameScope.rebuildWithIdent(id)(Call(_, exprs.map(rename(_))))
        case First(lval)                  => First(rename(lval))
        case Second(lval)                 => Second(rename(lval))
    }

def rename(
    expr: Expr[String, Typeless]
)(using env: Environment, 
    curScope: MutScope, 
    parentScope: Scope
): Expr[QualifiedName, Typeless] = 
    given Pos = expr.pos
    expr match {
        case Not(e)               => Not(rename(e))
        case Neg(e)               => Neg(rename(e))
        case Len(e)               => Len(rename(e))
        case Ord(e)               => Ord(rename(e))
        case Chr(e)               => Chr(rename(e))
        case Mul(x, y)            => Mul(rename(x), rename(y))
        case Div(x, y)            => Div(rename(x), rename(y))
        case Mod(x, y)            => Mod(rename(x), rename(y))
        case Add(x, y)            => Add(rename(x), rename(y))
        case Sub(x, y)            => Sub(rename(x), rename(y))
        case Greater(x, y)        => Greater(rename(x), rename(y))
        case GreaterEq(x, y)      => GreaterEq(rename(x), rename(y))
        case Less(x, y)           => Less(rename(x), rename(y))
        case LessEq(x, y)         => LessEq(rename(x), rename(y))
        case Eq(x, y)             => Eq(rename(x), rename(y))
        case NotEq(x, y)          => NotEq(rename(x), rename(y))
        case And(x, y)            => And(rename(x), rename(y))
        case Or(x, y)             => Or(rename(x), rename(y))
        case IntLit(n: Int)       => IntLit(n)
        case BoolLit(b: Boolean)  => BoolLit(b)
        case CharLit(c: Char)     => CharLit(c)
        case StrLit(s: String)    => StrLit(s)
        case PairLit()            => PairLit()(expr.pos)
        case ArrayElem(id, exprs) => curScope.rebuildWithIdent(id)(ArrayElem(_, exprs.map(rename(_))))
        case id: Ident[String, Typeless] => curScope.rebuildWithIdent(id)(identity(_))
    }

extension (curScope: MutScope) 
    def rebuildWithIdent[A](id: Ident[String, Typeless])
    (build: Ident[QualifiedName, Typeless] => A)
    (using parentScope: Scope, pos: Pos): A = 
        given Typeless = Typeless()
        build(Ident(curScope.get(id.name)
        .getOrElse(parentScope.get(id.name)
        .getOrElse(QualifiedName(id.name, UID_UNDECLARED)))))

extension (funcNameScope: FuncScope) 
    def rebuildWithIdent[A](id: Ident[String, Typeless])
    (build: Ident[QualifiedName, Typeless] => A)
    (using pos: Pos): A = 
        given Typeless = Typeless()
        build(Ident(funcNameScope.getOption(id.name)
        .getOrElse(QualifiedName(id.name, UID_UNDECLARED))))
