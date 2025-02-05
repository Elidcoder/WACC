package wacc.semantic.typecheck

import wacc.error.WaccErr
import wacc.semantic.typecheck.WaccErr.*
import wacc.ast.*
import wacc.semantic.QualifiedName
import wacc.semantic.Environment

enum Constraint {
    case Is(refT: Type)(using (Int, Int))
    case IsStringLike
    case IsFreeable
    case IsReadable
}
object Constraint {
    given (Int, Int) = (0,0)
    val Unconstrained = Is(?)
    val IsArray = Is(ArrayT(?))
    val IsPair  = Is(PairT(?, ?))
}
import Constraint.*

extension (t: Type) 
    def ~(refT: Type)(using pos: (Int, Int)): Option[Type] =
        given (Int, Int) = pos
        given Unit = ()
        (t, refT) match {
            case (?, refT) => Some(refT)
            case (t, ?)    => Some(t)
            case (PairT(t1, t2), PairT(refT1, refT2)) =>
                for {commonT1 <- t1 ~ refT1; commonT2 <- t2 ~ refT2}
                yield PairT(commonT1, commonT2)
            case (ArrayT(t), ArrayT(refT)) =>
                for {commonT <- t ~ refT}
                yield ArrayT(commonT)
            case (t, refT) if t == refT => Some(t)
            case _                      => None
        }
    def satisfies(c: Constraint)(using ctx: Context, pos: (Int, Int)): Option[Type] = 
        given (Int, Int) = pos
        given Unit = ()
        (t,c) match {
            case (ArrayT(CharT()), Is(StringT())) => Some(StringT())
            case (t, Is(refT)) => (t ~ refT).orElse {
                ctx.error(TypeMismatch(Ident[QualifiedName, Unit](QualifiedName("blah", -1)), refT))
            }
            case (?, _) => Some(?)
            case (ArrayT(CharT()) | StringT(), IsStringLike) => Some(t)
            case (kt, IsStringLike) => ctx.error(IsNotString(Ident(QualifiedName("blah", -1))))
            case (ArrayT(_) | PairT(_,_), IsFreeable) => Some(t)
            case (kt, IsFreeable) => ctx.error(IsNotFreeable(Ident(QualifiedName("blah", -1))))
            case (IntT() | CharT(), IsReadable) => Some(t)
            case (kt, IsReadable) => ctx.error(IsNotFreeable(Ident(QualifiedName("blah", -1))))
}


def check(prog: Program[QualifiedName, Unit], env: Environment): Either[List[WaccErr], Option[Program[QualifiedName, Type]]] = {
    given ctx: Context = new Context(Body.Main)
    given Environment = env
    val typedFuncs: Option[List[Func[QualifiedName, Type]]] = checkFuncs(prog.fs)
    val typedStmts: Option[List[Stmt[QualifiedName, Type]]] = {
        ctx.body = Body.Main 
        check(prog.x)
    }
    val errors = ctx.result
    if errors.isEmpty 
        then Right(for { fs <- typedFuncs; ss <- typedStmts} yield Program[QualifiedName, Type](fs, ss)(prog.pos))
        else Left(errors)
}

def checkFuncs(fs: List[Func[QualifiedName, Unit]])(using ctx: Context, env: Environment): Option[List[Func[QualifiedName, Type]]] = 
    fs.foldRight(Some(List.empty)) {
        (opt: Func[QualifiedName, Unit], acc: Option[List[Func[QualifiedName, Type]]]) =>
            for { xs <- acc; x <- check(opt) } yield x :: xs
        }

def check(f: Func[QualifiedName, Unit])(using ctx: Context, env: Environment): Option[Func[QualifiedName, Type]] = 
    given (Int, Int) = f.v.pos
    given Type = f.t
    ctx.body = Body.Function(f.t)
    for { tF <- check(f.s) } 
    yield Func(f.t, Ident[QualifiedName, Type](f.v.v), checkParams(f.l), tF)(f.pos)

def check(ss: List[Stmt[QualifiedName, Unit]])(using ctx: Context, env: Environment): Option[List[Stmt[QualifiedName, Type]]] = 
    ss.foldRight(Some(List.empty)) {
        (opt: Stmt[QualifiedName, Unit], acc: Option[List[Stmt[QualifiedName, Type]]]) =>
            for { xs <- acc; x <- check(opt) } yield x :: xs
        }

def check(s: Stmt[QualifiedName, Unit])(using ctx: Context, env: Environment): Option[Stmt[QualifiedName, Type]] =  
    given (Int, Int) = s.pos
    s match {
        case NewAss(t, v, r) => check(Assign(v, r))
        case Assign(l, r) => 
            val (lvalType, typedLval) = check(l, Unconstrained)
            val (rvalType, typedRval) = check(r, Is(lvalType.getOrElse(?)))
            for {l <- typedLval; r <- typedRval} yield Assign(l, r)
        case Exit(e) => 
            for {tE <- check(e, Is(IntT()))._2} yield Exit(tE)
        case Free(e) =>
            val (et, tE) = check(e, IsFreeable)
            for {t <- et; e <- tE; given Type = t} yield Free(e)
        case If(e, s1, s2) => 
            val (_, typedCond) = check(e, Is(BoolT()))
            val (typedS1, typedS2) = (check(s1), check(s2))
            for {te <- typedCond; ts1 <- typedS1; ts2 <- typedS2} yield If(te, ts1, ts2)
        case Nest(s) => 
            for {ts <- check(s)} yield Nest(ts)
        case Print(e) => 
            val (ot, otE) = check(e, Unconstrained)
            for { t <- ot; tE <- otE; given Type = t} yield Print(tE)
        case PrintLn(e) => 
            val (ot, otE) = check(e, Unconstrained)
            for { t <- ot; tE <- otE; given Type = t} yield PrintLn(tE)
        case Read(l) => 
            val (ot, otE) = check(l, IsReadable)
            for { t <- ot; tE <- otE; given Type = t} yield Read(tE)
        case Return(e) => ctx.body match {
            case Body.Main => ctx.error(ReturnInMainBody((1,1)))
            case Body.Function(returnType) => 
                given Type = returnType
                for { te <- check(e, Is(returnType))._2 } yield Return(te)
        }
        case Skip() => Some(Skip()(s.pos))
        case While(e, s) => 
            val (_, typedCond) = check(e, Is(BoolT()))
            val typedS = check(s)
            for {te <- typedCond; ts <- typedS} yield While(te, ts)
} 

def check(e: Expr[QualifiedName, Unit], c: Constraint)(using ctx: Context, env: Environment): (Option[Type], Option[Expr[QualifiedName, Type]]) = 
    given (Int, Int) = e.pos
    e match {
        case Add(x, y) => 
            val ot = IntT().satisfies(c)
            val (_, otx) = check(x, Is(IntT()))
            val (_, oty) = check(y, Is(IntT()))
            val oe = for { 
                tx <- otx; ty <- oty } yield Add(tx, ty)
            (ot, oe)
        case And(x, y) => ???
        case Or(x, y) => ???
        case Div(x, y) => ???
        case Eq(x, y) => ???
        case Greater(x, y) => ???
        case GreaterEq(x, y) => ???
        case Less(x, y) => ???
        case LessEq(x, y) => ???
        case NotEq(x, y) => ???
        case Mod(x, y) => ???
        case Mul(x, y) => ???
        case Sub(x, y) => ???
        case ArrayElem(i, x) => ???
        case PairLit() => ???
        case StrLit(s) => ???
        case BoolLit(b) => ???
        case CharLit(c) => ???
        case Chr(e) => ???
        case IntLit(n) => ???
        case Len(e) => ???
        case Not(e) => ???
        case Neg(e) => ???
        case Ord(e) => ???
        case i: Ident[QualifiedName, Unit] => ???
}

def checkParams(ps: List[Param[QualifiedName, Unit]]): List[Param[QualifiedName, Type]] = 
    ps.map(p => 
        given (Int, Int) = p.v.pos
        given Type = p.t
        Param(p.t, Ident[QualifiedName, Type](p.v.v))(p.pos)
    )

def check(e: LValue[QualifiedName, Unit], c: Constraint)(using env: Environment): (Option[Type], Option[LValue[QualifiedName, Type]]) = ???

def check(e: RValue[QualifiedName, Unit], c: Constraint)(using env: Environment): (Option[Type], Option[RValue[QualifiedName, Type]]) = ???
