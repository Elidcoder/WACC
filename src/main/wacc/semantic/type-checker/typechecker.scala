package wacc.semantic.typecheck

import wacc.semantic.renamedAst
import wacc.semantic.typedAst
import wacc.error.WaccErr
import wacc.semantic.typecheck.WaccErr.*
import wacc.semantic.renamedAst.Add
import wacc.semantic.renamedAst.And
import wacc.semantic.renamedAst.ArrayElem
import wacc.semantic.renamedAst.BoolLit
import wacc.semantic.renamedAst.CharLit
import wacc.semantic.renamedAst.Chr
import wacc.semantic.renamedAst.Div
import wacc.semantic.renamedAst.Eq
import wacc.semantic.renamedAst.Greater
import wacc.semantic.renamedAst.GreaterEq
import wacc.semantic.renamedAst.Less
import wacc.semantic.renamedAst.LessEq
import wacc.semantic.renamedAst.Mod
import wacc.semantic.renamedAst.Mul
import wacc.semantic.renamedAst.Sub
import wacc.semantic.renamedAst.IntLit
import wacc.semantic.renamedAst.Len
import wacc.semantic.renamedAst.Not
import wacc.semantic.renamedAst.Neg
import wacc.semantic.renamedAst.Ord
import wacc.semantic.renamedAst.Or
import wacc.semantic.renamedAst.PairLit
import wacc.semantic.renamedAst.NotEq
import wacc.semantic.renamedAst.StrLit

enum Constraint {
    case Is(refT: renamedAst.Type)
    case IsStringLike
    case IsFreeable
    case IsReadable
}
object Constraint {
    val Unconstrained = Is(renamedAst.?)
    val IsArray = Is(renamedAst.ArrayT(renamedAst.?))
    val IsPair  = Is(renamedAst.PairT(renamedAst.?, renamedAst.?))
}
import Constraint.*

extension (t: renamedAst.Type) 
    def ~(refT: renamedAst.Type): Option[renamedAst.Type] = (t, refT) match {
        case (renamedAst.?, refT)                                       => Some(refT)
        case (t, renamedAst.?)                                          => Some(t)
        case (renamedAst.PairT(t1, t2), renamedAst.PairT(refT1, refT2)) =>
            for {commonT1 <- t1 ~ refT1; commonT2 <- t2 ~ refT2}
            yield renamedAst.PairT(commonT1, commonT2)
        case (renamedAst.ArrayT(t), renamedAst.ArrayT(refT))            =>
            for {commonT <- t ~ refT}
            yield renamedAst.ArrayT(commonT)
        case (t, refT) if t == refT                                     => Some(t)
        case _                                                          => None
    }
    def satisfies(c: Constraint)(using ctx: Context): Option[renamedAst.Type] = (t,c) match {
    case (t, Is(refT)) => (t ~ refT).orElse {
        ctx.error(TypeMismatch((1,1), renamedAst.Ident("blah", -1, t), refT))
    }
    case (renamedAst.?, _) => Some(renamedAst.?)
    case (kt@(renamedAst.ArrayT(renamedAst.CharT()) | renamedAst.StringT()), IsStringLike) => Some(kt)
    case (kt, IsStringLike) => ctx.error(IsNotString((1,1), renamedAst.Ident("blah", -1, t)))
    case (kt@(renamedAst.ArrayT(_) | renamedAst.PairT(_,_)), IsFreeable) => Some(kt)
    case (kt, IsFreeable) => ctx.error(IsNotFreeable((1,1), renamedAst.Ident("blah", -1, t)))
    case (kt@(renamedAst.IntT() | renamedAst.CharT()), IsReadable) => Some(kt)
    case (kt, IsReadable) => ctx.error(IsNotFreeable((1,1), renamedAst.Ident("blah", -1, t)))
}


def check(prog: renamedAst.Program): Either[List[WaccErr], Option[typedAst.Program]] = {
    given ctx: Context = new Context(Body.Main)
    val typedFuncs: Option[List[typedAst.Func]] = checkFuncs(prog.fs)
    val typedStmts: Option[List[typedAst.Stmt]] = {
        ctx.body = Body.Main 
        check(prog.x)
    }
    val errors = ctx.result
    if errors.isEmpty 
        then Right(for { fs <- typedFuncs; ss <- typedStmts} yield typedAst.Program(fs, ss))
        else Left(errors)
}

def checkFuncs(fs: List[renamedAst.Func])(using ctx: Context): Option[List[typedAst.Func]] = 
    fs.foldRight(Some(List.empty)) {
        (opt: renamedAst.Func, acc: Option[List[typedAst.Func]]) =>
            for { xs <- acc; x <- check(opt) } yield x :: xs
        }

def check(f: renamedAst.Func)(using ctx: Context): Option[typedAst.Func] = 
    ctx.body = Body.Function(f.rt)
    for { tF <- check(f.s) } yield typedAst.Func(typedAst.Ident(f.v.oldName, f.v.uid, check(f.v.t)), tF)

def check(ss: List[renamedAst.Stmt])(using ctx: Context): Option[List[typedAst.Stmt]] = 
    ss.foldRight(Some(List.empty)) {
        (opt: renamedAst.Stmt, acc: Option[List[typedAst.Stmt]]) =>
            for { xs <- acc; x <- check(opt) } yield x :: xs
        }

def check(s: renamedAst.Stmt)(using ctx: Context): Option[typedAst.Stmt] =  s match {
    case renamedAst.Assign(l, r) => 
        val (lvalType, typedLval) = check(l, Unconstrained)
        val (rvalType, typedRval) = check(r, Is(lvalType.getOrElse(renamedAst.?)))
        for {l <- typedLval; r <- typedRval} yield typedAst.Assign(l, r)
    case renamedAst.Exit(e) => 
        for {tE <- check(e, Is(renamedAst.IntT()))._2} yield typedAst.Exit(tE)
    case renamedAst.Free(e) => 
        for {tE <- check(e, IsFreeable)._2} yield typedAst.Free(tE)
    case renamedAst.If(e, s1, s2) => 
        val (_, typedCond) = check(e, Is(renamedAst.BoolT()))
        val (typedS1, typedS2) = (check(s1), check(s2))
        for {te <- typedCond; ts1 <- typedS1; ts2 <- typedS2} yield typedAst.If(te, ts1, ts2)
    case renamedAst.Nest(s) => 
        for {ts <- check(s)} yield typedAst.Nest(ts)
    case renamedAst.Print(e) => 
        val (ot, otE) = check(e, Unconstrained)
        for { t <- ot; tE <- otE} yield typedAst.Print(tE, check(t))
    case renamedAst.PrintLn(e) => 
        val (ot, otE) = check(e, Unconstrained)
        for { t <- ot; tE <- otE} yield typedAst.PrintLn(tE, check(t))
    case renamedAst.Read(l) => 
        val (ot, otE) = check(l, IsReadable)
        for { t <- ot; tE <- otE} yield typedAst.Read(tE, check(t))
    case renamedAst.Return(e) => ctx.body match {
        case Body.Main => ctx.error(ReturnInMainBody((1,1)))
        case Body.Function(returnType) => 
            for { te <- check(e, Is(returnType))._2 } yield typedAst.Return(te, check(returnType))
    }
    case renamedAst.Skip() => Some(typedAst.Skip())
    case renamedAst.While(e, s) => 
        val (_, typedCond) = check(e, Is(renamedAst.BoolT()))
        val typedS = check(s)
        for {te <- typedCond; ts <- typedS} yield typedAst.While(te, ts)
} 

def check(e: renamedAst.Expr, c: Constraint): (Option[renamedAst.Type], Option[typedAst.Expr]) = e match {
    case Add(x, y) => ???
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
    case i: renamedAst.Ident => ???
}

def check(e: renamedAst.LValue, c: Constraint): (Option[renamedAst.Type], Option[typedAst.LValue]) = ???

def check(e: renamedAst.RValue, c: Constraint): (Option[renamedAst.Type], Option[typedAst.RValue]) = ???

def check(t: renamedAst.Type): typedAst.Type = ???

object Ident {
    def apply(v: wacc.semantic.renamedAst.Ident) =
        new typedAst.Ident(v.oldName, v.uid, wacc.semantic.typecheck.check(v.t))
}