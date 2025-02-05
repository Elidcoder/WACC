package wacc.semantic.typecheck

import wacc.error.WaccErr
import wacc.semantic.typecheck.WaccErr.*
import wacc.ast.*
import wacc.semantic.{QualifiedName, Environment}
import os.makeDir.all

object typechecker {
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
        given ctx: Context = new Context(Body.Main, env)
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

    private def checkFuncs(fs: List[Func[QualifiedName, Unit]])(using ctx: Context): Option[List[Func[QualifiedName, Type]]] = 
        fs.foldRight(Some(List.empty)) {
            (opt: Func[QualifiedName, Unit], acc: Option[List[Func[QualifiedName, Type]]]) =>
                for { xs <- acc; x <- check(opt) } yield x :: xs
            }

    private def check(f: Func[QualifiedName, Unit])(using ctx: Context): Option[Func[QualifiedName, Type]] = 
        given (Int, Int) = f.v.pos
        given Type = f.t
        ctx.body = Body.Function(f.t)
        for { tF <- check(f.s) } 
        yield Func(f.t, Ident[QualifiedName, Type](f.v.v), checkParams(f.l), tF)(f.pos)

    private def check(ss: List[Stmt[QualifiedName, Unit]])(using ctx: Context): Option[List[Stmt[QualifiedName, Type]]] = 
        ss.foldRight(Some(List.empty)) {
            (opt: Stmt[QualifiedName, Unit], acc: Option[List[Stmt[QualifiedName, Type]]]) =>
                for { xs <- acc; x <- check(opt) } yield x :: xs
            }

    private def check(s: Stmt[QualifiedName, Unit])(using ctx: Context): Option[Stmt[QualifiedName, Type]] =  
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

    private def check(e: Expr[QualifiedName, Unit], c: Constraint)(using ctx: Context): (Option[Type], Option[Expr[QualifiedName, Type]]) = 
        given (Int, Int) = e.pos
        e match {
            case Not(e) => checkUnOp(e, BoolT(), c, Is(BoolT()), Not.apply)
            case Neg(e) => checkUnOp(e, IntT(), c, Is(IntT()), Neg.apply)
            case Len(e) => checkUnOp(e, IntT(), c, IsArray, Len.apply)
            case Ord(e) => checkUnOp(e, IntT(), c, Is(CharT()), Ord.apply)
            case Chr(e) => checkUnOp(e, CharT(), c, Is(IntT()), Chr.apply)
            case Add(x, y) => checkBinOp(x, y, c, IntT(), Is(IntT()), Add.apply)
            case Sub(x, y) => checkBinOp(x, y, c, IntT(), Is(IntT()), Sub.apply)
            case Mul(x, y) => checkBinOp(x, y, c, IntT(), Is(IntT()), Mul.apply)
            case Div(x, y) => checkBinOp(x, y, c, IntT(), Is(IntT()), Div.apply)
            case Mod(x, y) => checkBinOp(x, y, c, IntT(), Is(IntT()), Mod.apply)
            case And(x, y) => checkBinOp(x, y, c, BoolT(), Is(BoolT()), And.apply)
            case Or(x, y) => checkBinOp(x, y, c, BoolT(), Is(BoolT()), Or.apply)
            case Greater(x, y) => checkBinOp(x, y, c, BoolT(), Is(BoolT()), Greater.apply)
            case GreaterEq(x, y) => checkBinOp(x, y, c, BoolT(), Is(BoolT()), GreaterEq.apply)
            case Less(x, y) => checkBinOp(x, y, c, BoolT(), Is(BoolT()), Less.apply)
            case LessEq(x, y) => checkBinOp(x, y, c, BoolT(), Is(BoolT()), LessEq.apply)
            case Eq(x, y) => checkBinOp(x, y, c, BoolT(), Unconstrained, Eq.apply)
            case NotEq(x, y) => checkBinOp(x, y, c, BoolT(), Unconstrained, NotEq.apply)
            case ArrayElem(i, x) => checkArrayElem(i, x, c)
            case PairLit() => (PairT(?, ?).satisfies(c), Some(PairLit()(e.pos)))
            case StrLit(s) => (StringT().satisfies(c), Some(StrLit(s)))
            case BoolLit(b) => (BoolT().satisfies(c), Some(BoolLit(b)))
            case CharLit(chr) => (CharT().satisfies(c), Some(CharLit(chr)))
            case IntLit(n) => (IntT().satisfies(c), Some(IntLit(n)))
            case i: Ident[QualifiedName, Unit] => check(i, c)
    }

    private def checkArrayElem(i: Ident[QualifiedName, Unit], es: List[Expr[QualifiedName, Unit]], c: Constraint)(using ctx: Context, pos: (Int, Int)): (Option[Type], Option[ArrayElem[QualifiedName, Type]]) = 
        val it: Type = ctx.getType(i.v.uid)
        val (ot, otes) = es.foldRight((Some(it), Some(List.empty))) {
            (e: Expr[QualifiedName, Unit], x: (Option[Type], Option[List[Expr[QualifiedName, Type]]])) =>
                val (ot, otes) = x
                val newOt = for { t <- ot; case ArrayT(nt) <- t.satisfies(IsArray)} yield nt
                val (_, ote) = check(e, Is(IntT()))
                val finalTes = for { tes <- otes; te <- ote } yield te :: tes
                (newOt, finalTes)
            }
        given Type = it
        val oElem = for { tes <- otes } yield ArrayElem(Ident[QualifiedName, Type](i.v), tes)
        ((for {t <- ot; ft <- t.satisfies(c)} yield ft) , oElem)

    private def checkBinOp( 
        x: Expr[QualifiedName, Unit],
        y: Expr[QualifiedName, Unit],
        returnCon: Constraint,
        returnType: Type,
        valCon: Constraint,
        build: (Expr[QualifiedName, Type], Expr[QualifiedName, Type]) => Expr[QualifiedName, Type]
    )(using ctx: Context, pos: (Int, Int)): (Option[Type], Option[Expr[QualifiedName, Type]]) = 
        val ot = returnType.satisfies(returnCon)
        val (_, otx) = check(x, valCon)
        val (_, oty) = check(y, valCon)
        val oe = for { 
            tx <- otx; ty <- oty
        } yield build(tx, ty)
        (ot, oe)

    private def checkUnOp(
        e: Expr[QualifiedName, Unit],
        returnType: Type, 
        returnCon: Constraint, 
        exprCon: Constraint,
        build: Expr[QualifiedName, Type] => Expr[QualifiedName, Type]
    )(using ctx: Context, pos: (Int, Int)): (Option[Type], Option[Expr[QualifiedName, Type]]) =
        val ot = returnType.satisfies(returnCon)
        val te = for { te <- check(e, exprCon)._2 } yield build(te)
        (ot, te)

    private def checkParams(ps: List[Param[QualifiedName, Unit]]): List[Param[QualifiedName, Type]] = 
        ps.map(p => 
            given (Int, Int) = p.v.pos
            given Type = p.t
            Param(p.t, Ident[QualifiedName, Type](p.v.v))(p.pos)
        )

    private def check(i: Ident[QualifiedName, Unit], c: Constraint)(using ctx: Context): (Option[Type], Option[Ident[QualifiedName, Type]]) = 
        given (Int, Int) = i.pos
        val ot = ctx.getType(i.v.uid).satisfies(c)
        val tI = for { t <- ot; given Type = t } yield Ident[QualifiedName, Type](i.v)
        (ot, tI)
    
    private def check(e: LValue[QualifiedName, Unit], c: Constraint)(using ctx: Context): (Option[Type], Option[LValue[QualifiedName, Type]]) =
        given Context = ctx
        given (Int, Int) = e.pos
        e match {
            case ArrayElem(i, xs) => checkArrayElem(i, xs, c)
            case l: PairElem[QualifiedName, Unit] => checkPairElem(l, c)
            case i: Ident[QualifiedName, Unit] => check(i, c)
    }

    private def checkPairElem(l: PairElem[QualifiedName, Unit], c: Constraint)(using ctx: Context): (Option[Type], Option[LValue[QualifiedName, Type]]) =
        given (Int, Int) = l.pos
        l match {
            case First(v) => 
                val (olt, olv) = check(v, IsPair)
                ((for { case PairT(f, _) <- olt; ft <- f.satisfies(c) } yield ft ), (for { fv <- olv } yield fv))
            case Second(v) =>
                val (olt, olv) = check(v, IsPair)
                ((for { case PairT(_, s) <- olt; st <- s.satisfies(c) } yield st ), (for { sv <- olv } yield sv))
    }

    private def checkArrayLit(exprs: List[Expr[QualifiedName, Unit]], c: Constraint)(using ctx: Context, pos: (Int, Int)): (Option[Type], Option[RValue[QualifiedName, Type]]) = 
        val (types, trees) = exprs.map(check(_, Unconstrained)).unzip
        val posElemsType = types.foldRight(
            Some(?))(
            (posCurType:Option[Type], posAccType:Option[Type]) => 
                for {
                    curType <- posCurType; 
                    accType <- posAccType; 
                    matchedType <- curType ~ accType
                } yield matchedType
        )
        val arrayType = posElemsType match {
            case None => None
            case Some(elemsType) => Some(ArrayT(elemsType))
        }

        val arrayTree = if (trees.contains(None)) {
            None
        }
        else {

            Some(ArrayLit(trees.map(_ match {case Some(tree) => tree})))
        }
        
        (for {defArrayType <- arrayType; checkedArrayType <- defArrayType.satisfies(c)} yield checkedArrayType , arrayTree)

    private def check(rVal: RValue[QualifiedName, Unit], c: Constraint)(using ctx: Context): (Option[Type], Option[RValue[QualifiedName, Type]]) = 
        given (Int, Int) = rVal.pos
        rVal match {
            case expr: Expr[QualifiedName, Unit] => check(expr, c)
            case pairElem: PairElem[QualifiedName, Unit] => check(pairElem, c)
            case newPair: NewPair[QualifiedName, Unit] => ???
            case call: Call[QualifiedName, Unit] => ???
            case ArrayLit(exprs) => checkArrayLit(exprs, c) 
        }
}