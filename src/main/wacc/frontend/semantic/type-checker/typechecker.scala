package wacc.semantic.typecheck

import wacc.ast._
import wacc.error.WaccErr
import wacc.error.TypeErr._
import wacc.semantic.{QualifiedName, Environment}

import java.io.File

object typechecker {
    enum Constraint {
        case Is(refT: SemType)
        case IsStringLike
        case IsFreeable
        case IsReadable
        case IsComparable
    }
    object Constraint {
        val Unconstrained = Is(?)
        val IsArray = Is(ArrayT(?))
        val IsPair  = Is(PairT(?, ?))
    }
    import Constraint.*

    extension (ty: SemType) 
        private def ~(refT: SemType)(using pos: Pos): Option[SemType] =
            given Typeless = Typeless()
            (ty, refT) match 
                case (StringT(), ArrayT(CharT())) => Some(StringT())
                case (ArrayT(CharT()), StringT()) => Some(StringT())
                case (t, refT) => t ~~ refT
                
        private def ~~(refT: SemType)(using pos: Pos): Option[SemType] =
            given Typeless = Typeless()
            (ty, refT) match {
                case (?, refT)  => Some(refT)
                case (ty, ?)    => Some(ty)
                case (PairT(lType, rType), PairT(lRefType, rRefType)) =>
                    for { commonT1 <- lType ~~ lRefType; commonT2 <- rType ~~ rRefType }
                    yield PairT(commonT1, commonT2)
                case (ArrayT(ty), ArrayT(refT)) =>
                    for { commonT <- ty ~~ refT }
                    yield ArrayT(commonT)
                case (FuncT(ty, types), FuncT(refType, refTypes)) =>
                    if types.size != refTypes.size then return None
                    val typesOpt = (types zip refTypes).foldRight(
                        Some(List.empty)
                        )( (curElem: (SemType, SemType), optAcc: Option[List[SemType]]) =>
                            for {
                                ty <- curElem._1 ~ curElem._2
                                acc <- optAcc
                            } yield ty :: acc
                    )
                    for {funcTypes <- typesOpt; funcType <- ty ~ refType}
                    yield FuncT(funcType, funcTypes)(pos)
                case (ty, refType) if ty == refType => Some(ty)
                case _                              => None
            }

        def satisfies(constr: Constraint)(using ctx: Context, pos: Pos): Option[SemType] = 
            given Typeless = Typeless()
            (ty, constr) match {
                case (StringT(), Is(ArrayT(CharT()))) => 
                    ctx.error(TypeMismatch(StringT(), ArrayT(CharT())))
                case (ty, Is(refT)) => (ty ~ refT).orElse {
                    ctx.error(TypeMismatch(ty, refT))
                }
                case (?, _)                                      => Some(?)
                case (ArrayT(CharT()) | StringT(), IsStringLike) => Some(ty)
                case (knownT, IsStringLike)                      => ctx.error(IsNotString(knownT))
                case (ArrayT(_) | PairT(_,_), IsFreeable)        => Some(ty)
                case (knownT, IsFreeable)                        => ctx.error(IsNotFreeable(knownT))
                case (IntT() | CharT(), IsReadable)              => Some(ty)
                case (knownT, IsReadable)                        => ctx.error(IsNotReadable(knownT))
                case (IntT() | CharT(), IsComparable)            => Some(ty)
                case (knownT, IsComparable)                      => ctx.error(IsNotComparable(knownT))
    }

    private def isKnown(semTy: SemType): Option[KnownType] = semTy match {
        case knownTy: KnownType => Some(knownTy)
        case ?                  => None
    }

    def check(
        prog: Program[QualifiedName, Typeless], 
        env: Environment, file: File
    ): Either[List[WaccErr], Option[Program[QualifiedName, KnownType]]] = {
        given ctx: Context = new Context(Body.Main, env, file)
        val typedFuncs: Option[List[Func[QualifiedName, KnownType]]] = checkFuncs(prog.funcs)
        val typedStmts: Option[List[Stmt[QualifiedName, KnownType]]] = {
            ctx.body = Body.Main 
            check(prog.stmts)
        }
        val errors = ctx.result
        if errors.isEmpty 
            then Right(for { funcs <- typedFuncs; stmts <- typedStmts } 
                       yield Program[QualifiedName, KnownType](funcs, stmts)(prog.pos))
            else Left(errors)
    }

    private def checkFuncs(
        funcs: List[Func[QualifiedName, Typeless]]
    )(using ctx: Context): Option[List[Func[QualifiedName, KnownType]]] = 
        funcs.foldRight(Some(List.empty)) {
            (curFunc: Func[QualifiedName, Typeless], 
             optFuncAcc: Option[List[Func[QualifiedName, KnownType]]]) =>
                for { funcAcc <- optFuncAcc; defFunc <- check(curFunc) } yield defFunc :: funcAcc
            }

    private def check(
        func: Func[QualifiedName, Typeless]
    )(using ctx: Context): Option[Func[QualifiedName, KnownType]] = 
        given Pos = func.id.pos
        ctx.body = Body.Function(func.retType)
        for { typeFunc <- check(func.stmts); knownTy <- isKnown(func.retType); given KnownType = knownTy } 
        yield Func(func.retType, 
                   Ident[QualifiedName, KnownType](func.id.name), 
                   checkParams(func.params), 
                   typeFunc
                  )(func.pos)

    private def check(
        stmts: List[Stmt[QualifiedName, Typeless]]
    )(using ctx: Context): Option[List[Stmt[QualifiedName, KnownType]]] = 
        stmts.foldRight(Some(List.empty)) {
            (opt: Stmt[QualifiedName, Typeless],
             optAcc: Option[List[Stmt[QualifiedName, KnownType]]]) =>
                for { stmts <- optAcc; stmt <- check(opt) } yield stmt :: stmts
            }

    private def check(
        stmt: Stmt[QualifiedName, Typeless]
    )(using ctx: Context): Option[Stmt[QualifiedName, KnownType]] =  
        given Pos = stmt.pos
        stmt match {
            case NewAss(newType, id, rval) => 
                given Typeless = Typeless()
                check(Assign(id, rval))
            case Assign(lval, rval) => 
                val (lvalTypeOpt, typedLvalOpt) = check(lval, Unconstrained)
                val (rvalTypeOpt, typedRvalOpt) = check(rval, Is(lvalTypeOpt.getOrElse(?)))
                if rvalTypeOpt == Some(?) then ctx.error(UnknownPairTypes())
                for { lval <- typedLvalOpt; rval <- typedRvalOpt; rvalType <- rvalTypeOpt;
                      knownTy <- isKnown(rvalType); given KnownType = knownTy } 
                yield Assign(lval, rval)
            case Exit(expr) => 
                for {typeExpr <- check(expr, Is(IntT()))._2} yield Exit(typeExpr)
            case Free(expr) =>
                val (exprTypeOpt, typedExprOpt) = check(expr, IsFreeable)
                for { exprType <- exprTypeOpt; expr <- typedExprOpt;
                      knownTy <- isKnown(exprType); given KnownType = knownTy } 
                yield Free(expr)
            case If(expr, stmtThen, stmtElse) => 
                val (_, typedCond) = check(expr, Is(BoolT()))
                val (typedStmtThenOpt, typedStmtElseOpt) = (check(stmtThen), check(stmtElse))
                for { typedExpr <- typedCond; 
                      typedStmtThen <- typedStmtThenOpt; 
                      typedStmtElse <- typedStmtElseOpt } 
                yield If(typedExpr, typedStmtThen, typedStmtElse)
            case Nest(stmt) => 
                for { typedStmt <- check(stmt) } 
                yield Nest(typedStmt)
            case Print(expr) => 
                val (typeOpt, typedExprOpt) = check(expr, Unconstrained)
                for { ty <- typeOpt; typedExpr <- typedExprOpt; 
                      knownTy <- isKnown(ty); given KnownType = knownTy } 
                yield Print(typedExpr)
            case PrintLn(expr) => 
                val (typeOpt, typedExprOpt) = check(expr, Unconstrained)
                for { ty <- typeOpt; typedExpr <- typedExprOpt; 
                      knownTy <- isKnown(ty); given KnownType = knownTy } 
                yield PrintLn(typedExpr)
            case Read(lval) => 
                val (typeOpt, typedExprOpt) = check(lval, IsReadable)
                if typeOpt == Some(?) then ctx.error(ReadUnknownType())
                for { ty <- typeOpt; typedExpr <- typedExprOpt; 
                      knownTy <- isKnown(ty); given KnownType = knownTy }
                yield Read(typedExpr)
            case Return(expr) => ctx.body match {
                case Body.Main => ctx.error(ReturnInMainBody())
                case Body.Function(returnType) => 
                    for { typedExpr <- check(expr, Is(returnType))._2
                          knownTy <- isKnown(returnType)
                          given KnownType = knownTy } 
                    yield Return(typedExpr)
            }
            case Skip() => Some(Skip[QualifiedName, KnownType]()(stmt.pos))
            case While(expr, stmt) => 
                val (_, typedExprOpt) = check(expr, Is(BoolT()))
                val typedStmtOpt = check(stmt)
                for { typedExpr <- typedExprOpt; typedStmt <- typedStmtOpt } 
                yield While(typedExpr, typedStmt)
        } 

    private def check(
        expr: Expr[QualifiedName, Typeless], 
        constr: Constraint
    )(using ctx: Context): (Option[SemType], Option[Expr[QualifiedName, KnownType]]) = 
        given Pos = expr.pos
        expr match {
            case Not(e)          => checkUnOp(e, BoolT(), constr, Is(BoolT()), Not.apply)
            case Neg(e)          => checkUnOp(e, IntT(), constr, Is(IntT()), Neg.apply)
            case Len(e)          => checkUnOp(e, IntT(), constr, IsArray, Len.apply)
            case Ord(e)          => checkUnOp(e, IntT(), constr, Is(CharT()), Ord.apply)
            case Chr(e)          => checkUnOp(e, CharT(), constr, Is(IntT()), Chr.apply)
            case Add(x, y)       => checkBinOp(x, y, constr, IntT(), Is(IntT()), Add.apply)
            case Sub(x, y)       => checkBinOp(x, y, constr, IntT(), Is(IntT()), Sub.apply)
            case Mul(x, y)       => checkBinOp(x, y, constr, IntT(), Is(IntT()), Mul.apply)
            case Div(x, y)       => checkBinOp(x, y, constr, IntT(), Is(IntT()), Div.apply)
            case Mod(x, y)       => checkBinOp(x, y, constr, IntT(), Is(IntT()), Mod.apply)
            case And(x, y)       => checkBinOp(x, y, constr, BoolT(), Is(BoolT()), And.apply)
            case Or(x, y)        => checkBinOp(x, y, constr, BoolT(), Is(BoolT()), Or.apply)
            case Greater(x, y)   => checkBinOp(x, y, constr, BoolT(), IsComparable, Greater.apply)
            case GreaterEq(x, y) => checkBinOp(x, y, constr, BoolT(), IsComparable, GreaterEq.apply)
            case Less(x, y)      => checkBinOp(x, y, constr, BoolT(), IsComparable, Less.apply)
            case LessEq(x, y)    => checkBinOp(x, y, constr, BoolT(), IsComparable, LessEq.apply)
            case Eq(x, y)        => checkBinOp(x, y, constr, BoolT(), Unconstrained, Eq.apply)
            case NotEq(x, y)     => checkBinOp(x, y, constr, BoolT(), Unconstrained, NotEq.apply)
            case ArrayElem(i, x) => checkArrayElem(i, x, constr)
            case PairLit()       => (PairT(?, ?).satisfies(constr), Some(PairLit()(expr.pos)))
            case StrLit(s)       => (StringT().satisfies(constr), Some(StrLit(s)))
            case BoolLit(b)      => (BoolT().satisfies(constr), Some(BoolLit(b)))
            case CharLit(chr)    => (CharT().satisfies(constr), Some(CharLit(chr)))
            case IntLit(n)       => (IntT().satisfies(constr), Some(IntLit(n)))
            case id: Ident[QualifiedName, Typeless] => check(id, constr)
    }

    private def checkArrayElem(
        id: Ident[QualifiedName, Typeless], 
        exprs: List[Expr[QualifiedName, Typeless]], 
        constr: Constraint
    )(using ctx: Context, pos: Pos): (Option[SemType], Option[ArrayElem[QualifiedName, KnownType]]) = 
        val identType: SemType = ctx.getType(id.name)
        val (typeOpt, typedExprsOpt) = exprs.foldRight((Some(identType), Some(List.empty))) {
            (expr: Expr[QualifiedName, Typeless],
             optAcc: (Option[SemType], Option[List[Expr[QualifiedName, KnownType]]])) =>
                val (typeOpt, typedExprsOpt) = optAcc
                val newTypeOpt = for { ty <- typeOpt; 
                                       case ArrayT(nestedType) <- ty.satisfies(IsArray) } 
                                 yield nestedType
                val (_, typedExprOpt) = check(expr, Is(IntT()))
                val finalTypedExprs = for { typedExprs <- typedExprsOpt; 
                                            typedExpr <- typedExprOpt } 
                                      yield typedExpr :: typedExprs
                (newTypeOpt, finalTypedExprs)
            }
        val typedArrElemOpt = for { typedExprs <- typedExprsOpt
                                    knownTy <- isKnown(identType)
                                    given KnownType = knownTy } 
                              yield ArrayElem(Ident[QualifiedName, KnownType](id.name), typedExprs)
        ((for {ty <- typeOpt; finalTypeOpt <- ty.satisfies(constr) } 
          yield finalTypeOpt), typedArrElemOpt)

    private def checkBinOp( 
        x: Expr[QualifiedName, Typeless],
        y: Expr[QualifiedName, Typeless],
        returnCon: Constraint,
        returnType: SemType,
        valConstr: Constraint,
        build: (Expr[QualifiedName, KnownType], Expr[QualifiedName, KnownType]) => Expr[QualifiedName, KnownType]
    )(using ctx: Context, pos: Pos): (Option[SemType], Option[Expr[QualifiedName, KnownType]]) = 
        val typeOpt = returnType.satisfies(returnCon)
        val (_, typedXOpt) = check(x, valConstr)
        val (_, typedYOpt) = check(y, valConstr)
        val exprOpt = for { 
            typedX <- typedXOpt; typedY <- typedYOpt
        } yield build(typedX, typedY)
        (typeOpt, exprOpt)

    private def checkUnOp(
        expr: Expr[QualifiedName, Typeless],
        returnType: SemType, 
        returnConstr: Constraint, 
        exprConstr: Constraint,
        build: Expr[QualifiedName, KnownType] => Expr[QualifiedName, KnownType]
    )(using ctx: Context, pos: Pos): (Option[SemType], Option[Expr[QualifiedName, KnownType]]) =
        val typeOpt = returnType.satisfies(returnConstr)
        val typedExprOpt = for { typedExpr <- check(expr, exprConstr)._2 } yield build(typedExpr)
        (typeOpt, typedExprOpt)

    private def checkParams(
        params: List[Param[QualifiedName, Typeless]]
    ): List[Param[QualifiedName, KnownType]] = 
        params.map(param => 
            given Pos = param.paramId.pos
            given KnownType = param.paramType
            Param(param.paramType, Ident[QualifiedName, KnownType](param.paramId.name))(param.pos)
        )

    private def check(
        id: Ident[QualifiedName, Typeless], 
        constr: Constraint
    )(using ctx: Context, pos: Pos): (Option[SemType], Option[Ident[QualifiedName, KnownType]]) = 
        val typeOpt = ctx.getType(id.name).satisfies(constr)
        val typedIdentOpt = for { ty <- typeOpt; 
                                  knownTy <- isKnown(ty); 
                                  given KnownType = knownTy } 
                            yield Ident[QualifiedName, KnownType](id.name)
        (typeOpt, typedIdentOpt)
    
    private def check(
        expr: LValue[QualifiedName, Typeless], 
        constr: Constraint)(using ctx: Context
    ): (Option[SemType], Option[LValue[QualifiedName, KnownType]]) =
        given Context = ctx
        given Pos = expr.pos
        expr match {
            case ArrayElem(id, exprs)                    => checkArrayElem(id, exprs, constr)
            case pair: PairElem[QualifiedName, Typeless] => checkPairElem(pair, constr)
            case id: Ident[QualifiedName, Typeless]      => check(id, constr)
    }

    private def checkPairElem(
        pair: PairElem[QualifiedName, Typeless], 
        constr: Constraint)(using ctx: Context
    ): (Option[SemType], Option[PairElem[QualifiedName, KnownType]]) =
        given Pos = pair.pos
        pair match {
            case First(lval) => 
                val (typeOpt, typedLvalOpt) = check(lval, IsPair)
                ((for { case PairT(typeFirst, _) <- typeOpt; 
                        typeFirstFinal <- typeFirst.satisfies(constr) } 
                  yield typeFirstFinal ), 
                 (for { typedLval <- typedLvalOpt } 
                  yield First(typedLval)))
            case Second(lval) =>
                val (typeOpt, typedLvalOpt) = check(lval, IsPair)
                ((for { case PairT(_, typeSecond) <- typeOpt; 
                        typeSecondFinal <- typeSecond.satisfies(constr) } 
                  yield typeSecondFinal ),
                 (for { typedLval <- typedLvalOpt }
                 yield Second(typedLval)))
    }

    private def checkArrayLit(
        exprs: List[Expr[QualifiedName, Typeless]], 
        constr: Constraint
    )(using ctx: Context, pos: Pos): (Option[SemType], Option[RValue[QualifiedName, KnownType]]) = 
        val (exprsOptTypes, exprsOptTrees) = exprs.map(check(_, Unconstrained)).unzip
        val optElemsType = exprsOptTypes.foldRight(
            Some(?))(
            (optCurType:Option[SemType], optAccType:Option[SemType]) => 
                for {
                    curType <- optCurType; 
                    accType <- optAccType; 
                    matchedType <- curType.satisfies(Is(accType))
                } yield matchedType
        )

        val arrayType = optElemsType.fold(None)(elemsType => Some(ArrayT(elemsType))) 
        val arrayTree = if (exprsOptTrees.contains(None)) {
            None
        }
        else {
            Some(ArrayLit(exprsOptTrees.map(_.get)))
        }
        
        (for { defArrayType <- arrayType; 
               checkedArrayType <- defArrayType.satisfies(constr) } 
         yield checkedArrayType , arrayTree)

    private def check(
        call: Call[QualifiedName, Typeless], 
        constr: Constraint
    )(using ctx: Context, pos: Pos): (Option[SemType], Option[RValue[QualifiedName, KnownType]]) = 
        val (typeFuncOpt, typedIdentOpt) = check(call.id, Unconstrained)
        val resTuple = for {
            case FuncT(funcType, paramTypes) <- typeFuncOpt
            if checkArgSize(paramTypes, call.exprs, call.id.name)
            finalType <- funcType.satisfies(constr)
            finalTypedExprs <- (call.exprs zip paramTypes)
                               .foldRight(Some(List.empty[Expr[QualifiedName, KnownType]])) { 
                (newTup: (Expr[QualifiedName, Typeless], SemType), 
                 optAcc: Option[List[Expr[QualifiedName, KnownType]]]) =>
                    val (expr, ty) = newTup
                    val (optType, optTree) = check(expr, Is(ty))
                    for { typedExpr <- optTree; typedExprs <- optAcc } 
                    yield typedExpr :: typedExprs
            }
        } yield (finalType, finalTypedExprs)

        (for { (finalType,_) <- resTuple }                                     
         yield finalType, 
         for { typedIdent <- typedIdentOpt; (_, finalTypedExprs) <- resTuple }
         yield Call(typedIdent, finalTypedExprs))
    
    private def checkArgSize(
        types: List[SemType], 
        exprs: List[Expr[QualifiedName, Typeless]],
        funcName: QualifiedName
    )(using ctx: Context, pos: Pos): Boolean =
        if types.size == exprs.size then
            true
        else
            ctx.error(WrongArgNums(exprs.size, types.size, funcName))
            false

    private def check(
        rval: RValue[QualifiedName, Typeless], 
        constr: Constraint
    )(using ctx: Context): (Option[SemType], Option[RValue[QualifiedName, KnownType]]) = 
        given Pos = rval.pos
        rval match {
            case expr: Expr[QualifiedName, Typeless]         => check(expr, constr)
            case pairElem: PairElem[QualifiedName, Typeless] => checkPairElem(pairElem, constr)
            case NewPair(lexpr, rexpr) => 
                val (typeLOpt, typedLexprOpt) = check(lexpr, Unconstrained)
                val (typeROpt, typedRexpropt) = check(rexpr, Unconstrained)
                val finalTypedPair = for { typedLexpr <- typedLexprOpt; 
                                           typedRexpr <- typedRexpropt } 
                                     yield NewPair(typedLexpr, typedRexpr)
                (for { typeL <- typeLOpt; 
                       typeR <- typeROpt; 
                       finalType <- PairT(typeL, typeR).satisfies(constr) } 
                 yield finalType, finalTypedPair)
            case call: Call[QualifiedName, Typeless] => check(call, constr)
            case ArrayLit(exprs)                     => checkArrayLit(exprs, constr) 
        }
}
