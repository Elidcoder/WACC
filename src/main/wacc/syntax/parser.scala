package wacc.syntax

import wacc.ast._
import wacc.syntax.lexer._

import parsley.{Parsley, Result}
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.{ErrorMethods, fail}
import parsley.quick.{atomic, many, notFollowedBy, option, some}
import parsley.expr.chain.postfix
import parsley.expr.{InfixL, InfixN, InfixR, Ops, Prefix, precedence}

import lexer.implicits.implicitSymbol

import java.io.File

private final val ERR_FILE_NOT_FOUND = -1

object parser {
    
    /* Function to read a given file and return it's parsing or a failure code. */
    def parse[Err: ErrorBuilder](file: File): Result[Err, Program[String, Typeless]] = 
        parser.parseFile(file).getOrElse {println("Error: File not found"); sys.exit(ERR_FILE_NOT_FOUND)}

    /* Checks if a given group of statements end in a return or exit. */
    private def isReturnStmt(stmts: List[Stmt[String, Typeless]]): Boolean = stmts.last match {
        case If(_, ifStmts, elseStmts) => isReturnStmt(ifStmts) && isReturnStmt(elseStmts)
        case While(_, wStmts)          => isReturnStmt(wStmts)
        case Nest(nStmts)              => isReturnStmt(nStmts)
        case Return(_)                 => true
        case Exit(_)                   => true
        case _                         => false
    }
    
    /* Ensure the whole input is consumed. */
    private val parser: Parsley[Program[String, Typeless]] = fully(program)
    
    /* Extract the main body of the program. */
    protected [syntax] lazy val program: Parsley[Program[String, Typeless]] = 
        "begin" ~> Program(many(func), stmts.explain("missing main program body")) <~ "end"

    /* Parse functions (but not calls).  */
    protected [syntax] lazy val func: Parsley[Func[String, Typeless]] = 
        atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))))) <*> ("is" ~> stmts.filter(isReturnStmt) <~ "end")

    /* Parse a block of statements. */
    private lazy val stmts: Parsley[List[Stmt[String, Typeless]]] = semiSep1(stmt)

    /* Parse an assignment. */
    private lazy val asgnmt = ("=" ~> rvalue).label("assignment")

    /* Parse a statment. */
    protected [syntax] lazy val stmt: Parsley[Stmt[String, Typeless]] = 
        (("skip" ~> Skip())
        | ("read" ~> Read(lvalue))
        | ("free" ~> Free(expr))
        | ("return" ~> Return(expr))
        | ("exit" ~> Exit(expr))
        | ("println" ~> PrintLn(expr))
        | ("print" ~> Print(expr))
        | "if" ~> 
            If((expr), 
                ("then".explain(
                    "the condition of an if statement must be closed with `then`") ~> stmts), 
                ("else".explain("all if statements must have an else clause") ~> stmts)
            ) <~ "fi".explain("unclosed if statement")
        | "while" ~> While(expr, ("do" ~> stmts)) <~ "done"
        | "begin" ~> Nest(stmts) <~ "end"
        | NewAss(ptype, ident, asgnmt)
        | Assign(lvalue, asgnmt)).label("statement")

    /* Error message taken from the WACC Reference Compiler. */
    val EXPR_ERR_MSG = 
        """expressions may start with an integer, string, character or boolean literals, identifiers, unary operators, null, or parentheses.
  in addition, expressions may contain array indexing operations and comparison, logical, or arithmetic operators."""

    /* Parse an array index (making use of an option). */
    private lazy val arridx = option(some(brackets(expr))).label("array index")

    /* Parse an expression, makesuse of the precedence function. */
    protected [syntax] lazy val expr: Parsley[Expr[String, Typeless]] = precedence(
        ("null" ~> PairLit()),
        BoolLit(("true" as true) | ("false" as false)),
        IntLit(integer),
        CharLit(asciiChar),
        StrLit(asciiString),
        ArrayOrIdent(ident, arridx),
        parens(expr))(
        Ops(Prefix)(
            (Not <# "!"),
            (notFollowedBy(integer) ~> (Neg <# "-")),
            (Len <# "len"),
            (Ord <# "ord"),
            (Chr <# "chr"),
        ),
        Ops(InfixL)(
            (Mul <# "*"),
            (Div <# "/"),
            (Mod <# "%"),
        ),
        Ops(InfixL)(
            (Add <# "+"),
            (Sub <# "-"),
        ),
        Ops(InfixN)(
            (GreaterEq <# ">="),
            (LessEq <# "<="),
            (Greater <# ">"),
            (Less <# "<"),
        ),
        Ops(InfixN)(
            (Eq <# "=="),
            (NotEq <# "!="),
        ),
        Ops(InfixR)(
            (And <# "&&"),
        ),
        Ops(InfixR)(
            (Or <# "||"),
        ),
    ).label("expression").explain(EXPR_ERR_MSG)

    /* LValue parser. */
    protected [syntax] lazy val lvalue: Parsley[LValue[String, Typeless]] = 
        pairElem
        | ArrayOrIdent(ident, arridx)

    /* RValue parser. */
    protected [syntax] lazy val rvalue: Parsley[RValue[String, Typeless]] = 
        pairElem
        | "newpair" ~> parens(NewPair(expr, ("," ~> expr)))
        | "call" ~> Call(ident, parens(commaSep(expr)))
        | ArrayLit(brackets(commaSep(expr)))
        | expr
    
    /* Pair of elements parser. */
    protected [syntax] lazy val pairElem: Parsley[PairElem[String, Typeless]] = 
        ("fst" ~> First(lvalue))
        | ("snd" ~> Second(lvalue))

    /* Pair of elements type parser. */
    private lazy val pairElemType: Parsley[SemType] = 
        nestPairCheck ~> postfix(baseType | ("pair" as PairT(?, ?)))(ArrayT <# "[]")
    
    /* Pair element type parser. */
    private lazy val pairType: Parsley[KnownType] = 
        ("pair" ~> PairT(("(" ~> pairElemType), ("," ~> pairElemType <~ ")")))

    /* Pair type parser. */
    protected [syntax] lazy val ptype: Parsley[KnownType] = 
        postfix(pairType | baseType)(ArrayT <# "[]")

    /* Parses checking for pair nesting, if a nested pair is found the relevent error message is output. */
    private lazy val nestPairCheck =
        notFollowedBy(atomic(pairType)) | fail("pair nesting not allowed in WACC")
        
    /* General type parser. */
    private lazy val baseType: Parsley[KnownType] = 
        ("int" as IntT()) |
        ("bool" as BoolT()) |
        ("char" as CharT()) | 
        ("string" as StringT())
}
