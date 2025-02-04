package wacc.syntax

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.expr.*
import parsley.expr.chain.postfix
import parsley.errors.ErrorBuilder
import parsley.errors.combinator._
import java.io.File

import lexer.implicits.implicitSymbol

import wacc.ast.*
import wacc.syntax.lexer.*
import scala.util.Success

object parser {
    def parse[Err: ErrorBuilder](input: File): Result[Err, Program[String, Unit]] =  parser.parseFile(input) match {
         case Success(x) => x
        case _          => sys.exit(-1)
    }

    def isReturnStmt(s: List[Stmt[String, Unit]]): Boolean = s.last match {
        case If(_, s1, s2) => isReturnStmt(s1) && isReturnStmt(s2)
        case While(_, s)   => isReturnStmt(s)
        case Return(_)     => true
        case Exit(_)       => true
        case _             => false
    }
    
    // TODO(Shld output ""all program body and function declarations must be within `begin` and `end`"" if fully fails)
    private val parser = fully(program)

    private lazy val end = "end".explain("a scope, function or the main body is unclosed")
    
    private lazy val program: Parsley[Program[String, Unit]] = Program("begin" ~> many(func), stmts <~ end)

    private lazy val func: Parsley[Func[String, Unit]] = atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))), ("is" ~> rtrnStmts <~ end)))

    private lazy val rtrnStmts: Parsley[List[Stmt[String, Unit]]] = decide(semiSep1(stmt) <**> (pure((s: List[Stmt[String, Unit]]) => if (isReturnStmt(s)) Some(s) else None)))

    // TODO(unexpected identifier should somehow pass the name/ character through so that it is clear) '| unexpected("identifier")'
    private lazy val asgnmt = ("=" ~> rvalue).label("assignment")

    private lazy val stmt: Parsley[Stmt[String, Unit]] = 
        ("skip" ~> Skip()) |
        ("read" ~> Read(lvalue)) |
        ("free" ~> Free(expr)) |
        ("return" ~> Return(expr)) |
        ("exit" ~> Exit(expr)) |
        ("println" ~> PrintLn(expr)) |
        ("print" ~> Print(expr)) |
        If(
            ("if" ~> expr), 
            ("then".explain("the condition of an if statement must be closed with `then`") ~> stmts), 
            ("else".explain("all if statements must have an else clause") ~> stmts <~ "fi".explain("unclosed if statement"))
        ) |
        While(("while" ~> expr), ("do" ~> stmts <~ "done")) |
        Nest("begin" ~> stmts <~ end) | 
        NewAss(ptype, ident, asgnmt ) | 
        Assign(lvalue, asgnmt )

    private lazy val stmts: Parsley[List[Stmt[String, Unit]]] = semiSep1(stmt).label("statement").explain("missing main program body")

    /* Error message taken from the WACC Reference Compiler. */
    val EXPR_ERR_MSG = "expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators";

    private lazy val arridx = option(some(brackets(expr))).label("array index")

    private lazy val expr: Parsley[Expr[String, Unit]] = 
        precedence(
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

    private lazy val lvalue: Parsley[LValue[String, Unit]] = 
        PElem(pairElem) |
        ArrayOrIdent(ident, arridx)

    private lazy val rvalue: Parsley[RValue[String, Unit]] = 
        PElem(pairElem) |
        NewPair(("newpair" ~> "(" ~> expr), ("," ~> expr <~ ")")) |
        Call(("call" ~> ident), parens(commaSep(expr))) |
        ArrayLit(brackets(sepBy(expr, ","))) | 
        expr
    
    private lazy val pairElem: Parsley[PairElem[String, Unit]] = 
        ("fst" ~> First(lvalue)) |
        ("snd" ~> Second(lvalue))

    private lazy val typeHelper: Parsley[Type] = 
        (("pair" ~> PairT(("(" ~> pairType), ("," ~> pairType <~ ")"))) |
        baseType)

    private lazy val ptype: Parsley[Type] = (postfix(typeHelper)(ArrayT <# "[]"))

    private lazy val baseType: Parsley[Type] = 
        ("int" as IntT()) |
        ("bool" as BoolT()) |
        ("char" as CharT()) | 
        ("string" as StringT())
    
    private lazy val pairType: Parsley[Type] = postfix(pairTypeHelper)(ArrayT <# "[]")
    
    private lazy val pairTypeHelper: Parsley[Type] = 
        baseType |
        ("pair" as RedPairT())
}
