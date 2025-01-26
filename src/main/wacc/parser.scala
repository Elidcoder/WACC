package wacc

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.expr.*
import parsley.expr.chain.postfix
import parsley.errors.ErrorBuilder
import parsley.errors.combinator._
import java.io.File

import lexer.implicits.implicitSymbol

import wacc.ast.*
import wacc.lexer.*
import scala.util.Success

object parser {
    def parse[Err: ErrorBuilder](input: File): Result[Err, Program] =  parser.parseFile(input) match {
        case Success(x) => x
        case _ => sys.exit(-1)
    }

    def isReturnStmt(s: List[Stmt]): Boolean = s.last match {
        case Return(_)      => true
        case Exit(_)        => true
        case If(_, s1, s2)  => isReturnStmt(s1) && isReturnStmt(s2)
        case While(_, s)    => isReturnStmt(s)
        case _              => false
    }    
    
    private val parser = fully(program)
    
    private lazy val program: Parsley[Program] = Program("begin" ~> many(func), stmts <~ "end")

    private lazy val func: Parsley[Func] = atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))), ("is" ~> rtrnStmts <~ "end")))

    private lazy val rtrnStmts: Parsley[List[Stmt]] = decide(semiSep1(stmt) <**> (pure((s: List[Stmt]) => if (isReturnStmt(s)) Some(s) else None)))

    private lazy val assignmentStmt: Parsley[Stmt] =
        ( NewAss(ptype, ident, "=" ~> rvalue)
        <|> Assign(lvalue, "=" ~> rvalue)
        )

    private lazy val stmt: Parsley[Stmt] = 
        (("skip" as Skip()) |
        ("read" ~> Read(lvalue)) |
        ("free" ~> Free(expr)) |
        ("return" ~> Return(expr)) |
        ("exit" ~> Exit(expr)) |
        ("println" ~> PrintLn(expr)) |
        ("print" ~> Print(expr)) |
        If(("if" ~> expr), ("then" ~> stmts), ("else" ~> stmts <~ "fi")) |
        While(("while" ~> expr), ("do" ~> stmts <~ "done")) |
        Nest("begin" ~> stmts <~ "end") | 
        assignmentStmt)

    private lazy val stmts: Parsley[List[Stmt]] = semiSep1(stmt)

    /* Error message taken from the WACC Reference Compiler. */
    val EXPR_ERR_MSG = "expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators";

    private lazy val expr: Parsley[Expr] = 
        precedence(
            ("null" as PairLit()),
            BoolLit(("true" as true) | ("false" as false)),
            IntLit(integer),
            CharLit(asciiChar),
            StrLit(asciiString),
            ArrayOrIdent(ident, option(some(brackets(expr)))),
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

    private lazy val lvalue: Parsley[LValue] = 
        (PElem(pairElem) |
        ArrayOrIdent(ident, option(some(brackets(expr)))))

    private lazy val rvalue: Parsley[RValue] = 
        (PElem(pairElem) |
        NewPair(("newpair" ~> "(" ~> expr), ("," ~> expr <~ ")")) |
        Call(("call" ~> ident), parens(commaSep(expr))) |
        ArrayLit(brackets(sepBy(expr, ","))) | 
        expr)
    
    private lazy val pairElem: Parsley[PairElem] = 
        (("fst" ~> First(lvalue)) |
        ("snd" ~> Second(lvalue))).label("FIRSTORSEND")

    private lazy val typeHelper: Parsley[Type] = 
        (("pair" ~> PairT(("(" ~> pairType), ("," ~> pairType <~ ")"))) |
        baseType)
    
    private lazy val ptype: Parsley[Type] = (postfix(typeHelper)(ArrayT <# "[]"))

    private lazy val baseType: Parsley[Type] = 
        (("int" as IntT()) |
        ("bool" as BoolT()) |
        ("char" as CharT()) | 
        ("string" as StringT()))
    
    private lazy val pairType: Parsley[Type] = postfix(pairTypeHelper)(ArrayT <# "[]")
    
    private lazy val pairTypeHelper: Parsley[Type] = 
        (baseType |
        ("pair" as RedPairT()))
}
