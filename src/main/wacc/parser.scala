package wacc

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.expr.*
import parsley.expr.chain.postfix
import parsley.errors.ErrorBuilder

import java.io.File

import lexer.implicits.implicitSymbol

import wacc.ast.*
import wacc.lexer.*
import scala.util.Success
import parsley.Failure

object parser {
    def parse(input: File): Result[String, Program] =  parser.parseFile(input) match {
        case Success(x) => x
        case _ => Failure("failed file read.")
    }
    
    private val parser = fully(program)
    
    private lazy val program: Parsley[Program] = Program("begin" ~> many(func), stmts <~ "end")

    private lazy val func: Parsley[Func] = atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))), ("is" ~> rtrnStmts <~ "end")))

    private lazy val rtrnStmts: Parsley[List[Stmt]] = (many(atomic(stmt <~ ";")) <~> rtrnStmt) map ((x: List[Stmt], y: Stmt) => x ++ List(y))
    private lazy val rtrnStmt: Parsley[Stmt] = 
        ("return" ~> Return(expr)) |
        ("exit" ~> Exit(expr)) |
        If(("if" ~> expr), ("then" ~> rtrnStmts), ("else" ~> rtrnStmts <~ "fi")) |
        While(("while" ~> expr), ("do" ~> rtrnStmts <~ "done"))

    private lazy val stmt: Parsley[Stmt] = 
        ("skip" as Skip()) |
        ("read" ~> Read(lvalue)) |
        ("free" ~> Free(expr)) |
        ("return" ~> Return(expr)) |
        ("exit" ~> Exit(expr)) |
        ("println" ~> PrintLn(expr)) |
        ("print" ~> Print(expr)) |
        If(("if" ~> expr), ("then" ~> stmts), ("else" ~> stmts <~ "fi")) |
        While(("while" ~> expr), ("do" ~> stmts <~ "done")) |
        Nest("begin" ~> stmts <~ "end") | 
        NewAss(ptype, ident, "=" ~> rvalue) |
        Assign(lvalue, "=" ~> rvalue)

    private lazy val stmts: Parsley[List[Stmt]] = semiSep1(stmt)

    private lazy val expr: Parsley[Expr] = 
        precedence(
            ("null" as PairLit()),
            BoolLit(("true" as true) | ("false" as false)),
            IntLit(integer),
            CharLit(asciiChar),
            StrLit(asciiString),
            atomic(ArrayElem(ident, some(brackets(expr)))),
            ident,
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
        )

    private lazy val lvalue: Parsley[LValue] = 
        PElem(pairElem) |
        atomic(ArrayElem(ident, some(brackets(expr)))) |
        ident

    private lazy val rvalue: Parsley[RValue] = 
        PElem(pairElem) |
        NewPair(("newpair" ~> "(" ~> expr), ("," ~> expr <~ ")")) |
        Call(("call" ~> ident), parens(commaSep(expr))) |
        ArrayLit(brackets(sepBy(expr, ","))) | 
        expr
    
    private lazy val pairElem: Parsley[PairElem] = 
        (("fst" ~> First(lvalue)) |
        ("snd" ~> Second(lvalue)))

    private lazy val typeHelper: Parsley[Type] = 
        ("pair" ~> PairT(("(" ~> pairType), ("," ~> pairType <~ ")"))) |
        baseType
    
    private lazy val ptype: Parsley[Type] = postfix(typeHelper)("[]" as (ArrayT(_)))

    private lazy val baseType: Parsley[Type] = 
        ("int" as IntT()) |
        ("bool" as BoolT()) |
        ("char" as CharT()) | 
        ("string" as StringT())
    
    private lazy val pairType: Parsley[Type] = postfix(pairTypeHelper)("[]" as (ArrayT(_)))
    
    private lazy val pairTypeHelper: Parsley[Type] = 
        baseType |
        ("pair" as RedPairT())
}
