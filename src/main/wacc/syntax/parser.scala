package wacc.syntax

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.expr.*
import parsley.expr.chain.postfix
import parsley.errors.ErrorBuilder

import java.io.File

import lexer.implicits.implicitSymbol

import wacc.ast.*
import wacc.syntax.lexer.*
import scala.util.Success

object parser {
    def parse[Err: ErrorBuilder](input: File): Result[Err, Program[String, Unit]] =  parser.parseFile(input) match {
        case Success(x) => x
        case _ => sys.exit(-1)
    }

    def isReturnStmt(s: List[Stmt[String, Unit]]): Boolean = s.last match {
        case Return(_)      => true
        case Exit(_)        => true
        case If(_, s1, s2)  => isReturnStmt(s1) && isReturnStmt(s2)
        case While(_, s)    => isReturnStmt(s)
        case _              => false
    }    
    
    private val parser = fully(program)
    
    private lazy val program: Parsley[Program[String, Unit]] = Program("begin" ~> many(func), stmts <~ "end")

    private lazy val func: Parsley[Func[String, Unit]] = atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))), ("is" ~> rtrnStmts <~ "end")))

    private lazy val rtrnStmts: Parsley[List[Stmt[String, Unit]]] = decide(semiSep1(stmt) <**> (pure((s: List[Stmt[String, Unit]]) => if (isReturnStmt(s)) Some(s) else None)))

    private lazy val stmt: Parsley[Stmt[String, Unit]] = 
        ("skip" as Skip[String, Unit]()) |
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

    private lazy val stmts: Parsley[List[Stmt[String, Unit]]] = semiSep1(stmt)

    private lazy val expr: Parsley[Expr[String, Unit]] = 
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
        )

    private lazy val lvalue: Parsley[LValue[String, Unit]] = 
        PElem(pairElem) |
        ArrayOrIdent(ident, option(some(brackets(expr))))

    private lazy val rvalue: Parsley[RValue[String, Unit]] = 
        PElem(pairElem) |
        NewPair(("newpair" ~> "(" ~> expr), ("," ~> expr <~ ")")) |
        Call(("call" ~> ident), parens(commaSep(expr))) |
        ArrayLit(brackets(sepBy(expr, ","))) | 
        expr
    
    private lazy val pairElem: Parsley[PairElem[String, Unit]] = 
        (("fst" ~> First(lvalue)) |
        ("snd" ~> Second(lvalue)))

    private lazy val typeHelper: Parsley[Type[String, Unit]] = 
        ("pair" ~> PairT(("(" ~> pairType), ("," ~> pairType <~ ")"))) |
        baseType
    
    private lazy val ptype: Parsley[Type[String, Unit]] = postfix(typeHelper)(ArrayT <# "[]")

    private lazy val baseType: Parsley[Type[String, Unit]] = 
        ("int" as IntT[String, Unit]()) |
        ("bool" as BoolT[String, Unit]()) |
        ("char" as CharT[String, Unit]()) | 
        ("string" as StringT[String, Unit]())
    
    private lazy val pairType: Parsley[Type[String, Unit]] = postfix(pairTypeHelper)(ArrayT <# "[]")
    
    private lazy val pairTypeHelper: Parsley[Type[String, Unit]] = 
        baseType |
        ("pair" as RedPairT())
}
