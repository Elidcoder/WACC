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
        case _          => 
            println("Error: File not found")
            sys.exit(-1)
    }

    def isReturnStmt(s: List[Stmt[String, Unit]]): Boolean = s.last match {
        case If(_, s1, s2) => isReturnStmt(s1) && isReturnStmt(s2)
        case While(_, s)   => isReturnStmt(s)
        case Nest(s)       => isReturnStmt(s)
        case Return(_)     => true
        case Exit(_)       => true
        case _             => false
    }
    
    // TODO(Shld output ""all program body and function declarations must be within `begin` and `end`"" if fully fails)
    private val parser: Parsley[Program[String, Unit]] = 
        fully(program)

    private lazy val end = 
        "end".explain("a scope, function or the main body is unclosed")
    
    // Program parser
    private lazy val program: Parsley[Program[String, Unit]] = 
        "begin" ~> Program(many(func), stmts.explain("missing main program body")) <~ end

    // Function parser
    private lazy val func: Parsley[Func[String, Unit]] = 
        atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))), ("is" ~> funcStmts <~ end)))

    // Statements parser for function body
    private lazy val funcStmts: Parsley[List[Stmt[String, Unit]]] = 
        semiSep1(stmt).filter(isReturnStmt)

    // TODO(unexpected identifier should somehow pass the name/ character through so that it is clear) '| unexpected("identifier")'
    private lazy val asgnmt = 
        ("=" ~> rvalue).label("assignment")

    // Statement parser
    private lazy val stmt: Parsley[Stmt[String, Unit]] = 
        ("skip" ~> Skip())
        | ("read" ~> Read(lvalue))
        | ("free" ~> Free(expr))
        | ("return" ~> Return(expr))
        | ("exit" ~> Exit(expr))
        | ("println" ~> PrintLn(expr))
        | ("print" ~> Print(expr))
        | "if" ~> 
            If(
                (expr), 
                ("then".explain("the condition of an if statement must be closed with `then`") ~> stmts), 
                ("else".explain("all if statements must have an else clause") ~> stmts)
            ) <~ "fi".explain("unclosed if statement")
        | "while" ~> While(expr, ("do" ~> stmts)) <~ "done"
        | "begin" ~> Nest(stmts) <~ end
        | NewAss(ptype, ident, asgnmt)
        | Assign(lvalue, asgnmt)

    // Statements parser
    private lazy val stmts: Parsley[List[Stmt[String, Unit]]] = 
        semiSep1(stmt).label("statement")

    /* Error message taken from the WACC Reference Compiler. */
    val EXPR_ERR_MSG = "expressions may start with integer, string, character or boolean literals; identifiers; unary operators; null; or parentheses in addition, expressions may contain array indexing operations; and comparison, logical, and arithmetic operators";

    // optional array index parser
    private lazy val arridx = 
        option(some(brackets(expr))).label("array index")

    // Expression parser
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

    // lvalue parser
    private lazy val lvalue: Parsley[LValue[String, Unit]] = 
        PElem(pairElem)
        | ArrayOrIdent(ident, arridx)

    // rvalue parser
    private lazy val rvalue: Parsley[RValue[String, Unit]] = 
        PElem(pairElem)
        | "newpair" ~> parens(NewPair(expr, ("," ~> expr)))
        | "call" ~> Call(ident, parens(commaSep(expr)))
        | ArrayLit(brackets(commaSep(expr)))
        | expr
    
    // pairElem parser
    private lazy val pairElem: Parsley[PairElem[String, Unit]] = 
        ("fst" ~> First(lvalue))
        | ("snd" ~> Second(lvalue))
    
    // PairType parser
    private lazy val pairType: Parsley[Type] = 
        ("pair" ~> PairT(("(" ~> pairElemType), ("," ~> pairElemType <~ ")")))

    // Type parser
    private lazy val ptype: Parsley[Type] = 
        postfix(pairType | baseType)(ArrayT <# "[]")
    
    // PairElemType parser
    private lazy val pairElemType: Parsley[Type] = 
        postfix(baseType | reducedPairType)(ArrayT <# "[]")

    // Reduced Pair Type parser
    private lazy val reducedPairType: Parsley[Type] = 
        "pair" as RedPairT()

    // Base Type parser
    private lazy val baseType: Parsley[Type] = 
        ("int" as IntT()) |
        ("bool" as BoolT()) |
        ("char" as CharT()) | 
        ("string" as StringT())
}
