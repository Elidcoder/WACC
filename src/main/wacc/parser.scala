package wacc

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.expr.*
import parsley.expr.chain.*

import lexer.implicits.implicitSymbol

import wacc.ast.*
import wacc.ast.Stmt.*
import wacc.ast.Expr.*
import wacc.lexer.*
import wacc.ast.RValue.*
import wacc.ast.PairElem.*
import wacc.ast.PairType.*
import wacc.ast.Type.*

object parser {
    def parse(input: String): Result[String, Program] = parser.parse(input)
    private val parser = fully(program)
    
    private lazy val program: Parsley[Program] = Program("begin" ~> many(func), stmts <~ "end")

    private lazy val func: Parsley[Func] = atomic(Func(ptype, ident, parens(commaSep(Param(ptype, ident))), ("is" ~> stmts <~ "end")))

    private lazy val stmt: Parsley[Stmt] = 
        ("skip" as Skip) |
        ("read" ~> Read(lvalue)) |
        ("free" ~> Free(expr)) |
        ("return" ~> Return(expr)) |
        ("exit" ~> Exit(expr)) |
        ("println" ~> PrintLn(expr)) |
        ("print" ~> Print(expr)) |
        (If(("if" ~> expr), ("then" ~> stmts), ("else" ~> stmts <~ "fi"))) |
        (While(("while" ~> expr), ("do" ~> stmts <~ "done"))) |
        (Nest("begin" ~> stmts <~ "end")) | 
        NewAss(ptype, ident, "=" ~> rvalue) |
        Assign(lvalue, "=" ~> rvalue)

    private lazy val stmts: Parsley[List[Stmt]] = semiSep1(stmt)

    private lazy val expr: Parsley[Expr] = 
        precedence(
            ("pair" as PairLit),
            BoolLit(("true" as true) | ("false" as false)),
            IntLit(integer),
            CharLit(asciiChar),
            StrLit(asciiString),
            atomic(ArrayElem(ident, some(brackets(expr)))),
            Ident(ident),
            "(" ~> expr <~ ")")(
            Ops(Prefix)(
                (Not <# "!"),
                (Neg <# "-"),
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
        LValue.PElem(pairElem) |
        atomic(LValue.ArrayElem(ident, some(brackets(expr)))) |
        LValue.Ident(ident)

    private lazy val rvalue: Parsley[RValue] = 
        PElem(pairElem) |
        NewPair(("newpair" ~> "(" ~> expr), ("," ~> expr <~ ")")) |
        Call(("call" ~> ident), parens(commaSep(expr))) |
        RValue.ArrayLit(brackets(sepBy(expr, ","))) | 
        RExpr(expr)
    
    private lazy val pairElem: Parsley[PairElem] = 
        (("fst" ~> First(lvalue)) |
        ("snd" ~> Second(lvalue)))

    private lazy val typeHelper: Parsley[Type] = 
        ("pair" ~> PairT(("(" ~> pairType), ("," ~> pairType <~ ")"))) |
        Base(baseType)
    
    private lazy val ptype: Parsley[Type] = postfix(typeHelper)("[]".as(ArrayT(_)))

    private lazy val baseType: Parsley[BaseType] = 
        ("int" as BaseType.Int) |
        ("bool" as BaseType.Bool) |
        ("char" as BaseType.Char) | 
        ("string" as BaseType.String)
    
    private lazy val pairType: Parsley[PairType] =
        atomic(PairType.PArrayT(postfix1(typeHelper)("[]".as(ArrayT(_))))) |
        PBase(baseType) |
        ("pair" as PPairT)
}
