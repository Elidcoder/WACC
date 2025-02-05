package wacc.syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.Success

import wacc.ast.*

val UNIT_TEST_CATS = List(
    "expr",
    "func",
    "prog",
    "stmt",
    "type",
)

class UnitTest extends AnyFlatSpec {
    
    /* Tests for atomics */
    it should "parse an integer literal successfully" in {
        parser.expr.parse("37") match {
            case Success(IntLit(x)) => x shouldBe 37
            case _                  => fail()
        }
    }

    it should "parse a boolean literal successfully" in {
        parser.expr.parse("true") match {
            case Success(BoolLit(x)) => x shouldBe true
            case _                   => fail()
        }
    }

    it should "parse a character literal successfully" in {
        parser.expr.parse("'w'") match {
            case Success(CharLit(x)) => x shouldBe 'w'
            case _                   => fail()
        }
    }

    it should "parse a string literal successfully" in {
        parser.expr.parse("\"Kevin Buzzard\"") match {
            case Success(StrLit(x)) => x shouldBe "Kevin Buzzard"
            case _                  => fail()
        }
    }

    it should "parse a pair literal successfully" in {
        parser.expr.parse("null") match {
            case Success(PairLit()) => succeed
            case _                  => fail()
        }
    }

    it should "parse an identifier successfully" in {
        parser.expr.parse("freddie") match {
            case Success(Ident(x)) => x shouldBe "freddie"
            case _                 => fail()
        }
    }

    it should "parse an array elem successfully" in {
        parser.expr.parse("freddie[0]") match {
            case Success(ArrayElem(Ident(x), y)) => (x, y).equals(("freddie", "0"))
            case _                 => fail()
        }
    }

    /* Tests for operators */
    it should "parse unary operator (!) successfully" in {
        parser.expr.parse("!true") match {
            case Success(Not(BoolLit(x))) => x shouldBe true
            case _                 => fail()
        }
    }

    it should "parse unary operator (-) successfully" in {
        parser.expr.parse("-37") match {
            case Success(IntLit(x)) => x shouldBe -37
            case _                       => fail()
        }
    }

    it should "parse unary operator (len) successfully" in {
        parser.expr.parse("len null") match {
            case Success(Len(PairLit())) => succeed
            case _                       => fail()
        }
    }

    it should "parse unary operator (ord) successfully" in {
        parser.expr.parse("ord null") match {
            case Success(Ord(PairLit())) => succeed
            case _                       => fail()
        }
    }
    
    it should "parse unary operator (chr) successfully" in {
        parser.expr.parse("chr null") match {
            case Success(Chr(PairLit())) => succeed
            case _                       => fail()
        }
    }

    it should "parse binary operator (*) successfully" in {
        parser.expr.parse("1*37") match {
            case Success(Mul(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                  => fail()
        }
    }

    it should "parse binary operator (/) successfully" in {
        parser.expr.parse("37/1") match {
            case Success(Div(IntLit(x), IntLit(y))) => (x, y).equals(37, 1)
            case _                                  => fail()
        }
    }

    it should "parse binary operator (%) successfully" in {
        parser.expr.parse("1%37") match {
            case Success(Mod(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                  => fail()
        }
    }

    it should "parse binary operator (+) successfully" in {
        parser.expr.parse("1+37") match {
            case Success(Add(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                  => fail()
        }
    }

    it should "parse binary operator (-) successfully" in {
        parser.expr.parse("1-37") match {
            case Success(Sub(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                  => fail()
        }
    }

    it should "parse binary operator (>) successfully" in {
        parser.expr.parse("1>37") match {
            case Success(Greater(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                      => fail()
        }
    }

    it should "parse binary operator (>=) successfully" in {
        parser.expr.parse("1>=37") match {
            case Success(GreaterEq(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                          => fail()
        }
    }

    it should "parse binary operator (<) successfully" in {
        parser.expr.parse("1<37") match {
            case Success(Less(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                   => fail()
        }
    }

    it should "parse binary operator (<=) successfully" in {
        parser.expr.parse("1<=37") match {
            case Success(LessEq(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                     => fail()
        }
    }

    it should "parse binary operator (==) successfully" in {
        parser.expr.parse("1==37") match {
            case Success(Eq(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                 => fail()
        }
    }

    it should "parse binary operator (!=) successfully" in {
        parser.expr.parse("1!=37") match {
            case Success(NotEq(IntLit(x), IntLit(y))) => (x, y).equals(1, 37)
            case _                                    => fail()
        }
    }

    it should "parse binary operator (&&) successfully" in {
        parser.expr.parse("true&&false") match {
            case Success(And(BoolLit(x), BoolLit(y))) => (x, y).equals(1, 37)
            case _                                    => fail()
        }
    }

    it should "parse binary operator (||) successfully" in {
        parser.expr.parse("true||false") match {
            case Success(Or(BoolLit(x), BoolLit(y))) => (x, y).equals(1, 37)
            case _                                   => fail()
        }
    }

    /* Test for types */
    it should "parse base type 'int' successfully" in {
        parser.ptype.parse("int") match {
            case Success(IntT()) => succeed
            case _               => fail()
        }
    }

    it should "parse base type 'bool' successfully" in {
        parser.ptype.parse("bool") match {
            case Success(BoolT()) => succeed
            case _                => fail()
        }
    }

    it should "parse base type 'char' successfully" in {
        parser.ptype.parse("char") match {
            case Success(CharT()) => succeed
            case _                => fail()
        }
    }

    it should "parse base type 'string' successfully" in {
        parser.ptype.parse("string") match {
            case Success(StringT()) => succeed
            case _                  => fail()
        }
    }

    it should "parse array type successfully" in {
        parser.ptype.parse("int[]") match {
            case Success(ArrayT(int)) => succeed
            case _                    => fail()
        }
    }

    it should "parse nested array types successfully" in {
        parser.ptype.parse("int[][]") match {
            case Success(ArrayT(ArrayT(int))) => succeed
            case _                            => fail()
        }
    }

    it should "parse pair type successfully" in {
        parser.ptype.parse("pair(int,string)") match {
            case Success(PairT(IntT(), StringT())) => succeed
            case _                                 => fail()
        }
    }

    it should "parse nested pair types successfully" in {
        parser.ptype.parse("pair(pair,pair)") match {
            case Success(PairT(_, _)) => succeed
            case _                    => fail()
        }
    }

    /* Test for Statements */
    it should "parse programs successfully" in {
        parser.program.parse("begin skip end") match {
            case Success(Program(_, List(Skip()))) => succeed
            case _                                 => fail()
        }
    }

    it should "parse functions successfully" in {
        parser.func.parse("int f() is return 1 end skip end") match {
            case Success(Func(IntT(), Ident(x), List(), List(Return(IntLit(1))))) => x shouldBe "f"
            case _                                                                => fail()
        }
    }
    
    it should "parse 'skip' statements successfully" in {
        parser.stmt.parse("skip") match {
            case Success(Skip()) => succeed
            case _               => fail()
        }
    }

    it should "parse new assignment statements successfully" in {
        parser.stmt.parse("int buzz = 37") match {
            case Success(NewAss(IntT(), Ident(x), IntLit(37))) => x shouldBe "buzz"
            case _                                             => fail()
        }
    }

    it should "parse non-new assignment statements successfully" in {
        parser.stmt.parse("buzz = 37") match {
            case Success(Assign(Ident(x), IntLit(37))) => x shouldBe "buzz"
            case _                                     => fail()
        }
    }

    it should "parse 'read' statements successfully" in {
        parser.stmt.parse("read serhii") match {
            case Success(Read(Ident(x))) => x shouldBe "serhii"
            case _                       => fail()
        }
    }

    it should "parse 'free' statements successfully" in {
        parser.stmt.parse("free 37") match {
            case Success(Free(IntLit(x))) => x shouldBe 37
            case _                        => fail()
        }
    }

    it should "parse 'return' statements successfully" in {
        parser.stmt.parse("return 37") match {
            case Success(Return(IntLit(x))) => x shouldBe 37
            case _                          => fail()
        }
    }

    it should "parse 'exit' statements successfully" in {
        parser.stmt.parse("exit 37") match {
            case Success(Exit(IntLit(x))) => x shouldBe 37
            case _                        => fail()
        }
    }

    it should "parse 'print' statements successfully" in {
        parser.stmt.parse("print 37") match {
            case Success(Print(IntLit(x))) => x shouldBe 37
            case _                         => fail()
        }
    }

    it should "parse 'println' statements successfully" in {
        parser.stmt.parse("println 37") match {
            case Success(PrintLn(IntLit(x))) => x shouldBe 37
            case _                           => fail()
        }
    }

    it should "parse 'if' statements successfully" in {
        parser.stmt.parse("if 37 then skip else skip fi") match {
            case Success(If(IntLit(x), List(Skip()), List(Skip()))) => x shouldBe 37
            case _                                                  => fail()
        }
    }

    it should "parse 'while' statements successfully" in {
        parser.stmt.parse("while 37 do skip done") match {
            case Success(While(IntLit(x), List(Skip()))) => x shouldBe 37
            case _                                       => fail()
        }
    }
    
    it should "parse nested statements successfully" in {
        parser.stmt.parse("begin skip end") match {
            case Success(Nest(List(Skip()))) => succeed
            case _                           => fail()
        }
    }

    it should "parse consecutive statements successfully" in {
        parser.stmt.parse("skip;skip") match {
            case Success(Skip()) => succeed
            case _               => fail()
        }
    }

    /* Tests for LValues */

}
