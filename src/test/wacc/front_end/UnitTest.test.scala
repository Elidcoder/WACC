package wacc.syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure, Result}

import wacc.ast.*
import org.scalactic.Bool

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

    /* Test for LValues */

    /* Test for types */

}
