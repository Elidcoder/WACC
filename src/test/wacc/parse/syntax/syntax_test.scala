package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.parser

val TEST_FILES = List("basic", "array", "expressions", "function", "if", "IO", "pairs", "runtimeErr", "scope", "sequence", "variables", "while" )
class syntax_test extends AnyFlatSpec {
    TEST_FILES.foreach { path => 
        path should "be able to parse " in {
            val ran_tests = get_tests("valid/" + path).map(parser.parse(_))
            ran_tests.forall { 
                case parsley.Success(_) => true
                case _        => false
            } shouldBe true
        }
    }
    
}