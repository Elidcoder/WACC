package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.parser

val TEST_FILES = List("basic", "array", "expressions", "function", "if", "IO", "pairs", "runtimeErr", "scope", "sequence", "variables", "while" )
class syntax_test extends AnyFlatSpec {
    TEST_FILES.foreach { path => 
        path should "be able to parse all test cases" in {
        val ranTests = get_tests(s"valid/$path").map(program => program -> parser.parse(program))
        
        val failedTests = ranTests.collect {
            case (program, parsley.Failure(err)) => err
        }
        
        failedTests shouldBe empty 
        }
    }
}