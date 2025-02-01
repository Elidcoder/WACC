package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.syntax.parser
import parsley.Success
import wacc.pipeline

val VALID_FILES = List("basic", "array", "expressions", "function", "if", "IO", "pairs", "runtimeErr", "scope", "sequence", "variables", "while" )
val SYN_ERR_FILES = List("array", "expressions", "if", "pairs", "sequence", "while", "basic", "function", "literals", "print", "variables")
class syntax_test extends AnyFlatSpec {
    VALID_FILES.foreach { path => 
        path should "be able to parse all test cases" in {
        val ranTests = get_tests(s"valid/$path").map(program => program -> pipeline(program))
        
        val failedTests = ranTests.collect {
            case (program, 100) => program
            case (program, 200) => program
        }
        
        failedTests shouldBe empty 
        }
    }
    SYN_ERR_FILES.foreach { path =>
        path should "fail with syntax error on all test cases" in {
        val ranTests = get_tests(s"invalid/syntaxErr/$path").map(program => program -> pipeline(program))
        
        val successfulTests = ranTests.collect {
            case (program, 0) => program
        }
        
        successfulTests shouldBe empty 
        }
    }
}