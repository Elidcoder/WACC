package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.pipeline

val SYN_ERR_FILES = List(
    "basic",
    "array",
    "expressions",
    "function",
    "if",
    "literals",
    "pairs",
    "print",
    "sequence",
    "variables",
    "while"
)

class SyntaxTest extends AnyFlatSpec {
    SYN_ERR_FILES.filter{ dir =>
        val key = s"tests.syntax.$dir"
        getProperties().get(key).exists(_.toBoolean)
    }.foreach { dir =>
        val ranTests = getTests(s"invalid/syntaxErr/$dir")
        ranTests.foreach { file =>
            s"Program [${file.getName()}] from directory [$dir]" should "fail with syntax error code (100)" in {
                val result = pipeline(file)
                println(s"Testing program [${file.getName()}] from [$dir]: Result = $result")
                
                result should be (100)
            }
        }
    }
}
