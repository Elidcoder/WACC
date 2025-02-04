package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.pipeline

val VALID_FILES = List(
    "basic",
    "array",
    "expressions",
    "function",
    "if",
    "IO",
    "pairs",
    "runtimeErr",
    "scope",
    "sequence",
    "variables",
    "while"
)

class ValidTest extends AnyFlatSpec {
    VALID_FILES.filter{ dir =>
        val key = s"tests.valid.$dir"
        getProperties().get(key).exists(_.toBoolean)
    }.foreach { dir =>
        val ranTests = getTests(s"valid/$dir")
        ranTests.foreach { file =>
            s"Program [${file.getName()}] from directory [$dir]" should "compile without error with code (0)" in {
                val result = pipeline(file)
                println(s"Testing program [${file.getName()}] from [$dir]: Result = $result")
                
                result should be (0)
            }
        }
    }
}