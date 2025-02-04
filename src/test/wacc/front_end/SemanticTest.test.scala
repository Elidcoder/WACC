package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.pipeline

val SEM_ERR_FILES = List(
    "array",
    "exit",
    "expressions",
    "function",
    "if",
    "IO",
    "multiple",
    "pairs",
    "print",
    "read",
    "scope",
    "variables",
    "while"
)

class SemanticTest extends AnyFlatSpec {
    SEM_ERR_FILES.filter{ dir =>
        val key = s"tests.semantic.$dir"
        getProperties().get(key).exists(_.toBoolean)
    }.foreach { dir =>
        val ranTests = getTests(s"invalid/semanticErr/$dir")
        ranTests.foreach { file =>
            s"Program [${file.getName()}] from directory [$dir]" should "fail with semantic error code (200)" in {
                val result = pipeline(file)
                println(s"Testing program [${file.getName()}] from [$dir]: Result = $result")
                
                result should be (200)
            }
        }
    }
}