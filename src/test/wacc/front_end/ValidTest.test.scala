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
    VALID_FILES.foreach{ dir =>
        val key = s"tests.valid.${dir.toLowerCase}"
        val ranTests = getTests(s"valid/$dir")
        ranTests.foreach { file =>
            it should s"compile without error in [${dir}/${file.getName()}]" in {
                assume(getProperties().get(key).exists(_.toBoolean))
                val result = pipeline(file)
                result shouldBe (0)
            }
        }
    }
}