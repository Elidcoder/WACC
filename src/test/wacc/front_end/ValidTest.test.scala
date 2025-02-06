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

class ValidTest extends AnyFlatSpec with ConditionalTest {
    val flags = getProperties()
    VALID_FILES.foreach{ dir =>
        val key = s"tests.valid.${dir.toLowerCase}"
        val tests = getTests(s"valid/$dir")
        tests.foreach { file =>
            val name = s"should compile without error in [${dir}/${file.getName()}]"
            conditionalTest(flags, name, key) {
                val result = pipeline(file)
                result shouldBe 0
            }
        }
    }
}
