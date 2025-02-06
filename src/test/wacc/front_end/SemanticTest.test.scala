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

class SemanticTest extends AnyFlatSpec with ConditionalTest {
    val flags = getProperties()
    SEM_ERR_FILES.foreach{ dir =>
        val key = s"tests.semantic.${dir.toLowerCase}"
        val tests = getTests(s"invalid/semanticErr/$dir")
        tests.foreach { file =>
            val name = s"should fail with semantic error (200) in [${dir}/${file.getName()}]"
            conditionalTest(flags, name, key) {
                val result = pipeline(file)
                result shouldBe 200
            }
        }
    }
}