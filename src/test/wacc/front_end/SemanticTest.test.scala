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
    SEM_ERR_FILES.foreach{ dir =>
        val key = s"tests.semantic.${dir.toLowerCase}"
        val ranTests = getTests(s"invalid/semanticErr/$dir")
        ranTests.foreach { file =>
            it should s"fail with semantic error (200) in [${dir}/${file.getName()}]" in {
                assume(getProperties().get(key).exists(_.toBoolean))
                val result = pipeline(file)
                result shouldBe (200)
            }
        }
    }
}