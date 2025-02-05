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
    SYN_ERR_FILES.foreach{ dir =>
        val key = s"tests.syntax.${dir.toLowerCase}"
        val ranTests = getTests(s"invalid/syntaxErr/$dir")
        ranTests.foreach { file =>
            it should s"fail with syntax error (100) in [${dir}/${file.getName()}]" in {
                assume(getProperties().get(key).exists(_.toBoolean))
                val result = pipeline(file)
                result shouldBe (100)
            }
        }
    }
}
