package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import wacc.pipeline
import wacc.CODE_SYNTAX_ERR

private final val SYN_ERR_FILES = List(
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

class SyntaxTest extends AnyFlatSpec with ConditionalTest {
    val flags = getProperties()
    SYN_ERR_FILES.foreach{ dir =>
        val key = s"tests.syntax.${dir.toLowerCase}"
        val tests = getTests(s"invalid/syntaxErr/$dir")
        tests.foreach { file =>
            val name = s"should fail with syntax error (100) in [${dir}/${file.getName()}]"
            conditionalTest(flags, name, key) {
                val result = pipeline(file)
                result shouldBe CODE_SYNTAX_ERR
            }
        }
    }
}
