package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.sys.process._
import java.io.File

import wacc.pipeline
import wacc.CODE_SUCCESS

private final val BACKEND_FILES = List(
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

class BackEndTest extends AnyFlatSpec with ConditionalTest {
    val flags = getProperties()
    BACKEND_FILES.foreach{ dir =>
        val key = s"tests.backend.${dir.toLowerCase}"
        val tests = getTests(s"valid/$dir")
        tests.foreach { file =>
            val name = s"should compile without error in [${dir}/${file.getName()}]"
            conditionalTest(flags, name, key) {
                val result = pipeline(file)
                result shouldBe CODE_SUCCESS
                
                val baseName = file.getName.stripSuffix(".wacc")
                val ass = new File(s"./${baseName}.s")
                ass.exists() shouldBe true

                val exit = s"gcc -z noexecstack -o ${baseName}.o ${baseName}.s" .!
                exit shouldBe 0
            }
        }
    }
}
