package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.sys.process._
import java.io.File

import wacc.pipeline
import wacc.CODE_SUCCESS
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

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

class BackEndIntegrationTest extends AnyFlatSpec with ConditionalTest {
    val flags = getProperties()
    BACKEND_FILES.foreach{ dir =>
        val key = s"tests.backend.${dir.toLowerCase}"
        val tests = getTests(s"valid/$dir")
        tests.foreach { file =>
            val name = s"compile and run [${dir}/${file.getName()}] with correct output"
            conditionalTest(flags, name, key) {
                val result = pipeline(file)
                result shouldBe CODE_SUCCESS
                
                val baseName = file.getName.stripSuffix(".wacc")
                val ass = new File(s"./${baseName}.s")
                ass.exists() shouldBe true
                
                val exec = s"./${baseName}.o"
                val cmpExit = s"gcc -z noexecstack -o ${baseName}.o ${baseName}.s" .!
                cmpExit shouldBe 0

                val (inputOpt, expectedOutputOpt, expectedExitOpt) = parseExample(file)

                val outputBuilder = new StringBuilder
                val processLogger = ProcessLogger(line => outputBuilder.append(line + "\n"))

                val actualExit = (for {
                    inputStr    <- inputOpt
                    inputStream = new ByteArrayInputStream(inputStr.getBytes(StandardCharsets.UTF_8))
                } yield Process(exec) #< inputStream ! processLogger)
                .getOrElse(Process(exec) ! processLogger)
                
                actualExit.shouldBe(expectedExitOpt.getOrElse(0))
                if (expectedOutputOpt.getOrElse("").contains("#runtime_error#")) {
                    outputBuilder.toString should include("fatal error:")
                } else {
                    for (expectedOutput <- expectedOutputOpt) yield { 
                        outputBuilder.toString.trim.replaceAll("0x[0-9a-fA-F]+", "#addrs#") shouldBe expectedOutput.trim.replaceAll("0x[0-9a-fA-F]+", "#addrs#")
                    }
                }
            }
        }
    }
}
