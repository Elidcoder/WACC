package wacc.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

trait ConditionalTest extends AnyFlatSpec {
    def conditionalTest(
        flags: Map[String, String], 
        name: String, key: String
    )(testBlock: => Any): Unit = {
        it should name in {
            if (flags.get(key).exists(_.toBoolean))
                failAfter(20.seconds){ testBlock}
            else
                pending
        }
    }
}
