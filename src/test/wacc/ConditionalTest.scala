package wacc.test

import org.scalatest.flatspec.AnyFlatSpec

trait ConditionalTest extends AnyFlatSpec {
    def conditionalTest(
        flags: Map[String, String], 
        name: String, key: String
    )(testBlock: => Any): Unit = {
        it should name in {
            if (flags.get(key).exists(_.toBoolean))
                testBlock
            else
                pending
        }
    }
}
