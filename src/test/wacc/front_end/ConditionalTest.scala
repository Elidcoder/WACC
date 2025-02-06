package wacc.test

import org.scalatest.flatspec.AnyFlatSpec

trait ConditionalTest extends AnyFlatSpec {
    def conditionalTest(flags: Map[String, String], name: String, key: String)(testBlock: => Any): Unit = {
        if (flags.get(key).exists(_.toBoolean))
            registerTest(name)(testBlock)
        else
            registerIgnoredTest(name)(testBlock)
    }
}
