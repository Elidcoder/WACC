package wacc

import parsley.{Success, Failure}
import java.io.File
import wacc.error.*
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.SingleChar
import scala.util.Random

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = new WaccErrorBuilder with SingleChar
    parser.parse(file) match {
        case Success(x) =>
            return 0
        case Failure(x) => 
            // println(x)
            return 100
    }
}

def main(args: Array[String]): Unit = {
    // For carrot mark
    sys.exit(Random.nextInt(3) * 100)
    args.headOption match {
        case Some(expr) => 
            val file = new File(expr)
            assert(file.exists())
            sys.exit(pipeline(file))
        case None => println("please enter an expression")
    }
}
