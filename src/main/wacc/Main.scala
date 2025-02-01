package wacc

import parsley.{Success, Failure}
import java.io.File
import wacc.error.*
import wacc.syntax.parser
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.SingleChar

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = new WaccErrorBuilder with SingleChar
    parser.parse(file) match {
        case Success(x) => 0
        case Failure(x) => 100
    }
}

def main(args: Array[String]): Unit = {
    args.headOption match {
        case Some(expr) => 
            val file = new File(expr)
            assert(file.exists())
            sys.exit(pipeline(file))
        case None => println("please enter an expression")
    }
}
