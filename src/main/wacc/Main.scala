package wacc

import wacc.syntax.parser

import parsley.{Success, Failure}
import java.io.File
import wacc.error.*
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.SingleChar

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = new WaccErrorBuilder with SingleChar
    parser.parse(file) match {
        case Success(x) =>
            return 0
        case Failure(x) => 
            return 100
    }
}

def main(args: Array[String]): Unit = {
    args.headOption match {
        case Some(expr) => 
            val file = new File(expr)
            assert(file.exists())
            parser.parse(file) match {
                case Success(x) => sys.exit(0)
                case Failure(msg) => 
                    print("Syntax error ")
                    println(msg)
                    sys.exit(100)
        }
        case None => println("please enter an expression")
    }
}
