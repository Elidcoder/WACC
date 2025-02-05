package wacc

import wacc.syntax.parser
import wacc.semantic.rename
// import wacc.semantic.typecheck.check

import parsley.{Success, Failure}
import java.io.File
import wacc.error.*
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.SingleChar

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = new WaccErrorBuilder with SingleChar
    parser.parse(file) match 
        case Success(x) => 
            val (renamedTree, env) = rename(x)
            /* temporarily disabled typechecking, delete temp match when finished */
            // check(renamedTree, env) match
            Right(Some(0)): Either[List[WaccErr], Option[Int]] match
                case Left(errs) => 
                    errs.foreach((err: WaccErr) => println(err.format()))
                    200
                case Right(value) => value match
                    case Some(finalTree) => 
                        0
                    case None => 
                        println("Tree should not be None")
                        200
        case Failure(x) => 
            println(x.format())
            100
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
