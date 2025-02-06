package wacc

import wacc.error.*
import wacc.syntax.parser
import wacc.semantic.rename
import wacc.semantic.typecheck.typechecker

import java.io.File

import parsley.{Success, Failure}
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.SingleChar

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = new SyntaxErrBuilder with SingleChar
    parser.parse(file) match 
        case Success(x) => 
            /* Successfully parsed, attempt rename. */
            val (renamedTree, env) = rename(x)

            /* Attempt rename and match on result of both rename & typecheck. */
            typechecker.check(renamedTree, env, file) match
                /* Failure in one or both of typechecker & renamer, exit with error code 200. */
                case Left(errs) => 
                    errs.foreach((err: WaccErr) => println(err.format()))
                    200

                /* Renamer & typechecker ran successfully. */
                case Right(value) => 
                    value.get 
                    /* Exit with error code 0 if the final tree exists. */
                    0
                        
        /* Failed to parse, print error and exit with error code 100. */
        case Failure(x) => 
            // println(x.format())
            100
}

def main(args: Array[String]): Unit = {
    args.headOption match {
        /* Ensure that the filepath is given and the file created is valid. */
        case Some(filePath) => 
            val file = new File(filePath)
            assert(file.exists())

            /* Run the compiler on the file, exiting with the matching output code. */
            sys.exit(pipeline(file))

        /* Invalid filepath. */
        case None => println("please enter a valid filepath")
    }
}
