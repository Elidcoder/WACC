package wacc

import wacc.error.WaccErr
import wacc.syntax.parser
import wacc.semantic.rename
import wacc.syntax.lexer.lexErrBuilder
import wacc.semantic.typecheck.typechecker

import parsley.{Failure, Success}
import parsley.errors.ErrorBuilder

import java.io.File

final val CODE_SUCCESS      = 0
final val CODE_SYNTAX_ERR   = 100
final val CODE_SEMATNIC_ERR = 200

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = lexErrBuilder
    println("Starting parsing...")
    parser.parse(file) match 
        case Success(syntaxTree) => 
            println("Success!\nStarting renaming...")
            /* Successfully parsed, attempt rename. */
            val (renamedTree, env) = rename(syntaxTree)

            println("Success!\nStarting typechecking...")
            /* Attempt typecheck and match on result of both rename & typecheck. */
            typechecker.check(renamedTree, env, file) match
                /* Failure in one or both of typechecker & renamer, exit with error code 200. */
                case Left(errs) => 
                    errs.foreach((err: WaccErr) => println(err.format()))
                    CODE_SEMATNIC_ERR

                /* Renamer & typechecker ran successfully. */
                case Right(value) => 
                    println("Success")
                    value.get 
                    /* Exit with error code 0 if the final tree exists. */
                    CODE_SUCCESS
                        
        /* Failed to parse, print error and exit with error code 100. */
        case Failure(errors) => 
            println(errors.format())
            CODE_SYNTAX_ERR
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
