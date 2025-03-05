package wacc

import wacc.error.WaccErr
import wacc.syntax.parser
import wacc.semantic.rename
import wacc.syntax.lexer.lexErrBuilder
import wacc.semantic.typecheck.typechecker
import wacc.backend.generator.generator
import wacc.backend.referencing.referencer
import wacc.backend.formatter.formatter.formatBlocks

import parsley.{Failure, Success}
import parsley.errors.ErrorBuilder

import java.io.{File, FileWriter}

final val CODE_SUCCESS      = 0
final val CODE_SYNTAX_ERR   = 100
final val CODE_SEMANTIC_ERR = 200

def pipeline(file: File): Int = {
    given ErrorBuilder[WaccErr] = lexErrBuilder
    parser.parse(file) match 
        case Success(syntaxTree) => 
            /* Successfully parsed, attempt rename. */
            val (renamedTree, env) = rename(syntaxTree)

            /* Attempt typecheck and match on result of both rename & typecheck. */
            typechecker.check(renamedTree, env, file) match
                /* Failure in one or both of typechecker & renamer, exit with error code 200. */
                case Left(errs) => 
                    errs.foreach((err: WaccErr) => println(err.format()))
                    CODE_SEMANTIC_ERR

                /* Renamer & typechecker ran successfully. */
                case Right(value) => 
                    val finalTree = value.get

                    val writer = new FileWriter(getAssFile(file))

                    formatBlocks(
                        generator.generate(finalTree)(using referencer.reference(finalTree)),
                        writer
                    )
                    /* Exit with error code 0 if compilation succeeds */
                    CODE_SUCCESS
                        
        /* Failed to parse, print error and exit with error code 100. */
        case Failure(errors) => 
            println(errors.format())
            CODE_SYNTAX_ERR
}

private def getAssFile(file: File): File = {
    val baseName = file.getName.stripSuffix(".wacc")
    new File(s"./${baseName}.s")
}

def main(args: Array[String]): Unit = {
    args.headOption match {
        /* Ensure that the filepath is given and the file created is valid. */
        case Some(filePath) => 
            val file = new File(filePath)

            /* Run the compiler on the file, exiting with the matching output code. */
            sys.exit(pipeline(file))

        /* Invalid filepath. */
        case None => println("please enter a valid filepath")
    }
}
