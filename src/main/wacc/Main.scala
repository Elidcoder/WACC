package wacc

import parsley.{Success, Failure}
import java.io.File

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
