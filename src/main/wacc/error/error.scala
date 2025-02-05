package wacc.error

/* R2 = (line: Int, column: Int) */
type R2 = (Int, Int)

/* LineInformation = (line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int) */
type LineInformation = (String, Seq[String], Seq[String], Int, Int)

/* Takes in line information and a string builder
 * Parses the line information nicely into the stringBuilder */
def parseLineInfo(lineInfo: LineInformation, strBuilder: StringBuilder) = {
    /* Display the line before the line with the error. */
    if (!lineInfo._2.isEmpty) {
        strBuilder ++= lineInfo._2(0)
        strBuilder ++= "\n>"
    }

    /* Display the line with the error. */
    strBuilder ++= lineInfo._1
    strBuilder ++= "\n>"

    /* Pointer(s) to the erroring character(s). */
    strBuilder ++= " " * lineInfo._4 
    strBuilder ++= "^" * lineInfo._5
    strBuilder ++= "\n>"

    /* Display the line after the line with the error. */
    if (!lineInfo._3.isEmpty) {
        strBuilder ++= lineInfo._3(0)
        strBuilder ++= "\n"
    }
}

/* Representation of an error within the wacc compilation pipeline. */
case class WaccErr(
    errorPos: R2,
    errStyle: ErrLines,
    fileName: Option[String],
    errType: String
    ) {

    def format():String = fileName match {
        case None => "Bad filename, no error message could be built"
        case Some(fName) => 
            /* Build title of the error message. */
            val outputBuilder: StringBuilder = new StringBuilder()
            outputBuilder ++= errType
            outputBuilder ++= " error in file '"
            outputBuilder ++= fName
            outputBuilder ++= "' (line "
            outputBuilder ++= errorPos._1.toString()
            outputBuilder ++= ", column "
            outputBuilder ++= errorPos._2.toString()
            outputBuilder ++= "):\n"
            
            /* Build body of the error message. */
            errStyle match {
                case ErrLines.SpecialisedError(msgs, errStyle) =>
                    /* Join the given messages. */
                    msgs.mkString(",\n") 
                    
                    /* Display code near the error. */
                    parseLineInfo(errStyle, outputBuilder)

                case ErrLines.VanillaError(unexpected, expecteds, reasons, errStyle) => 
                    /* Unexpected ... line of the error message. */
                    unexpected match {
                        case None => 
                            outputBuilder ++= "  No 'unexpected item' found"
                        case Some(unexpectd) => 
                            outputBuilder ++= "  unexpected "
                            unexpectd match {
                                case ErrItem.EndOfInput => 
                                    outputBuilder ++= "end of input"
                                case ErrItem.Named(item) =>
                                    outputBuilder ++= item
                                case ErrItem.Raw(item) =>
                                    outputBuilder ++= "identifier \""
                                    outputBuilder ++= item
                                    outputBuilder ++= "\""
                                
                            }
                    }

                    /* Expected ... line of the error message. */
                    outputBuilder ++= "\n  expected "
                    outputBuilder ++= expecteds.map(_ match {
                            case ErrItem.Named(item) => item
                            case ErrItem.EndOfInput => "end of input"
                            case ErrItem.Raw(item) => item
                        }).mkString(", or ")
                    
                    /* Explanations for the error. */
                    if (!reasons.isEmpty) {
                        outputBuilder ++= "\n  "
                        outputBuilder ++= reasons.mkString("\n  ")
                    }
                    
                    /* Display code near the error. */
                    outputBuilder ++= "\n\n>"
                    parseLineInfo(errStyle, outputBuilder)
            }
            outputBuilder.result()
    }
}
