package wacc.error

import parsley.errors.ErrorBuilder

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
    strBuilder ++= "\n"

    /* Pointer(s) to the erroring character(s). */
    strBuilder ++= " " * lineInfo._4
    strBuilder ++= "^" * lineInfo._5
    strBuilder ++= "\n>"

    /* Display the line after the line with the error. */
    if (!lineInfo._3.isEmpty) {
        strBuilder ++= lineInfo._3(0)
        strBuilder ++= "\n>"
    }
}

case class WaccErr(
    errorPos: R2,
    errStyle: ErrLines,
    fileName: Option[String],
    errType: String){

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
                    /* Display code near the error. */
                    parseLineInfo(errStyle, outputBuilder)
                case ErrLines.VanillaError(unexpected, expecteds, reasons, errStyle) => 
                    /* Unexpected ... line of the error message. */
                    unexpected match {
                        case None => 
                            outputBuilder ++= "  No unexpected item found\n"
                        case Some(unexpectd) => 
                            outputBuilder ++= "  unexpected "
                            unexpectd match {
                                case ErrItem.EndOfInput => 
                                    outputBuilder ++= "end of input"
                                case ErrItem.Named(item) =>
                                    outputBuilder ++= "keyword "
                                    outputBuilder ++= item
                                case ErrItem.Raw(item) =>
                                    outputBuilder ++= "identifier \""
                                    outputBuilder ++= item
                                    outputBuilder ++= "\""
                                outputBuilder ++= "\n"
                            }
                    }

                    /* Exoected ... line of the error message. */
                    outputBuilder ++= "  expected "
                    outputBuilder ++= expecteds.map(i => 
                        i match {
                            case ErrItem.Named(item) => item
                            case ErrItem.EndOfInput => "end of input"
                            case ErrItem.Raw(item) => item
                        }).mkString(", or ")
                    
                    
                    /* Explanations for the error. */
                    if (!reasons.isEmpty) {
                        outputBuilder ++= "\n  "
                        outputBuilder ++= reasons.mkString("\n  ")
                    }

                    outputBuilder ++= "\n\n>"
                    
                    /* Display code near the error. */
                    parseLineInfo(errStyle, outputBuilder)
            }
            outputBuilder.result()
    }
}

enum ErrItem {
    case Raw(item: String)
    case Named(item: String)
    case EndOfInput
}

enum ErrLines {
    case VanillaError(unexpected: Option[ErrItem], expecteds: Set[ErrItem], reasons: Set[String], lineInfo: LineInformation)
    case SpecialisedError(msgs: Set[String], lineInfo: LineInformation)
}

abstract class WaccErrorBuilder extends ErrorBuilder[WaccErr] {
    override def build(pos: Position, source: Source, lines: ErrorInfoLines): WaccErr = WaccErr(pos, lines, source, "Syntax")

    type Position = R2
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Option[String]
    override def source(sourceName: Option[String]): Option[String] = sourceName

    type ErrorInfoLines = ErrLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
        ErrLines.VanillaError(unexpected, expected, reasons, line)
    }

    override def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines = ErrLines.SpecialisedError(msgs, line)

    type ExpectedItems = Set[Item]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    type Messages = Set[Message]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    type UnexpectedLine = Option[Item]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
    type ExpectedLine = ExpectedItems
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = LineInformation
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], lineNum: Int, errorPointsAt: Int, errorWidth: Int): LineInformation = new LineInformation(line, linesBefore, linesAfter, errorPointsAt, errorWidth)

    override val numLinesBefore: Int = 1
    override val numLinesAfter: Int = 1

    type Item = ErrItem
    type Raw = ErrItem.Raw
    type Named = ErrItem.Named
    type EndOfInput = ErrItem.EndOfInput.type
    
    override def raw(item: String): Raw = new Raw(item)
    override def named(item: String): Named = new Named(item)

    override val endOfInput: EndOfInput = ErrItem.EndOfInput
}
