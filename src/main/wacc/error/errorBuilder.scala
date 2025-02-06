package wacc.error

import wacc.ast.Pos

import parsley.errors.ErrorBuilder

/* A constant representing the number of lines of code either side of the erroring line shown. */
private final val CODE_RADIUS_SIZE = 1

/* Representation of an err on a line can be either:
 * VanillaError -> Contains info about what went wrong, where, why and the code in the area, or,
 * SpecialisedError -> Contains only a set of messages & info on the area of code with the error */
enum ErrLines {
    case VanillaError(unexpected: Option[ErrItem], expecteds: Set[ErrItem], reasons: Set[String], lineInfo: LineInformation)
    case SpecialisedError(msgs: Set[String], lineInfo: LineInformation)
}

/* A builder of WaccErrs that should be used to produce them for syntax errors only. */
abstract class SyntaxErrBuilder extends ErrorBuilder[WaccErr] {
    override def build(pos: Position, source: Source, lines: ErrorInfoLines): WaccErr = WaccErr(pos, lines, source, "Syntax")

    type Position = R2
    override def pos(line: Int, col: Int): Position = Pos(line, col)

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
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], lineNum: Int, errorPointsAt: Int, errorWidth: Int): LineInformation = 
        new LineInformation(line, linesBefore, linesAfter, errorPointsAt, errorWidth)

    override val numLinesBefore: Int = CODE_RADIUS_SIZE
    override val numLinesAfter: Int = CODE_RADIUS_SIZE

    type Item = ErrItem
    type Raw = ErrItem.Raw
    type Named = ErrItem.Named
    type EndOfInput = ErrItem.EndOfInput.type
    
    override def raw(item: String): Raw = new Raw(item)
    override def named(item: String): Named = new Named(item)

    override val endOfInput: EndOfInput = ErrItem.EndOfInput
}
