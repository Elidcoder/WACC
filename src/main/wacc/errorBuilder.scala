package wacc.error

import parsley.errors.ErrorBuilder

enum ErrItem {
    case Raw(item: String)
    case Named(item: String)
    case EndOfInput
}

enum ErrLines {
    case VanillaError(unexpected: Option[ErrItem], expecteds: Set[ErrItem], reasons: Set[String], lineInfo: LineInformation)
    case SpecialisedError(msgs: Set[String], lineInfo: LineInformation)
}

/* A builder of WaccErrs that should be used to produce them for syntax errors only. */
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
