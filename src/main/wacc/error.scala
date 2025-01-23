package wacc.error

import parsley.errors.ErrorBuilder
type R2 = (Int, Int)

case class WaccErr(
    errorPos: R2,
    lines: ErrLines,
    fileName: Option[String]
)

enum ErrItem {
    case Raw(item: String)
    case Named(item: String)
    case EndOfInput
}

enum ErrLines {
    case VanillaError(unexpected: Option[ErrItem], expecteds: Set[ErrItem], reasons: Set[String], width: Int)
    case SpecialisedError(msgs: Set[String], width: Int)
}

abstract class WaccErrorBuilder extends ErrorBuilder[WaccErr] {
    override def build(pos: Position, source: Source, lines: ErrorInfoLines): WaccErr = WaccErr(pos, lines, source)

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

    type LineInfo = Int
    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], lineNum: Int, errorPointsAt: Int, errorWidth: Int): Int = errorWidth

    override val numLinesBefore: Int = 0
    override val numLinesAfter: Int = 0

    type Item = ErrItem
    type Raw = ErrItem.Raw
    type Named = ErrItem.Named
    type EndOfInput = ErrItem.EndOfInput.type
    
    override def raw(item: String): Raw = new Raw(item)
    override def named(item: String): Named = new Named(item)

    override val endOfInput: EndOfInput = ErrItem.EndOfInput
}
