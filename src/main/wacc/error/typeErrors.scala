package wacc.error

import wacc.semantic.typecheck.Context
import wacc.ast.{Type, Pos}
import java.io.File

/* The possible type errors that can occur during type checking. */
object TypeErr {
    case object TypeMismatch {
        def apply(readType: Type, expectedType: Type)(using ctx: Context, pos: Pos) = 
            val badToken = readType.toString()
            WaccErr(
                pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Named(expectedType.toString)),
                    Set("Types must match"),
                    getLineInfo(ctx.file, pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotString {
        def apply(readType: Type)(using ctx: Context, pos: Pos) =
                val badToken = readType.toString()
                WaccErr(
                pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                    Set("Must be of String like type"),
                    getLineInfo(ctx.file, pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotFreeable {
        def apply(givenType: Type)(using ctx: Context, pos: Pos) = 
            val badToken = givenType.toString()
            WaccErr(
                pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Raw("Pair(_,_)"), ErrItem.Raw("Char[]")),
                    Set("Tried to free an unfreeable type"),
                    getLineInfo(ctx.file, pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotReadable {
        def apply(givenType: Type)(using ctx: Context, pos: Pos) = 
            val badToken = givenType.toString()
            WaccErr(
                pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                    Set("Must be a readable type"),
                    getLineInfo(ctx.file, pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotComparable {
        def apply(actualType: Type)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(actualType.toString)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a comparable type"),
                getLineInfo(ctx.file, pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

    case object ReturnInMainBody {
        def apply(pos: R2)(using ctx: Context) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named("return")),
                Set(),
                Set("Return in main body is not allowed"),
                getLineInfo(ctx.file, pos, 6)
            ),
            Option(ctx.file.getName()),
            "Return Placement"
        )
    }
    case object OutOfScope {
        def apply(varName: String, pos: R2)(using ctx: Context) =
            WaccErr(
                pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(varName)),
                    Set(),
                    Set("Variable used before declaration"),
                    getLineInfo(ctx.file, pos, varName.size)
                ),
                Option(ctx.file.getName()),
                "Scope"
            )
    }

    case object AlreadyDeclared {
        def apply(varName: String, pos: R2)(using ctx: Context) =
            WaccErr(
                pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(varName)),
                    Set(),
                    Set("This variable has already been declared"),
                    getLineInfo(ctx.file, pos, varName.size)
                ),
                Option(ctx.file.getName()),
                "Scope"
            )
    }
    
    case object UnknownPairTypes {
        def apply()(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.SpecialisedError(
                Set(
                    "attempting to exchange values between pairs of unknown types",
                    "pair exchange is only legal when the type of at least one of the sides is known or specified"
                ),
                getLineInfo(ctx.file, pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

    case object ReadUnknownType {
        def apply()(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.SpecialisedError(
                Set(
                    "attempting to read from unknown type",
                    "reading from a nested pair extraction is not legal due to pair erasure"
                ),
                getLineInfo(ctx.file, pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

    /* Takes in a file as well as a position within a file
    * Returns a lineInformation created using the information in the file */
    private def getLineInfo(file: File, pos: R2, badTokWidth: Int = 1): LineInformation = 
        /* Read the lines from the file. */
        val source = scala.io.Source.fromFile(file)
        val lines = source.getLines().toList
        source.close()

        /* Build the line information from the file contents. */
        val zeroIndexRow = pos.row - 1
        val linesBefore = lines.slice((0).max(zeroIndexRow - LinesOfCodeRadius), zeroIndexRow)
        val linesAfter = lines.slice(zeroIndexRow + 1, (lines.size).min(zeroIndexRow + LinesOfCodeRadius + 1))
        new LineInformation(lines(zeroIndexRow), linesBefore, linesAfter, pos.col - 1, badTokWidth)
}
