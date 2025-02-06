package wacc.error

import wacc.ast.{SemType, Pos}
import wacc.semantic.QualifiedName
import wacc.semantic.typecheck.Context

import java.io.File

/* The possible type errors that can occur during type checking. */
object TypeErr {
    case object TypeMismatch {
        def apply(readType: SemType, expectedType: SemType)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(readType.toString)),
                Set(ErrItem.Named(expectedType.toString)),
                Set(),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotString {
        def apply(readType: SemType)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(readType.toString)),
                Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                Set("Must be of String like type"),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotFreeable {
        def apply(givenType: SemType)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(givenType.toString)),
                Set(ErrItem.Raw("pair"), ErrItem.Raw("array")),
                Set("Tried to free an unfreeable type"),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotReadable {
        def apply(givenType: SemType)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(givenType.toString)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a readable type"),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotComparable {
        def apply(actualType: SemType)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(actualType.toString)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a comparable type"),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

    case object ReturnInMainBody {
        def apply()(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named("return")),
                Set(),
                Set("Return in main body is not allowed"),
                getLineInfo(ctx.file, "return".length())
            ),
            Option(ctx.file.getName()),
            "Return Placement"
        )
    }
    case object OutOfScope {
        def apply(varName: String)(using ctx: Context, pos: Pos) =
            WaccErr(
                pos,
                ErrLines.SpecialisedError(
                    Set(s"variable $varName has not been declared in this scope"),
                    getLineInfo(ctx.file, varName.size)
                ),
                Option(ctx.file.getName()),
                "Scope"
            )
    }

    case object AlreadyDeclared {
        def apply(varName: String)(using ctx: Context, pos: Pos) =
            WaccErr(
                pos,
                ErrLines.SpecialisedError(
                    Set(s"illegal redeclaration of variable $varName"),
                    getLineInfo(ctx.file, varName.size)
                ),
                Option(ctx.file.getName()),
                "Scope"
            )
    }

    case object FuncAlreadyDeclared {
        def apply(funcName: String)(using ctx: Context, pos: Pos) =
            WaccErr(
                pos,
                ErrLines.SpecialisedError(
                    Set(s"illegal redefinition of function $funcName"),
                    getLineInfo(ctx.file, funcName.size)
                ),
                Option(ctx.file.getName()),
                "Function redefinition"
            )
    }
    
    case object UnknownPairTypes {
        def apply()(using ctx: Context,  pos: Pos) = WaccErr(
            pos,
            ErrLines.SpecialisedError(
                Set(
                    "attempting to exchange values between pairs of unknown types",
                    "pair exchange is only legal when the type of at least one of the sides is known or specified"
                ),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

    case object ReadUnknownType {
        def apply()(using ctx: Context,  pos: Pos) = WaccErr(
            pos,
            ErrLines.SpecialisedError(
                Set(
                    "attempting to read from unknown type",
                    "reading from a nested pair extraction is not legal due to pair erasure"
                ),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

        case object WrongArgNums {
        def apply(actNum: Int, expNum: Int, name: QualifiedName)(using ctx: Context, pos: Pos) = WaccErr(
            pos,
            ErrLines.SpecialisedError(
                Set(
                    s"wrong number of arguments provided to function ${name.oldName}",
                    s"unexpected $actNum arguments",
                    s"expected $expNum arguments",
                    s"(function ${name.oldName} has type ${ctx.getType(name).toString()})"
                ),
                getLineInfo(ctx.file)
            ),
            Option(ctx.file.getName()),
            "Function call"
        )
    }

    /* Takes in a file as well as a position within a file
     * Returns a lineInformation created using the information in the file */
    private def getLineInfo(file: File, badTokWidth: Int = 1)(using pos: Pos): LineInformation = 
        /* Read the lines from the file. */
        val source = scala.io.Source.fromFile(file)
        val lines  = source.getLines().toList
        source.close()

        /* Build the line information from the file contents. */
        val zeroIndexRow = pos.row - 1
        val linesBefore  = lines.slice((0).max(zeroIndexRow - CODE_RADIUS_SIZE), zeroIndexRow)
        val linesAfter   = lines.slice(zeroIndexRow + 1, (lines.size).min(zeroIndexRow + CODE_RADIUS_SIZE + 1))
        new LineInformation(lines(zeroIndexRow), linesBefore, linesAfter, pos.col - 1, badTokWidth)
}
