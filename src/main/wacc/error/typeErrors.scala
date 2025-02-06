package wacc.error

import wacc.semantic.typecheck.Context
import wacc.ast.{Type, Ident, Typeless}
import wacc.semantic.QualifiedName
import java.io.File

/* The possible type errors that can occur during type checking. */
object TypeErr {
    case object TypeMismatch {
        def apply(id: Ident[QualifiedName, Typeless], expectedType: Type)(using ctx: Context) = 
            val badToken = id.v.oldName
            WaccErr(
                id.pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Named(expectedType.toString)),
                    Set("Types must match"),
                    getLineInfo(ctx.file, id.pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotString {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = 
                val badToken = id.v.oldName
                WaccErr(
                id.pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                    Set("Must be of String like type"),
                    getLineInfo(ctx.file, id.pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotFreeable {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = 
            val badToken = id.v.oldName
            WaccErr(
                id.pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Raw("Pair(_,_)"), ErrItem.Raw("Char[]")),
                    Set("Tried to free an unfreeable type"),
                    getLineInfo(ctx.file, id.pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object IsNotReadable {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = 
            val badToken = id.v.oldName
            WaccErr(
                id.pos,
                ErrLines.VanillaError(
                    Some(ErrItem.Named(badToken)),
                    Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                    Set("Must be a readable type"),
                    getLineInfo(ctx.file, id.pos, badToken.size)
                ),
                Option(ctx.file.getName()),
                "Type"
            )
    }
    case object ReturnInMainBody {
        def apply(pos: R2)(using ctx: Context) = WaccErr(
            pos,
            ErrLines.VanillaError(
                None,
                Set(),
                Set("Return in main body is not allowed"),
                getLineInfo(ctx.file, pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object OutOfScope {
        def apply(pos: R2)(using ctx: Context) = WaccErr(
            pos,
            ErrLines.VanillaError(
                None,
                Set(),
                Set("Variable used before decleration"),
                getLineInfo(ctx.file, pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }

    case object AlreadyDeclared {
        def apply(pos: R2)(using ctx: Context) = WaccErr(
            pos,
            ErrLines.VanillaError(
                None,
                Set(),
                Set("This variable has already been declared"),
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
        val linesBefore = lines.slice(zeroIndexRow + 1, (lines.size).min(zeroIndexRow + LinesOfCodeRadius + 1))
        val linesAfter = lines.slice((0).max(zeroIndexRow - LinesOfCodeRadius), zeroIndexRow)
        new LineInformation(lines(zeroIndexRow), linesBefore, linesAfter, pos.col, badTokWidth)
}
