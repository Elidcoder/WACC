package wacc.error

import wacc.semantic.typecheck.Context
import wacc.ast.{Type, Ident, Typeless}
import wacc.semantic.QualifiedName
import java.io.File

object SemanticWaccErr {
    case object TypeMismatch {
        def apply(id: Ident[QualifiedName, Typeless], expectedType: Type)(using ctx: Context) = WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Named(expectedType.toString)),
                Set("Types must match"),
                getLineInfo(ctx.file, id.pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotString {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                Set("Must be of string like type"),
                getLineInfo(ctx.file, id.pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotFreeable {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("Pair(_,_)"), ErrItem.Raw("Char[]")),
                Set("Must be of a freeable type"),
                getLineInfo(ctx.file, id.pos)
            ),
            Option(ctx.file.getName()),
            "Type"
        )
    }
    case object IsNotReadable {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a readable type"),
                getLineInfo(ctx.file, id.pos)
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
                Set("This variable has not been declared"),
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
}

/* Takes in a file as well as a position within a file
 * Returns a lineInformation created using the information in the file */
def getLineInfo(file: File, pos: R2): LineInformation = 
    /* Read the lines from the file. */
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines().toList
    source.close()

    /* Build the line information from the file contents. */
    val zeroIndexRow = pos.row - 1
    val linesBefore = lines.slice(zeroIndexRow + 1, (lines.size).min(zeroIndexRow + LinesOfCodeRadius + 1))
    val linesAfter = lines.slice((0).max(zeroIndexRow - LinesOfCodeRadius), zeroIndexRow)
    new LineInformation(lines(zeroIndexRow), linesBefore, linesAfter, pos.col, 1)
