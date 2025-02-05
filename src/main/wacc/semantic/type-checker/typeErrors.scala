package wacc.semantic.typecheck

import wacc.error.{WaccErr, R2, ErrLines, ErrItem, LineInformation}
import wacc.ast.{Type, Ident, ?, Typeless}
import wacc.semantic.{QualifiedName, Environment}
import java.io.File

object WaccErr {
    case object TypeMismatch {
        def apply(id: Ident[QualifiedName, Typeless], expectedType: Type)(using ctx: Context) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Named(expectedType.toString)),
                Set("Types must match"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            Option(ctx.file.getPath()),
            "Type"
        )
    }
    case object IsNotString {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                Set("Must be of string like type"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            Option(ctx.file.getPath()),
            "Type"
        )
    }
    case object IsNotFreeable {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("Pair(_,_)"), ErrItem.Raw("Char[]")),
                Set("Must be of a freeable type"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            Option(ctx.file.getPath()),
            "Type"
        )
    }
    case object IsNotReadable {
        def apply(id: Ident[QualifiedName, Typeless])(using ctx: Context) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a readable type"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            Option(ctx.file.getPath()),
            "Type"
        )
    }
    case object ReturnInMainBody {
        def apply(pos: R2)(using ctx: Context) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                None,
                Set(),
                Set("Return in main body is not allowed"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            Option(ctx.file.getPath()),
            "Type"
        )
    }
}

// def getLineInfo(file: File, pos: Pos): LineInformation = 
//     val source = scala.io.Source.fromFile(file)
//     val lines = source.getLines().toList
//     source.close()
//     lines

enum Body {
    case Function(returnType: Type)
    case Main
}

class Context(var body: Body, val env: Environment, val file: File) {
    def getType(uid: Int): Type = uid match
        case -1 => 
            // TODO(Scope error: Undeclared)
            ?
        case -2 => 
            // TODO(Scope error: Already declared)
            ?
        case n => env.get(n)
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
