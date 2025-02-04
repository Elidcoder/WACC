package wacc.semantic.typecheck

import wacc.error.{WaccErr, R2, ErrLines, ErrItem}
import wacc.semantic.renamedAst.Ident
import wacc.semantic.renamedAst.Type
import wacc.error.LineInformation

object WaccErr {
    case object TypeMismatch {
        def apply(pos: R2, id: Ident, expectedType: Type) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.oldName)),
                Set(ErrItem.Raw(expectedType.toString)),
                Set("Types must match"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            None
        )
    }
    case object IsNotString {
        def apply(pos: R2, id: Ident) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.oldName)),
                Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                Set("Must be of string like type"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            None
        )
    }
    case object IsNotFreeable {
        def apply(pos: R2, id: Ident) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.oldName)),
                Set(ErrItem.Raw("Pair(_,_)"), ErrItem.Raw("Char[]")),
                Set("Must be of a freeable type"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            None
        )
    }
    case object IsNotReadable {
        def apply(pos: R2, id: Ident) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.oldName)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a readable type"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            None
        )
    }
    case object ReturnInMainBody {
        def apply(pos: R2) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                None,
                Set(),
                Set("Return in main body is not allowed"),
                new LineInformation("", Seq.empty, Seq.empty, 0, 0)
            ),
            None
        )
    }
}

enum Body {
    case Function(returnType: wacc.semantic.renamedAst.Type)
    case Main
}

class Context(var body: Body) {
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
