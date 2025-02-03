package wacc.semantic.typecheck

import wacc.error.{WaccErr, R2, ErrLines, ErrItem}
import wacc.semantic.renamedAst.Ident
import wacc.semantic.renamedAst.Type

object WaccErr {
    case object TypeMismatch {
        def apply(pos: R2, id: Ident, expectedType: Type) = wacc.error.WaccErr(
            pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.oldName)),
                Set(ErrItem.Raw(expectedType.toString)),
                Set("Types must match"),
                10
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
                10
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
                10
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
                10
            ),
            None
        )
    }
}

class Context() {
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr): Option[Type] = 
        errors += err
        None
}
