package wacc.semantic.typecheck

import wacc.error.{WaccErr, R2, ErrLines, ErrItem}
import wacc.ast.*
import wacc.semantic.QualifiedName

object WaccErr {
    case object TypeMismatch {
        def apply(id: Ident[QualifiedName, Unit], expectedType: Type) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw(expectedType.toString)),
                Set("Types must match"),
                10
            ),
            None
        )
    }
    case object IsNotString {
        def apply(id: Ident[QualifiedName, Unit]) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("String"), ErrItem.Raw("Char[]")),
                Set("Must be of string like type"),
                10
            ),
            None
        )
    }
    case object IsNotFreeable {
        def apply(id: Ident[QualifiedName, Unit]) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("Pair(_,_)"), ErrItem.Raw("Char[]")),
                Set("Must be of a freeable type"),
                10
            ),
            None
        )
    }
    case object IsNotReadable {
        def apply(id: Ident[QualifiedName, Unit]) = wacc.error.WaccErr(
            id.pos,
            ErrLines.VanillaError(
                Some(ErrItem.Named(id.v.oldName)),
                Set(ErrItem.Raw("Int"), ErrItem.Raw("Char")),
                Set("Must be a readable type"),
                10
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
                10
            ),
            None
        )
    }
}

enum Body {
    case Function(returnType: Type)
    case Main
}

class Context(var body: Body) {
    private val errors = List.newBuilder[WaccErr]
    def result: List[WaccErr] = errors.result()
    def error(err: WaccErr) = 
        errors += err
        None
}
