package wacc.backend.referencer

import wacc.ast._
import wacc.backend.Context
import wacc.semantic.QualifiedName

object referencer {
    def reference(prog: Program[QualifiedName, KnownType])(using ctx: Context): Unit = {
        ???
    }
    
    def reference(func: Func[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        ???
    }

    def reference(stmts: List[Stmt[QualifiedName, KnownType]])(using ctx: Context): Unit  = {
        ???
    }

    def reference(stmt: Stmt[QualifiedName, KnownType])(using ctx: Context): Unit  = {
        ???
    }
}

/*
RoData
labelling + Reference

Variables
into references

Prebuilt Widgets
set flags to add prebuilt functions

refencer

program

func:
    args = 0
    varoffset = 0
    referencer - list stmt
    store varoffset

args++
argsoffset
offset -- ++size


map - QName to reference
map - string to roData
map - func Qname to offset
list of flags for widgets
*/