package wacc.semantic.typecheck

import wacc.semantic.renamedAst
import wacc.semantic.typedAst
import wacc.error.WaccErr

def check(prog: renamedAst.Program): Either[List[WaccErr], typedAst.Program] = {
    given ctx: Context = new Context()
    val typedFuncs: List[typedAst.Func] = checkFuncs(prog.fs)
    val typedStmts: List[typedAst.Stmt] = check(prog.x)
    val errors = ctx.result
    if errors.isEmpty then Right(typedAst.Program(typedFuncs, typedStmts)) else Left(errors)
}

def checkFuncs(fs: List[renamedAst.Func])(using ctx: Context): List[typedAst.Func] = 
    fs.map(check(_))

def check(f: renamedAst.Func)(using ctx: Context): typedAst.Func = 
    typedAst.Func(typedAst.Ident(f.v), check(f.s))

def check(ss: List[renamedAst.Stmt])(using ctx: Context): List[typedAst.Stmt] = ??? 

def check(t: renamedAst.Type) = ???
