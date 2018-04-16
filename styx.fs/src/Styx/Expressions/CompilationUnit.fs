namespace Styx.Expressions

open Styx

type CompilationUnit(exprs: Expr list) =

    member __.Expressions = exprs
