namespace Styx.Expressions

open Styx
open Styx.Text

type TupleConstructor(span) =
    inherit Expr(span)
    
    override __.Resolve() = None

    override __.State = Unbound

    override __.Compile(ilg) = ()
