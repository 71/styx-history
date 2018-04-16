namespace Styx

open Styx.Reactive
open Styx.Text

/// Defines a Styx meta expression.
[<AbstractClass>]
type Meta(binder: Binder, span) =
    inherit Expr(span)

    let mutable executed = false

    new(binder) = Meta(binder, Span.Default)

    override __.Resolve() = Some Type.Void

    override this.Compile(_) =
        if not executed then
            executed <- true
            this.Execute(binder)

    abstract Execute : Binder -> unit

    default __.Execute(_) = ()

[<AutoOpen>]
module MetaExprUtils =

    type Expression with
        member this.IsMeta = match this with | :? Meta -> true | _ -> false

        member this.AsMeta = match this with | :? Meta as m -> Some m | _ -> None
