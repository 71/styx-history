namespace Styx.Expressions

open Styx
open Styx.Reactive

type HoleExpression(scope: Scope, span) as this =
    inherit Expr(span)

    do (scope.Functions >>= fun _ -> this :> Expr).Subscribe(this) |> ignore
    override __.Resolve() = None

    override __.Seal(r) =
        r.Hole({ Message = "Unable to parse expression." ; Span = span })

    override __.Compile(il) =
        il.ThrowHole()
