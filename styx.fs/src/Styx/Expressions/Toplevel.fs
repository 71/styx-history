namespace Styx.Expressions

open Styx
open Styx.Reactive
open System

type Namespace(declarations: Expr list, binder, span) as this =
    inherit Meta(binder, span)

    do for decl in declarations do
        decl.Subscribe(this) |> ignore

    override __.State = state declarations


type ClassDeclaration(inner: Expr, binder, span) as this =
    inherit Meta(binder, span)

    do inner.Subscribe(this) |> ignore

    override __.State = inner.State

    override __.Compile(_) = ()

/// Defines a 'static' expression.
type Static(expressions: Expr list, span) =
    inherit Expr(span)

    let mutable index = 0
    let expressions = List.toArray expressions
    let mutable subscription = Unchecked.defaultof<IDisposable>

    let rec next = fun (x: Expression) ->
        if x.IsBound then
            subscription.Dispose()

            index <- index + 1

            if index <> expressions.Length then
                subscription <- expressions.[index].Subscribe(next)

    do if expressions.Length <> 0 then
        subscription <- expressions.[0].Subscribe(next)

    override __.Resolve() = Some Type.Void
    override __.Compile(_) = ()

    override __.State =
        if index = expressions.Length then
            Bound
        else
            Unbound

