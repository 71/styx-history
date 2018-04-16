namespace Styx.Expressions

open Styx
open Styx.Text
open Styx.Reactive

/// Defines a Styx binary expression.
type Binary(fn: Future<Function>, left: Expr, right: Expr) as this =
    inherit Extension(left.Span >!> right.Span)

    do (fn >>= fun _ -> this :> Expr).Subscribe(this) |> ignore
       left.Subscribe(this) |> ignore
       right.Subscribe(this) |> ignore

    let span = left.Span >!> right.Span

    member __.Operator = fn

    member __.Left = left

    member __.Right = right

    override __.State = left.State + right.State + (fn :> IBindable).State

    override __.Lower() = Call(fn, [left; right], span) :> Expr

    override __.ToString() = sprintf "%O %O %O" left fn right


/// Defines a Styx unary expression.
type Unary(fn: Future<Function>, operand: Expr, span) as this =
    inherit Extension(span)

    do (fn >>= fun _ -> this :> Expr).Subscribe(this) |> ignore
       operand.Subscribe(this) |> ignore

    member __.Operator = fn

    member __.Operand = operand

    override __.State = operand.State + (fn :> IBindable).State

    override __.Lower() = Call(fn, [operand], span) :> Expr

    override __.ToString() = sprintf "%O %O" fn operand
