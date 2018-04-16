namespace Styx.Expressions

open Styx.Text
open Styx
open Styx.Library
open Styx.Reactive
open System.Collections.Generic
open System.Linq
open System.Reflection.Emit

/// Defines a Styx if-then-else conditional expression.
type Conditional(cond: Expr, cons: Expr, altr: Expr, span) as this =
    inherit Expr(span)

    do cond.Subscribe(this) |> ignore
       cons.Subscribe(this) |> ignore
       altr.Subscribe(this) |> ignore

    member __.Condition = cond

    member __.Consequence = cons

    member __.Alternative = altr

    override __.State = cond.State + cons.State + altr.State

    override __.Resolve() = cons.OptionalType

    override this.Compile(ilg) =
        if this.IsBound then
            let after = ilg.DefineLabel()
            let altern = ilg.DefineLabel()

            // Emit branch to 'altern' if condition is false,
            // else keep going
            ilg.Emit(cond)
            ilg.Emit(OpCodes.Brfalse, altern)

            // Emit body of truthy expression
            ilg.Emit(cons)
            ilg.Emit(OpCodes.Br, after)
            ilg.MarkLabel(altern)

            // Emit body of falsy expression
            ilg.Emit(altr)
            ilg.MarkLabel(after)
        else
            ilg.ThrowHole()

    override __.ToString() = sprintf "if %O then %O else %O" cond cons altr


/// Defines a Styx pattern matching expression.
type Match(value: Expr, cases: (Pattern * Expr) list, span) =
    inherit Expr(span)

    do assert(not cases.IsEmpty)

    override __.State = value.State + state (cases.Select(snd))

    override __.Resolve() = Some Type.Void

    override __.Compile(ilg) =
        ()

    override __.ToString() =
        let p = String.concat "\n|" (List.map (fun (p, e) -> sprintf "%O -> %O" p e) cases)
        sprintf "match %O with\n %O" value p

/// Defines a block of expressions.
type Block(inner: Expr list) =
    inherit Expr(match inner with
                 | []  -> Span.Default
                 | [x] -> x.Span
                 | xs  -> xs.First().Span >!> xs.Last().Span)

    let exprs = inner.ToArray()

    member __.Expressions = exprs :> IReadOnlyList<_>

    override __.State = state exprs

    override __.Resolve() =
        if exprs.Length = 0 then
            Some Type.Void
        else
            exprs.[exprs.Length - 1].OptionalType

    override __.Compile(ilg) =
        let rec compile il : Expr list -> unit = function
        | [] -> ()
        | (x::xs) -> x.Compile(il) ; compile il xs

        compile ilg inner
