namespace Styx.Expressions

open Styx
open Styx.Text
open System
open System.Linq

module private PatternUtils =
    let trueLit = Literal.TryCreate({ Kind = Ident("true") ; Indent = 0 ; Span = Span.Default }).Value :> Expr

type Guarded(inner: Pattern, cond: Expr, span) =
    inherit Pattern(span)

    member __.Pattern = inner

    member __.Guard = cond

    override __.Extract(x) = inner.Extract x

    override __.IsMatch(binder, x) =
        let lhs = inner.IsMatch(binder, x)
        let rhs = cond
        
        Binary(binder.LookupFunction(Span.Default, "&&", [lhs ; rhs]), lhs, rhs) :> Expr

    override __.ToString() = sprintf "%O when %O" inner cond

type Many(inner: Pattern list, span) =
    inherit Pattern(span)

    member __.Patterns = inner

    override __.Extract(x) = inner.SelectMany(fun pat -> pat.Extract x)

    override __.IsMatch(binder, x) =
        match inner with
        | [] -> PatternUtils.trueLit
        | (p::ps) ->
            let andOp = binder.LookupFunction(Span.Default, "&&", [ PatternUtils.trueLit ; PatternUtils.trueLit ])
            let mutable lhs = p.IsMatch(binder, x)

            for p in ps do
                lhs <- Binary(andOp, lhs, p.IsMatch(binder, x))
            lhs

    override __.ToString() = String.Join(" ", inner)

type Simple(name: string, span) =
    inherit Pattern(span)

    member __.Name = name

    override __.Extract(x) = Seq.singleton <| (idents name |>> fun _ -> Expressions.Variable(name, x, span) :> Expr)

    override __.IsMatch(_, _) = PatternUtils.trueLit

    override __.ToString() = name

type Catchall(span) =
    inherit Pattern(span)

    override __.Extract(_) = Seq.empty

    override __.IsMatch(_, _) = PatternUtils.trueLit

    override __.ToString() = "_"
