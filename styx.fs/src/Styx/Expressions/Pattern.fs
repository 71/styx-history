namespace Styx.Expressions

open Styx
open System
open System.Linq

/// Represents a pattern in a pattern matching expression, function definition,
/// or variable binding.
[<AbstractClass>]
type Pattern(span: Text.Span) =

    /// Gets the span in which the pattern was defined.
    member __.Span = span

    /// Extracts all inner variables from the given expression,
    /// returning a list of parsers introduced by it. 
    abstract Extract : Expr -> Text.Parser<Expr> seq

    /// Gets an expression that returns a boolean that indicates
    /// whether the match was successful.
    abstract IsMatch : Binder * Expr -> Expr


/// Defines a Styx let-in binding expression.
type Binding(bindings: (Pattern * Expr) list, body: Expr, span) =
    inherit Expr(span)

    member __.Bindings = bindings

    member __.Body = body

    override __.State = body.State + state (bindings.Select(snd))

    override __.Resolve() = body.OptionalType

    override __.Compile(_ilg) = ()

    override __.ToString() =
        let b = String.concat ", " (List.map (fun (a, b) -> sprintf "%O = %O" a b) bindings)
        sprintf "let %s in %O" b body


/// Represents a Styx local variable.
type Variable(name: string, value: Expr, span) =
    inherit Expr(span)

    /// Gets the name of the variable.
    member __.Name = name

    override __.Resolve() = value.OptionalType

    override __.State = value.State

    override __.Compile(ilg) =
        ()

    override __.ToString() = name