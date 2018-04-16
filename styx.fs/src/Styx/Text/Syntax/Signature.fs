namespace Styx.Text

open Styx
open Styx.Expressions
open System

[<AutoOpen>]
module SignatureSyntax =

    /// Parses a component.
    let private componentSyntax, private componentRef = forwarded()

    /// Parses a component as a type variable.
    let private componentAsVariable = token >>| fun tok ->
        match tok.Kind with
        | Ident i when Char.IsLower i.[0] -> Ok (SignatureVariable(i, tok.Span) :> Expr)
        | _ -> expected "type variable" tok.Span

    /// Parses a component as a type.
    let rec private componentAsType = spanned (qualifiedString .>>. manyOrEmpty expr) |>> fun ((ty, args), span, scope) ->
        TypeExpression(ty, scope.Binder, args, span) :> Expr

    componentRef := choose [ componentAsVariable ; componentAsType ]


    /// Parses a top-level signature declaration.
    let signature = parse {
        let! start       = position
        let! names       = (sepBy identString comma) .>> colon

        do! addParser componentSyntax

        let! constraints = opt (lparen >>. (sepBy (binary comma) comma) .>> rparen .>> (idents "=>"))
        let! components  = sepBy expr arrow

        do! popParser

        let! span        = span start

        return Signatures(names, defaultArg constraints [], components, span) :> Expr
    }
