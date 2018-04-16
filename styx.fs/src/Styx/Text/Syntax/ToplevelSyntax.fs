namespace Styx.Text

open Styx.Expressions
open System

#nowarn "58"

[<AutoOpen>]
module ToplevelSyntax =
    /// Parses a top-level expression.
    let toplevel, internal toplevelRef = forwarded()

    /// Parses a top-level namespace expression.
    let namespaceSyntax = parse {
        let! start = position

        let! _  = idents "namespace"
        let! ns = qualifiedString

        let! state  = state

        let! decls = many toplevel
        let! span  = span start
        let! scope = scope

        do! restore state

        return Namespace(decls, scope.Binder, span) :> Styx.Expr
    }

    /// Parses a top-level static expression.
    let staticSyntax = spanned (idents "static" >>. block) |>> fun (x, span, _) -> Static(List.ofSeq x.Expressions, span) :> Styx.Expr

    /// Parses a top-level function declaration.
    let fnSyntax = spanned (identString .>>. fullPattern .>>. binaryend) |>> fun (((name, guard), body), span, _) ->
        FunctionBody(name, guard, body, span)

    let typeDefinition = choose [
        spanned integer |>> fun (x, span, scope) -> SizedType(x, scope.Binder, span) :> TypeDeclaration
    ]

    /// Parses a top-level type declaration.
    let typeSyntax = idents "type" >>. identString .>> eq .>>. typeDefinition


    /// Parses a top-level type declaration.
    let classSyntax =
        idents "class" >>. many (identString >>|
            fun i -> if i = "where" then
                        expected "not 'where'" Span.Default
                     else Ok i)
        .>> eq


    toplevelRef := choose [ namespaceSyntax ; staticSyntax ; signature ]
