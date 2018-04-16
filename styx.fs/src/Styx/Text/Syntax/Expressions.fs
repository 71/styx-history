namespace Styx.Text

open Styx
open Styx.Expressions

[<AutoOpen>]
module ExpressionsSyntax =

    /// Parses a literal expression.
    let literal = token >>| fun x ->
        match Literal.TryCreate(x) with
        | Some lit -> Ok lit
        | None -> expected "literal token" x.Span

    /// Parses a parenthesized expression.
    let parenthesized = lparen >>. binaryd rparen

    /// Parses a previously defined variable.
    let variable name value span = idents name |>> fun _ -> Variable(name, value, span)

    /// Parses a single let-binding clause.
    let private binding s =
        match (pattern .>>. (idents "=" >>. binaryd (idents "in" <|> comma))) s with
        | Failed err -> err
        | Parsed (pat, value) as v ->
            s.Parsers <- s.Parsers.AddRange(pat.Extract(value))
            v

    /// Parses a let-binding expression.
    let letin = parse {
        let! s        = position
        let! c        = parsers

        let! _        = idents "let"
        let! bindings = many binding
        let! body     = binaryend

        let! span     = span s

        do! setParsers c

        return Binding(bindings, body, span) :> Expr
    }

    /// Parses an if-then-else expression.
    let conditional = parse {
        let! s    = position

        let! _    = idents "if"
        let! cond = binaryd (idents "then")
        let! cons = binaryd (idents "else")
        let! altr = binaryend

        let! span = span s

        return Conditional(cond, cons, altr, span) :> Expr
    }

    let argument = expr

    let call = spanned (qualifiedString .>>. manyOrEmpty argument) |>> fun ((fn, args), span, scope) ->
        Call(scope.LookupFunction(span, fn, args), args, span)

    /// Parses a pattern guard optional guard.
    let guard = idents "when" >>. binary (idents "=")

    /// Parses a full pattern, with optional guard.
    let fullPattern = many pattern .>>. (opt guard) .>> idents "=" |>> fun (pat, guard) ->
        match guard with
        | Some g -> Guarded(Many(pat, Span.Default), g, Span.Default) :> Pattern
        | None -> Many(pat, Span.Default) :> Pattern

    /// Parses a block expression.
    let block = sepBy (binary semicolon) semicolon |>> Block

    exprRef := fun s ->
        match (lookAhead parseOp) s with
        | Ok (t,_,_) -> expected "not a binary operator" t.Span
        | _ -> choose [ scoped ; !< literal ; letin ; conditional ; parenthesized ; !< call ] s
