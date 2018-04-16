namespace Styx.Text

open Styx
open Styx.Expressions
open System

#nowarn "0667"

[<AutoOpen>]
module CoreSyntax =
    /// Parses a terminal expression.
    let expr, internal exprRef = forwarded<Expr>()

    let private isWordChar c = Char.IsLetterOrDigit c || c = '_'
    let private isOperatorChar c = c <> '(' && c <> ')' && not (Char.IsLetterOrDigit c)

    /// Parses an expression using the specified parser in a multi-line context
    /// starting at the current indentation.
    let inline multiline p (s: TokenStream) =
        let min = s.MinimumIndent
        let curr = s.CurrentIndent

        if curr < min then
            Expect [ "expression block", s.Span ]
        else
            s.MinimumIndent <- curr
            let r = p s
            s.MinimumIndent <- min
            r

    /// Returns the parsers registered by the stream.
    let inline parsers (s: TokenStream) = Ok s.Parsers

    /// Updates the parsers registered by the stream.
    let inline updateParsers (f: _ -> _) (s: TokenStream) =
        s.Parsers <- f s.Parsers
        Ok ()

    /// Sets the parsers registered by the stream.
    let inline setParsers p (s: TokenStream) =
        s.Parsers <- p
        Ok ()

    /// Adds the given custom parser to the current state.
    let inline addParser p (s: TokenStream) =
        s.Parsers <- s.Parsers.Add(p)
        Ok ()

    /// Pops the last added custom parser from th current state.
    let inline popParser (s: TokenStream) =
        s.Parsers <- s.Parsers.RemoveAt(s.Parsers.Length - 1)
        Ok ()

    let inline spanned (p: Parser<_>) (s: TokenStream) =
        let pos = s.Position
        
        match p s with
        | Ok r -> let span = { Start = pos ; Length = s.Span.End - pos ; File = s.Span.File } in Ok (r, span, s.Scope)
        | err  -> castfail err


    /// Parses the next expression using scope-imported parsers.
    let scoped (s: TokenStream) = choose s.Parsers s

    let inline (!<) (expr: Parser<_>) = expr |>> fun x -> x :> Expr

    /// Parses a qualified symbol name (ie: 'System.String.++').
    let qualified = sepBy anyIdent dot

    /// Parses a qualified symbol name (ie: 'System.String.++').
    let qualifiedString = sepBy identString dot |>> fun x -> String.Join(".", x)

    let hole (until: Parser<_>) (s: TokenStream) =
        let start = s.Span.Start
        let mutable reply = Unchecked.defaultof<_>

        while LanguagePrimitives.PhysicalEquality reply Unchecked.defaultof<_> do
            match lookAhead until s with
            | Parsed _ ->
                let span = { Start = start ; Length = s.Span.End - start ; File = s.Span.File }

                reply <- Ok (HoleExpression(s.Scope, span) :> Expr)
            | Failed _ ->
                let next = tok s

                if next.Kind = End then
                    reply <- expected "not end of expression" next.Span

                s.Position <- s.Position + 1
        
        reply
