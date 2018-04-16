namespace Styx.Text

[<AutoOpen>]
module ParseTokens =

    /// Parses a single token.
    let inline token s =
        let t = tok s

        if t.Indent < s.MinimumIndent then
            Ok { t with Kind = End }
        else
            s.Position <- s.Position + 1
            Ok t

    /// Parses a single token that matches the given predicate.
    let inline tokenw s p = token >>| fun x ->
        if p x then
            Ok x
        else
            Expect [ s, x.Span ]

    /// Parses a doc-comment.
    let docs = tokenw "doc comment" <| fun x -> match x.Kind with Doc _ -> true | _ -> false

    /// Parses a comma.
    let comma = tokenw "comma" <| fun x -> x.Kind = Comma

    /// Parses a semicolon.
    let semicolon = tokenw "semicolon" <| fun x -> x.Kind = Semicolon

    /// Parses a left-paren.
    let lparen = tokenw "left parenthesis" <| fun x -> x.Kind = LParen

    /// Parses a right-paren.
    let rparen = tokenw "right parenthesis" <| fun x -> x.Kind = RParen

    /// Parses an integer.
    let integer = token >>| fun x ->
        match x.Kind with
        | Integer i -> Ok i
        | _ -> expected "integer" x.Span


    /// Parses an identifier token.
    let anyIdent = token >>| fun x ->
        match x.Kind with
        | Ident _ -> Ok x
        | _ -> expected "identifier" x.Span

    /// Parses any identifier.
    let identString = token >>| fun x ->
        match x.Kind with
        | Ident i -> Ok i
        | _ -> expected "identifier" x.Span

    /// Parses an identifier matching the given predicate.
    let inline ident p msg = token >>| fun x ->
        match x.Kind with
        | Ident w when p w -> Ok x
        | _ -> expected msg x.Span

    /// Parses an identifier matching the given name.
    let inline idents expect = token >>| fun x ->
        match x.Kind with
        | Ident w when w = expect -> Ok x
        | _ -> expected (sprintf "keyword or operator '%s'" expect) x.Span

    /// Parses a dot '.'.
    let dot = idents "."

    /// Parses an arrow '->'.
    let arrow = idents "->"

    /// Parses an equal sign '='.
    let eq = idents "="

    /// Parses a colon ':'.
    let colon = idents ":"