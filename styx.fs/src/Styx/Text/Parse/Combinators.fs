namespace Styx.Text

[<AutoOpen>]
module Combinators =

    /// Creates a parser that forwards its underlying implementation
    /// to another parser set later on.
    let inline forwarded<'a>() : Parser<'a> * Parser<'a> ref =
        let dummy = fun _ -> failwith "Attempted to parse an expression using a uninitialized Forward parser."
        let r = ref dummy

        (fun s -> !r s), r

    /// Parses an expression which returns another parser, and use the latter
    /// to parse an expression.
    let inline (>>=) (parser: Parser<_>) (f: _ -> Parser<_>) (s: TokenStream) =
        match parser s with
        | Ok x -> match (f x) s with
                  | Ok y -> Ok y
                  | err -> err

        | err -> castfail err

    /// Parses an expression using the provided parser,
    /// and returns a new reply selected from its result.
    let inline (>>|) (parser: Parser<_>) select s =
        match parser s with
        | Ok r     -> select r
        | Expect e -> Expect e
        | Error e  -> Error e

    /// Parses an expression using the provided parser,
    /// and returns a new expression selected from its result.
    let inline (|>>) (parser: Parser<_>) select s =
        match parser s with
        | Ok r -> Ok (select r)
        | Expect e -> Expect e
        | Error e -> Error e


    /// Parses an expression, discards its result, and returns the
    /// result of the second parser.
    let inline (>>.) (a: Parser<_>) (b: Parser<_>) s =
        match a s with
        | Ok _     -> b s
        | Expect e -> Expect e
        | Error e  -> Error e

    /// Parses an expression, then a second one, discarding the second
    /// result and returning the first one.
    let inline (.>>) (a: Parser<_>) (b: Parser<_>) s =
        match a s with
        | Ok r -> match b s with
                  | Ok _     -> Ok r
                  | Expect e -> Expect e
                  | Error e  -> Error e
        | err -> err

    /// Parses an expression, then a second one, and returns a pair of both results.
    let inline (.>>.) (a: Parser<_>) (b: Parser<_>) s =
        match a s with
        | Ok x -> match b s with
                  | Ok y     -> Ok (x, y)
                  | Expect e -> Expect e 
                  | Error e  -> Error e
        | Expect e -> Expect e
        | Error e  -> Error e

    /// Attempts to parse using the given parser. If it fails, the
    /// previous state will be restored.
    let inline (!?) (a: Parser<_>) (s: TokenStream) = 
        let st = s.State
        let r = a s

        if r.Failure then
            s.Restore st
        r

    /// Parsers an expression, or another one.
    let inline (<|>) (a: Parser<_>) (b: Parser<_>) s =
        match !?a s with
        | Ok x     -> Ok x
        | Error e  -> Error e
        | Expect x -> match !?a s with
                      | Ok y     -> Ok y
                      | Error e  -> Error e
                      | Expect y -> Expect (x @ y)

    /// Parses the next expression using the provided parsers,
    /// backtracking everytime an error is encountered.
    let choose (parsers: Parser<_> seq) (s: TokenStream) =
        let iter = parsers.GetEnumerator()

        let mutable reply = Unchecked.defaultof<_>
        let mutable parse = true
        let mutable errors = []
        let st = s.State

        while parse && iter.MoveNext() do
            match iter.Current s with
            | Ok r     -> reply <- Ok r
                          parse <- false
            | Error e  -> reply <- Error e
                          parse <- false
            | Expect e -> errors <- e @ errors
                          s.Restore st
        
        if parse then
            Expect errors
        else
            reply

    /// Parses many times the given parser in a row. Fails if it parses no time at all.
    let many (parser: Parser<_>) (s: TokenStream) =
        let mutable prev = s.State

        match parser s with
        | Ok x ->
            let mutable parse = true
            let mutable result = [x]

            prev <- s.State

            while parse do
                match parser s with
                | Ok x     -> result <- x::result
                              prev <- s.State

                | _        -> s.Restore prev
                              parse <- false

            Ok result

        | err -> s.Restore prev
                 castfail err

    let manyOrEmpty (parser: Parser<_>) (s: TokenStream) =
        match many parser s with
        | Ok r -> Ok r
        | _ -> Ok []

    let opt (parser: Parser<_>) (s: TokenStream) =
        let st = s.State

        match parser s with
        | Ok x -> Ok <| Some x
        | _ -> s.Restore st ; Ok None

    /// Parses the content of the given parser separated by another parser.
    let sepBy (parser: Parser<_>) (sep: Parser<_>) (s: TokenStream) =
        let initial = s.State

        match parser s with
        | Failed r -> s.Restore initial ; castfail r
        | Parsed r ->
            let mutable l = [r]
            let mutable parse = true
            let mutable err = None
            let mutable prev = s.State

            while parse do
                printfn "Attempting to parse %O with %O" (tok s) sep
                if (sep s).Failure then
                    parse <- false
                    s.Restore prev
                else
                    match parser s with
                    | Failed r -> parse <- false
                                  err <- Some r
                                  s.Restore initial
                    | Parsed r -> l <- r::l
                                  prev <- s.State

            match err with
            | None -> Ok l
            | Some r -> castfail r

    /// Parses an expression using the given parser, restores the previous state,
    /// and returns the result of the parsing operation.
    let lookAhead (parser: Parser<_>) (s: TokenStream) =
        let state = s.State
        let r = parser s

        s.Restore state
        r


    [<Sealed>]
    type ParseExpressionBuilder() =
        member __.Return(x) : Parser<_> = fun (_: TokenStream) -> Ok x
        member __.ReturnFrom(p: Parser<_>) = p

        member __.Bind(p, f) = p >>= f
        member __.Delay(f: unit -> Parser<_>) : Parser<_> = fun s -> (f()) s
        member __.Zero() : Parser<_> = fun (_: TokenStream) -> Ok ()

    let parse = ParseExpressionBuilder()
