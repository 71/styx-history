namespace Styx.Text

open Styx
open Styx.Expressions

[<AutoOpen>]
module OperatorsSyntax =

    /// Defines the associativity of a binary operator.
    type Assoc = Left  =  0
               | Right = -1  // value of -1 to avoid a branch in parseBinary
                             // -> we can type 'precedence + assoc' directly thanks to this


    /// Returns the precedence of the given token as a (precedence, isLeftAssociative) pair,
    /// or none if no matching operator was found.
    let getPrecedence s =
        match s with
        | "->" | "=>" -> 1, Assoc.Left
        | "||"        -> 2, Assoc.Left
        | "^^"        -> 3, Assoc.Left
        | "&&"        -> 4, Assoc.Left
        | "==" | "<>" -> 5, Assoc.Left

        | ">"  | "<"  | ">=" | "<=" -> 6, Assoc.Left
        | ">>" | "<<"               -> 7, Assoc.Left

        | "+" | "-" -> 10, Assoc.Left
        | "*" | "/" | "//" | "%" -> 11, Assoc.Left

        | "$" -> 1, Assoc.Right
        | "^" -> 10, Assoc.Right

        | _ -> 0, Assoc.Left


    /// Parses a binary operator.
    let inline parseOp(s: TokenStream) =
        match anyIdent s with
        | Failed reply -> castfail reply
        | Parsed op  -> let p, a = getPrecedence <| op.Kind.ToString()
                        if p = 0 then
                            expected "binary operator" op.Span
                        else
                            Ok (op, p, a)
    
    /// Parses a binary expression, given the current state and its end parser.
    let rec private parseBinary(leftprec: int, left: Expr, term: Parser<Expr>, over: Parser<_>, s: TokenStream) =
        // Some ugly pattern matching, which should make things faster than
        // using the 'parse' syntax or monads.

        match lookAhead over s with       // make sure we haven't reached the end of the binary expression
        | Parsed _ -> Ok left
        | Failed _ ->
            let st = s.State

            match parseOp s with             // find operator precedence / associativity
            | Failed _ -> hole over s        // if this fails, that's because a binary operator doesn't follow
            | Parsed (op, prec, assoc) ->

                if prec < leftprec then
                    // reached end of current expression
                    s.Restore st

                    Ok left         
                else
                    // can continue parsing
                    match term s with
                    | Failed reply -> castfail reply
                    | Parsed right ->
                        match parseBinary(prec + int assoc, right, term, over, s) with
                        | Failed reply -> castfail reply
                        | Parsed r ->
                            let fn = s.Scope.LookupFunction(op.Span, op.Kind.ToString(), [ left ; right ])
                            let binary = Binary(fn, left, r)

                            parseBinary(prec + int assoc, binary, term, over, s)


    /// Parses a binary expression, given the current state and its end parser.
    let binary (over: Parser<_>) (s: TokenStream) =
        // Some ugly pattern matching, which should make things faster than
        // using the 'parse' syntax or monads.
        match expr s with
        | Failed _ -> hole over s
        | Parsed left -> parseBinary(0, left, expr, over, s)

    /// Parses a binary expression, and discards the end-of-expression token on success.
    let binaryd over s =
        match binary over s with
        | Ok r -> let d = over s
                  assert d.Success
                  Ok r
        | err -> err

    /// Parses a binary expression that ends at the end of the current block.
    let binaryend = binary (tokenw "end" <| fun x -> x.Kind = End)

