namespace rec Styx.Text

open Styx
open System.Collections.Immutable

/// Represents the result of a parsing operation.
type Reply<'T> = Ok     of 'T
               | Expect of (string * Span) list
               | Error  of (string * Span)
with
    member this.Success = match this with | Ok _ -> true | _ -> false
    member this.Failure = match this with | Ok _ -> false | _ -> true


/// Defines a stream of tokens and its state.
type TokenStream = { Tokens  : ImmutableArray<Token>
                   ; Scope   : Scope

                   ; mutable Parsers       : ImmutableArray<Parser<Expr>>
                   ; mutable MinimumIndent : int
                   ; mutable CurrentIndent : int
                   ; mutable Position      : int
                   }
with
    member this.Span =
        let pos = this.Position

        assert(pos >= 0)

        let tokens = this.Tokens
        let len = tokens.Length

        if len = 0 then
            Span.Default
        elif pos >= len then
            tokens.[len - 1].Span
        else
            tokens.[pos].Span

    member this.Clone()
        = { Tokens = this.Tokens ; Scope = this.Scope
          ; Parsers = this.Parsers
          ; MinimumIndent = this.MinimumIndent
          ; CurrentIndent = this.CurrentIndent
          ; Position = this.Position
          }

    member this.State = { Parsers   = this.Parsers
                        ; MinIndent = this.MinimumIndent
                        ; CurIndent = this.CurrentIndent
                        ; Position  = this.Position
                        }

    member this.Restore(st: TokenStreamState) =
        this.Parsers       <- st.Parsers
        this.MinimumIndent <- st.MinIndent
        this.CurrentIndent <- st.CurIndent
        this.Position      <- st.Position

    static member Create(scope, tokens)
        = { Tokens = tokens ; Scope = scope
          ; Parsers = ImmutableArray.Empty
          ; MinimumIndent = 0
          ; CurrentIndent = 0
          ; Position      = 0
          }

    static member Create(scope: Scope, string: string)
        = TokenStream.Create(scope, tokenize(scope.InputName, string))

/// Defines the state of a token stream.
type TokenStreamState = { Parsers   : ImmutableArray<Parser<Expr>>
                        ; MinIndent : int
                        ; CurIndent : int
                        ; Position  : int
                        }


/// Defines a Styx syntax parser.
type Parser<'T> = TokenStream -> Reply<'T>

[<AutoOpen>]
module TokenStreamUtils =

    let inline (|Parsed|Failed|) (reply: Reply<_>) =
        match reply with
        | Ok r -> Parsed r
        | _ -> Failed reply

    let inline castfail (reply: Reply<_>) =
        match reply with
        | Ok _     -> failwith "Cannot cast failure of successful reply."
        | Expect e -> Expect e
        | Error e  -> Error e

    /// Returns an 'Expect' error with the given string and span.
    let inline expected s span = Expect [ s, span ]

    /// Returns the current token from the stream, or 'End' if the end has been reached.
    let inline current (s: TokenStream) =
        let pos = s.Position
        let tokens = s.Tokens

        assert(pos >= 0)

        if pos >= tokens.Length then
            { Kind = End ; Span = Span.Default ; Indent = 0 }
        else
            tokens.[pos]

    /// Returns the next token from the stream, or 'End' if the end has been reached.
    let inline tok (s: TokenStream) =
        let pos = s.Position
        let tokens = s.Tokens

        assert(pos >= 0)

        if pos >= tokens.Length then
            { Kind = End ; Span = Span.Default ; Indent = 0 }
        else
            tokens.[pos]

    let inline position (s: TokenStream) = Ok (current s).Span.Start

    let inline span (pos: int) (s: TokenStream) =
        let s = (current s).Span
        Ok { Start = pos ; Length = s.End - pos ; File = s.File }

    let inline scope (s: TokenStream) = Ok s.Scope
    let inline binder (s: TokenStream) = Ok s.Scope.Binder

    let inline stream (s: TokenStream) = Ok s
    let inline state (s: TokenStream) = Ok s.State
    let inline restore st (s: TokenStream) = s.Restore st ; Ok ()
