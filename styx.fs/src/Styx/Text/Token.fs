namespace Styx.Text

/// Defines the span of a Styx token or expression.
open System.Text
[<Struct>]
type Span = { Start : int ; Length : int ; File : string }
with
    member this.End = this.Start + this.Length

    member this.DefinitionString = this.ToString()

    override this.ToString() = sprintf "%s %d:%d" this.File this.Start (this.Start + this.Length)

    static member Default = { Start = 0; Length = 0 ; File = "<unknown>" }

/// Defines the kind of a token of Styx code.
[<Struct>]
type TokenKind = LParen
               | RParen
               | Comma
               | Semicolon
               | Backslash
               | Ident   of w: string
               | Integer of i: int
               | Real    of d: double
               | Char    of c: char
               | Str     of s: StringBuilder      // https://github.com/Microsoft/visualfsharp/issues/1678
               | Doc     of doc: string * k: char
               | End
               | Error
with
    override this.ToString() =
        match this with
        | LParen -> "("
        | RParen -> ")"
        | Comma  -> ","
        | Semicolon -> ";"
        | Backslash -> "\\"
        | Ident s   -> s
        | Integer i -> i.ToString()
        | Real r    -> r.ToString()
        | Char c    -> sprintf "'%O'" c
        | Str s     -> sprintf "\"%O\"" s
        | _ -> ""

/// Defines a token of Styx code.
[<Struct>]
type Token = { Kind : TokenKind ; Span : Span ; Indent : int }
with
    override this.ToString() = sprintf "[%O] %O" this.Span this.Kind

[<AutoOpen>]
module TextUtils =
    let inline (>!>) (start : Span) (en : Span)
        = { Start = start.Start; Length = en.End - start.Start ; File = start.File }
