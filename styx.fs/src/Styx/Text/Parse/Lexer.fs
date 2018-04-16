namespace Styx.Text

open System
open System.Collections.Immutable
open System.Text

[<AutoOpen>]
module Lexer =

    [<Literal>]
    let EndChar = '\x00'

    let inline private ch (s: string) (position: int) (len: int) =
        assert(position >= 0)

        if position >= len then
            EndChar
        else
            s.[position]

    let inline private read (s: string) (position: int ref) (len: int) =
        let c = ch s !position len
        let pos = 1 + !position

        assert(pos > 0)

        position := min len pos

        c

    let inline private peekBy (by: int) (s: string) (position: int) (len: int) =
        let pos = by + position

        assert(pos >= 0)

        if pos >= len then
            EndChar
        else
            s.[pos]

    let inline private peek s = peekBy 0 s

    let inline private skipBy (by: int) (position: int) (len: int) =
        let pos = by + position

        assert(pos >= 0)

        if pos >= len then
            len
        else
            pos

    let inline private skip p = skipBy 1 p

    let inline isEnd pos len =
        assert(pos >= 0)

        pos >= len

    type private Lex() =
        static member SkipWhitespace(s: string, position: int, indent: int byref, indenting: bool, len: int) =
            match ch s position len with
            | '\n' -> indent <- 0
                      Lex.SkipWhitespace(s, position + 1, &indent, true, len)
            | ' '  -> if indenting then
                        indent <- indent + 1
                        Lex.SkipWhitespace(s, position + 1, &indent, true, len)
                      else
                        Lex.SkipWhitespace(s, position + 1, &indent, false, len)
            
            | '\r' -> Lex.SkipWhitespace(s, position + 1, &indent, indenting, len)
            | _    -> position

        static member TillNextLine(s: string, position: int, len: int) =
            match ch s position len with
            | EndChar | '\n' -> position
            | _ -> Lex.TillNextLine(s, position + 1, len)

        /// Lexes a token.
        static member RawToken (s, pos: int ref, indent: int byref, len: int, file: string) =
            pos := Lex.SkipWhitespace(s, !pos, &indent, (!pos = 0), len)

            let start = !pos
            let c = read s pos len

            if c = '{' && peek s !pos len = '-' then
                // multi-line comment
                pos := skipBy 2 !pos len

                while not (isEnd !pos len) && read s pos len <> '-' || read s pos len <> '}' do ()

                Lex.RawToken(s, pos, &indent, len, file)
            else
            let k =
                if c = '-' && (peek s !pos len) = '-' then
                    // one-line comment
                    pos := !pos + 1

                    let id = read s pos len
                    pos := Lex.TillNextLine(s, !pos, len)

                    match id with
                    | '^' | '|' -> Doc(s.Substring(start + 3, !pos - start - 3), id)

                    | _ -> (Lex.RawToken(s, pos, &indent, len, file)).Kind

                else
                    match c with
                    | EndChar -> End

                    | ','  -> Comma
                    | ';'  -> Semicolon
                    | '('  -> LParen
                    | ')'  -> RParen
                    | '\\' -> Backslash

                    | ch when ch >= '0' && ch <= '9' ->
                        // number
                        let mutable float = false
                        let mutable hex = false
                        let mutable bin = false

                        let str = StringBuilder(ch.ToString())
                        let mutable parse = true

                        while parse do
                            match peek s !pos len with
                            | '_' -> () // support for underscores in number literals

                            | '.' when not bin && not hex && not float -> str.Append('.') |> ignore; float <- true
                            | 'x' when not bin && not hex && str.Length = 1 -> str.Clear() |> ignore ; hex <- true
                            | 'b' when not bin && not hex && str.Length = 1 -> str.Clear() |> ignore ; bin <- true

                            | ch when bin && ch = '0' || ch = '1' -> str.Append(ch) |> ignore       // binary

                            | ch when not bin && ch >= '0' && ch <= '9' -> str.Append(ch) |> ignore // digit

                            | ch when hex && (ch >= 'A' && ch <= 'F')                               // hexadecimal
                                          || (ch >= 'a' && ch <= 'f') -> str.Append(ch) |> ignore   //

                            | _ -> parse <- false

                            if parse then pos := !pos + 1

                        let str = str.ToString()
                        
                        if float then
                            Real(Convert.ToDouble(str))
                        else
                            Integer(Convert.ToInt32(str, if hex then 16 else if bin then 2 else 10))

                    | ch when ch = '_' || Char.IsLetter ch ->
                        // word-like identifier
                        let str = StringBuilder(ch.ToString())
                        let mutable ch = peek s !pos len

                        while ch = '_' || ch = '-' || Char.IsLetterOrDigit ch do
                            pos := !pos + 1
                            str.Append(ch) |> ignore
                            ch <- peek s !pos len

                        Ident(str.ToString())

                    | ('"' | '\'' | '`') as terminator ->
                        // character or string
                        let mutable escaping = false
                        let mutable parse = true

                        let str = StringBuilder()

                        while parse do
                            if escaping then
                                escaping <- false
                                str.Append(match read s pos len with
                                           | 'n'  -> '\n'
                                           | 't'  -> '\t'
                                           | 'r'  -> '\r'
                                           | '0'  -> char 0x0
                                           | '"'  -> '"'
                                           | '\\' -> '\\'
                                           | _    -> char 0x0) |> ignore
                            else
                                match read s pos len with
                                | EndChar -> parse <- false
                                | term when term = terminator -> parse <- false

                                | '\\' -> escaping <- true

                                | ch -> str.Append(ch) |> ignore
                        
                        match terminator with
                        | '"' -> Str(str)
                        | '`' -> Ident(str.ToString())
                        |  _  -> TokenKind.Char(if str.Length > 0 then str.[0] else '#')

                    | ch ->
                        // operator-like indentifier
                        let str = StringBuilder(ch.ToString())
                        let mutable parse = true

                        while parse do
                            parse <- match peek s !pos len with
                                     | EndChar -> false
                                     | '(' | ')' | ',' | ';' | '"' | '\'' -> false
                                     | ch when Char.IsLetterOrDigit ch -> false
                                     | ch when Char.IsWhiteSpace ch -> false

                                     | ch -> str.Append(ch) |> ignore; true
                            
                            if parse then pos := !pos + 1
                        
                        Ident(str.ToString())

            let span = { Start = start ; Length = !pos - start ; File = file }

            { Span = span ; Kind = k ; Indent = int indent }

    /// Transforms a string into a stream of tokens.
    let tokenize (file: string, s: string) =
        let array = ImmutableArray.CreateBuilder()

        let mutable pos = ref 0
        let mutable indent = 0

        let len = s.Length
        let mutable token = Lex.RawToken(s, pos, &indent, len, file)

        while token.Kind <> End do
            array.Add token
            token <- Lex.RawToken(s, pos, &indent, len, file)

        array.ToImmutable()
