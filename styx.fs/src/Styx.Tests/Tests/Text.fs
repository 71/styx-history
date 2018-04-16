module Text

open FsUnit
open FsUnitTyped
open NUnit.Framework

open Styx
open Styx.Library
open Styx.Text
open System.Collections.Generic
open System.Text


[<TestOf("Styx.Text.Lexer")>]
module ``Lexer tests`` =

    let newStream(s: string) =
        TokenStream.Create(Binder.Create(), s)

    let span (s: string) =
        let stream = newStream(s)
        (tok stream).Span

    let indent (s: string) =
        let stream = newStream(s)
        (tok stream).Indent

    let token (s: string) =
        let stream = newStream(s)
        (tok stream).Kind

    let tokens (s: string) =
        let stream = newStream(s)

        let ltokens = List<TokenKind>()
        let mutable parse = true

        while parse do
            let token = tok stream

            if token.Kind = End then
                parse <- false
            else
                stream.Position <- stream.Position + 1
                ltokens.Add(token.Kind)

        ltokens.ToArray()

    [<Test>]
    let ``should return correct spans``() =
        (span "'a'").Start   |> shouldEqual 0
        (span "'a'").Length  |> shouldEqual 3
        (span "  45 ").Start |> shouldEqual 2
        (span "  45 ").End   |> shouldEqual 4

    [<Test>]
    let ``should return correct indents``() =
        indent "a"   |> shouldEqual 0
        indent " a"  |> shouldEqual 1
        indent "  a" |> shouldEqual 2

    [<Test>]
    let ``should skip comments correctly``() =
        let result = [| (Ident "foo"); (Ident "bar") |]

        tokens "{- Hello world -} foo bar" |> shouldEqual result
        tokens "foo {- Hello world -} bar" |> shouldEqual result
        tokens "foo bar -- a comment"      |> shouldEqual result

        (tokens "--  Hello").Length |> shouldEqual 0
        (tokens "--| Hello").Length |> shouldEqual 1
        (tokens "--^ Hello").Length |> shouldEqual 1
        (tokens "--@ Hello").Length |> shouldEqual 0


    [<Test>]
    let ``should lex tokens correctly``() =
        token "aaa" |> shouldEqual (Ident "aaa")
        token "+!+" |> shouldEqual (Ident "+!+")

        token " `foo bar!` " |> shouldEqual (Ident "foo bar!")
        
        (token """ "Hello \"world\".\n" """).ToString() |> shouldEqual <| (Str <| StringBuilder "Hello \"world\".\n").ToString()

        tokens "1 2 3" |> shouldEqual
            [| (Integer 1); (Integer 2); (Integer 3) |]

        tokens "b+a" |> shouldEqual
            [| (Ident "b"); (Ident "+"); (Ident "a") |]

        tokens "1+1+a" |> shouldEqual
            (let one = Integer 1
             let plus = Ident "+"
             [| one; plus; one; plus; (Ident "a") |])

