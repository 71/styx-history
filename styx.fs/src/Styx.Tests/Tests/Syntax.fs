module Syntax

open FsUnit
open FsUnitTyped
open NUnit.Framework

open Styx
open Styx.Expressions
open Styx.Library
open Styx.Reactive
open Styx.Text

staticallyInitialize()

let parse (parser: Parser<_>) (s: string) =
    let binder = Binder.Create()
    let stream = TokenStream.Create(binder, s)
    let parsed = parser stream

    parsed

let parseAny = parse binaryend

let inline ok parser s =
    match parse parser s with
    | Ok r -> r
    | _ -> failwith "Expected successfully parsed expression."

let inline ok' reply =
    match reply with
    | Ok r -> r
    | _ -> failwith "Expected successful reply."

let inline shouldBe<'T> (v: obj) =
    match v with
    | :? 'T as t -> t
    | _ -> failwithf "Expected expression of type %s." ((typeof<'T>).ToString())

let inline shouldBe'<'T> (v: obj) = shouldBe<'T>(v) |> ignore


[<TestOf("Styx.Text.Core")>]
module ``Core tests`` =

    let inline lit s = ok literal s

    [<Test>]
    let ``should parse literals``() =
        (lit "24").Value    |> should equal 24
        (lit "2.625").Value |> should equal 2.625
        (lit "true").Value  |> should equal true
        (lit "false").Value |> should equal false
        (lit "'c'").Value   |> should equal 'c'


[<TestOf("Styx.Text.Expressions")>]
module ``Expressions tests`` =

    [<Test>]
    let ``should parse if-then-else expressions``() =
        let r = parse conditional "if true then 5 else 4"
        r.Success |> shouldEqual true


[<TestOf("Styx.Text.Operators")>]
module ``Operators tests`` =

    let inline bin s = shouldBe<Binary> (ok binaryend s)
    let inline lv (lit: Literal) = lit.Value

    [<Test>]
    let ``should parse binary operators``() =
        do
            let test = (parse binaryend "let a = 2 in a * a")

            let test = shouldBe<Binding> (ok' test)

            test.Bindings.Length |> shouldEqual 1
            test.Body |> shouldBe'<Binary>

        do
            let test = bin "1 + 3"
            (test.Left |> shouldBe<Literal>).Value |> should equal 1
            (test.Right |> shouldBe<Literal>).Value |> should equal 3

            test.Operator.ToString() |> shouldEqual "+"

        do
            let test = bin "1 + 2 * 3 + 1"            // ( 1 + (2 * 3) ) + 1

            (test.Right |> shouldBe<Literal>).Value |> should equal 1
            
            let test = test.Left |> shouldBe<Binary>  // 1 + (2 * 3)

            (test.Left  |> shouldBe<Literal>).Value |> should equal 1
            
            let test = test.Right |> shouldBe<Binary> // 2 * 3

            (test.Left  |> shouldBe<Literal>).Value |> should equal 2
            (test.Right |> shouldBe<Literal>).Value |> should equal 3

        do
            let test = bin "1 + 2 / 3 ^ 4 - 5"      // ( 1 + (2 / 3) ) ^ (4 - 5)

            let le = test.Left  |> shouldBe<Binary> //   1 + (2 / 3)
            let ri = test.Right |> shouldBe<Binary> //                    4 - 5

            le.Left  |> shouldBe<Literal> |> lv |> should equal 1

            ri.Left  |> shouldBe<Literal> |> lv |> should equal 4
            ri.Right |> shouldBe<Literal> |> lv |> should equal 5

            let lr = le.Right |> shouldBe<Binary>   //        2 / 3

            lr.Left  |> shouldBe<Literal> |> lv |> should equal 2
            lr.Right |> shouldBe<Literal> |> lv |> should equal 3

    [<Test>]
    let ``should override precedence in parenthesized expressions``() =
        let test = bin "(1 + 2) * 3"

        test.Left  |> shouldBe<Binary>  |> fun x ->
           x.Left  |> shouldBe<Literal> |> lv |> should equal 1
           x.Right |> shouldBe<Literal> |> lv |> should equal 2
        test.Right |> shouldBe<Literal> |> lv |> should equal 3


[<TestOf("Styx.Text.Toplevel")>]
module ``Top-level tests`` = ()


[<TestOf("Styx.Text.Patterns")>]
module ``Pattern-matching tests`` = ()
