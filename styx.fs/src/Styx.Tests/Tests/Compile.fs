module Compile

open FsUnit
open FsUnitTyped
open NUnit.Framework

open Styx
open Styx.Library
open Styx.Text
open System
open System.Reflection.Emit

[<TestOf("Styx.Core")>]
module ``Simple expressions tests`` =

    let mutable binder : Binder = Binder.Create()

    let parse (sc: Scope) (s: string) = binaryend <| (TokenStream.Create(sc, s))
    let compute s = match parse binder s with
                    | Ok x ->
                        let ty = x.Type.ILType
                        let mb = DynamicMethod("test", ty, Type.EmptyTypes)
                        let il = mb.GetILGenerator()

                        x.Compile(il)

                        il.Emit(OpCodes.Ret)

                        mb.Invoke(null, [||])

                    | err -> raise(Exception(err.ToString()))

    [<OneTimeSetUp>]
    do
        staticallyInitialize()

    [<SetUp>]
    do
        binder <- Binder.Create()

    [<Test>]
    let ``should compile correct literals``() =
        compute "50" |> should equal 50
        compute "true" |> should equal true

        compute "true && false" |> should equal false
        compute "1 + 2" |> should equal 3
