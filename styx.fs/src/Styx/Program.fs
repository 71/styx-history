module Program

open Styx
open Styx.Library
open Styx.Reactive
open Styx.Text
open System
open System.Reflection
open System.Reflection.Emit

let parse scope (s: string) =
    for t in tokenize("<stdin>", s) do
        printf "'%O' " t.Kind
    printfn ""

    if s.[0] = ':' then
        toplevel <| TokenStream.Create(scope, s.Substring 1)
    else
        binaryend <| TokenStream.Create(scope, s)

let compute sc s = match parse sc s with
                   | Ok x ->
                        match x.State with
                        | Unbound -> false, "Could not resolve whole expression." :> obj
                        | Bound ->
                            let mb = DynamicMethod("test", x.Type.ILType, Type.EmptyTypes)
                            let il = mb.GetILGenerator()

                            x.Compile(il)

                            il.Emit(OpCodes.Ret)

                            try
                                true, mb.Invoke(null, [||])
                            with
                            | :? TargetInvocationException as x ->
                                false, x.InnerException :> obj

                   | err -> false, err.ToString() :> obj

/// Entry point for the REPL.
[<EntryPoint>]
let main _ =
    staticallyInitialize()

    // REPL-like
    let binder = Binder.Create()

    while true do
        Console.Write("?> ")

        let input = Console.ReadLine()

        if input = "exit" || input = "" then
            exit 0

        let scope = Scope(Some binder, Some (binder :> _), "repl", input)
        let success, result = compute scope input

        if success && not <| isNull result then
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine("-> {0}", result)
        else
            let reporter = Reporter()

            scope.Functions.Seal(reporter)
            scope.Types.Seal(reporter)

            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine("!> {0}", result)
            Console.ForegroundColor <- ConsoleColor.Yellow

            for hole in reporter.Holes do
                Console.WriteLine("~> [{0}] {1}", hole.Span, hole.Message)

        Console.ResetColor()
        Console.WriteLine()

    0
