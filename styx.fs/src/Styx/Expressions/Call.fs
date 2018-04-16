namespace Styx.Expressions

open Styx
open Styx.Reactive
open System.Collections.Generic
open System.Reflection.Emit


type Call(fn: Future<Function>, args: Expr list, span) as this =
    inherit Expr(span)

    let arguments = List.toArray args

    do (fn >>= fun _ -> this :> Expr).Subscribe(this) |> ignore

       for arg in args do
           arg.Subscribe(this) |> ignore

    member __.Function = fn

    member __.Arguments = arguments :> IReadOnlyList<_>

    override __.State = (fn :> IBindable).State + state args


    override __.Resolve() =
        match fn.Value with
        | None -> None
        | Some v ->
            let types = Array.zeroCreate arguments.Length
            let mutable i = 0

            while i < types.Length do
                let arg = arguments.[i]

                match arg.State with
                | Bound   -> types.[i] <- arg.Type
                | Unbound -> i <- arguments.Length
                
                i <- i + 1

            if i = types.Length then
                Some <| v.Signature.GetReturnType(types, Verifier()).Value
            else
                None

    override this.Compile(ilg) =
        if this.IsBound then
            let types = Array.zeroCreate args.Length
            let mutable i = 0

            for arg in args do
                types.[i] <- arg.Type
                arg.Compile(ilg) |> ignore
                i <- i + 1

            ilg.Emit(OpCodes.Call, fn.Value.Value.GetIL(types))
        else
            ilg.ThrowHole()

    override __.ToString() = fn.ToString()

type Argument(parameter: Parameter, ty, span) =
    inherit Expr(span)

    member __.Parameter = parameter

    override __.Resolve() = Some ty

    override __.State = (ty :> IBindable).State

    override __.Compile(ilg) =
        ilg.Emit(OpCodes.Ldarg, parameter.Index)
