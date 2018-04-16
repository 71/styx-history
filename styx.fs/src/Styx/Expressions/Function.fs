namespace Styx.Expressions

open Styx
open Styx.Library
open Styx.Reactive
open System.Reflection
open System.Reflection.Emit

type private Binder = Styx.Binder

type Signatures(names: string list, constraints: Expr list, components: Expr list, span) as this =
    inherit Expr(span)

    let constraints = Array.ofList constraints
    let components = Array.ofList components

    do for expr in Seq.append constraints components do
        expr.Subscribe(this) |> ignore

    override __.Resolve() =
        if state constraints = Bound && state components = Bound then
            Some <| Types.TypeInstance.Instance()
        else
            None


    override __.Compile(il) =
        let parameters = seq {
            for i = 0 to components.Length - 2 do
                yield { Name = "a" ; Index = i ; Component = Dynamic components.[i] }
        }

        for name in names do
            let signature = Signature("", name, constraints, Array.ofSeq parameters, components.[components.Length - 1])

            il.EmitConstant(signature)


type FunctionBody(name: string, pattern: Pattern, body: Expr, span) as this =
    inherit Expr(span)

    do body.Subscribe(this) |> ignore

    override __.Resolve() = body.OptionalType

    override __.State = Unbound

    override __.Compile(_) = ()


/// Represents a variable in a signature expression.
type SignatureVariable(name: string, span) =
    inherit Expr(span)

    member __.Name = name

    override __.Resolve() = Some <| Types.TypeDefinition.Instance()

    override __.Compile(il) =
        il.Emit(Signature.MapOpCode)

        il.Emit(OpCodes.Ldstr, name)
        il.Emit(OpCodes.Callvirt, typeof<VariableMap>.GetProperty("Item").GetMethod)


/// Represents an expression that returns a type.
type TypeExpression(name: string, binder: Binder, args: Expr list, span) =
    inherit Expr(span)

    member __.Arguments = args

    member __.TargetName = name

    member val TargetDefinition : Future<TypeDefinition> =
        TypeLookupFuture(binder.Types, "", name) :> _

    override this.State = (this.TargetDefinition :> IBindable).State

    override __.Resolve() = Some <| Types.TypeInstance.Instance()

    override this.Compile(il) =

        let propValTy = typeof<PropertyValue>

        il.EmitConstant(this.TargetDefinition.Value)
        il.Emit(OpCodes.Newarr, propValTy)
        il.Emit(OpCodes.Dup)

        let fromMethod = typeof<Value>.GetMethod("From")
        let exactMethod = typeof<PropertyValue>.GetMethod("Exact")

        let mutable i = 0

        for arg in args do
            il.Emit(OpCodes.Dup)

            arg.Compile(il)                     // Stack: Type
            il.EmitConstant(Types.TypeInstance) // Stack: Type, Type
            il.Emit(OpCodes.Call, fromMethod)   // Stack: Value
            il.Emit(OpCodes.Call, exactMethod)  // Stack: PropertyValue
            
            il.Emit(OpCodes.Ldc_I4, i)          // Stack: PropertyValue, int
            
            il.Emit(OpCodes.Stelem, propValTy)  // Stack: /

            i <- i + 1

        il.Emit(OpCodes.Call, System.Type.GetType("Microsoft.FSharp.Collections.Array").GetMethod("toList"))
        il.Emit(OpCodes.Call, typeof<TypeDefinition>.GetRuntimeMethod("Instance", [| typeof<PropertyValue list> |]))
