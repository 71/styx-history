namespace rec Styx

open Styx.JIT
open Styx.Reactive
open System
open System.Collections.Generic
open System.Linq
open System.Reflection

type BindingResult = Success
                   | Split of Scope
                   | Error

/// Defines a lexical scope used during binding.
type Scope internal(binder: Binder option, parent: Scope option, name: string, input: string) as this =
    let functions = ObservableList<Function>()
    let types = ObservableList<TypeDefinition>()

    let binder = match binder with | Some b -> b | None -> this :?> _
    let parent = match parent with | Some p -> p | None -> this

    /// Gets the functions defined and to be defined by this binder.
    member __.Functions = functions

    /// Gets the types defined and to be defined by this binder.
    member __.Types = types

    member __.AllFunctions =
        if LanguagePrimitives.PhysicalEquality this parent then
            functions :> ISealable<_>
        else
            (functions :> ISealable<_>).Concat(parent.AllFunctions) :> _

    member __.AllTypes =
        if LanguagePrimitives.PhysicalEquality this parent then
            types :> ISealable<_>
        else
            (types :> ISealable<_>).Concat(parent.AllTypes) :> _

    /// Gets the enclosing scope.
    member __.EnclosingScope = parent

    /// Gets the binder to which this scope belongs.
    member __.Binder = binder

    member __.IsRoot = not <| LanguagePrimitives.PhysicalEquality this parent

    /// Gets the name of the input.
    member __.InputName = name

    /// Gets the content of the input.
    member __.Input = input

    /// Runs the scope on the given input.
    member __.Run() = failwith "Not implemented."

    /// Gets the name of the scope, which is the namespace in which
    /// defined symbols will be placed.
    member val Namespace = "" with get, set

    /// Gets the visibility of the scope, which is the visibility that all
    /// defined functions will have.
    member val Visibility = Public with get, set

    member __.LookupFunction(span, ns, name, [<ParamArray>] args) =
        FunctionLookupFuture(this.AllFunctions, span, ns, name, args) :> Future<_>

    member __.LookupFunction(span, fullname: string, [<ParamArray>] args) =
        let split = fullname.LastIndexOf('.')

        if split = -1 then
            FunctionLookupFuture(this.AllFunctions, span, "", fullname, args) :> Future<_>
        else
            let ns = fullname.Substring(0, split - 1)
            let name = fullname.Substring(split + 1)

            FunctionLookupFuture(this.AllFunctions, span, ns, name, args) :> Future<_>

    member __.LookupType(ns, name) =
        TypeLookupFuture(this.AllTypes, ns, name) :> Future<_>

    member __.LookupType(fullname: string) =
        let split = fullname.LastIndexOf('.')

        if split = -1 then
            TypeLookupFuture(this.AllTypes, "", fullname) :> Future<_>
        else
            let ns = fullname.Substring(0, split - 1)
            let name = fullname.Substring(split + 1)

            TypeLookupFuture(this.AllTypes, ns, name) :> Future<_>

    member __.DefineFunction(fn: Function) =
        if this.Visibility = Public && not this.IsRoot then
            parent.DefineFunction(fn)
        else
            functions.Add(fn)

    member __.DefineType(ty: TypeDefinition) =
        if this.Visibility = Public && not this.IsRoot then
            parent.DefineType(ty)
        else
            types.Add(ty)

/// Represents the result of a binding operation.
type Output = { Success  : bool
              ; Reporter : Reporter
              }

/// Represents a structure that stores the state of the binding operation.
type Binder internal() as this =
    inherit Scope(None, None, "global", "")

    /// Gets the emitter used to emit JIT data.
    member val Emitter = new Emitter()

    /// Gets the compiler used to emit IL data.
    member val Compiler = Compiler()

    interface IDisposable with
        /// Disposes of the binder, freeing all its allocated memory.
        member this.Dispose() = (this.Emitter :> IDisposable).Dispose()

    member __.GetType(ty: System.Type) =
        this.Types.First(fun x -> x.ILType = ty)

    member this.Bind([<ParamArray>] inputs: (string * string)[]) =
        if inputs.Length = 0 then
            raise <| ArgumentException("Cannot bind empty input.")

        let queue = Queue()
        let reporter = Reporter()

        for name, content in inputs do
            queue.Enqueue <| Scope(Some this, Some (this :> _), name, content)

        let mutable success = true

        while queue.Count > 0 do
            let scope = queue.Dequeue()
            
            match scope.Run() with
            | Success -> ()
            | Split s -> queue.Enqueue(s) ; queue.Enqueue(scope)
            | Error   -> success <- false

        this.Functions.Seal(reporter)
        this.Types.Seal(reporter)

        { Success = success ; Reporter = reporter }
