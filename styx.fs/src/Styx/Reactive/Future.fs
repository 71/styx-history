namespace Styx.Reactive

open Styx
open System
open System.Collections.Generic
open System.Runtime.CompilerServices


/// Represents a value that may be resolved in the future.
type Future<'T when 'T :> IBindable>() =
    inherit Sealable<'T>()

    /// Gets the value of the future, if it has one.
    member val Value : 'T option = None with get, set 

    /// Gets a boolean that indicates whether the future has a value.
    member this.HasValue = this.Value.IsSome

    interface IBindable with
        member this.State =
            match this.Value with
            | Some v -> v.State
            | None -> Unbound


    /// Returns a future that always returns the given value.
    static member From(v) =
        let future = Future()
        future.Value <- Some v
        future


/// Represents a future that gets its value from an observable sequence and
/// a score function.
type ObservingFuture<'T when 'T :> IBindable>(sequence: ISealable<'T>, score: ('T -> int)) as this =
    inherit Future<'T>()

    let mutable valueScore = -1

    do sequence.Subscribe((fun x ->
        let s = score(x)

        if s > valueScore then
            valueScore <- s
            this.Value <- Some x
            this.Next(x)), this.Seal) |> ignore

    /// Gets the sequence to which this future is subscribed.
    member __.Sequence = sequence


/// Represents a future whose value is the last returned item of the given sequence.
type BackingFuture<'T when 'T :> IBindable>(sequence: ISealable<'T>) as this =
    inherit Future<'T>()

    do sequence.Subscribe((fun x -> this.Value <- Some x ; this.Next(x)), this.Seal) |> ignore

type CollectingFuture<'T>() =
    inherit Sealable<'T>()

    let list = List<'T>()

    override __.Next(v) = list.Add(v)
                          base.Next(v)

    override __.Subscribe(sub) = for item in list do sub.Next(item)
                                 base.Subscribe(sub)


[<Extension>]
type FutureExtensions =
    /// Transforms the observable sequence into a future that selects
    /// elements based on the given scoring method.
    [<Extension>]
    static member inline ToFuture<'T when 'T :> IBindable>(obs: ISealable<'T>, score) =
        ObservingFuture(obs, score) :> Future<_>

    /// Transforms the observable sequence into a future that selects
    /// elements in the order in which they arrive.
    [<Extension>]
    static member inline ToFuture<'T when 'T :> IBindable>(obs: ISealable<'T>) =
        ObservingFuture(obs, fun _ -> -1) :> Future<_>

    /// Collects all elements returned by the given 
    [<Extension>]
    static member Collect<'T when 'T :> IBindable>(inner: ISealable<'T>) =
        let future = CollectingFuture()
        inner.Subscribe(future) |> ignore
        future

    /// Returns a future that only resolves when the items returned by the given
    /// sealable stream match the given predicate.
    [<Extension>]
    static member Where<'T when 'T :> IBindable>(inner: ISealable<'T>, filter: 'T -> bool) =
        let future = { new CollectingFuture<'T>() with
            override __.Next(v) = if filter v then base.Next(v)
        }

        inner.Subscribe(future) |> ignore
        future

    [<Extension>]
    static member Concat<'T>(a: ISealable<'T>, b: ISealable<'T>) =
        let future = CollectingFuture()

        a.Subscribe(future) |> ignore
        b.Subscribe(future) |> ignore
        future

[<AutoOpen>]
module FutureUtils =
    /// Maps the result of the given future to a function.
    let inline (>>=) (inner: ISealable<_>) f =
        let future = Future<'T>()

        inner.Subscribe(f >> future.Next, future.Seal) |> ignore

        future
