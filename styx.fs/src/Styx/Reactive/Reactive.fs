namespace Styx.Reactive

open Styx
open Styx.Text
open System
open System.Collections.Generic

type Hole = { Message : string ; Span : Span }

/// Defines a type that can report errors.
type Reporter() =
    let holes = List<Hole>()

    member __.Holes = holes :> IReadOnlyList<_>

    /// Reports the given hole.
    member __.Hole(hole) = holes.Add(hole)


/// Defines a type that can be sealed.
type ISealable<'T> =
    abstract Seal : Reporter -> unit
    abstract Next : 'T -> unit

    abstract IsSealed : bool

    abstract Subscribe : ISealable<'T> -> IDisposable

[<AbstractClass>]
type Sealable<'T>() =
    let mutable isSealed = false
    let subs = List<ISealable<'T>>()

    member __.IsSealed = isSealed

    abstract Seal : Reporter -> unit

    default __.Seal(r) = if not isSealed then
                            isSealed <- true

                            for sub in subs do sub.Seal(r)

    abstract Next : 'T -> unit

    default __.Next(v) = for sub in subs do sub.Next(v)

    abstract Subscribe : ISealable<'T> -> IDisposable

    default __.Subscribe(sub) = subs.Add(sub)
                                { new IDisposable with override __.Dispose() = subs.Remove(sub) |> ignore }

    interface ISealable<'T> with
        override x.IsSealed = x.IsSealed
        override x.Seal(r) = x.Seal(r)
        override x.Next(v) = x.Next(v)
        override x.Subscribe(sub) = x.Subscribe(sub)

    interface IObserver<'T> with
        override x.OnNext(v) = x.Next(v)
        override __.OnError(_) = ()
        override __.OnCompleted() = ()


[<AutoOpen>]
module SealableUtilities =

    let sealedEvent(r) =
        { new ISealable<'T> with
            override __.IsSealed = true

            override __.Subscribe(sub) =
                sub.Seal(r)
                
                { new IDisposable with override __.Dispose() = () }

            override __.Seal(_) = ()
            override __.Next(_) = ()
        }

    let singletonEvent(v) =
        { new Sealable<'T>() with
            override __.Subscribe(sub) =
                sub.Next(v)
                base.Subscribe(sub)
        }

    let seqEvent(vs) =
        { new Sealable<'T>() with
            override __.Subscribe(sub) =
                for v in vs do sub.Next(v)
                base.Subscribe(sub)
        }

    type ISealable<'T> with
        member this.Subscribe(next: 'T -> unit, seal: Reporter -> unit) =
            let mutable isSealed = false
            let subs = List()

            this.Subscribe({ new ISealable<'T> with
                override __.IsSealed = isSealed
                override __.Next(v) = next(v)
                override __.Seal(r) = seal(r)
                override __.Subscribe(sub) =
                    subs.Add(sub)
                    { new IDisposable with override __.Dispose() = subs.Remove(sub) |> ignore }
            })
            
        member this.Subscribe(next: 'T -> unit) = this.Subscribe(next, ignore)

        member this.SubscribeTo(source: ISealable<'T>) = source.Subscribe(this)
