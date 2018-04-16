namespace Styx.Reactive

open System
open System.Collections
open System.Collections.Generic

/// Defines a sealable and observable list.
type ObservableList<'T>() as this =
    inherit Sealable<'T>()

    let list = List()

    let ensureNotSealed() = if this.IsSealed then
                                raise(InvalidOperationException("Cannot manipulate a sealed list."))

    override __.Next(v) =
        ensureNotSealed()

        list.Add(v)
        base.Next(v)

    override __.Subscribe(sub) =
        if not sub.IsSealed && not this.IsSealed then
            for elem in list do
                sub.Next(elem)

        base.Subscribe(sub)

    /// Adds the given element to the list.
    member __.Add(element) = this.Next(element)

    /// Adds the given elements to the list.
    member __.AddRange(elements: seq<_>) =
        ensureNotSealed()

        match elements with
        | :? IReadOnlyCollection<'T> as c -> list.Capacity <- list.Count + c.Count
        | _ -> ()

        for element in elements do
            list.Add(element)
            base.Next(element)

    interface IEnumerable with
        override __.GetEnumerator() = list.GetEnumerator() :> IEnumerator

    interface IEnumerable<'T> with
        override __.GetEnumerator() = list.GetEnumerator() :> IEnumerator<'T>

