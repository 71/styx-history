namespace Styx

/// Defines the state of a symbol or expression.
[<Struct>]
type BindingState =
    /// Fully bound and valid.
    | Bound

    /// Waiting for more data.
    | Unbound

with
    /// Returns a new state that corresponds to the addition of the two given states.
    static member (+) (a, b) =
        match (a, b) with
        | Bound, Bound -> Bound
        | _            -> Unbound

/// Defines an object that has a binding state.
type IBindable =
    abstract State : BindingState

[<AutoOpen>]
module State =

    /// Combines multiple states together.
    let combine (states: BindingState seq) =
        if Seq.forall ((=) Bound) states then
            Bound
        else
            Unbound

    /// Returns the state of multiple bindable objects.
    let state<'T when 'T :> IBindable>(bindables: 'T seq) =
        combine <| Seq.map (fun (x: 'T) -> x.State) bindables
