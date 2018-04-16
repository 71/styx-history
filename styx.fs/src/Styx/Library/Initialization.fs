namespace Styx.Library

open Styx
open Styx.Text

[<AutoOpen>]
module Initialization =

    open Functions
    open Types

    let private AllTypes = [| Void ; Never ; String ; Boolean ; Char ; Int ; UInt ; Double |]

    let private AllFunctions b =
        [| LogicalAnd b ; LogicalOr b ; LogicalXor b ; Plus b ; Minus b
         ; Times b ; DivI b ; DivF b ; Pow b ; Exit b
        |]

    /// Statically initializes the Styx library.
    let staticallyInitialize() =
        Styx.Compiler |> ignore
        ToplevelSyntax.toplevelRef |> ignore

    /// Returns the given binder with its built-in functions and types populated.
    let private initialized(binder: Binder) =
        binder.Functions.AddRange(AllFunctions binder)
        binder.Types.AddRange(AllTypes)
        binder

    type Binder with
        /// Creates a new binder with an empty state.
        static member Create() = new Binder() |> initialized

