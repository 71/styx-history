namespace Styx.Reactive

open Styx
open System

type internal FunctionLookupFuture(sequence: ISealable<Function>, span, ns: string, name: string, [<ParamArray>] args: Expr list) =
    inherit BackingFuture<Function>(sequence.Where(fun x -> x.Name = name))

    member __.Namespace = ns

    member __.Name = name

    member __.Arguments = args

    override this.Seal(r) =
        if not this.HasValue then
            r.Hole({ Message = sprintf "Could not find function '%s'." name ; Span = span })

    override __.ToString() =
        if ns = "" then name else sprintf "%s.%s" ns name

type internal TypeLookupFuture(sequence: ISealable<TypeDefinition>, ns: string, name: string) =
    inherit BackingFuture<TypeDefinition>(sequence.Where(fun x -> x.Name = name))

    member __.Namespace = ns

    member __.Name = name

    override __.ToString() =
        if ns = "" then name else sprintf "%s.%s" ns name
