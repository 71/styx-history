namespace Styx.Expressions

open Styx
open System.Reflection
open System.Reflection.Emit

[<AbstractClass>]
type TypeDeclaration(binder, span) =
    inherit Meta(binder, span)

    override this.Execute(binder) =
        this.BuildType(binder) |> ignore

    abstract BuildType : Styx.Binder -> TypeBuilder

type SizedType(size: int, binder, span) =
    inherit TypeDeclaration(binder, span)

    override __.State = Bound

    override __.BuildType(binder) =
        binder.Compiler.Module.DefineType(
            span.DefinitionString,
            TypeAttributes.Public ||| TypeAttributes.Sealed,
            typeof<obj>, size)

type EnumType(span) = class end
