namespace Styx.Library

open Styx
open Styx.Expressions
open Styx.Reactive
open Styx.Text
open System
open System.Reflection

[<AutoOpen>]
module Native =
    open System.Reflection.Emit

    type Constant(ty, value: obj) =
        inherit Expr(Span.Default)

        override __.State = Bound

        override __.Resolve() = Some ty

        override __.Compile(il) =
            il.EmitConstant(value)

    type NativeCall(ty, fn: MethodInfo, args: Expr array) =
        inherit Expr(Span.Default)

        override __.State = Bound

        override __.Resolve() = Some ty

        override __.Compile(ilg) =
            for arg in args do
                arg.Compile(ilg)

            ilg.Emit(OpCodes.Call, fn)

module Bridge =

    type Binder = Styx.Binder

    let private getName = function
    | "op_BooleanAnd" -> "&&"
    | "op_BooleanOr"  -> "||"
    | "op_Equality"   -> "=="
    | "op_Inequality" -> "<>"
    | "op_Addition"   -> "+"
    | "op_Subtraction"-> "-"
    | "op_Multiply"   -> "*"
    | "DivInteger"    -> "//"
    | "Div"           -> "/"
    | "PowInteger"    -> "^"
    | n -> n

    /// Converts a .NET type into a Styx type.
    let styxifyType(binder: Binder, ty: Type) =
        { Name = ty.Name ; Namespace = ty.Namespace ; Properties = [] ; ILType = ty }

    /// Converts a .NET method into a Styx function.
    let styxifyFunction(binder: Binder, method: MethodInfo) =
        let plen = method.GetParameters().Length
        let arguments  = Array.zeroCreate plen
        let parameters = Array.zeroCreate plen

        for i = 0 to plen - 1 do
            let param = method.GetParameters().[i]
            let styxType = styxifyType(binder, param.ParameterType)

            let parameter = { Name = param.Name ; Index = param.Position ; 
                              Component = Component.Type(styxType, [])   }

            arguments.[i]  <- Argument(parameter, styxType.Instance(), Span.Default) :> Expr
            parameters.[i] <- parameter

        let rty = Constant(Types.TypeInstance.Instance(), styxifyType(binder, method.ReturnType).Instance())
        let sign = Signature(method.DeclaringType.FullName, getName method.Name, Array.empty, parameters, rty)

        Function(sign, [ NativeCall(Types.Boolean.Instance(), method, arguments) ])

    open Microsoft.FSharp.Quotations.Patterns

    /// Converts the target of the quoted call to a Styx function.
    let rec styxifyTarget(binder: Binder, [<ReflectedDefinition>] del: Quotations.Expr) =
        match del with
        | Call(_, method, _) -> styxifyFunction(binder, method)
        | Lambda(_, body) -> styxifyTarget(binder, body)
        | _ -> failwith "Invalid expression."
