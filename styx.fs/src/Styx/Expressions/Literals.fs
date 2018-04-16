namespace Styx.Expressions

open Styx
open Styx.Library
open Styx.Reactive
open Styx.Text
open System.Reflection.Emit

/// Represents a empty (or void) expression, also known as 'unit'.
type Empty(span) =
    inherit Expr(span)

    override __.State = Bound

    override __.Resolve() = Some Type.Void

    override __.Compile(_) = ()


/// Represents a constant value expression.
type Constant(value, ty, span) =
    inherit Expr(span)

    member __.Value = value

    override __.State = Bound

    override __.Resolve() = Some ty

    override __.Compile(il) = il.EmitConstant(value)


/// Represents a literal expression.
type Literal private(token: Token, ty: Type) =
    inherit Expr(token.Span)

    override __.State = Bound
    override __.Resolve() = Some ty

    override __.Compile(ilg) =
        match token.Kind with
        | Ident "true"  -> ilg.Emit(OpCodes.Ldc_I4_1)
        | Ident "false" -> ilg.Emit(OpCodes.Ldc_I4_0)
        | Str s         -> ilg.Emit(OpCodes.Ldstr, s.ToString())
        | Integer i     -> ilg.Emit(OpCodes.Ldc_I8, int64 i)
        | Real r        -> ilg.Emit(OpCodes.Ldc_R8, r)
        | Char c        -> ilg.Emit(OpCodes.Ldc_I4, int c)
                           ilg.Emit(OpCodes.Conv_U2)

        | _ -> () // Cannot happen, it's been checked before.

    member __.Value =
        match token.Kind with
        | Ident "true"  -> true  :> obj
        | Ident "false" -> false :> obj
        | Str s         -> s     :> obj
        | Integer i     -> i     :> obj
        | Real r        -> r     :> obj
        | Char c        -> c     :> obj
        | _             -> null

    static member TryCreate(token: Token) =
        match token.Kind with
        | Ident ("true" | "false") -> Some <| Literal(token, Types.Boolean.Instance())
        | Integer _i -> Some <| Literal(token, Types.Int.Instance())
        | Real _r    -> Some <| Literal(token, Types.Double.Instance())
        | Str _s     -> Some <| Literal(token, Types.String.Instance())
        | Char _c    -> Some <| Literal(token, Types.Char.Instance())
        | _ -> None

    override this.ToString() =
        match token.Kind with
        | Str s -> sprintf "\"%O\"" s
        | _ -> this.Value.ToString()
