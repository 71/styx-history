namespace Styx.Library

open Styx

module Functions =

    let inline styxify expr = fun (b: Binder) -> Bridge.styxifyTarget(b, expr)

    type Impl =
        static member DivInteger(a: int, b: int) = a / b
        static member Div(a: int, b: int) = (double a) / (double b)

    let LogicalOr b = styxify <@ (||) @> b
    let LogicalAnd b = styxify <@ (&&) @> b
    let LogicalXor b = styxify <@ (<>) @> b
    let Plus b = styxify <@ (+) @> b
    let Minus b = styxify <@ (-) @> b
    let Times b = styxify <@ (*) @> b
    let Pow b = styxify <@ pown 0 0 @> b
    let DivI b = styxify <@ Impl.DivInteger(1, 1) @> b
    let DivF b = styxify <@ Impl.Div(1, 1) @> b
    let Exit b = styxify <@ exit 0 @> b

