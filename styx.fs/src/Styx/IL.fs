namespace Styx

open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Reflection.Emit

[<AutoOpen>]
module IL =

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type T() =
        static member val Constants = List<obj>()
    
    let private getConstants = Type.GetType("Styx.IL+T").GetProperty("Constants").GetMethod
    let private getStackSize =
        let fld = typeof<ILGenerator>.GetField("m_maxMidStackCur", BindingFlags.Instance ||| BindingFlags.NonPublic)
        let dyn = DynamicMethod("GetStackSize", typeof<int>, [| typeof<ILGenerator> |], typeof<ILGenerator>, true)
        let ilg = dyn.GetILGenerator(8)

        ilg.Emit(OpCodes.Ldarg_0)
        ilg.Emit(OpCodes.Ldfld, fld)
        ilg.Emit(OpCodes.Ret)

        dyn.CreateDelegate(typeof<Func<ILGenerator, int>>) :?> Func<ILGenerator, int>
    let private holeCtor = typeof<HoleException>.GetConstructor(Type.EmptyTypes)
        

    type ILGenerator with

        /// Gets the current size of the stack.
        member il.StackSize = getStackSize.Invoke(il)

        /// Emits the given expression.
        member il.Emit(expr: Expr) =
            expr.Compile(il)

        /// Emits the given value as a constant value.
        member il.EmitConstant(value: obj) =
            match value with
            | null -> il.Emit(OpCodes.Ldnull)
            | item ->
                let i = T.Constants.Count
                let t = item.GetType()

                T.Constants.Add(item)

                // The constant is simply loaded from the 'constants' list above.
                il.Emit(OpCodes.Call, getConstants)
                il.Emit(OpCodes.Ldc_I4, i)
                il.Emit(OpCodes.Callvirt, typeof<List<obj>>.GetProperty("Item").GetMethod)
                
                // And then cast to its original type.
                if t <> typeof<obj> then
                    il.Emit(OpCodes.Unbox_Any, t)

        /// Emits throwing a 'hole' exception.
        member il.ThrowHole() =
            // Clean stack.
            for _i = 1 to il.StackSize do
                il.Emit(OpCodes.Pop)

            // Emits throwing the exception.
            il.Emit(OpCodes.Newobj, holeCtor)
            il.Emit(OpCodes.Throw)
