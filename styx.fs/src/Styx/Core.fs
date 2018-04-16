namespace rec Styx

open Styx.Reactive
open Styx.Text
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

/// Defines the visibility of a function.
type Visibility = Public | Internal | Private

/// Defines a Styx value.
type Value = { Inner : obj  ;
               Type  : Type }
with
    static member From(inner: obj, ty) = { Inner = inner ; Type = ty }

/// Defines an object that can be dropped when leaving its scope.
type IDroppable =
    abstract Drop : unit -> unit

/// Defines a type property, which defines what is known about a type
/// instance.
type Property = { Name     : string ;
                  IsUnique : bool   ;
                  Type     : Type   }

/// Defines the value of a type property.
type PropertyValue = Exact of Value
                   | Range of Value option * Value option
                   | Not   of Value
                   | Or    of Value list
                   | And   of Value list
with
    /// Normalizes the property value.
    ///
    /// This method ensures two different properties are said to be equal
    /// if their mathematical definitions match.
    member this.Normalize() = failwith "TODO"; this


/// Defines a type and its properties.
type TypeDefinition = { Name       : string        ;
                        Namespace  : string        ;
                        Properties : Property list ;
                        ILType     : System.Type   }
with
    /// Creates a new instance of this type definition, give the
    /// value of each property.
    member this.Instance(args: PropertyValue list) =
        assert(args.Length = this.Properties.Length)

        { Definition = this ; Arguments = args ; Properties = [] }

    /// Creates a new instance of this type definition.
    member this.Instance() =
        assert(this.Properties.Length = 0)

        { Definition = this ; Arguments = [] ; Properties = [] }

    interface IBindable with
        member __.State = Bound

    /// Returns the definition of the void type.
    static member Void = CoreTypes.Void

    /// Returns the definition of the never type.
    static member Never = CoreTypes.Never 


module private CoreTypes =
    let Void = { Namespace = "System" ; Name = "Void" ; Properties = [] ; ILType = System.Type.GetType("System.Void") }
    let Never = { Namespace = "System" ; Name = "Never" ; Properties = [] ; ILType = System.Type.GetType("System.Void") }


/// Defines an instance of a type.
type Type = { Definition : TypeDefinition     ;
              Properties : PropertyValue list ;
              Arguments  : PropertyValue list }
with
    /// Returns an instance of the void type.
    static member Void = TypeDefinition.Void.Instance()

    /// Returns an instance of the never type.
    static member Never = TypeDefinition.Never.Instance()

    /// Computes whether the given type can be assigned to the current type.
    member this.IsAssignableFrom(other) =
        if this = Type.Never || other = Type.Never then
            // The 'never' type can be assigned from and to anything
            true
        else
            this = other

    /// Returns the .NET type that corresponds to this type.
    member this.ILType = this.Definition.ILType

    interface IBindable with
        member this.State = (this.Definition :> IBindable).State


/// Defines a type class.
type TypeClass = { Name       : string         ;
                   Namespace  : string         ;
                   Operations : Signature list }


/// Defines a Styx expression.
[<AbstractClass>]
type Expression(span: Span) =
    inherit Sealable<Expression>()

    let mutable ty = None
    let mutable triedResolve = false

    new() = Expression(Span.Default)

    /// Gets the span of the expression in source code.
    member __.Span = span

    /// Gets a boolean that represents whether the expression is bound.
    member this.IsBound = this.State = Bound

    /// Gets the type of the expression.
    member this.Type = 
        if not triedResolve then
            ty <- this.Resolve()
            triedResolve <- true

        match this.OptionalType with
        | Some ty -> ty
        | None -> Type.Never

    member this.OptionalType =
        if not triedResolve then
            ty <- this.Resolve()
            triedResolve <- true
        ty

    /// Gets the state of the expression.
    abstract State : BindingState

    default this.State = match this.OptionalType with
                         | Some _ -> Bound
                         | None -> Unbound

    /// Compiles this expression into IL.
    abstract Compile : ILGenerator -> unit

    /// Attempts to resolve the type of the expression.
    abstract Resolve : unit -> Type option

    override this.Next(v) =
        base.Next(v)

        if this.OptionalType = None then
            ty <- this.Resolve()

    interface IBindable with
        override this.State = this.State

/// Defines a Styx extension expression.
[<AbstractClass>]
type Extension(span) =
    inherit Expr(span)

    new() = Extension(Span.Default)

    /// Lowers this expression into a lower-level expression.
    abstract Lower : unit -> Expression

    /// Lowers this expression into a non-extension expression.
    member this.LowerFully() =
        match this.Lower() with
        | :? Extension as e -> e.Lower()
        | e -> e

    override this.Resolve() = this.LowerFully().Resolve()

    override this.Compile(ilg) = this.Lower().Compile(ilg)


/// Defines a function parameter.
type Parameter = { Name        : string          ;
                   Index       : int             ;
                   Component   : Component       }

/// Defines a function component.
type Component = Type of TypeDefinition * Component list
               | Variable of string
               | Dynamic of Expr
with
    member __.IsMatch(ty: Type, map: Dictionary<string, TypeDefinition>) = true

/// Defines the signature of a Styx function.
type Signature(ns: string, name: string, constraints: Expr[], parameters: Parameter[], returns: Expr) =
    let mutable verif = None

    new(ns, name, returns) = Signature(ns, name, [||], [||], returns)

    member __.Name = name

    member __.Namespace = ns

    member __.Constraints = constraints

    member __.Parameters = parameters

    member __.ReturnComponent = returns

    /// Gets the verification delegate used to verify if
    /// the provided arguments match the signature.
    member this.Verification =
        match verif with
        | None ->
            let v = this.ComputeVerification()
            verif <- Some v
            v
        | Some v -> v


    /// Attempts to get the return type of the function, given the type of the
    /// passed arguments.
    /// If the function is invalid, `None` will be returned, and one or more errors
    /// will have been logged through the verifier.
    member this.GetReturnType(argumentTypes: Type[], verifier: Verifier) =
        match this.Verification.Invoke(this, argumentTypes, verifier) with
        | t when isNull (t :> obj) -> None
        | t -> Some t

    static member MapOpCode = OpCodes.Ldloc_1

    /// Computes the verification delegate.
    member private __.ComputeVerification() : Verification =
        // Functions signatures in Styx are made of one or more components,
        // which are expressions that return type instances.
        // Thus, variables set by some components can be used in the following ones
        // to customize the static behaviour of the function.
        let sty = typeof<Signature>
        let vty = typeof<Dictionary<string, TypeDefinition>>
         
        let dm = DynamicMethod("Verify", MethodAttributes.Public ||| MethodAttributes.Static, CallingConventions.Standard,
                               typeof<Type>, [| sty ; typeof<Type[]> ; typeof<Verifier> |], sty, false)
        let il = dm.GetILGenerator()

        let mutable step = 0

        let paramsc = parameters.Length
        let locParams = il.DeclareLocal(typeof<Parameter[]>) // 0th loc
        let locVars = il.DeclareLocal(vty)                   // 1st loc

        // Some helpers =====================================

        let nonPublicInstance = BindingFlags.NonPublic ||| BindingFlags.Instance

        let inline log (str: string) =
            il.Emit(OpCodes.Ldstr, str)
            let m = typeof<System.Diagnostics.Debug>.GetRuntimeMethod("WriteLine", [| typeof<string> |])
            il.Emit(OpCodes.Call, m)

        let inline emitArgumentType (i: int) =
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldc_I4, i)
            il.Emit(OpCodes.Ldelem, typeof<Type>)

        let inline emitParameter (i: int) =
            il.Emit(OpCodes.Ldloc, locParams)
            il.Emit(OpCodes.Ldc_I4, i)
            il.Emit(OpCodes.Ldelem, typeof<Parameter>)
        
        let inline emitComponent (i: int) =
            emitParameter i
            il.Emit(OpCodes.Ldfld, typeof<Parameter>.GetField("Component@", nonPublicInstance))
        
        let emitErrorUnless (br: OpCode) (msg: string) =
            // Jumps forward if the given branch instruction succeeds
            let jmp = il.DefineLabel()
            
            il.Emit(br, jmp)

            // No jump: log error
            il.Emit(OpCodes.Ldarg_2)

            if step <> 0 then
                il.Emit(OpCodes.Ldstr, msg)

            match step with
            | 0 -> il.Emit(OpCodes.Call, typeof<Verifier>.GetMethod("InvalidArgumentsCount"))
            | 1 -> il.Emit(OpCodes.Call, typeof<Verifier>.GetMethod("TypeError"))
            | 2 -> il.Emit(OpCodes.Call, typeof<Verifier>.GetMethod("ConstraintError"))
            | _ -> assert false

            il.MarkLabel(jmp)

        let inline nextStep () =
            step <- step + 1

            // Check if errors have been reported; if so, return null immediately
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Call, typeof<Verifier>.GetProperty("IsValid").GetMethod)
            il.Emit(OpCodes.Brtrue_S, sbyte 2)

            il.Emit(OpCodes.Ldnull)
            il.Emit(OpCodes.Ret)


        // Start of procedure ===============================

        // Initialize locals
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, sty.GetField("parameters", nonPublicInstance))
        il.Emit(OpCodes.Stloc_0)

        il.Emit(OpCodes.Newobj, vty.GetConstructor(Type.EmptyTypes))
        il.Emit(OpCodes.Stloc_1)

        // Check length of input
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Ldlen)
        il.Emit(OpCodes.Conv_I4)
        il.Emit(OpCodes.Ldc_I4_S, sbyte paramsc)

        emitErrorUnless OpCodes.Beq_S "Invalid arguments count." // Different length: return null


        // For each component, make sure it matches the matching argument
        // and define variables that correspond to every signature variable.
        // This is done by computing every expression.
        nextStep()

        let defProp = typeof<Type>.GetProperty("Definition")
        let isMatchMethod = typeof<Component>.GetMethod("IsMatch")

        for i = 0 to paramsc - 1 do
            // Call 'component.IsMatch(argumentType.Definition)'

            emitComponent i                          // On stack: Component

            emitArgumentType i
            il.Emit(OpCodes.Call, defProp.GetMethod) // On stack: Component, TypeDefinition
            il.Emit(OpCodes.Ldloc, locVars)          // On stack: Component, TypeDefinition, Dictionary
            il.Emit(OpCodes.Call, isMatchMethod)     // On stack: bool

            emitErrorUnless OpCodes.Brtrue_S (sprintf "Invalid argument at %i." i) // Not a match: return null


        // Then, if no error was reported, compute every defined constraint
        // using the previously declared and set variables.
        // If every constraint matches the signature, keep going.
        nextStep()

        for i = 0 to constraints.Length - 1 do
            // Emit expr; if it returns 'false', return null
            constraints.[i].Compile(il)

            emitErrorUnless OpCodes.Brtrue_S (sprintf "Unrespected constraint at %i." i)


        // Finally, compute the return type using the previous variables,
        // and return it.
        nextStep()

        Signature.CompileAsReturn <- true
        returns.Compile(il)
        Signature.CompileAsReturn <- false

        il.Emit(OpCodes.Ret)


        // Return compiled delegate
        dm.CreateDelegate(typeof<Verification>) :?> Verification

    static member val CompileAsReturn = false with get, set

/// Defines a Styx function.
type Function(sign: Signature, bodies: Expression list) =
    let mutable repr = None

    member __.Signature = sign

    member __.Name = sign.Name

    member __.Namespace = sign.Namespace

    member __.Bodies = bodies

    interface IBindable with
        member __.State = state bodies

    member __.GetIL(args) =
        assert not bodies.IsEmpty

        let ret  = sign.GetReturnType(args, Verifier()).Value
        let ptys = Array.map (fun x -> x.Definition.ILType) args
        let dyn  = DynamicMethod(sign.Name, ret.ILType, ptys)

        if bodies.Length = 1 then
            bodies.[0].Compile(dyn.GetILGenerator())
        else
            failwith "Not implemented"
        
        dyn.GetILGenerator().Emit(OpCodes.Ret)
        dyn


/// Defines a function invocation verifier.
type Verifier() =
    let mutable hasCountError = false
    let typeErrors = List<string>()
    let constraintErrors = List<string>()

    /// Indicates that the number of given arguments is incorrect.
    member __.InvalidArgumentsCount() = hasCountError <- true

    /// Reports a type error.
    member __.TypeError(s) = typeErrors.Add(s)

    /// Reports a constraint error.
    member __.ConstraintError(s) = constraintErrors.Add(s)

    /// Gets a boolean that represents whether the verifier has encountered errors.
    member __.HasErrors = hasCountError || typeErrors.Count <> 0 || constraintErrors.Count <> 0

    /// Gets a boolean that represents whether the analyzed arguments are invalid.
    member this.IsInvalid = this.HasErrors

    /// Gets a boolean that represents whether the analyzed arguments are valid.
    member this.IsValid = not this.HasErrors


/// Represents a verification of type arguments.
type Verification = delegate of Signature * Type[] * Verifier -> Type

/// Represents a map from a variable name to a type definition.
type VariableMap = Dictionary<string, TypeDefinition>

/// <summary>Type alias for <cref="Expression"/>.</summary>
type Expr = Expression
