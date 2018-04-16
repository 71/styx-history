namespace Styx

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open Styx.Text

/// Defines the Styx expression-to-IL compiler.
type Compiler() =
    
    [<Literal>]
    let FnFlags = MethodAttributes.Static ||| MethodAttributes.Public
    [<Literal>]
    let TyFlags = TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Abstract

    let name = AssemblyName("Styx.Generated")
    let assembly = AssemblyBuilder.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndCollect)
    let modul = assembly.DefineDynamicModule("Styx.Generated")
    let rootType = modul.DefineType("root", TyFlags)
    let root = rootType.DefineMethod("global", FnFlags, Type.GetType("System.Void"), Type.EmptyTypes)

    let mutable nth = 0
    let gen() = nth <- nth + 1
                nth

    let addSpanToType (span: Span) (t: TypeBuilder) =
        let ctor = typeof<System.ComponentModel.DescriptionAttribute>.GetConstructor([| typeof<string> |])
        let attr = CustomAttributeBuilder(ctor, [| span.ToString() |])
        t.SetCustomAttribute(attr)
        t
    let addSpanToMethod (span: Span) (t: MethodBuilder) =
        let ctor = typeof<System.ComponentModel.DescriptionAttribute>.GetConstructor([| typeof<string> |])
        let attr = CustomAttributeBuilder(ctor, [| span.ToString() |])
        t.SetCustomAttribute(attr)
        t

    member __.Module = modul
    
    member __.Assembly = assembly

    member __.RootFunction = root

    member __.CreateFunction(name: string, span: Span, rty, [<ParamArray>] ptys) =
        rootType.DefineMethod(sprintf "%s<%d>" name (gen()), MethodAttributes.Public ||| MethodAttributes.Static, rty, ptys)
        |> addSpanToMethod span

    member __.CreateType(name: string, span: Span) =
        modul.DefineType(sprintf "%s<%d>" name (gen()), TyFlags)
        |> addSpanToType span

    member __.CreateInterface(name: string, span: Span) =
        modul.DefineType(sprintf "%s<%d>" name (gen()), TypeAttributes.Interface)
        |> addSpanToType span

    member __.SaveToFile(_path: string) =
        failwith "Not supported."
        // let currentDir = Environment.CurrentDirectory
        // let dir = Path.GetDirectoryName(path)
        // let name = Path.GetFileName(path)

        // if not <| String.IsNullOrEmpty dir then
        //     Environment.CurrentDirectory <- dir

        // let il = root.GetILGenerator()

        // while il.StackSize > 0 do
        //     il.Emit(OpCodes.Pop)
        
        // il.Emit(OpCodes.Ret)

        // for ty in modul.GetTypes() do
        //     match ty with
        //     | :? TypeBuilder as ty -> if not (ty.IsCreated()) then ty.CreateType() |> ignore
        //     | _ -> ()

        // assembly.SetEntryPoint(root)
        // assembly.Save(name)
        
        // Environment.CurrentDirectory <- currentDir
