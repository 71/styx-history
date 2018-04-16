namespace Styx.Expressions

open Styx
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Reflection.Emit

module private Interop =

    [<Literal>]
    let LIBC = "libc.so.6"
    [<Literal>]
    let LIBS = "libSystem.dynlib"
    [<Literal>]
    let KERN = "kernel32.dll"

    [<DllImport(LIBC, CallingConvention = CallingConvention.Cdecl, EntryPoint = "dlopen")>]
    extern void* LinuxLoad(string filename, int flag)

    [<DllImport(LIBC, CallingConvention = CallingConvention.Cdecl, EntryPoint = "dlclose")>]
    extern int LinuxFree(void* handle)

    [<DllImport(LIBC, CallingConvention = CallingConvention.Cdecl, EntryPoint = "dlsym")>]
    extern void* LinuxResolve(void* handle, string symbol)


    [<DllImport(LIBS, CallingConvention = CallingConvention.Cdecl, EntryPoint = "dlopen")>]
    extern void* OsxLoad(string filename, int flag)

    [<DllImport(LIBS, CallingConvention = CallingConvention.Cdecl, EntryPoint = "dlclose")>]
    extern int OsxFree(void* handle)

    [<DllImport(LIBS, CallingConvention = CallingConvention.Cdecl, EntryPoint = "dlsym")>]
    extern void* OsxResolve(void* handle, string symbol)


    [<DllImport(KERN, EntryPoint = "LoadLibrary")>]
    extern void* WindowsLoad(string filename)

    [<DllImport(KERN, EntryPoint = "FreeLibrary")>]
    extern bool WindowsFree(void* handle)

    [<DllImport(KERN, EntryPoint = "GetProcAddress")>]
    extern void* WindowsResolve(void* handle, string procname)

    let Modules = Dictionary<string, nativeint>()

/// A foreign function call.
type Foreign(lib: string, name: string, span) =
    inherit Expr(span)

    override __.State = Unbound

    override __.Resolve() = None

    override __.Compile(il) =
        let modl =
            match Interop.Modules.TryGetValue(lib) with
            | true, m -> m
            | _ ->
                let m =
                    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                        Interop.WindowsLoad(lib)
                    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                        Interop.OsxLoad(lib, 0x001)
                    else
                        Interop.LinuxLoad(lib, 0x001)

                if m = IntPtr.Zero then
                    raise <| Exception (sprintf "Cannot open library %s." lib)
                    
                Interop.Modules.Add(lib, m)
                m

        let fn =
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                Interop.WindowsResolve(modl, name)
            elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                Interop.OsxResolve(modl, name)
            else
                Interop.LinuxResolve(modl, name)

        if fn = IntPtr.Zero then
            raise <| Exception (sprintf "Cannot load %s in %s." name lib)
                
        let delty = typeof<Action>
        let gm = typeof<Marshal>.GetMethod("GetDelegateForFunctionPointer", [| typeof<IntPtr> |])

        il.Emit(OpCodes.Call, gm.MakeGenericMethod([| delty |]))
        il.Emit(OpCodes.Callvirt, delty.GetMethod("Invoke"))
