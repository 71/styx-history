namespace Styx.JIT

open ExpandableAllocator

open System
open System.Runtime.InteropServices

/// Defines an object that can emit machine code in memory, and
/// then execute it.
type Emitter() =
    let alloc = Allocator.Create(Protection.ReadWriteExecute, 1_000_000L)
    let stream = new ExpandableStream(alloc, false)

    /// Gets the underlying allocator.
    member __.Allocator = alloc

    /// Gets the underlying stream.
    member __.Stream = stream

    interface IDisposable with
        member __.Dispose() = alloc.Dispose()

    /// Invokes the function at the given offset with the given arguments.
    member __.Invoke offset args =
        Marshal.GetDelegateForFunctionPointer(offset) args

    /// Gets a delegate to the function at the given offset.
    member __.GetFunction offset =
        Marshal.GetDelegateForFunctionPointer(offset)
