namespace Styx

open System
open System.Collections.Generic

/// Represents an exception thrown after trying to execute
/// an expression containing a hole.
type HoleException() =
    inherit Exception()
    
    override __.ToString() = ""