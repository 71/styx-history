namespace Styx.Text

open Styx
open Styx.Expressions

[<AutoOpen>]
module PatternsSyntax =
    /// Parses any pattern.
    let pattern, private patternRef = forwarded()

    /// Parses a simple binding pattern.
    let simplePattern = anyIdent |>> fun x -> Simple(x.Kind.ToString(), x.Span) :> Pattern

    /// Parses a catch-all pattern.
    let catchallPattern = idents "_" |>> fun x -> Catchall(x.Span) :> Pattern

    patternRef := choose [ catchallPattern ; simplePattern ]
