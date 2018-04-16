//! A prelude that exports symbols commonly used in any situation that involves Styx.

pub use arch::Architecture;
pub use binder::Binder;
pub use builder::Builder;
pub use context::Context;
pub use diagnostics::{Diagnostic, DiagnosticBag};
pub use expr::Expr;
pub use lexer::{HasSpan, Token};
pub use typesystem::{Fun, Ty, Typed};
pub use visitor::Visitor;
pub use vm::{Vm, VirtualMachine};


/// A mutable reference to a `DiagnosticBag`.
pub type Diags<'r> = &'r mut DiagnosticBag;

/// A mutable reference to a `Context`.
pub type Ctx<'r, 'cx> = &'r mut Context<'cx>;

/// Parsing-related prelude.
pub mod parse {
    pub use binder::Binder;
    pub use diagnostics::{Diagnostic, DiagnosticBag, DiagnosticCodes, DiagnosticSeverity};
    pub use lexer::*;
    pub use parser::*;
    pub use symbols::{Sym, Symbol};
}

