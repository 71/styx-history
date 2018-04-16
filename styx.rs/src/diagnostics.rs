//! Utilities for reporting errors, warnings and logging messages.
//!
//! [`Diagnostic`]s are reported to the [`DiagnosticBag`], and displayed to the user
//! using the [`DiagnosticPrinter`].
#![macro_use]

use input::InputFile;
use lexer::{HasSpan, Span};

use std::fmt::{self, Display, Formatter};
use std::io;

/// Defines the result of a compilation, which is either successful,
/// or a fatal `Diagnostic`.
pub type CompileResult<T> = Result<T, Diagnostic>;

/// Defines the result of an operation which may return a fatal `Diagnostic`.
pub type Diagn<T> = Result<T, Diagnostic>;


// //==========================================================================//
// // SEVERITY                                                                 //
// //==========================================================================//

/// Represents the severity of a `Diagnostic`.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum DiagnosticSeverity {
    /// Unrecoverable error that instantly stops the compilation.
    Fatal,

    /// Unrecoverable error.
    Error,

    /// Warning that should be fixed, but does not keep the program from being interpreted.
    Warning,

    /// Simple information for the end user.
    Info
}

impl Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &DiagnosticSeverity::Error | &DiagnosticSeverity::Fatal => f.write_str("error"),
            &DiagnosticSeverity::Warning => f.write_str("warning"),
            &DiagnosticSeverity::Info    => f.write_str("info")
        }
    }
}


// //==========================================================================//
// // BAG                                                                      //
// //==========================================================================//

/// Represents a list of diagnostics.
#[derive(Clone, Debug, Default)]
pub struct DiagnosticBag {
    file: u64,
    bag: Vec<Diagnostic>,
    is_err: bool,
    ignoring: bool
}

impl From<Diagnostic> for DiagnosticBag {
    fn from(diagnostic: Diagnostic) -> Self {
        DiagnosticBag { file: 0, is_err: diagnostic.is_error(), bag: vec!(diagnostic), ignoring: false }
    }
}

impl<'a> From<&'a InputFile> for DiagnosticBag {
    fn from(file: &'a InputFile) -> Self {
        DiagnosticBag { file: file.hash(), bag: Vec::new(), is_err: false, ignoring: false }
    }
}

impl AsRef<[Diagnostic]> for DiagnosticBag {
    fn as_ref(&self) -> &[Diagnostic] {
        &self.bag
    }
}

impl DiagnosticBag {
    /// Reports the given `Diagnostic` by adding it to the `DiagnosticBag`,
    /// and returns it if it is fatal.
    pub fn report(&mut self, diagnostic: Diagnostic) {
        if self.ignoring {
            return
        }
        if diagnostic.is_error() {
            self.is_err = true;
        }

        self.bag.push(diagnostic.with_file(self.file));
    }

    /// Returns the count of `Diagnostic`s stored in this bag.
    pub fn len(&self) -> usize {
        self.bag.len()
    }

    /// Returns whether the diagnostic bag is completely empty.
    pub fn is_empty(&self) -> bool {
        self.bag.is_empty()
    }

    /// Returns a `bool` representing whether or not this bag contains an error `Diagnostic`.
    pub fn has_error(&self) -> bool {
        self.is_err
    }

    /// Sets whether all diagnostics reported from now on should be ignored.
    pub fn set_ignore(&mut self, state: bool) -> &mut Self {
        self.ignoring = state;
        self
    }

    /// Ignores all diagnostics reported from now on.
    pub fn ignore(&mut self) -> &mut Self {
        self.ignoring = true;
        self
    }

    /// Stops ignoring all diagnostics reported from now on.
    pub fn no_ignore(&mut self) -> &mut Self {
        self.ignoring = false;
        self
    }

    /// Returns a `bool` that indicates whether the bag is currently ignoring reported diagnostics.
    pub fn ignoring(&self) -> bool {
        self.ignoring
    }

    /// Merges two `DiagnosticBag`s together.
    pub fn merge(&mut self, other: &mut Self) {
        self.bag.append(&mut other.bag);

        if other.is_err {
            self.is_err = true;
        }
    }

    /// Sets the current file.
    pub(crate) fn set_active_file(&mut self, file: u64) -> &mut Self {
        self.file = file;
        self
    }

    /// Returns the slice containing all diagnostics in this bag.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.bag
    }
}


// //==========================================================================//
// // PRINTER                                                                  //
// //==========================================================================//

/// Represents a struct that helps print diagnostics to standard output.
pub struct DiagnosticPrinter<'a> {
    target: &'a mut io::Write,
    file: &'a InputFile,
    line_map: Vec<u64>
}

impl<'a> DiagnosticPrinter<'a> {
    /// Creates a new diagnostic printer, given an output stream,
    /// and the file from which the errors originate.
    pub fn new(target: &'a mut io::Write, file: &'a InputFile) -> Self {
        fn make_map(file: &InputFile) -> Vec<u64> {
            let mut vec = Vec::new();

            for (pos, ch) in file.source().bytes().enumerate() {
                if ch == b'\n' {
                    vec.push(pos as _);
                }
            }

            vec
        }

        DiagnosticPrinter {
            target, file,
            line_map: make_map(file)
        }
    }

    fn line(&self, span: Span) -> Option<(Vec<String>, u64, u64)> {
        let map = &self.line_map;

        let (start, linec) = {
            let mut i = 0;
            let mut prev = 0;
            let index = span.start() as u64;

            loop {
                match map.get(i) {
                    Some(ls) if *ls >= index => break (prev as u64, i as u64),
                    Some(ls) => prev = *ls,
                    None => return None
                }

                i += 1;
            }
        };

        let mut result = Vec::new();
        let mut line = String::new();

        for (i, ch) in self.file.source()[start as usize..].chars().enumerate() {
            if ch == '\n' {
                result.push(::std::mem::replace(&mut line, String::new()));

                if i >= span.len() {
                    break
                }
            } else {
                line.push(ch);
            }
        }

        Some((result, start, linec))
    }

    /// Prints the specified diagnostic to the output stream.
    #[allow(unused_io_amount)]
    pub fn print(&mut self, diagnostic: &Diagnostic) {
        use yansi::Color::*;
        use std::iter;

        let werr = "Error writing the diagnostic to the stream.";

        let filename = self.file.name();
        let span = diagnostic.span;

        let color = match diagnostic.severity() {
            DiagnosticSeverity::Info => Blue,
            DiagnosticSeverity::Warning => Yellow,
            _ => Red
        };

        writeln!(self.target, "{} [{:02}]: {}",
            color.paint(&diagnostic.severity).bold(),
            color.paint(&diagnostic.code).bold(),
            White.paint(&diagnostic.message).bold()
        ).expect(werr);

        if span.is_empty() {
            return
        }

        macro_rules! padleft {
            ( $till: expr ) => {
                for _ in 0..$till {
                    self.target.write(b" ").expect(werr);
                }
            }
        }

        if let Some((lines, start, mut linenb)) = self.line(span) {
            let padding = 1 + ((linenb + lines.len() as u64) as f64).log10() as usize;

            padleft!(padding);
            writeln!(self.target, " > {}:", color.paint(filename).bold()).expect(werr);

            macro_rules! linenbr {
                ( ) => {{
                    write!(self.target, "{:padding$} | ", "", padding = padding).expect(werr);
                }};

                ( $nbr: expr ) => {{
                    write!(self.target, "{:padding$} | ", color.paint($nbr + 1), padding = padding).expect(werr);
                }};

                ( $nbr: expr, $line: expr ) => {{
                    linenbr!($nbr);
                    self.target.write($line.as_bytes()).expect(werr);
                    self.target.write(b"\n").expect(werr);
                }};
            }

            if lines.len() == 1 {
                linenbr!(linenb, lines[0]);
                padleft!(span.start() as u64 - start + 4);

                let underline: String = iter::repeat('^').take(span.len()).collect();

                write!(self.target, "{}\n", color.paint(underline)).expect(werr);
            } else if lines.len() > 1 {
                // Write first line
                linenbr!(linenb, lines[0]);
                padleft!(span.start() as u64 - start + 4);
                linenb += 1;

                write!(self.target, "{}\n", color.paint('^')).expect(werr);

                // Write lines in-between
                if lines.len() > 5 {
                    linenbr!();
                    write!(self.target, "...").expect(werr);

                    linenbr!(linenb + 3, lines[3]);
                    linenbr!(linenb + 4, lines[4]);

                    linenbr!();
                    write!(self.target, "...").expect(werr);
                } else if lines.len() > 2 {
                    for line in lines[1..lines.len() - 2].iter() {
                        linenb += 1;
                        linenbr!(linenb, line);
                        self.target.write(b"\n").expect(werr);
                    }
                }

                // Count number of chars read till last line
                let charsc: usize = lines.iter().take(lines.len() - 1).map(String::len).sum();

                // Write last line
                linenbr!(linenb, lines[lines.len() - 1]);

                padleft!(span.len() - charsc);

                write!(self.target, "{}\n", color.paint('^')).expect(werr);
            }

            self.target.flush().expect(werr);
        }
    }

    /// Prints all diagnostics returned by the given iterator.
    pub fn print_many<'b, I: Iterator<Item = &'b Diagnostic>>(&mut self, iterator: I) {
        for diagnostic in iterator {
            self.print(diagnostic);
        }
    }

    /// Prints all diagnostics in the specified diagnostic bag.
    pub fn print_bag(&mut self, bag: &DiagnosticBag) {
        for diagnostic in bag.as_ref() {
            self.print(diagnostic);
        }
    }
}


// //==========================================================================//
// // DIAGNOSTIC                                                               //
// //==========================================================================//

/// Represents a diagnostic.
#[derive(Clone, Debug, PartialEq)]
pub struct Diagnostic {
    file: u64,
    code: usize,
    message: String,
    span: Span,
    severity: DiagnosticSeverity
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}) {}: {} [{}]", self.span, self.severity, self.message, self.code)
    }
}

#[allow(needless_pass_by_value)] // Let S be passed by value; otherwise, compiler error & string has to be cloned
impl Diagnostic {
    /// Creates and returns a fatal diagnostic.
    ///
    /// # Example
    /// ```
    /// use styx::diagnostics::{Diagnostic, DiagnosticSeverity};
    /// use styx::lexer::Span;
    ///
    /// assert_eq!(
    ///     Diagnostic::fatal(0, "Error", Span::default()).severity(),
    ///     DiagnosticSeverity::Fatal);
    /// ```
    #[inline]
    pub fn fatal<S: ToString>(code: usize, message: S, span: Span) -> Self {
        Diagnostic { file: 0, code, message: message.to_string(), span, severity: DiagnosticSeverity::Fatal }
    }

    /// Creates and returns an error diagnostic.
    ///
    /// # Example
    /// ```
    /// use styx::diagnostics::{Diagnostic, DiagnosticSeverity};
    /// use styx::lexer::Span;
    ///
    /// assert_eq!(
    ///     Diagnostic::error(0, "Error", Span::default()).severity(),
    ///     DiagnosticSeverity::Error);
    /// ```
    #[inline]
    pub fn error<S: ToString>(code: usize, message: S, span: Span) -> Self {
        Diagnostic { file: 0, code, message: message.to_string(), span, severity: DiagnosticSeverity::Error }
    }

    /// Creates and returns an informational diagnostic.
    ///
    /// # Example
    /// ```
    /// use styx::diagnostics::{Diagnostic, DiagnosticSeverity};
    /// use styx::lexer::Span;
    ///
    /// assert_eq!(
    ///     Diagnostic::info(0, "Informational message", Span::default()).severity(),
    ///     DiagnosticSeverity::Info);
    /// ```
    #[inline]
    pub fn info<S: ToString>(code: usize, message: S, span: Span) -> Self {
        Diagnostic { file: 0, code, message: message.to_string(), span, severity: DiagnosticSeverity::Info }
    }

    /// Creates and returns a warning diagnostic.
    ///
    /// # Example
    /// ```
    /// use styx::diagnostics::{Diagnostic, DiagnosticSeverity};
    /// use styx::lexer::Span;
    ///
    /// assert_eq!(
    ///     Diagnostic::warning(0, "Warning", Span::default()).severity(),
    ///     DiagnosticSeverity::Warning);
    /// ```
    #[inline]
    pub fn warning<S: ToString>(code: usize, message: S, span: Span) -> Self {
        Diagnostic { file: 0, code, message: message.to_string(), span, severity: DiagnosticSeverity::Warning }
    }

    /// Returns the hash of the file in which this diagnostic was reported.
    #[inline]
    pub fn hash(&self) -> u64 {
        self.file
    }

    /// Returns the code of the diagnostic, which can be found in [`DiagnosticCodes`] for
    /// compiler-generated diagnostics.
    #[inline]
    pub fn code(&self) -> usize {
        self.code
    }

    /// Returns the message that describes the diagnostic to human beings.
    #[inline]
    pub fn message(&self) -> &str {
        self.message.as_str()
    }

    /// Returns whether the diagnostic represents an error, and should halt compilation.
    ///
    /// # Example
    /// ```
    /// use styx::diagnostics::Diagnostic;
    /// use styx::lexer::Span;
    ///
    /// let span = Span::default();
    ///
    /// assert!(!Diagnostic::warning(0, "Foo", span).is_error());
    /// assert!(!Diagnostic::info(0, "Buu", span).is_error());
    /// assert!(Diagnostic::error(0, "Bar", span).is_error());
    /// assert!(Diagnostic::fatal(0, "Baz", span).is_error());
    /// ```
    #[inline]
    pub fn is_error(&self) -> bool {
        self.severity == DiagnosticSeverity::Error || self.severity == DiagnosticSeverity::Fatal
    }

    /// Returns the severity of this diagnostic.
    ///
    /// # Example
    /// ```
    /// use styx::diagnostics::{Diagnostic, DiagnosticSeverity};
    /// use styx::lexer::Span;
    ///
    /// assert_eq!(
    ///     Diagnostic::warning(0, "W", Span::default()).severity(),
    ///     DiagnosticSeverity::Warning);
    /// ```
    #[inline]
    pub fn severity(&self) -> DiagnosticSeverity {
        self.severity
    }

    /// Changes the hash of the file in which this diagnostic was reported.
    pub fn with_file(mut self, file: u64) -> Diagnostic {
        self.file = file;
        self
    }

    /// Changes the severity of this diagnostic to the given value.
    pub fn with_severity(mut self, severity: DiagnosticSeverity) -> Diagnostic {
        self.severity = severity;
        self
    }

    /// Changes the span of this diagnostic to the given value.
    pub fn with_span(mut self, span: Span) -> Diagnostic {
        self.span = span;
        self
    }
}

impl HasSpan for Diagnostic {
    fn span(&self) -> Span {
        self.span
    }
}


/// Struct used to store constants representing the codes
/// of all compiler-declared `Diagnostic`s.
pub struct DiagnosticCodes;

macro_rules! define_diagnostic {
    ( $severity: ident, $code: expr, $name: ident, $msg: expr ) => {
        impl DiagnosticCodes {
            /// Identifiying code of the `$name` `Diagnostic`.
            #[allow(non_upper_case_globals)]
            pub const $name: usize = $code;
        }

        impl Diagnostic {
            /// ($code) `Diagnostic` with message $msg.
            #[inline]
            pub fn $name(span: Span) -> Self {
                Self::$severity(DiagnosticCodes::$name, $msg, span)
            }
        }
    };

    ( $severity: ident, $code: expr, $name: ident, $( $p: ident: $t: ty ),* => $msg: expr ) => {
        impl DiagnosticCodes {
            /// Identifiying code of the `$name` `Diagnostic`.
            #[allow(non_upper_case_globals)]
            pub const $name: usize = $code;
        }

        impl Diagnostic {
            /// ($code) `Diagnostic` with message $msg.
            #[inline]
            pub fn $name(span: Span, $( $p: $t ),* ) -> Self {
                Self::$severity(DiagnosticCodes::$name, format!($msg, $( $p ),*), span)
            }
        }
    };
}

macro_rules! def_error {
    ( $code: expr, $name: ident, $msg: expr ) => (define_diagnostic!(error, $code, $name, $msg););
    ( $code: expr, $name: ident, $( $p: ident: $t: ty ),* => $msg: expr ) => (define_diagnostic!(error, $code, $name, $($p: $t),* => $msg););
}
macro_rules! def_warning {
    ( $code: expr, $name: ident, $msg: expr ) => (define_diagnostic!(error, $code, $name, $msg););
    ( $code: expr, $name: ident, $( $p: ident: $t: ty ),* => $msg: expr ) => (define_diagnostic!(warning, $code, $name, $($p: $t),* => $msg););
}
#[allow(unused_macros)]
macro_rules! def_info {
    ( $code: expr, $name: ident, $msg: expr ) => (define_diagnostic!(error, $code, $name, $msg););
    ( $code: expr, $name: ident, $( $p: ident: $t: ty ),* => $msg: expr ) => (define_diagnostic!(info, $code, $name, $($p: $t),* => $msg););
}

use std::path::Display as PathDisplay;

// VM errors:
def_error!(0x00, unsupported_running_arch, "Unsupported architecture.");
def_error!(0x00, cant_open_file, path: PathDisplay => "Unable to open file '{}'.");

// Lexer errors:
def_error!(0x10, unexpected_eof, "Unexpected end of expression.");
def_error!(0x11, unreadable_char, "Unreadable UTF-8 character.");
def_error!(0x12, invalid_char, "Invalid character literal.");
def_error!(0x13, invalid_number, "Invalid number literal.");
def_error!(0x14, unknown_escape_char, "Unknown escape character.");

// Parser errors:
def_error!(0x20, undefined_symbol, symbol: &str => "Undefined symbol '{}'.");
def_error!(0x21, no_match, "Input syntax not recognized.");
def_error!(0x22, undefined_binary_op, op: &str => "Undefined binary operator '{}'.");
def_error!(0x23, undefined_unary_op, op: &str => "Undefined unary operator '{}'.");
def_error!(0x24, expected_token, tt: &str => "Expected {} token.");
def_error!(0x25, duplicate_fn, "Duplicate function definition.");
def_error!(0x26, duplicate_ty, "Duplicate type definition.");
def_error!(0x27, not_a_function, "Not a function.");
def_error!(0x28, not_a_type, "Not a type.");
def_error!(0x29, incomplete_expr, "Expression could not be fully parsed.");
def_error!(0x2A, undefined_function, symbol: &str => "Undefined function '{}'.");
def_error!(0x2B, undefined_type, symbol: &str => "Undefined type '{}'.");
def_error!(0x2C, signature_mismatch, "Function signature mismatch.");
def_error!(0x2D, ambiguous_match, "Ambiguous match.");

// Compiler errors:
def_error!(0x30, io, "Error encountered when emitting an expression.");
def_error!(0x31, unknown_target, "Unknown target for a jump instruction.");
def_error!(0x32, invalid_argument, "Invalid argument in a jump or call instruction: the given value does not have the desired size.");
def_error!(0x33, non_terminated_block, "Block does not terminate.");

// Assembler errors:
def_error!(0x40, unknown_opcode, opcode: &str => "Unknown opcode: '{}'.");
def_error!(0x41, unsupported_arch, "Expression not supported on the target architecture.");

// Warnings:
def_warning!(0x100, duplicate_bin_op, "Precedence of a binary operator set more than once.");
def_warning!(0x101, duplicate_unary_op, "Fixity of an unary operator set more than once.");

