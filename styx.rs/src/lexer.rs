//! This module defines the [`Lexer`] and the [`Token`] types,
//! as well as utilities related to them (such as the [`Span`] type, and associated traits).

use diagnostics::*;

use std::fmt::{self, Debug, Display, Formatter, Write};
use std::ops::{Deref, DerefMut, Range};
use std::str;


// //==========================================================================//
// // SPAN & LOCATION                                                          //
// //==========================================================================//

/// Represents the position of a `Token` in source code.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Span {
    start: usize,
    length: usize
}

/// Describes an object that has a [`Span`] in source code.
pub trait HasSpan {
    /// Returns the [`Span`] of the object in source code.
    fn span(&self) -> Span;
}

impl HasSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl From<Range<Span>> for Span {
    fn from(range: Range<Span>) -> Self {
        let start = range.start.start;
        let length = range.start.length + range.end.length;

        Span { start, length }
    }
}

/// A spanned `T` object.
pub struct Spanned<T> {
    span: Span,
    value: T
}

impl<T> Spanned<T> {
    /// Returns a new `Spanned` structure wrapping the given value.
    pub fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }

    /// Consumes the `Spanned` structure, returning the object it contains.
    pub fn value(self) -> T {
        self.value
    }
}

impl<T> HasSpan for Spanned<T> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[{}] {:?}", self.span, self.value)
    }
}

impl Span {
    /// Returns a new `Span` that starts at the given index, and with a length of 1.
    #[inline]
    pub fn with_start(start: usize) -> Self {
        Span { start: start, length: 1 }
    }

    /// Returns a new `Span` that starts at the given index, and with a specified length.
    #[inline]
    pub fn with_start_and_length(start: usize, length: usize) -> Self {
        Span { start: start, length: length }
    }

    /// Returns a new `Span` that starts at the given index, and ends at the other one.
    #[inline]
    pub fn with_start_and_end(start: usize, end: usize) -> Self {
        Span { start: start, length: end - start }
    }

    /// Returns the starting index of this `Span`.
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the length of this `Span`.
    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    /// Returns whether this span is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Returns the starting index of this `Span`.
    #[inline]
    pub fn end(&self) -> usize {
        self.start + self.length
    }
}

impl Default for Span {
    /// Returns a `Span` with a null starting Span, and null length.
    #[inline]
    fn default() -> Self {
        Span { start: 0, length: 0 }
    }
}

impl From<usize> for Span {
    /// Constructs a `Span` from its starting position.
    #[inline]
    fn from(start: usize) -> Self {
        Span { start: start, length: 1 }
    }
}

impl From<(usize, usize)> for Span {
    /// Constructs a `Span` from a tuple containing its starting position and length.
    #[inline]
    fn from(tup: (usize, usize)) -> Self {
        Span { start: tup.0, length: tup.1 }
    }
}

impl From<Range<usize>> for Span {
    /// Constructs a `Span` from a range going from the start to the end.
    #[inline]
    fn from(range: Range<usize>) -> Self {
        Span { start: range.start, length: range.end - range.start }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_empty() {
            Display::fmt(&self.start(), f)
        } else {
            Display::fmt(&self.start(), f).and(f.write_char(':')).and(Display::fmt(&self.end(), f))
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}


// //==========================================================================//
// // TOKEN                                                                    //
// //==========================================================================//

/// Defines a primitive token of Styx source code.
#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    /// End-Of-File.
    EOF(usize),

    /// Ignored token (comment, for example).
    Ignored,

    /// A documentation comment.
    Doc(Span, String, bool),
    /// An indentation token.
    Indent(usize, usize),

    /// '(' character.
    LParen(usize),
    /// ')' character.
    RParen(usize),

    /// '{' character.
    LBracket(usize),
    /// '}' character.
    RBracket(usize),

    /// '<' character.
    LAngle(usize),
    /// '>' character.
    RAngle(usize),

    /// ',' character.
    Comma(usize),
    /// ';' character.
    Semicolon(usize),
    /// '$' character.
    Dollar(usize),

    /// An identifier; as defined by the following regular expression: [a-zA-Z_]\w* | [^\d\w\s"'<>()]+.
    Ident(Span, String),

    /// An integer.
    Int(Span, i64),
    /// A floating-point number.
    Real(Span, f64),
    /// A literal string.
    Str(Span, String),
    /// A literal character.
    Char(Span, char)
}

impl Token {
    /// Returns whether or not the `Token` represents the end of the file.
    pub fn is_eof(&self) -> bool {
        if let &Token::EOF(_) = self {
            true
        } else {
            false
        }
    }
}

impl HasSpan for Token {
    fn span(&self) -> Span {
        use self::Token::*;

        match self {
            &Ignored => Span::default(),
            &Indent(start, end) => Span::with_start_and_end(start, end),

            &EOF(start) | &Comma(start) | &Semicolon(start) | &Dollar(start) |
            &LAngle(start) | &RAngle(start) |
            &LParen(start) | &RParen(start) |
            &LBracket(start) | &RBracket(start) => Span::with_start(start),

            &Doc(loc, _, _) | &Ident(loc, _) |
            &Int(loc, _)    | &Real(loc, _) | &Str(loc, _) | &Char(loc, _) => loc
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token::Ignored
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Comma(_)     => f.write_char(','),
            Token::Semicolon(_) => f.write_char(';'),
            Token::Dollar(_)    => f.write_char('$'),
            Token::LParen(_)    => f.write_char('('),
            Token::RParen(_)    => f.write_char(')'),
            Token::LAngle(_)    => f.write_char('<'),
            Token::RAngle(_)    => f.write_char('>'),
            Token::LBracket(_)  => f.write_char('{'),
            Token::RBracket(_)  => f.write_char('}'),

            Token::Ident(_, ref ident) => f.write_str(ident),
            Token::Int(_, n)           => fmt::Display::fmt(&n, f),
            Token::Real(_, n)          => fmt::Display::fmt(&n, f),
            Token::Char(_, ch)         => write!(f, "'{}'", ch),

            Token::Str(_, ref string)  => {
                f.write_char('"').and(f.write_str(string)).and(f.write_char('"'))
            },

            _ => Ok(())
        }
    }
}


// //==========================================================================//
// // LEXER                                                                    //
// //==========================================================================//

/// Defines the result of a lexing operation.
pub type LexResult = CompileResult<Token>;

/// Defines the state of the lexer.
pub struct State(usize, usize);

/// Represents a `Lexer`, which transforms Styx source code
/// into a `Token` stream.
#[derive(Clone)]
pub struct Lexer<'a> {
    byte_pos: usize,
    char_pos: usize,
    source: &'a str,
    reached_end: bool
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(source: &'a str) -> Self {
        Lexer::new(source)
    }
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `str`.
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Lexer { source, byte_pos: 0, char_pos: 0, reached_end: false }
    }

    /// Creates a new `Lexer`, given its source `str` and its previous state.
    #[allow(needless_pass_by_value)] // Passing by value by design
    pub fn new_with_state(source: &'a str, state: State) -> Self {
        Lexer { source, byte_pos: state.0, char_pos: state.1, reached_end: false }
    }

    /// Gets a new state that allows to restore the `Lexer` to a previous state.
    #[inline]
    pub fn state(&self) -> State {
        State(self.byte_pos, self.char_pos)
    }

    /// Restores the `Lexer` to a previous state.
    #[inline]
    #[allow(needless_pass_by_value)] // Passing by value by design
    pub fn restore(&mut self, state: State) {
        self.byte_pos = state.0;
        self.char_pos = state.1;
    }

    /// Gets the source `str` of the `Lexer`.
    #[inline]
    pub fn source(&self) -> &'a str {
        self.source
    }

    /// Gets the position of the `Lexer` in its input string.
    #[inline]
    pub fn position(&self) -> usize {
        self.char_pos
    }

    /// Attempts to get the character following the given byte by the given offset,
    /// and returns a tuple containg the parsed character and the position of the next one.
    #[inline]
    pub fn following(src: &'a str, byte: usize, offset: usize) -> Option<(char, usize)> {
        match src.get(byte..) {
            Some(slice) => {
                let mut remaining = offset;
                let mut chars = slice.chars();
                let mut bytes = 0;

                while remaining > 0 {
                    match chars.next() {
                        Some(ch) => bytes += ch.len_utf8(),
                        None => return None
                    }

                    remaining -= 1;
                }

                match chars.next() {
                    Some(ch) => Some((ch, ch.len_utf8() + byte + bytes)),
                    None => None
                }
            },
            None => None
        }
    }

    /// Returns whether or not the lexer has reached the end of the file.
    #[inline]
    pub fn at_end(&self) -> bool {
        self.byte_pos >= self.source.len()
    }

    /// Attempts to lex a `Token` from the input code.
    /// If an error is encountered, it will be added to the diagnostics. If the error is fatal,
    /// an error will be returned, instead of a token.
    #[allow(needless_return, while_let_loop, cyclomatic_complexity)]
    pub fn lex(&mut self, diagnostics: &mut DiagnosticBag) -> LexResult {
        let src = self.source;

        let mut startc = self.char_pos;
        let mut startb = self.byte_pos;

        let mut pos = startc;
        let mut byte = startb;

        /// Advances the current character in the peekable character stream,
        /// and on the `pos` variable.
        macro_rules! next {
            ($e: expr) => {{
                match Self::following(src, byte, 0) {
                    Some((ch, b)) => {
                        byte = b;
                        pos += 1;

                        ch
                    },
                    _ => $e
                }
            }};

            () => {
                next!(return Err(Diagnostic::unexpected_eof(span!(startc => pos))))
            };
        }

        /// Updates the current position (and returns).
        macro_rules! update {
            () => {{
                self.char_pos = pos;
                self.byte_pos = byte;
            }};

            ($e: expr) => {{
                self.char_pos = pos;
                self.byte_pos = byte;
                return $e;
            }};
        }

        macro_rules! peek {
            () => {
                match Self::following(src, byte, 0) {
                    Some((ch, _)) => ch,
                    _ => return Err(Diagnostic::unexpected_eof(span!(startc => pos)))
                }
            };

            ($e: expr) => {
                match Self::following(src, byte, 0) {
                    Some((ch, _)) => ch,
                    _ => $e
                }
            };
        }

        // skip whitespaces
        loop {
            {
                if !peek!(return Ok(Token::EOF(pos))).is_whitespace() {
                    break;
                }
            }

            next!();
        }

        startc = pos;
        startb = byte;

        let result = match next!(update!(Ok(Token::EOF(startc)))) {
            '(' => Ok(Token::LParen(startc)),
            ')' => Ok(Token::RParen(startc)),
            '{' => Ok(Token::LBracket(startc)),
            '}' => Ok(Token::RBracket(startc)),
            ',' => Ok(Token::Comma(startc)),
            ';' => Ok(Token::Semicolon(startc)),
            '<' => Ok(Token::LAngle(startc)),
            '>' => Ok(Token::RAngle(startc)),
            '$' => Ok(Token::Dollar(startc)),

            '0' ... '9' => {
                // Parse number literal
                let mut is_float = false;

                loop {
                    let ch = peek!(break);

                    // Parse float.
                    if !ch.is_digit(16) {
                        if ch == '.' {
                            is_float = true;
                        } else {
                            break;
                        }
                    }

                    next!();
                }

                let span = span!(startc => pos);

                Ok(match src[startb..byte].parse() {
                    Ok(nbr) => if is_float {
                        Token::Real(span, nbr)
                    } else {
                        Token::Int(span, nbr as i64)
                    },
                    _ => {
                        diagnostics.report(Diagnostic::invalid_number(span));
                        Token::Real(span, ::std::f64::NAN)
                    }
                })
            },

            'a' ... 'z' | 'A' ... 'Z' | '_' => {
                // Parse (word-like) identifier
                loop {
                    let ch = peek!(break);

                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '\u{a0}' && ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    next!();
                }

                Ok(Token::Ident(span!(startc => pos), src[startb..byte].to_string()))
            },

            terminator @ '\'' | terminator @ '"' => {
                // Parse string / char literal
                let mut escaping = false;
                let mut string = String::new();

                loop {
                    let ch = peek!();

                    next!();

                    if escaping {
                        escaping = false;

                        string.push(match ch {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '0' => '\0',
                            '"' => '"' ,
                            '\\' => '\\',
                            _ => {
                                diagnostics.report(Diagnostic::unknown_escape_char(span!(pos - 2 => pos)));
                                '\0'
                            }
                        });
                    } else if ch == '\\' {
                        escaping = true;
                    } else if ch == terminator {
                        break;
                    } else {
                        string.push(ch);
                    }
                }

                let location = span!(startc => pos);

                if terminator == '"' {
                    Ok(Token::Str(location, string))
                } else {
                    if string.len() != 1 {
                        diagnostics.report(Diagnostic::invalid_char(location));
                    }

                    Ok(Token::Char(location, string.chars().next().unwrap_or('#')))
                }
            },

            ch => {
                // Parse (operator-like) identifier
                // But first, ensure this is no comment
                if ch == '/' {
                    let next = peek!();

                    if next == '/' {
                        // Single-line comment / doc comment.
                        next!();

                        let next = next!(update!(Ok(Token::EOF(startc))));

                        loop {
                            if next!(break) == '\n' {
                                break;
                            }
                        }

                        update!(match next {
                            '/' | '!' => Ok(Token::Doc(span!(startc => pos), src[startb..byte].to_string(), ch == '!')),
                            _ => Ok(Token::Ignored)
                        });
                    } else if next == '*' {
                        // Multi-line comment.
                        loop {
                            if next!() == '/' {
                                break;
                            }
                        }

                        update!(Ok(Token::Ignored));
                    }
                }

                loop {
                    let ch = peek!(break);

                    match ch {
                        // Parenthesis / comma / other exception: break.
                        '(' | ')' | ',' | ';' | '{' | '}' | '"' | '\'' => break,

                        // Whitespace (except non-breaking space): break.
                        ch if ch.is_whitespace() && ch != '\u{a0}' => break,

                        // Alphanumeric character (which corresponds to a word-like identifier): break.
                        ch if ch.is_alphanumeric() => break,

                        // Anything else: add it to the identifier.
                        _ => { next!(); }
                    }
                }

                Ok(Token::Ident(span!(startc => pos), src[startb..byte].to_string()))
            }
        };

        // update stored position, and return
        update!(result);
    }

    /// Consumes the `Lexer`, producing a vector containing all tokens of the source input.
    pub fn tokenize(mut self, diagnostics: &mut DiagnosticBag) -> CompileResult<Vec<Token>> {
        let mut vec = Vec::new();

        loop {
            match self.lex(diagnostics) {
                Ok(Token::EOF(_))  => break,
                Ok(Token::Ignored) => continue,

                Ok(token)  => vec.push(token),

                Err(error) => return Err(error)
            }
        }

        Ok(vec)
    }
}

#[cfg(test)]
describe! lexer {

    describe! failures {
        it "fails immediately on invalid characters" {
            // fn should_fail(input: &str) {
            //     assert!(Lexer::new(input).tokenize(&mut DiagnosticBag::default()).is_err());
            // }
            //
            // fn should_not_fail(input: &str) {
            //     assert!(Lexer::new(input).tokenize(&mut DiagnosticBag::default()).is_ok());
            // }

            // unsafe {
            //     // NOTE: This is currently not testable, as the lexer only accepts str's,
            //     //       and Rust str's are always valid. When going the unsafe route,
            //     //       the standard library will fail without our help.
            //     should_fail(str::from_utf8_unchecked(&[ b'h', b'e', 0xfe ]));
            //     should_fail(str::from_utf8_unchecked(&[ b'h', b'e', 0xc3, 0x28 ]));
            //     should_not_fail(str::from_utf8_unchecked(&[ b'h', b'e', 0xc3, 0xb1 ]));
            // }
        }

        it "fails lazily on invalid tokens" {
            fn should_fail(input: &str) {
                let mut diags = DiagnosticBag::default();

                assert!(Lexer::new(input).tokenize(&mut diags).is_ok());
                assert!(diags.has_error());
            }

            fn should_not_fail(input: &str) {
                let mut diags = DiagnosticBag::default();

                assert!(Lexer::new(input).tokenize(&mut diags).is_ok());
                assert!(!diags.has_error());
            }

            should_fail("'single-quoted string'");
            should_fail("'aa'");
            should_fail("'\\y'");

            should_not_fail("'\\n' '\\t'");
            should_not_fail("'a'");

            should_fail("1.11.1");
            should_fail("0.1.1");

            should_not_fail("1.11");
            should_not_fail("0.1111");
        }
    }

    before_each {
        fn tokenize(input: &str) -> Vec<Token> {
            Lexer::new(input).tokenize(&mut DiagnosticBag::default()).unwrap()
        }
        fn lex(input: &str) -> Vec<String> {
            tokenize(input).iter().map(|t| format!("{}", t)).collect()
        }
    }

    it "handles unicode characters" {
        assert_eq!(tokenize("👍🏼 👎🏼").len(), 2);
    }

    it "separates tokens correctly." {
        assert_eq!(lex("hello world everyone"),   [ "hello", "world", "everyone" ]);
        assert_eq!(lex("hello+world+everyone"),   [ "hello", "+", "world", "+", "everyone" ]);
        assert_eq!(lex("hello++world++everyone"), [ "hello", "++", "world", "++", "everyone" ]);

        assert_eq!(lex("+()<>++*$*-- $+++"), [ "+", "(", ")", "<", ">", "++*$*--", "$", "+++" ]);
    }

    it "does not separate tokens that belong together." {
        assert_eq!(lex("and he said \"foo bar\"!"), [ "and", "he", "said", "\"foo bar\"", "!" ]);
        assert_eq!(lex("non breaking space"), [ "non breaking space" ]);
    }
}

