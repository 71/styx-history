//! This module defines all parsing facilities defined in Styx.
//!
//! The `Parser` can parse expressions using user-defined parsers,
//! and binary expressions, and some concepts such as `Fixity` and `Precedence` are also defined
//! here.

use prelude::*;
use binder::{Binder, BinderParent};
use diagnostics::DiagnosticBag;
use expr::{Binary, BuiltIns, Expr, Literal, Unary, Unit};
use lexer::{HasSpan, Span, Token};

use std::mem;


/// The visibility of a symbol.
pub enum Visibility {
    /// Available to all.
    Public,

    /// Available to expressions in the scope of the symbol only.
    Private,

    /// Available to all expressions in the assembly that declared the symbol.
    Internal,

    /// Available to expressions in this scope, and all derived scopes.
    Protected
}


// //==========================================================================//
// // FIXITY / PRECEDENCE                                                      //
// //==========================================================================//

/// The fixity of an operator.
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Fixity {
    /// Left-associative unary operator, or left fixity.
    Left,

    /// Right-associative unary operator, or right fixity.
    Right
}

impl Default for Fixity {
    fn default() -> Self {
        Fixity::Left
    }
}

/// The precedence of a binary operator.
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct Precedence {
    precedence: u32,
    fixity: Fixity
}

impl Precedence {
    /// Creates a new structure that represents the precedence of a binary operator,
    /// given its precedence and fixity.
    #[inline]
    pub fn new(precedence: u32, fixity: Fixity) -> Self {
        Precedence { precedence, fixity }
    }

    /// Returns a null precedence.
    ///
    /// # Example
    /// ```
    /// use styx::parser::{Fixity, Precedence};
    ///
    /// let prec = Precedence::null();
    ///
    /// assert_eq!(prec.precedence(), 0);
    /// assert_eq!(prec.fixity(), Fixity::default());
    /// ```
    #[inline]
    pub fn null() -> Self {
        Precedence { precedence: 0, fixity: Fixity::default() }
    }

    /// Creates a left-associative precedence, given its value.
    ///
    /// # Example
    /// ```
    /// use styx::parser::{Fixity, Precedence};
    ///
    /// let prec = Precedence::left_associative(15);
    ///
    /// assert_eq!(prec.precedence(), 15);
    /// assert_eq!(prec.fixity(), Fixity::Left);
    /// ```
    #[inline]
    pub fn left_associative(precedence: u32) -> Self {
        Precedence { precedence, fixity: Fixity::Left }
    }

    /// Creates a right-associative precedence, given its value.
    ///
    /// # Example
    /// ```
    /// use styx::parser::{Fixity, Precedence};
    ///
    /// let prec = Precedence::right_associative(5);
    ///
    /// assert_eq!(prec.precedence(), 5);
    /// assert_eq!(prec.fixity(), Fixity::Right);
    /// ```
    #[inline]
    pub fn right_associative(precedence: u32) -> Self {
        Precedence { precedence, fixity: Fixity::Right }
    }

    /// Returns the value of the precedence.
    #[inline]
    pub fn precedence(&self) -> u32 {
        self.precedence
    }

    /// Returns the fixity (left or right) of the precedence.
    #[inline]
    pub fn fixity(&self) -> Fixity {
        self.fixity
    }
}

impl Default for Precedence {
    fn default() -> Self {
        Precedence { precedence: 1, fixity: Fixity::default() }
    }
}

impl ::std::cmp::PartialOrd for Precedence {
    #[inline]
    fn partial_cmp(&self, other: &Precedence) -> Option<::std::cmp::Ordering> {
        self.precedence.partial_cmp(&other.precedence)
    }
}

impl ::std::ops::Add<u32> for Precedence {
    type Output = Precedence;

    #[inline]
    fn add(self, other: u32) -> Precedence {
        Precedence { precedence: self.precedence + other, .. self }
    }
}


// //==========================================================================//
// // PARSING                                                                  //
// //==========================================================================//

/// Retains information about the slice that a [`Parser`] is supposed to parse.
#[derive(Clone, Copy)]
pub(crate) struct ParseGroup {
    pub from: usize,
    pub to: usize,
    pub position: usize,
    pub index: usize
}

/// A type that transforms a token tree into an expression.
pub struct Parser<'p, 'cx: 'p, 'vm: 'cx> {
    binder: &'p mut Binder<'cx, 'vm>,  // the reference's lifetime is actually lower than that,
                                       // but it's easier to only have two lifetimes;
                                       // in any case the binder outlives the parser
    tokens: &'p [Token],
    position: usize,
    tokens_count: usize
}

impl<'p, 'cx, 'vm> Parser<'p, 'cx, 'vm> {
    /// Creates a new parser, given its declaring scope and its source string.
    pub(crate) fn new<'a>(binder: &'p mut Binder<'cx, 'vm>, tokens: &'p [Token], group: ParseGroup) -> Self {
        Parser { binder, tokens_count: group.to - group.from, tokens: &tokens[group.from..group.to], position: group.position }
    }

    pub(crate) fn binder<'a>(&'a self) -> &'a Binder<'cx, 'vm> {
        self.binder
    }

    pub(crate) fn vm<'a>(&'a self) -> Vm<'vm> {
        self.binder.vm()
    }

    pub(crate) fn context<'a>(&'a self) -> &'a Context<'cx> {
        self.binder.context()
    }

    /// Returns whether or not other expressions can be parsed after this one.
    #[inline]
    pub fn at_end(&self) -> bool {
        self.position >= self.tokens_count
    }

    /// Returns the current position of the builder in its input vector.
    #[inline]
    pub fn position(&self) -> usize {
        self.position
    }

    /// Returns the [`Token`] currently being parsed, and reports an error if it the end of the
    /// input has been reached.
    #[inline]
    pub fn current_or_report(&self, diagnostics: Diags) -> Result<&Token, Failure> {
        match self.tokens.get(self.position) {
            Some(token) => Ok(token),
            None => {
                let span = span!(self.tokens[self.tokens_count - 1].span().start());
                report!(diagnostics, Diagnostic::unexpected_eof(span))
            }
        }
    }

    /// Returns the [`Token`] currently being parsed.
    #[inline]
    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    /// Returns the [`Span`] of the token currently being parsed.
    #[inline]
    pub fn current_span(&self, diagnostics: Diags) -> Result<Span, Failure> {
        self.current_or_report(diagnostics).map(Token::span)
    }

    /// Advances to the next token, and returns an error if an error is encountered,
    /// or the end of the input reached.
    pub fn advance(&mut self) {
        let next_pos = self.position + 1;

        if next_pos <= self.tokens_count {
            self.position = next_pos;
        }
    }

    /// Expects a token matching the given predicate.
    ///
    /// If a token can be found and it matches the given predicate, it will be returned.
    /// Else, `Failure::Match` will be returned.
    pub fn expect<P: Fn(&Token) -> bool>(&mut self, diagnostics: Diags, predicate: &P) -> Result<&Token, Failure> {
        // The tokens never change, no one cares bout mutable references
        let token = dup!(mut self => Self).current_or_report(diagnostics)?;

        if predicate(token) {
            self.advance();

            Ok(token)
        } else {
            Err(Failure::Match(token.span()))
        }
    }

    /// Returns the current operator and advances. Unlike `current`, which considers that angles are tokens
    /// in themselves, this method treats consecutive angled brackets as a single operator.
    pub fn advance_if_operator(&mut self) -> Option<Token> {
        let (start, fch) = match self.current()?.clone() {
            Token::LAngle(pos) => (pos, '<'),
            Token::RAngle(pos) => (pos, '>'),
            Token::Dollar(pos) => (pos, '$'),

            token @ Token::Ident(..) => {
                self.advance();
                return Some(token)
            },

            token => return Some(token)
        };

        let mut pos = start;
        let mut full_op = String::with_capacity(1);

        full_op.push(fch);

        loop {
            self.advance();

            match self.current()? {
                &Token::LAngle(p) if p == pos => full_op.push('<'),
                &Token::RAngle(p) if p == pos => full_op.push('>'),
                &Token::Dollar(p) if p == pos => full_op.push('$'),
                &Token::Ident(l, ref s) if l.start() == pos => full_op.push_str(s),

                _ => return Some(Token::Ident(span!(start => pos), full_op))
            }

            pos += 1;
        }
    }

    /// Parses an object of type `T`, represented by the specified [`Ty`].
    pub fn parse<T>(&mut self, ty: &'cx Ty<'cx>, diagnostics: Diags) -> Result<T, Failure> {
        let span = self.current_span(diagnostics)?;
        let original_pos = self.position;

        let mut i = 0;
        let mut syntaxc = self.binder.syntaxes().len();

        while i < syntaxc {
            // Logically, the binder shouldn't be modified unless the expression is successfully
            // bound. However, this might not always be the case, so we'll iterate over the syntax
            // items one by one, changing the syntax count variable if needed.
            #[allow(unused_unsafe)]
            let syntax = unsafe {
                dup!(self.binder => Binder).syntaxes().get_unchecked(i)
            };

            if syntax.ty() != ty {
                i += 1;
                continue
            }

            match syntax.parse(self, diagnostics) {
                Ok(ok) => return Ok(ok),
                Err(Failure::Fatal) => return Err(Failure::Fatal),
                Err(Failure::Match(_)) => {
                    self.position = original_pos;

                    syntaxc = self.binder.syntaxes().len();
                    i += 1;
                }
            }
        }

        Err(Failure::Match(span))
    }

    /// Parses a top-level expression.
    pub fn parse_expression(&mut self, diagnostics: Diags) -> ParseResult<'cx> {
        if self.at_end() {
            // specific case: empty input
            return Ok(Expr::native(Unit::new(), span!(), BuiltIns::unit()))
        }

        let result = self.parse_binary(diagnostics,
                                       &|token| match token {
                                           &Token::Semicolon(_) | &Token::RBracket(_) => true,
                                           _ => false
                                       })?;

        if !self.at_end() {
            diagnostics.report(Diagnostic::incomplete_expr(result.span()));
        }

        Ok(result)
    }

    /// Parses a binary expression, given a predicate that indicates its end.
    pub fn parse_binary<E>(&mut self, diagnostics: &mut DiagnosticBag, end: &E)
                           -> ParseResult<'cx> where E: Fn(&Token) -> bool {
        let left = match self.parse_unary(diagnostics) {
            Ok(expr) => if expr.is_empty() || self.at_end() {
                return Ok(expr)
            } else {
                expr
            },
            err => return err
        };

        self.parse_binary_inner(diagnostics, Precedence::left_associative(0), left, end)
    }

    fn parse_binary_inner<E>(&mut self, diagnostics: &mut DiagnosticBag, prec: Precedence, mut left: Expr<'cx>, expected: &E)
        -> ParseResult<'cx> where E: Fn(&Token) -> bool {

        macro_rules! get_str {
            ( $token: expr, $tok: ident => $else: expr ) => (
                match $token {
                    Token::LAngle(_) => "<".to_string(),
                    Token::RAngle(_) => ">".to_string(),
                    Token::Ident(_, ref word) => word.to_string(),
                    $tok => $else
                }
            )
        }

        let (mut curr, mut curr_op, mut curr_prec) = {
            let curr = match self.advance_if_operator() {
                Some(token) => token,
                None => return Ok(left)
            };

            if self.at_end() || expected(&curr) {
                return Ok(left)
            }

            let curr_op = get_str!(curr, token =>
                report!(diagnostics, Diagnostic::expected_token(token.span(), "operator"))
            );

            let curr_prec = self.binder().get_precedence(curr.span(), &curr_op, diagnostics);

            if curr_prec < prec {
                return Ok(left)
            }

            (curr, curr_op, curr_prec)
        };

        macro_rules! binary {
            ( $left: expr, $right: expr ) => ({
                let span = span!($left.span().start() => $right.span().end());
                let fun = self.binder().lookup_function(curr.span(), curr_op.as_str(), &[ &$left, &$right ], diagnostics);

                Expr::extension(Binary::new(fun, $left, $right), span,
                                BuiltIns::binary())
            });
        }

        loop {
            // TODO: Handle right fixity / unary postfix operators.
            let mut right = self.parse_unary(diagnostics)?;

            let (next, next_op, next_prec) = {
                let next = self.advance_if_operator();

                if let Some(next) = next {
                    if self.at_end() || expected(&next) {
                        return Ok(binary!(left, right))
                    }

                    let next_op = get_str!(next, token =>
                        report!(diagnostics, Diagnostic::expected_token(token.span(), "operator"))
                    );

                    let prec = self.binder().get_precedence(next.span(), &next_op, diagnostics);

                    (next, next_op, prec)
                } else {
                    (Token::default(), String::default(), Precedence::null())
                }
            };

            if curr_prec < next_prec {
                right = self.parse_binary_inner(diagnostics, curr_prec + 1, right, expected)?;
            }

            left = binary!(left, right);

            if self.at_end() {
                return Ok(left)
            }

            curr = next;
            curr_prec = next_prec;
            curr_op = next_op;
        }
    }

    /// Parses a unary expression, which starts with an unary operator
    /// and ends with an operand.
    fn parse_unary(&mut self, diagnostics: &mut DiagnosticBag) -> ParseResult<'cx> {
        // try quote / parenthesized expressions first
        let (quote, paren, start) = match self.current_or_report(diagnostics)? {
            &Token::LParen(s) => (false, true, s),
            &Token::Dollar(s) => (true, false, s),

            _ => (false, false, 0)
        };

        if quote {
            self.advance();

            let expr = self.parse_parenthesized(diagnostics)?;
            let span = span!(start => expr.span().end());

            return Ok(Expr::quote(expr, span));
        }
        if paren {
            return self.parse_parenthesized(diagnostics);
        }

        // try unary operator
        let original_pos = self.position;
        let (start, op) = {
            let tok = match self.advance_if_operator() {
                Some(Token::Ident(loc, ref op)) => Some((loc.start(), op.clone())),
                _ => None
            };

            match tok {
                Some(tok) => tok,
                None => return self.parse_primary(diagnostics)
            }
        };

        if !self.binder().is_prefix(&op) {
            // raw primary expression
            self.position = original_pos;

            return self.parse_primary(diagnostics)
        }

        let operand = self.parse_primary(diagnostics)?;
        let span = span!(start => operand.span().end());
        let fun = self.binder().lookup_function(span, op.as_str(), &[ &operand ], diagnostics);

        Ok(Expr::extension(Unary::new(fun, operand), span, BuiltIns::unary()))
    }

    /// Parses a parenthesized expression.
    fn parse_parenthesized(&mut self, diagnostics: Diags) -> ParseResult<'cx> {
        let start = self.current_or_report(diagnostics)?.span().start();

        if cfg!(debug) {
            match self.current_or_report(diagnostics)? {
                &Token::LParen(_) => (),
                _ => panic!("Cannot call parse_parenthesized if not on a parenthesis.")
            }
        }

        self.advance();

        let mut expr = self.parse_binary(diagnostics, &|token| match token {
            &Token::RParen(_) => true,
            _ => false
        }).unwrap_or_else(|_| Expr::native(Unit::new(), span!(), BuiltIns::unit()));

        let end = self.current_or_report(diagnostics)?.span().end();

        expr.set_span(span!(start => end));

        self.advance();

        Ok(expr)
    }

    /// Parses a primary expression.
    fn parse_primary(&mut self, diagnostics: &mut DiagnosticBag) -> ParseResult<'cx> {
        if let Some((tok, type_name)) = match self.current_or_report(diagnostics)? {
            tok @ &Token::Char(_, _) => Some((tok.clone(), "System.Char")),
            tok @ &Token::Str(_, _)  => Some((tok.clone(), "System.String")),
            tok @ &Token::Int(_, _)  => Some((tok.clone(), "System.I32")),
            tok @ &Token::Real(_, _) => Some((tok.clone(), "System.F32")),

            &Token::Ident(loc, ref ident) => if ident == "true" || ident == "false" {
                Some((Token::Ident(loc, ident.clone()), "System.Boolean"))
            } else {
                None
            },

            _ => None
        } {
            let ty = self.binder().lookup_type(tok.span(), type_name, diagnostics);
            let span = tok.span();

            self.advance();

            Ok(Expr::native(Literal::new(ty, tok), span, BuiltIns::literal()))
        } else {
            // try custom syntaxes
            let mut binder = dup!(mut self => Self).binder();
            let mut recovering = false;
            let original_pos = self.position;

            let span = self.current()
                           .expect("Cannot happen; an error would have been reported above.")
                           .span();

            loop {
                let mut parsersc = binder.syntaxes().len();
                let mut i = 0;

                while i < parsersc {
                    let mut parser = unsafe { binder.syntaxes().get_unchecked(i) };

                    if !parser.is_expression() {
                        i += 1;
                        continue
                    }

                    match parser.parse_expression(dup!(mut self), diagnostics) {
                        Ok(expr) => return Ok(expr),
                        Err(Failure::Fatal) => {
                            info!("Fatal error when parsing input using syntax '{}'.", parser.ty());
                            self.position = original_pos;
                            return Err(Failure::Fatal)
                        },
                        _ => {
                            info!("Failed to parse input using syntax '{}'.", parser.ty());
                            self.position = original_pos;
                            i += 1
                        }
                    }
                }

                match binder.parent {
                    BinderParent::Binder(p) => unsafe {
                        binder = &mut *p;
                    },
                    _ => {
                        if recovering {
                            return Err(Failure::Match(span))
                        }

                        // Error recovery: attempt to parse until end of declaration...
                        recovering = true;

                        loop {
                            self.advance();

                            match self.current() {
                                Some(&Token::RParen(_)) | Some(&Token::RBracket(_)) | Some(&Token::RAngle(_)) => break,
                                Some(_) => continue,
                                None => return Err(Failure::Match(span))
                            }
                        }
                    }
                }
            }
        }
    }
}

impl<'p, 'cx, 'vm> HasSpan for Parser<'p, 'cx, 'vm> {
    fn span(&self) -> Span {
        unsafe {
            match self.tokens_count {
                0 => Span::default(),
                1 => self.tokens.get_unchecked(0).span(),
                n => span!(self.tokens.get_unchecked(0).span().start()
                        => self.tokens.get_unchecked(n - 1).span().end())
            }
        }
    }
}


/// The reason for a failure in a parsing operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Failure {
    /// The given expression does not match the declared syntax.
    Match(Span),

    /// The given expression does match the declared syntax, but contains an unrecoverable error.
    Fatal
}

/// The result of a parsing operation.
pub type ParseResult<'cx> = Result<Expr<'cx>, Failure>;

/// A function that attempts to parse data.
pub type ParseFn<T> = for<'a, 'p, 'cx, 'vm> fn(&'a (), parser: &mut Parser<'p, 'cx, 'vm>, diagnostics: Diags<'a>) -> Result<T, Failure>;

/// A syntax parser.
pub struct Syntax<'cx> {
    ty: &'cx Ty<'cx>,
    data: Vec<u8>,
    parse: ParseFn<()>,
    parses_expr: bool
}

impl<'cx> Syntax<'cx> {
    /// Creates a new context-independant `Syntax`, given the type of value it produces, and the
    /// function used.
    pub fn new<T>(ty: &'cx Ty<'cx>, parse: ParseFn<T>) -> Self {
        Syntax { ty, data: Vec::new(), parse: unsafe { mem::transmute(parse) }, parses_expr: false }
    }

    /// Creates a new context-dependant `Syntax`, given the type of value it produces, the
    /// function used, and its data.
    pub fn new_with_state<T>(ty: &'cx Ty<'cx>, data: Vec<u8>, parse: ParseFn<T>) -> Self {
        Syntax { ty, data, parse: unsafe { mem::transmute(parse) }, parses_expr: false }
    }

    /// Creates a new context-independant `Syntax`, given the type of value it produces, and the
    /// function used.
    pub fn new_x(ty: &'cx Ty<'cx>, parse: ParseFn<Expr<'cx>>) -> Self {
        Syntax { ty, data: Vec::new(), parse: unsafe { mem::transmute(parse) }, parses_expr: true }
    }

    /// Creates a new context-dependant `Syntax`, given the type of value it produces, the
    /// function used, and its data.
    pub fn new_with_state_x(ty: &'cx Ty<'cx>, data: Vec<u8>, parse: ParseFn<Expr<'cx>>) -> Self {
        Syntax { ty, data, parse: unsafe { mem::transmute(parse) }, parses_expr: true }
    }

    /// Attempts to parse the content of the given parser using this syntax.
    pub fn parse<'p, 'vm, T>(&self, parser: &mut Parser<'p, 'cx, 'vm>, diagnostics: Diags) -> Result<T, Failure> {
        unsafe {
            let parse: ParseFn<T> = mem::transmute(self.parse);
            (parse)(&*(self.data.as_ptr() as *const ()), parser, diagnostics)
        }
    }

    /// Attempts to parse the content of the given parser using this syntax.
    pub fn parse_expression<'p, 'vm>(&self, parser: &mut Parser<'p, 'cx, 'vm>, diagnostics: Diags) -> ParseResult<'cx> {
        unsafe {
            let parse: ParseFn<Expr<'cx>> = mem::transmute(self.parse);
            (parse)(&*(self.data.as_ptr() as *const ()), parser, diagnostics)
        }
    }

    /// Returns a value that indicates whether this syntax represents an [`Expr`].
    pub fn is_expression(&self) -> bool {
        self.parses_expr
    }
}

impl<'cx> Typed<'cx> for Syntax<'cx> {
    /// Returns the type of the expression or value that is produced by this syntax parser.
    fn ty(&self) -> &'cx Ty<'cx> {
        self.ty
    }
}

