//! The [`Binder`] translates a [`Token`] stream into bound expressions
//! that can then be translated into machine code.

use prelude::*;
use prelude::parse::*;
use builder::Builder;
use core::syscore;
use expr::{BuiltIns, Unit};
use input::{Input, InputFile};
use intrinsics::import_intrinsics;
use typesystem::{Member, MemberTree};

use std::fmt::{self, Debug, Formatter};
use std::io::{self, Write};
use std::mem;

use futures::prelude::*;

/// Parent of a `Binder`.
/// - If the `Binder` is a root binder, the parent is a `VirtualMachine`,
///   and some fields are only declared for the root.
/// - If the `Binder` is a child binder, the parent is another `Binder`.
///
/// # Safety
/// The parent `Binder` is pointed to using a pointer (instead of a reference)
/// since Rust has some doesn't allow direct parent-child relationships (except
/// through smart pointers).
/// However, the root `Binder` cannot be dropped until all inner `Binder`s are done,
/// which means that no parent can be dropped during execution. All is thus safe.
pub(crate) enum BinderParent<'cx, 'vm: 'cx> {
    Root(RootBinder<'cx, 'vm>),
    Binder(*mut Binder<'cx, 'vm>)
}

/// Represents a root `Binder`.
pub(crate) struct RootBinder<'cx, 'vm: 'cx> {
    vm: Vm<'vm>,
    context: Context<'cx>,
    file: InputFile
}

impl<'cx, 'vm> RootBinder<'cx, 'vm> {
    pub fn new(vm: Vm<'vm>, file: InputFile) -> Self {
        RootBinder {
            vm, context: Context::new(vm), file
        }
    }
    pub fn _new_in_context<'pcx: 'cx>(vm: Vm<'vm>, file: InputFile, context: &'cx Context<'pcx>) -> Self {
        RootBinder {
            vm, context: Context::child(context), file
        }
    }
}

/// State of a `Binder` after executing some work.
pub enum BinderState {
    /// Completely processed its input.
    Done,

    /// Stalled, and unable to finish parsing its input.
    Stalled,

    /// Awaiting a new symbol.
    Pending
}

/// Represents a structure that dispatches parsers in a token stream.
pub struct Binder<'cx, 'vm: 'cx> {
    pub(crate) parent: BinderParent<'cx, 'vm>,
    #[allow(dead_code)]
    symbol: Sym,
    children: usize,
    parse_groups: Vec<ParseGroup>,
    imported_trees: Vec<&'cx MemberTree<'cx>>,
    syntaxes: Vec<Syntax<'cx>>,
    pending_symbols: Vec<Spanned<Sym>>,
    expressions: Vec<Expr<'cx>>,
    tokens: Vec<Token>
}

impl<'cx, 'vm: 'cx> Binder<'cx, 'vm> {
    /// Creates a new root `Binder` belonging to the specified virtual machine and binding the specified
    /// file, and returns it.
    ///
    /// # Errors
    /// If producing a token stream is impossible, `Err` will be returned.
    pub fn new(vm: Vm<'vm>, file: InputFile, diagnostics: Diags) -> Result<Self, InputFile> {
        let tokens = Lexer::new(file.source()).tokenize(diagnostics);

        if tokens.is_err() {
            return Err(file)
        }

        let tokens = tokens.unwrap();

        if tokens.is_empty() {
            return Err(file)
        }

        let mut binder = Binder {
            tokens,
            symbol: Sym::default(),
            parent: BinderParent::Root(RootBinder::new(vm, file)),
            imported_trees: Vec::new(),
            syntaxes: Vec::new(),
            parse_groups: Vec::new(),
            pending_symbols: Vec::new(),
            expressions: Vec::new(),
            children: 0
        };

        syscore::initialize(&mut binder, vm);
        import_intrinsics(&mut binder);
        Self::initialize(&mut binder, diagnostics);

        Ok(binder)
    }

    /// Creates a new empty `Binder`, with no diagnostics nor input.
    pub fn empty(vm: Vm<'vm>) -> Self {
        let mut binder = Binder {
            tokens: Vec::new(),
            symbol: Sym::default(),
            parent: BinderParent::Root(RootBinder::new(vm, InputFile::new(String::new(), String::new(), 0))),
            imported_trees: Vec::new(),
            syntaxes: Vec::new(),
            parse_groups: Vec::new(),
            pending_symbols: Vec::new(),
            expressions: Vec::new(),
            children: 0
        };

        syscore::initialize(&mut binder, vm);
        import_intrinsics(&mut binder);

        binder
    }

    /// Creates a new `Binder` whose context is given by another binder, in order
    /// to bind a given file.
    pub fn resume<'a>(binder: &'a mut Binder<'cx, 'vm>, file: InputFile, diagnostics: Diags) -> Result<&'a mut Self, InputFile> {
        let tokens = Lexer::new(file.source()).tokenize(diagnostics);

        if tokens.is_err() {
            return Err(file)
        }

        let tokens = tokens.unwrap();

        if tokens.is_empty() {
            return Err(file)
        }

        binder.tokens = tokens;

        Self::initialize(binder, diagnostics);

        Ok(binder)
    }

    /// Creates a `Binder` that acts as a "child" of the current binder,
    /// but having a smaller scope and a specific set of tokens.
    pub fn child<'c>(&'c mut self, symbol: Sym, tokens: Vec<Token>, diagnostics: Diags) -> Binder<'c, 'vm> {
        self.children += 1;

        let mut binder = Binder {
            tokens, symbol,
            parent: BinderParent::Binder(unsafe { mem::transmute(self as *mut _) }),
            imported_trees: Vec::new(),
            syntaxes: Vec::new(),
            parse_groups: Vec::new(),
            pending_symbols: Vec::new(),
            expressions: Vec::new(),
            children: 0
        };

        Self::initialize(&mut binder, diagnostics);

        binder
    }

    /// Initializes the `Binder`, populating its sub-parsers.
    fn initialize<'a, 'lcx>(binder: &'a mut Binder<'lcx, 'vm>, diagnostics: Diags) {
        // We're dividing tokens into different groups in order to parse groups in any order.
        let mut size = 0;
        let mut depth = 0;
        let mut index = 0;

        let tokens = &binder.tokens;
        let mut groups = Vec::new();

        /// Groups multiple tokens together into a `ParseGroup`.
        macro_rules! group {
            () => ({
                // Adds a new parser to the parsers vector, populating it with every token
                // encountered till now
                let nth = groups.len();

                groups.push(ParseGroup { from: index, to: index + size, position: 0, index: nth });

                index += size;
                size = 0;

                // The previous assignements raise warnings for no reason
                // FIX: https://github.com/rust-lang/rust/issues/24580
                move || assert!(index != size); // Anonymous closure is never actually called
            });
        }

        /// Gets the `Token` at the current position, performing a few checks beforehand.
        macro_rules! token {
            () => (match tokens.get(size) {
                Some(token) => {
                    size += 1;
                    token
                },
                None => {
                    if depth != 0 {
                        // Invalid end of statement.
                        let last_token = unsafe {
                            // safe to do, since depth is 0 when size is 0
                            tokens.get_unchecked(size - 1)
                        };

                        diagnostics.report(Diagnostic::unexpected_eof(last_token.span()))
                    } else if size != 0 {
                        // Non-empty statement, regroup everything
                        group!();
                    }
                    break
                }
            });
        }

        // Collect tokens into different groups
        loop {
            match token!() {
                // End of statement
                &Token::Semicolon(_) => group!(),

                // Start of a bracketed scope, increase depth and start parsing inner
                &Token::LBracket(_)  => {
                    depth += 1;

                    loop {
                        match token!() {
                            &Token::LBracket(_) => depth += 1,
                            &Token::RBracket(_) => {
                                depth -= 1;

                                if depth == 0 {
                                    group!();
                                    continue
                                }
                            },

                            _ => ()
                        }
                    }
                },

                _ => ()
            }
        }

        // Set parsers
        mem::replace(&mut binder.parse_groups, groups);
    }

    /// Parses, binds and executes the content given to this `Binder`.
    ///
    /// If `quote` is `true`, then the content will not be executed,
    /// and an expression will be returned instead.
    pub fn process(&mut self, quote: bool, diagnostics: Diags) -> BinderState {
        let mut stalled = 0;
        let mut i = 0;

        // Making a refernce to the tokens that the borrow checked won't care about,
        // since once tokens have been produced, they are NEVER modified.
        let tokens = dup!(self.tokens.as_slice());

        // Take care of every parser, one by one, starting at the first one
        'outer: loop {
            let parsed = loop {
                // Some strange scopes to make the borrow checker happy...
                let reason = {
                    let mut parser = match self.parse_groups.get(i).cloned() {
                        Some(group) => Parser::new(self, &tokens, group),
                        None => return BinderState::Done
                    };

                    match parser.parse_expression(diagnostics) {
                        Ok(parsed) => break parsed,
                        Err(reason) => reason
                    }
                };

                if let Failure::Match(span) = reason {
                    stalled += 1;

                    if stalled == self.parse_groups.len() {
                        diagnostics.report(Diagnostic::no_match(span));
                        return BinderState::Stalled
                    }
                } else {
                    self.parse_groups.remove(i);
                }

                continue 'outer
            };

            if diagnostics.has_error() {
                i += 1;
                continue
            }

            // We got this far, which means that the parsed expression was parsed successfully.
            // We can add it to the parsed expressions, or execute it.
            let group = self.parse_groups.remove(i);

            if quote {
                self.expressions.insert(group.index, parsed)
            } else {
                self.compute(&parsed, diagnostics)
            }
        }
    }

    fn compute(&mut self, expr: &Expr<'cx>, diagnostics: Diags) {
        let mut builder = Builder::new::<_, String>(
            self.ctx(),
            ::arch::Architecture::current().unwrap(),
            expr.span(),
            diagnostics, "<tmp>", &[]);

        expr.build(&mut builder);
    }

    /// Returns a reference to the [`Context`] to which this binder belongs.
    pub fn context(&self) -> &Context<'cx> {
        &self.root().context
    }

    /// Returns a mutable reference to the [`Context`] to which this binder belongs.
    pub fn ctx<'a>(&'a mut self) -> &'cx mut Context<'cx> {
        unsafe {
            &mut *(&mut self.mut_root().context as *mut _)
        }
    }

    /// Returns a [`Vm`] object that represents the virtual machine that created this binder.
    pub fn vm<'s>(&'s self) -> Vm<'vm> {
        self.root().vm
    }

    pub(crate) fn root(&self) -> &RootBinder<'cx, 'vm> {
        match &self.parent {
            &BinderParent::Root(ref root) => root,
            &BinderParent::Binder(binder) => unsafe { (*binder).root() }
        }
    }

    pub(crate) fn mut_root(&mut self) -> &mut RootBinder<'cx, 'vm> {
        match &mut self.parent {
            &mut BinderParent::Root(ref mut root) => root,
            &mut BinderParent::Binder(binder) => unsafe { (*binder).mut_root() }
        }
    }

    /// Returns a reference to the parent binder of this binder, if it has one.
    pub fn parent(&self) -> Option<&Self> {
        match self.parent {
            BinderParent::Root(_) => None,
            BinderParent::Binder(binder) => unsafe { binder.as_ref() }
        }
    }

    /// Returns a mutable reference to the parent binder of this binder, if it has one.
    pub fn mut_parent(&mut self) -> Option<&mut Self> {
        match self.parent {
            BinderParent::Root(_) => None,
            BinderParent::Binder(binder) => unsafe { binder.as_mut() }
        }
    }

    /// Returns whether or not this binder is pending, which means that it has jobs pending.
    pub fn pending(&self) -> bool {
        self.parse_groups.is_empty() || self.children > 0
    }

    /// Returns the file to which this binder corresponds.
    pub fn file(&self) -> &InputFile {
        &self.root().file
    }
}

unsafe impl<'cx, 'vm: 'cx> Send for Binder<'cx, 'vm> { }
unsafe impl<'cx, 'vm: 'cx> Sync for Binder<'cx, 'vm> { }

impl<'cx, 'vm: 'cx> Binder<'cx, 'vm> {
    /// Returns whether the given operand is a unary prefix operator.
    pub fn is_prefix(&self, op: &str) -> bool {
        self.vm().read().unary_operators.contains_key(op)
    }

    /// Returns the precedence of the given binary operator.
    ///
    /// # Errors
    /// If the specified operator is unknown, a `Diagnostic` will be returned.
    pub fn get_precedence(&self, span: Span, op: &str, diagnostics: Diags) -> Precedence {
        match self.vm().read().binary_operators.get(op) {
            Some(op) => *op,
            None => {
                diagnostics.report(Diagnostic::undefined_binary_op(span, op));
                Precedence::default()
            }
        }
    }

    /// Looks up the type matching the given name.
    ///
    /// # Errors
    /// If the type cannot be found, a `Diagnostic` will be reported, and [`Ty::unknown`] will be
    /// returned.
    pub fn lookup_type(&self, span: Span, name: &str, diagnostics: Diags) -> &'cx Ty<'cx> {
        let sym = Sym::from(name);

        let diag = match self.context().members().get(&sym) {
            Some(&Member::Type(ty)) => return ty,
            Some(_) => Diagnostic::not_a_type(span),
            None => Diagnostic::undefined_type(span, name)
        };

        if let Some(&Member::Type(ty)) = self.vm().read().context().members().get(&sym) {
            return unsafe { mem::transmute(ty) }
        }

        diagnostics.report(diag);
        Ty::unknown()
    }

    /// Looks up the function matching the given name and arguments.
    ///
    /// # Errors
    /// If the function cannot be found, a `Diagnostic` will be reported, and [`Fun::unknown`] will be
    /// returned.
    pub fn lookup_function(&self, span: Span, name: &str, args: &[&Expr<'cx>], diagnostics: Diags) -> &'cx Fun<'cx> {
        fn closeness<'cx>(args: &[&Expr<'cx>], fun: &'cx Fun<'cx>) -> usize {
            let diff = (args.len() as isize) - (fun.parameters().len() as isize);

            // ensure that both functions have the same number of parameters
            if diff != 0 {
                // parameter count mismatch, return large difference
                return diff.abs() as usize * 100
            }

            let mut diff = 0;

            for (arg, param) in args.iter().zip(fun.parameters()) {
                // ensuring the type corresponds:
                // type equality, or expression type
                if arg.ty() == param.ty() {
                    continue
                }

                if let Some(ty) = param.ty().generic_parameters().get(0) {
                    if ty == arg.ty() && param.ty() == Ty::expression() {
                        continue
                    }
                }

                diff += 1;
            }

            diff
        }

        fn lookup_tree<'cx>(symbol: &Sym, args: &[&Expr<'cx>], tree: &MemberTree<'cx>, matches: &mut Vec<(&'cx Fun<'cx>, usize)>) {
            let lasthash = symbol.parts()[symbol.parts().len() - 1];
            let node = match tree.lookup(symbol) {
                Some(node) => node,
                None => return
            };

            for item in node.siblings() {
                if let Some(&Member::Function(fun)) = item.value() {
                    if item.hash() == lasthash {
                        let closeness = closeness(args, fun);
                        matches.push((fun, closeness));
                    }
                }
            }
        }

        // find all functions that bear the given name, finding how much their parameters match the
        // given arguments
        let mut matches = Vec::new();
        let sym = Sym::from(name);

        for tree in &self.imported_trees {
            lookup_tree(&sym, args, tree, &mut matches);
        }

        lookup_tree(&sym, args, self.context().members(), &mut matches);

        unsafe {
            lookup_tree(&sym, args, mem::transmute(self.vm().read().context().members()), &mut matches);
        }

        // no function bear the given name, report this and return
        if matches.is_empty() {
            diagnostics.report(Diagnostic::undefined_function(span, name));
            return Fun::unknown()
        }

        // find the closest match
        // note that the closer 'closest' is to 0, the closer the actual match;
        // 'closest' == 0 <=> perfect match
        let (mut target, mut closest) = matches.swap_remove(0);

        for (fun, closeness) in matches.drain(..) {
            if closeness >= closest {
                if closest == 0 && closeness == 0 && fun != target {
                    // ambiguous perfect match
                    diagnostics.report(Diagnostic::ambiguous_match(span));
                    return Fun::unknown()
                }

                // equal (or lower) match; we keep the previous function, because its
                // importing scope is closer to the call
                continue
            }

            // greater match, update target
            target = fun;
            closest = closeness;
        }

        if closest == 0 {
            // perfect match, return it
            target
        } else {
            // no match; report the error, displaying the closest match
            diagnostics.report(Diagnostic::signature_mismatch(span));
            Fun::unknown()
        }
    }

    /// Returns a slice containing all parsers in this scope.
    pub fn syntaxes(&self) -> &[Syntax<'cx>] {
        &self.syntaxes
    }

    /// Declares a member at the context level.
    #[allow(needless_pass_by_value)] // Probably will need this later on
    pub fn declare(&self, _member: Member<'cx>) {
        unimplemented!()
    }

    /// Imports the specified tree in this scope, making all of its direct members
    /// available in this binder.
    pub fn import_tree<'s>(&'s mut self, node: &'cx MemberTree<'cx>) {
        let nodes = match node.child() {
            Some(child) => child.siblings(),
            None => return
        };

        for sibl in nodes {
            self.imported_trees.push(sibl)
        }
    }

    /// Imports the given syntax in this context.
    pub fn import_syntax(&mut self, syntax: Syntax<'cx>) {
        self.syntaxes.push(syntax);
    }
}

/// A `Future` that represents a group of parsers and binders
/// running on an input.
pub struct Bind<'cx, 'vm: 'cx> {
    vm: Vm<'vm>,
    diagnostics: DiagnosticBag,
    input_stream: Input,
    binders: Vec<Binder<'cx, 'vm>>,
    pending_symbols: Vec<Sym>,
    failing: bool
}

impl<'cx, 'vm> Bind<'cx, 'vm> {
    /// Creates a new binding stream that returns
    /// expressions as they are parsed.
    pub(crate) fn new(vm: Vm<'vm>, input: Input) -> Self {
        Bind {
            vm,
            binders: Vec::with_capacity(input.len()),
            input_stream: input,
            diagnostics: DiagnosticBag::default(),
            pending_symbols: Vec::new(),
            failing: false
        }
    }
}

impl<'cx, 'vm> Stream for Bind<'cx, 'vm> {
    type Item  = Bound<'cx>;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        macro_rules! take_diagnostics {
            () => ( mem::replace(&mut self.diagnostics, DiagnosticBag::default()) );
        }

        // take care of unfinished file reads
        if let Async::Ready(Some(file)) = self.input_stream.poll()? {
            let hash = file.hash();

            match Binder::<'cx, 'vm>::new(self.vm, file, self.diagnostics.set_active_file(hash)) {
                Ok(binder) => self.binders.push(binder),
                Err(file) => {
                    let span = file.span();

                    return Ok(Async::Ready(Some(Bound {
                        succeeded: false, file,
                        expression: Expr::native(Unit::new(), span, BuiltIns::unit()),
                        diagnostics: take_diagnostics!(),
                        missing_symbols: Vec::new()
                    })));
                }
            }
        }

        // process current tasks
        let binders = &mut self.binders;

        if binders.is_empty() {
            return Ok(Async::Ready(None))
        }

        macro_rules! fail {
            () => {{
                let Binder { parent, pending_symbols, mut expressions, .. } = binders.swap_remove(0);
                let file = match parent {
                    BinderParent::Root(RootBinder { file, .. }) => file,
                    _ => panic!("Not possible")
                };

                let expression = if expressions.len() == 1 {
                    expressions.swap_remove(0)
                } else if expressions.len() == 0 {
                    Expr::native(Unit::new(), file.span(), BuiltIns::unit())
                } else {
                    Expr::block(expressions, file.span())
                };

                return Ok(Async::Ready(Some(Bound {
                    succeeded: false,
                    expression, file,
                    diagnostics: take_diagnostics!(),
                    missing_symbols: pending_symbols
                })))
            }};
        }

        if self.failing {
            fail!()
        }

        let mut pending = 0;
        let mut i = 0;

        while i < binders.len() {
            let file = binders[i].file().hash();
            let succeeded = match binders[i].process(false, self.diagnostics.set_active_file(file)) {
                BinderState::Pending => {
                    pending += 1;
                    i += 1;
                    continue
                },
                BinderState::Stalled => false,
                BinderState::Done => true
            };

            let Binder { parent, pending_symbols, mut expressions, .. } = binders.swap_remove(i);

            let file = match parent {
                BinderParent::Root(RootBinder { file, .. }) => file,
                _ => panic!("Non-root binder cannot be returned.")
            };

            let expression = if expressions.len() == 1 {
                expressions.swap_remove(0)
            } else {
                Expr::block(expressions, file.span())
            };

            let bound = Bound {
                expression, file,
                diagnostics: take_diagnostics!(),
                missing_symbols: pending_symbols,
                succeeded
            };

            return Ok(Async::Ready(Some(bound)))
        }

        // return if we're simply waiting for more binders to finish their work
        if pending != binders.len() {
            return Ok(Async::NotReady)
        }

        // ugh, it failed
        self.failing = true;
        fail!()
    }
}

impl<'cx, 'vm> Debug for Bind<'cx, 'vm> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("Bind")
            .field("pending", &self.pending_symbols.len())
            .field("diagnostics", &self.diagnostics.len())
            .finish()
    }
}

/// A bound `Expr`.
///
/// The `expression` field will always return a big `Expr` that contains
/// every expression declared in the file in an AST.
#[derive(Debug)]
pub struct Bound<'cx> {
    /// The bound expression.
    /// If the binding phase failed, this field will be `None`.
    pub expression: Expr<'cx>,

    /// The diagnostics reported during the parsing of this file.
    pub diagnostics: DiagnosticBag,

    /// The symbols that were missing.
    pub missing_symbols: Vec<Spanned<Sym>>,

    /// The file whose content was parsed and bound.
    pub file: InputFile,

    /// Whether or not the binding operation was entirely successful.
    pub succeeded: bool
}

impl<'a, 'cx, 'vm> Bound<'cx> where 'vm: 'cx, 'vm: 'a {
    /// Creates a new `Bound` structure, given the binder that contains the data,
    /// and the diagnostics gotten thus far.
    pub fn new(binder: &'a mut Binder<'cx, 'vm>, diagnostics: DiagnosticBag) -> Bound<'cx> {
        let file = match &mut binder.parent {
            &mut BinderParent::Root(RootBinder { ref mut file, .. }) => mem::replace(file, InputFile::empty()),
            _ => panic!("Cannot get bound node from child binder")
        };
        let expression = {
            let mut exprs: Vec<Expr<'cx>> = mem::replace(&mut binder.expressions, Vec::new());

            if exprs.len() == 1 {
                exprs.remove(0)
            } else {
                Expr::block(exprs, file.span())
            }
        };

        Bound {
            succeeded: !diagnostics.has_error(),
            expression,
            diagnostics,
            file,
            missing_symbols: mem::replace(&mut binder.pending_symbols, Vec::new())
        }
    }
}

impl<'cx> Bound<'cx> {
    /// Generates a `Diagnostic` for every missing symbol.
    pub fn generate_missing_symbols_diagnostics(&mut self) {
        for symbol in &self.missing_symbols {
            self.diagnostics.report(Diagnostic::undefined_symbol(symbol.span(), symbol.full_name()));
        }
    }

    /// Prints the diagnostics encountered during the binding operation to the given stream.
    pub fn print_diagnostics(&self, out: &mut Write, errors: bool, warnings: bool, infos: bool) {
        use diagnostics::{DiagnosticPrinter, DiagnosticSeverity};

        let mut printer = DiagnosticPrinter::new(out, &self.file);

        let diagnostics = self.diagnostics.as_ref().iter().filter(|diag| match diag.severity() {
            DiagnosticSeverity::Fatal               => true,
            DiagnosticSeverity::Error if errors     => true,
            DiagnosticSeverity::Warning if warnings => true,
            DiagnosticSeverity::Info if infos       => true,
            _ => false
        });

        printer.print_many(diagnostics);
    }
}

