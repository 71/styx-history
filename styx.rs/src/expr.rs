//! The built-in expression types.
//!
//! The Styx expression can be one of two kinds:
//! - [`Extension`], which gets lowered into another expression;
//! - [`Native`], which produces IR through a [`Builder`].
//!
//! All expressions have a type and are spanned, and may computed during compilation.

use prelude::*;
use builder::{Value, ValueKind};
use lexer::{Span, Spanned};
use symbols::Sym;

use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;
use std::mem;


// //==========================================================================//
// // EXPRESSION                                                               //
// //==========================================================================//

/// A Styx expression.
#[derive(Debug)]
pub enum Expression<'cx> {
    /// Expression that can output IR.
    Native(Native<'cx>),

    /// Expression built on top of other expressions.
    Extension(Extension<'cx>),

    /// Quoted expression.Expression represented as an expression.
    Quote(Box<Expr<'cx>>)
}

impl<'cx> Typed<'cx> for Expression<'cx> {
    #[inline]
    fn ty(&self) -> &'cx Ty<'cx> {
        match self {
            &Expression::Native(ref nat)    => nat.ty,
            &Expression::Extension(ref ext) => ext.ty,
            &Expression::Quote(ref _quote)  => unimplemented!()
        }
    }
}

/// The signature of the `Extension::expand` function.
pub type ExpandFn<'cx> = for<'a> fn(*const (), &'a Context<'cx>) -> Expression<'cx>;
/// The signature of the `Native::emit` function.
pub type EmitFn<'cx> = for<'a, 'b: 'a> fn(*const (), &'a mut Builder<'b, 'cx>) -> Value<'b, 'cx>;

/// The signature of the `Extension::children` and `Native::children` functions.
pub type ChildrenFn = fn(*const ()) -> usize;
/// The signature of the `Extension::child` and `Native::child` functions.
pub type ChildFn<'cx> = fn(*const (), usize) -> Option<*const Expr<'cx>>;
/// The signature of the `Extension::mut_child` and `Native::mut_child` functions.
pub type MutChildFn<'cx> = fn(*mut (), usize) -> Option<*mut Expr<'cx>>;

fn def_children_fn(_self: *const ()) -> usize { 0 }
fn def_child_fn<'cx>(_self: *const (), _i: usize) -> Option<*const Expr<'cx>> { None }
fn def_mut_child_fn<'cx>(_self: *mut (), _i: usize) -> Option<*mut Expr<'cx>> { None }


// //==========================================================================//
// // EXTENSION                                                                //
// //==========================================================================//

/// Represents an `Expression` that can be lowered into a lower-level `Expression`.
pub struct Extension<'cx> {
    ty: &'cx Ty<'cx>,
    target_ty: &'cx Ty<'cx>,
    data: Vec<u8>,
    expand_fn: ExpandFn<'cx>,
    children_fn: ChildrenFn,
    child_fn: ChildFn<'cx>,
    mut_child_fn: MutChildFn<'cx>
}

impl<'cx> Extension<'cx> {
    /// Creates a new extension expression, given its beauty.
    pub fn new<T>(ty: &'cx Ty<'cx>, target_ty: &'cx Ty<'cx>, data: Box<T>,
                  expand_fn: ExpandFn<'cx>,
                  children_fn: Option<ChildrenFn>,
                  child_fn: Option<ChildFn<'cx>>,
                  mut_child_fn: Option<MutChildFn<'cx>>) -> Self {
        Extension {
            ty, target_ty,
            data: unsafe {
                Vec::from_raw_parts(Box::into_raw(data) as *mut u8, mem::size_of::<T>(), mem::size_of::<T>())
            },
            expand_fn,
            children_fn: children_fn.unwrap_or(def_children_fn),
            child_fn: child_fn.unwrap_or(def_child_fn),
            mut_child_fn: mut_child_fn.unwrap_or(def_mut_child_fn)
        }
    }

    /// Returns the type to which this extension expands.
    pub fn expands_to(&self) -> &'cx Ty<'cx> {
        self.target_ty
    }

    /// Expands the extension expression into a lower-level `Expression`.
    pub fn expand(&self, context: &Context<'cx>) -> Expression<'cx> {
        (self.expand_fn)(self.data.as_ptr() as *const (), context)
    }

    /// Expands the extension expression until it is either `Native` or `Raw`.
    pub fn expand_fully(&self, context: &Context<'cx>) -> Expression<'cx> {
        let mut expanded = self.expand(context);

        while let Expression::Extension(ext) = expanded {
            expanded = ext.expand(context);
        }

        expanded
    }

    /// Returns an integer that corresponds to the number of children that this expression has.
    pub fn children(&self) -> usize {
        (self.children_fn)(self.data.as_ptr() as *const ())
    }

    /// Returns the nth child of the expression.
    pub fn child(&self, index: usize) -> Option<&Expr<'cx>> {
        match (self.child_fn)(self.data.as_ptr() as *const (), index) {
            Some(ptr) => Some(unsafe { &*ptr }),
            None => None
        }
    }

    /// Mutably returns the nth child of the expression.
    pub fn mut_child(&mut self, index: usize) -> Option<&mut Expr<'cx>> {
        match (self.mut_child_fn)(self.data.as_ptr() as *mut (), index) {
            Some(ptr) => Some(unsafe { &mut *ptr }),
            None => None
        }
    }
}

impl<'cx> Debug for Extension<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("Extension").field("ty", &self.ty).finish()
    }
}


// //==========================================================================//
// // NATIVE                                                                   //
// //==========================================================================//

/// Represents an `Expression` that can emit machine code directly.
pub struct Native<'cx> {
    ty: &'cx Ty<'cx>,
    data: Vec<u8>,
    emit_fn: EmitFn<'cx>,
    children_fn: ChildrenFn,
    child_fn: ChildFn<'cx>,
    mut_child_fn: MutChildFn<'cx>
}

impl<'cx> Native<'cx> {
    /// Creates a new native expression given its type, data and emit function.
    pub fn new<T>(ty: &'cx Ty<'cx>, data: Box<T>,
                  emit_fn: EmitFn<'cx>,
                  children_fn: Option<ChildrenFn>,
                  child_fn: Option<ChildFn<'cx>>,
                  mut_child_fn: Option<MutChildFn<'cx>>) -> Self {
        Native {
            ty,
            data: unsafe {
                Vec::from_raw_parts(Box::into_raw(data) as *mut u8, mem::size_of::<T>(), mem::size_of::<T>())
            },
            emit_fn,
            children_fn: children_fn.unwrap_or(def_children_fn),
            child_fn: child_fn.unwrap_or(def_child_fn),
            mut_child_fn: mut_child_fn.unwrap_or(def_mut_child_fn)
        }
    }

    /// Compiles the expression to IR using the specified builder, and returns a `Value`
    /// that represents the computed value of the expression.
    pub fn emit<'b>(&self, builder: &mut Builder<'b, 'cx>) -> Value<'b, 'cx> {
        (self.emit_fn)(self.data.as_ptr() as *mut (), builder)
    }

    /// Returns an integer that corresponds to the number of children that this expression has.
    pub fn children(&self) -> usize {
        (self.children_fn)(self.data.as_ptr() as *const ())
    }

    /// Returns the nth child of the expression.
    pub fn child(&self, index: usize) -> Option<&Expr<'cx>> {
        match (self.child_fn)(self.data.as_ptr() as *const (), index) {
            Some(ptr) => Some(unsafe { &*ptr }),
            None => None
        }
    }

    /// Mutably returns the nth child of the expression.
    pub fn mut_child(&mut self, index: usize) -> Option<&mut Expr<'cx>> {
        match (self.mut_child_fn)(self.data.as_ptr() as *mut (), index) {
            Some(ptr) => Some(unsafe { &mut *ptr }),
            None => None
        }
    }
}

impl<'cx> Debug for Native<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("Native").field("ty", &self.ty).finish()
    }
}


// //==========================================================================//
// // EXPR                                                                     //
// //==========================================================================//

/// Represents an `Expression` in source code.
pub struct Expr<'cx> {
    exty: &'cx Ty<'cx>,
    data: Expression<'cx>,
    immediate: bool,
    span: Span
}

impl<'cx> Expr<'cx> {
    /// Creates a new `Expr`, given its inner `Expression` and its `Location`.
    pub fn new(data: Expression<'cx>, span: Span, exty: &'cx Ty<'cx>) -> Self {
        Expr { data, span, exty, immediate: false }
    }

    /// Creates a new `Expressions::Native` expression, given its data and span.
    pub fn native<N: Into<Native<'cx>>>(data: N, span: Span, exty: &'cx Ty<'cx>) -> Self {
        Expr { data: Expression::Native(data.into()), span, exty, immediate: false }
    }

    /// Creates a new `Expressions::Extension` expression, given its data and span.
    pub fn extension<E: Into<Extension<'cx>>>(data: E, span: Span, exty: &'cx Ty<'cx>) -> Self {
        Expr { data: Expression::Extension(data.into()), span, exty, immediate: false }
    }

    /// Creates a new `Expressions::Native` expression, given its data and span.
    pub fn spanned_native<N: Into<Native<'cx>>>(data: Spanned<N>, exty: &'cx Ty<'cx>) -> Self {
        let span = data.span();
        Expr { data: Expression::Native(data.value().into()), span, exty, immediate: false }
    }

    /// Creates a new `Expressions::Extension` expression, given its data and span.
    pub fn spanned_extension<E: Into<Extension<'cx>>>(data: Spanned<E>, exty: &'cx Ty<'cx>) -> Self {
        let span = data.span();
        Expr { data: Expression::Extension(data.value().into()), span, exty, immediate: false }
    }

    /// Creates a new `Expressions::Block` expression, given its content and span.
    pub fn block(expressions: Vec<Expr<'cx>>, span: Span) -> Self {
        Expr::native(Block::new(expressions), span, BuiltIns::block())
    }

    /// Creates a new `Expressions::Quote` expression, given the expression it quotes and span.
    pub fn quote(expr: Expr<'cx>, span: Span) -> Self {
        Expr { data: Expression::Quote(box expr), span, exty: Ty::quote(), immediate: false }
    }

    /// Creates a new `Expressions::Return` expression.
    pub fn retn(expr: Expr<'cx>) -> Self {
        let span = expr.span();

        Expr::native(Return::new(expr), span, BuiltIns::ret())
    }

    /// Returns this expression, with its `immediate` field set to `true`.
    pub fn into_immediate(mut self) -> Self {
        self.immediate = true;
        self
    }

    pub(crate) fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

impl<'cx> Expr<'cx> {
    /// Translates the current expression into native code through the specified builder.
    pub fn build<'b>(&self, builder: &mut Builder<'b, 'cx>) -> Value<'b, 'cx> {
        match &self.data {
            &Expression::Quote(ref _quote)  => unimplemented!(),
            &Expression::Extension(ref ext) => Expr::new(ext.expand_fully(builder.ctx), self.span(), ext.expands_to()).build(builder),
            &Expression::Native(ref nat)    => nat.emit(builder)
        }
    }

    /// Computes the value of the expression right away, and returns it.
    ///
    /// # Errors
    /// If the value cannot be computed at compile-time, returns `None`.
    pub fn compute<'t, T>(&self, context: &'t Context<'cx>, diagnostics: Diags) -> Option<T> where 'cx: 't {
        use assembler::{AssemblyGraph, AssemblyStyle};
        use std::ptr;

        if !self.is_constant() {
            return None
        }

        let vm = context.vm();

        unsafe {
            // build expression
            let copy: Expr<'t> = ptr::read(mem::transmute(self as *const Expr<'cx>));
            let fun = Fun::anonymous(copy);
            let mut context = Context::child(context);

            {
                let mut graph = AssemblyGraph::
                    new(Architecture::current().unwrap(), diagnostics, &mut context, mem::transmute(&fun))
                   .expect("Could not create graph.");

                if diagnostics.has_error() {
                    return None
                }

                graph.process(AssemblyStyle::DS, diagnostics, vm);

                if diagnostics.has_error() {
                    return None
                }
            }

            // execute expression
            let procd = take_ok!(context.procedures().iter().find(|p| p.function() as *const _ == &fun as *const _));
            let procd = take_ok!(procd.ds_proc());

            let f: fn() -> T = mem::transmute(procd.ptr);

            // fetch return value
            Some(f())
        }
    }

    /// Returns whether or not this expression is immediate.
    pub fn is_immediate(&self) -> bool {
        self.immediate
    }

    /// Right now everything is constant (can be computed at compile-time).
    pub fn is_constant(&self) -> bool {
        true
    }

    /// Returns the inner data of this expression.
    pub fn data(&self) -> &Expression<'cx> {
        &self.data
    }

    /// Returns the inner data of this expression.
    pub fn mut_data(&mut self) -> &mut Expression<'cx> {
        &mut self.data
    }

    /// Returns the type of the expression (and not of the data it encapsulates).
    pub fn expr_ty(&self) -> &'cx Ty<'cx> {
        self.exty
    }

    /// Returns a number that indicates how many children this expression has.
    pub fn child_count(&self) -> usize {
        match &self.data {
            &Expression::Extension(ref ext) => ext.children(),
            &Expression::Native(ref nat) => nat.children(),
            _ => 0
        }
    }

    /// Returns the child expression at the given index.
    pub fn child<'a>(&'a self, nth: usize) -> Option<&'a Expr<'cx>> {
        match &self.data {
            &Expression::Extension(ref ext) => ext.child(nth),
            &Expression::Native(ref nat) => nat.child(nth),
            _ => None
        }
    }

    /// Returns the mutable child expression at the given intex.
    pub fn mut_child<'a>(&'a mut self, nth: usize) -> Option<&'a mut Expr<'cx>> {
         match &mut self.data {
            &mut Expression::Extension(ref mut ext) => ext.mut_child(nth),
            &mut Expression::Native(ref mut nat) => nat.mut_child(nth),
            _ => None
        }
    }
}

impl<'cx> Typed<'cx> for Expr<'cx> {
    #[inline]
    fn ty(&self) -> &'cx Ty<'cx> {
        self.data.ty()
    }
}

impl<'cx> HasSpan for Expr<'cx> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> Default for Expr<'cx> {
    /// Returns a new [`Unit`] expression with the default span.
    fn default() -> Self {
        Expr::native(Unit::new(), span!(), BuiltIns::unit())
    }
}

impl<'cx> From<Span> for Expr<'cx> {
    /// Returns a new [`Unit`] expression with the given span.
    fn from(span: Span) -> Self {
        Expr::native(Unit::new(), span, BuiltIns::unit())
    }
}

impl<'cx> Debug for Expr<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}) {:?}", self.exty, self.data)
    }
}


// //==========================================================================//
// // BUILT-IN                                                                 //
// //==========================================================================//

/// Defines all built-in expression types.
pub struct BuiltIns;

/// Macro that provides an easy way to declare a `Native` expression.
#[macro_export]
macro_rules! native {
    (
        $(#[$attr:meta])*
        struct $name: ident <$cx: tt> as $lc_name: ident {
            $( $fldn: ident: $fldty: ty ),*
        }

        type $self: ident => $type: expr;

        emit ($builder: ident) => $body: expr;
    ) => {
        native! {
            $(#[$attr])*
            struct $name<$cx> as $lc_name {
                $( $fldn: $fldty ),*
            }

            type $self => $type;

            emit ($builder) => $body;
            children () => 0;
            child (_i) => None;
            mut_child (_i) => None;
        }
    };

    (
        $(#[$attr:meta])*
        struct $name: ident <$cx: tt> as $lc_name: ident {
            $( $fldn: ident: $fldty: ty ),*
        }

        type $self: ident => $type: expr;

        emit ($builder: ident) => $body: expr;
        children () => $children: expr;
        child ($ci: ident) => $child: expr;
        mut_child ($mci: ident) => $mchild: expr;
    ) => {
        $(#[$attr])*
        pub struct $name<$cx> {
            $( $fldn: $fldty ),*
        }

        impl BuiltIns {
            /// Returns the static type associated with the `Styx.Expressions.{}` expression type.
            pub fn $lc_name<'cx>() -> &'cx Ty<'cx> {
                static_type! {
                    TY, Ty::raw(Sym::from(format!("Styx.Expressions.{}", stringify!($name))), span!(), 0)
                           .with_generic_parameters(vec!(Ty::generic(Sym::from("T"), span!())))
                }

                unsafe {
                    ::std::mem::transmute(&*TY)
                }
            }
        }

        impl<$cx> $name<$cx> {
            #[allow(unused_variables)]
            fn transform<'a, 'b: 'a>(s: *mut (), $builder: &'a mut Builder<'b, $cx>) -> Value<'b, $cx> {
                let $self = unsafe { &mut *(s as *mut Self) };
                $body
            }

            /// Returns the specified native expression as a `$name`.
            pub unsafe fn from_native<'a>(native: &'a Native<$cx>) -> &'a Self {
                &*(native.data.as_ptr() as *const Self)
            }
        }

        impl<$cx> Into<Native<$cx>> for $name<$cx> {
            #[allow(unused_variables)]
            fn into(self) -> Native<$cx> {
                let ty = {
                    let $self = &self;
                    $type
                };

                Native::new(ty, box self,
                            unsafe { ::std::mem::transmute(Self::transform as *const ()) },
                            Some(|s| {
                                let $self = unsafe { &*(s as *const Self) };
                                $children
                            }), Some(|s, $ci| {
                                let $self = unsafe { &*(s as *const Self) };
                                $child
                            }), Some(|s, $mci| {
                                let $self = unsafe { &mut *(s as *mut Self) };
                                $mchild
                            }))
            }
        }
    }
}

/// Macro that provides an easy way to declare an `Extension` expression.
#[macro_export]
macro_rules! extension {
    (
        $(#[$attr:meta])*
        struct $name: ident <$cx: tt> in ($target: expr) {
            $( $fldn: ident: $fldty: ty ),*
        }

        type $self: ident => $type: expr;

        expand ($ctx: ident) => $expand: expr;
    ) => {
        extension! {
            $(#[$attr])*
            struct $name<$cx> in ($target) {
                $( $fldn: $fldty ),*
            }

            type $self => $type;

            expand ($ctx) => $expand;
            children () => 0;
            child (_i) => None;
            mut_child (_i) => None;
        }
    };

    (
        $(#[$attr:meta])*
        struct $name: ident <$cx: tt> in ($target: expr) {
            $( $fldn: ident: $fldty: ty ),*
        }

        type $self: ident => $type: expr;

        expand ($ctx: ident) => $expand: expr;
        children () => $children: expr;
        child ($ci: ident) => $child: expr;
        mut_child ($mci: ident) => $mchild: expr;
    ) => {
        $(#[$attr])*
        pub struct $name<$cx> {
            $( $fldn: $fldty ),*
        }

        impl BuiltIns {
            /// Returns the static type associated with the `Styx.Expressions.{}` expression type.
            pub fn $self<'cx>() -> &'cx Ty<'cx> {
                static_type! {
                    TY, Ty::raw(Sym::from(format!("Styx.Expressions.{}", stringify!($name))), span!(), 0)
                           .with_generic_parameters(vec!(Ty::generic(Sym::from("T"), span!())))
                }

                unsafe {
                    ::std::mem::transmute(&*TY)
                }
            }
        }

        impl<'cx> $name<'cx> {
            /// Returns the specified expression as a `$name`.
            pub unsafe fn from_extension<'a>(extension: &'a Extension<'cx>) -> &'a Self {
                &*(extension.data.as_ptr() as *const Self)
            }
        }

        impl<$cx> Into<Extension<$cx>> for $name<$cx> {
            #[allow(unused_variables)]
            fn into(self) -> Extension<$cx> {
                let ty = {
                    let $self = &self;
                    $type
                };

                #[allow(unused_variables)]
                #[allow(unused_mut)]
                Extension::new(ty, $target, box self, |s, $ctx| {
                    let $self = unsafe { *Box::from_raw(s as *mut Self) };
                    $expand
                }, Some(|s| {
                    let $self = unsafe { &*(s as *const Self) };
                    $children
                }), Some(|s, $ci| {
                    let $self = unsafe { &*(s as *const Self) };
                    $child
                }), Some(|s, $mci| {
                    let $self = unsafe { &mut *(s as *mut Self) };
                    $mchild
                }))
            }
        }
    }
}

native! {
    #[doc = "A call expression."]
    struct Call<'cx> as call {
        fun: &'cx Fun<'cx>,
        args: Vec<Expr<'cx>>,
        inline: bool
    }

    type this => this.fun.ty();

    emit (builder) => {
        let mut args = Vec::with_capacity(this.args.len());

        for arg in &this.args {
            let arg: &Expr = unsafe { ::std::mem::transmute(arg) };
            let arg = arg.build(builder);
            args.push(arg);
        }

        unsafe {
            #[allow(never_loop)] // using 'break' to exit early
            while this.fun.is_pure() && this.inline {
                use assembler::{AssemblyGraph, AssemblyStyle};
                use diagnostics::DiagnosticBag;

                // attempt to inline
                let mut diags = DiagnosticBag::default();
                let mut context = Context::child(::std::mem::transmute(builder.ctx));
                let vm = context.vm();

                let mut graph = match AssemblyGraph::new(vm.read().target_architecture(), &mut diags, &mut context, this.fun) {
                    Ok(graph) => graph,
                    _ => break
                };

                if diags.has_error() {
                    break
                }

                graph.process(AssemblyStyle::DS, &mut diags, vm);

                if diags.has_error() {
                    break
                }

                return builder.emit_inline(&graph, &args)
            }

            builder.emit_call(this.fun, &args)
        }
    };

    children () => this.args.len();
    child (i) => this.args.get(i).map(|x| x as *const _);
    mut_child (i) => this.args.get_mut(i).map(|x| x as *mut _);
}

impl<'cx> Call<'cx> {
    /// Creates a new call expression, given its target function and arguments.
    pub fn new(fun: &'cx Fun<'cx>, args: Vec<Expr<'cx>>) -> Self {
        Call { fun, args, inline: false }
    }

    /// Creates a new call expression that is to be inlined.
    pub fn new_inline(fun: &'cx Fun<'cx>, args: Vec<Expr<'cx>>) -> Self {
        Call { fun, args, inline: true }
    }

    /// Returns the target function of the call.
    pub fn target(&self) -> &'cx Fun<'cx> {
        self.fun
    }
}

native! {
    #[doc = "A literal value."]
    struct Literal<'cx> as literal {
        ty: &'cx Ty<'cx>,
        token: Token
    }

    type this => this.ty;

    emit (builder) => {
        match &this.token {
            &Token::Int(_, i) => builder.new_value(4, "int", ValueKind::Immediate(i as _)),
            &Token::Ident(_, ref ident) => match ident.as_str() {
                "true" => builder.new_value(1, "true", ValueKind::Immediate(1)),
                "false" => builder.new_value(1, "false", ValueKind::Immediate(0)),
                _ => unimplemented!()
            },
            _ => unimplemented!()
        }
    };
}

impl<'cx> Literal<'cx> {
    /// Creates a new literal expression, given its target type and representing token.
    pub fn new(ty: &'cx Ty<'cx>, token: Token) -> Self {
        Literal { ty, token }
    }
}

native! {
    #[doc = "An expression that executes a callback when compiled."]
    struct Magic<'cx> as magic {
        ty: &'cx Ty<'cx>,
        cb: for<'b, 'bcx, 'bvm: 'bcx> fn(&mut Builder<'b, 'bcx>)
    }

    type this => this.ty;

    emit (builder) => {
        (this.cb)(builder);
        builder.null_value()
    };
}

impl<'cx> Magic<'cx> {
    /// Creates a new "magic" expression, given its type and callback.
    pub fn new(ty: &'cx Ty<'cx>, cb: for<'b, 'bcx, 'bvm: 'bcx> fn(&mut Builder<'b, 'bcx>)) -> Self {
        Magic { ty, cb }
    }
}

native! {
    #[doc = "A switch expression."]
    struct Switch<'cx> as switch {
        ty: &'cx Ty<'cx>,
        value: Expr<'cx>,
        cases: Vec<(String, Expr<'cx>, Expr<'cx>)>,
        default: Expr<'cx>
    }

    type this => this.ty;

    emit (builder) => {
        let size = this.ty.size().unwrap();

        let cmpv = this.value.build(builder);
        let main = builder.block_index();

        // create 'after-switch' block
        let after = builder.create_block("after-switch", Some(size)).index();

        for &(ref name, ref value, ref body) in &this.cases {
            let block = builder.create_block(name.to_string(), None).index();
            let value = value.build(builder);

            // emit condition jump to block in main block
            builder.emit2("cmp", cmpv, value);
            builder.emit_cond_jump("je", block, None);

            // emit body in new block, and jump to after block
            builder.position_at_nth(block);

            let retv = body.build(builder);

            builder.emit_jump(after, Some(retv));

            // go back to main block
            builder.position_at_nth(main);
        }

        // emit default body
        let defaultv = this.default.build(builder);
        builder.emit_jump(after, Some(defaultv));

        // go back to 'after-switch' block, and set block parameter as value
        builder.position_at_nth(after);
        builder.block_argument().unwrap()
    };

    children () => this.cases.len() * 2 + 2; // two expr in each case + default + value

    child (i) => match i {
        0 => Some(&this.value),
        1 => Some(&this.default),
        n => match this.cases.get(n) {
            Some(&(_, ref v, ref c)) => Some(if n & 1 == 0 { v } else { c }),
            None => None
        }
    };

    mut_child (i) => match i {
        0 => Some(&mut this.value),
        1 => Some(&mut this.default),
        n => match this.cases.get_mut(n) {
            Some(&mut (_, ref mut v, ref mut c)) => Some(if n & 1 == 0 { v } else { c }),
            None => None
        }
    };
}

impl<'cx> Switch<'cx> {
    /// Creates a new switch expression, given its type, value to compare, default case, and other
    /// cases.
    pub fn new(ty: &'cx Ty<'cx>, value: Expr<'cx>, default: Expr<'cx>, cases: Vec<(String, Expr<'cx>, Expr<'cx>)>) -> Self {
        Switch { ty, value, default, cases }
    }
}

native! {
    #[doc = "A conditional if-then-else expression."]
    struct Conditional<'cx> as conditional {
        condition: Expr<'cx>,
        then: Expr<'cx>,
        otherwise: Expr<'cx>
    }

    type this => this.then.ty();

    emit (builder) => {
        let cond = &this.condition;
        let mut bag = DiagnosticBag::default();
        let computation: Option<bool> = cond.compute(&builder.ctx, &mut bag);

        if let Some(value) = computation {
            // Condition is constant: compute it right away
            return if value {
                this.then.build(builder)
            } else {
                this.otherwise.build(builder)
            }
        }

        let size = this.then.ty().size().unwrap();
        let main = builder.block_index();

        // create blocks
        let then = builder.create_block("then", None).index();
        let otws = builder.create_block("else", None).index();
        let after = builder.create_block("after-conditional", Some(size)).index();

        // emit condition jump to 'then' block in main block
        let condv = this.condition.build(builder);
        let zerov = builder.new_value(condv.size(), "zero", ValueKind::Immediate(0));

        builder.emit2("cmp", condv, zerov);
        builder.emit_cond_jump("jne", then, None);
        builder.emit_jump(otws, None);

        // emit 'then' block
        builder.position_at_nth(then);
        let thenv = this.then.build(builder);
        builder.emit_jump(after, Some(thenv));

        // emit 'then' block
        builder.position_at_nth(otws);
        let elsev = this.otherwise.build(builder);
        builder.emit_jump(after, Some(elsev));

        // go back to 'after-conditional' block, and set block parameter as value
        builder.position_at_nth(after);
        builder.block_argument().unwrap()
    };

    children () => 3;

    child (i) => match i {
        0 => Some(&this.condition),
        1 => Some(&this.then),
        2 => Some(&this.otherwise),
        _ => None
    };

    mut_child (i) => match i {
        0 => Some(&mut this.condition),
        1 => Some(&mut this.then),
        2 => Some(&mut this.otherwise),
        _ => None
    };
}

impl<'cx> Conditional<'cx> {
    /// Creates a new conditional expression, given its condition, then body, and otherwise body.
    pub fn new(condition: Expr<'cx>, then: Expr<'cx>, otherwise: Expr<'cx>) -> Self {
        Conditional { condition, then, otherwise }
    }
}

native! {
    #[doc = "A returning expression."]
    struct Return<'cx> as ret {
        expr: Expr<'cx>
    }

    type this => Ty::void();

    emit (builder) => {
        let main = this.expr.build(builder);
        builder.emit_ret(main);
        builder.null_value()
    };
}

impl<'cx> Return<'cx> {
    /// Creates a new `return` expression, given the expression whose value will be returned.
    pub fn new(expr: Expr<'cx>) -> Self {
        Return { expr }
    }
}

native! {
    #[doc = "An expression that returns nothing (of type void)."]
    #[derive(Default, Clone, Copy)]
    struct Unit<'cx> as unit {
        _phantom: PhantomData<&'cx ()>
    }

    type _this => Ty::void();

    emit (builder) => {
        builder.null_value()
    };
}

impl<'cx> Unit<'cx> {
    /// Creates a new unit.
    pub fn new() -> Self {
        Unit { _phantom: PhantomData }
    }
}

native! {
    #[doc = "An expression block, whose last expression defines the value of the expression."]
    #[derive(Default)]
    struct Block<'cx> as block {
        expressions: Vec<Expr<'cx>>
    }

    type this => if this.expressions.is_empty() {
        Ty::void()
    } else {
        this.expressions[this.expressions.len() - 1].ty()
    };

    emit (builder) => {
        let exprs = &this.expressions;

        if exprs.is_empty() {
            builder.null_value()
        } else {
            for expr in &exprs[0..exprs.len() - 1] {
                expr.build(builder);
            }

            exprs[exprs.len() - 1].build(builder)
        }
    };

    children () => this.expressions.len();
    child (i) => this.expressions.get(i).map(|x| x as *const _);
    mut_child (i) => this.expressions.get_mut(i).map(|x| x as *mut _);
}

impl<'cx> Block<'cx> {
    /// Creates a new block, given its inner expressions.
    pub fn new(expressions: Vec<Expr<'cx>>) -> Self {
        Block { expressions }
    }
}


/// The target of a `Global` expression.
pub enum GlobalTarget<'a> {
    /// The target is the virtual machine.
    VirtualMachine(Vm<'a>),

    /// The target is the closest context.
    Context(&'a Context<'a>),

    /// The target is the current binder (can be null).
    Binder(&'a Binder<'a, 'a>)
}

extension! {
    #[doc = "A compiler-inserted global constant guaranteed to live at least as long as the expression."]
    struct Global<'cx> in (BuiltIns::literal()) {
        target: GlobalTarget<'cx>
    }

    type this => Ty::pointer(Architecture::default());

    expand (ctx) => {
        use std::ops::Deref;

        let ptr = Ty::pointer(ctx.vm().read().target_architecture());

        Expression::Native(Literal::new(ptr, Token::Int(span!(), match this.target {
            GlobalTarget::VirtualMachine(vm) => vm.read().deref() as *const _ as usize,
            GlobalTarget::Context(ctx) => ctx as *const _ as usize,
            GlobalTarget::Binder(bind) => bind as *const _ as usize
        } as _)).into())
    };
}

impl<'cx> Global<'cx> {
    /// Creates a new global expression, given the data it points to.
    pub fn new(target: GlobalTarget<'cx>) -> Self {
        Global { target }
    }

    /// Creates a new global expression, given the VM it points to.
    pub fn vm(vm: Vm<'cx>) -> Self {
        Self::new(GlobalTarget::VirtualMachine(unsafe { mem::transmute(vm) }))
    }

    /// Creates a new global expression, given the binder it points to.
    pub fn binder<'bcx, 'bvm>(binder: &'cx Binder<'bcx, 'bvm>) -> Self {
        Self::new(GlobalTarget::Binder(unsafe { mem::transmute(binder) }))
    }

    /// Creates a new global expression, given the context it points to.
    pub fn context<'bcx>(context: &'cx Context<'bcx>) -> Self {
        Self::new(GlobalTarget::Context(unsafe { mem::transmute(context) }))
    }
}

impl<'cx> Debug for Global<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.target {
            &GlobalTarget::VirtualMachine(vm) => write!(f, "VirtualMachine({:?})", vm),
            &GlobalTarget::Binder(_) => write!(f, "Binder"),
            &GlobalTarget::Context(ctx) => write!(f, "Context({:?})", ctx)
        }
    }
}


extension! {
    #[doc = "A binary expression."]
    struct Binary<'cx> in (BuiltIns::call()) {
        operator: &'cx Fun<'cx>,
        lhs: Expr<'cx>,
        rhs: Expr<'cx>
    }

    type binary => binary.operator.ty();

    expand (_ctx) => {
        Expression::Native(Call::new(binary.operator, vec!(binary.lhs, binary.rhs)).into())
    };

    children () => 2;

    child (i) => match i {
        0 => Some(&binary.lhs),
        1 => Some(&binary.rhs),
        _ => None
    };

    mut_child (i) => match i {
        0 => Some(&mut binary.lhs),
        1 => Some(&mut binary.rhs),
        _ => None
    };
}

impl<'cx> Binary<'cx> {
    /// Creates a new binary expression, given its operator, and both operands.
    pub fn new(operator: &'cx Fun<'cx>, lhs: Expr<'cx>, rhs: Expr<'cx>) -> Self {
        Binary { operator, lhs, rhs }
    }
}


extension! {
    #[doc = "An unary expression."]
    struct Unary<'cx> in (BuiltIns::call()) {
        operator: &'cx Fun<'cx>,
        operand: Expr<'cx>
    }

    type unary => unary.operator.ty();

    expand (_ctx) => {
        Expression::Native(Call::new(unary.operator, vec!(unary.operand)).into())
    };

    children () => 1;

    child (i) => match i {
        0 => Some(&unary.operand),
        _ => None
    };

    mut_child (i) => match i {
        0 => Some(&mut unary.operand),
        _ => None
    };
}

impl<'cx> Unary<'cx> {
    /// Creates a new unary expression, given its operator and operand.
    pub fn new(operator: &'cx Fun<'cx>, operand: Expr<'cx>) -> Self {
        Unary { operator, operand }
    }
}


