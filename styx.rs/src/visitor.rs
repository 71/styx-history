//! The Styx [`Visitor`] trait, and implementations.

use prelude::*;
use expr::BuiltIns;
use typesystem::TyParameters;


/// A structure that can go through an expression tree in order to visit it immutably,
/// or mutably.
pub trait Visitor<T> {
    /// Visits the given expression, returning an object of type `T`.
    fn visit(&mut self, expr: &Expr) -> T;

    /// Mutably visits the given expression, optionally rewriting it and its children,
    /// and returning an object of type `T`.
    fn rewrite(&mut self, expr: &mut Expr) -> T;

    /// Visits the children of the given expression.
    fn visit_children(&mut self, expr: &Expr) -> Vec<T> {
        let childc = expr.child_count();
        let mut vec = Vec::with_capacity(childc);

        for i in 0..childc {
            vec.push(self.visit(expr.child(i).unwrap()))
        }

        vec
    }

    /// Rewrites the children of the given expression.
    fn rewrite_children(&mut self, expr: &mut Expr) -> Vec<T> {
        let childc = expr.child_count();
        let mut vec = Vec::with_capacity(childc);

        for i in 0..childc {
            vec.push(self.rewrite(expr.mut_child(i).unwrap()))
        }

        vec
    }
}


/// A `Visitor` that verifies that an expression tree can be emitted.
pub struct Verifier<'a, 'cx: 'a> {
    #[allow(dead_code)]
    diagnostics: Diags<'a>,
    #[allow(dead_code)]
    parameters: &'a TyParameters<'a, 'cx>,
    invalid: bool
}

impl<'a, 'cx> Verifier<'a, 'cx> {
    /// Creates a new verifying visitor, given the generic parameters used in this context.
    pub fn new(diagnostics: Diags<'a>, parameters: &'a TyParameters<'a, 'cx>) -> Self {
        Verifier { diagnostics, parameters, invalid: false }
    }

    /// Returns whether the visited expression was valid, and can thus be compiled.
    pub fn is_valid(&self) -> bool {
        !self.invalid
    }
}

impl<'a, 'cx> Visitor<()> for Verifier<'a, 'cx> {
    fn visit(&mut self, _expr: &Expr) -> () {
        
    }

    fn rewrite(&mut self, expr: &mut Expr) -> () {
        self.visit(expr)
    }
}

/// A [`Visitor`] that inlines method calls in an expression tree.
///
/// # Note
/// This type only inlines calls at the *expression* level, but not at the IR level.
///
/// # Panics
/// This type assumes that the given expressions have been verified and are valid.
/// If an invalid expression is encountered, a panic will take place.
///
/// Additionally, expression trees can only be mutably visited (using [`Visitor::rewrite`]).
pub struct Inliner<'a, 'cx: 'a> {
    #[allow(dead_code)]
    parameters: &'a TyParameters<'a, 'cx>
}

impl<'a, 'cx> Inliner<'a, 'cx> {
    /// Creates a new inlining visitor, given the generic parameters used in this context.
    pub fn new(parameters: &'a TyParameters<'a, 'cx>) -> Self {
        Inliner { parameters }
    }
}

impl<'a, 'cx> Visitor<()> for Inliner<'a, 'cx> {
    fn visit(&mut self, _: &Expr) -> () {
        panic!("Inliner cannot visit immutable expressions.")
    }

    fn rewrite(&mut self, expr: &mut Expr) -> () {
        if expr.expr_ty() != BuiltIns::call() {
            return
        }
    }
}

/// A closure-invoking [`Visitor`].
pub struct ClosureVisitor<'a, T: 'a> {
    closure: &'a Fn(&mut ClosureVisitor<'a, T>, &Expr) -> T
}

impl<'a, T> ClosureVisitor<'a, T> {
    /// Creates a new closure-based visitor, given the closure to invoke on all expressions.
    pub fn new(visit: &'a Fn(&mut Self, &Expr) -> T) -> Self {
        ClosureVisitor { closure: visit }
    }
}

impl<'a, T> Visitor<T> for ClosureVisitor<'a, T> {
    fn visit(&mut self, expr: &Expr) -> T {
        (self.closure)(self, expr)
    }

    fn rewrite(&mut self, expr: &mut Expr) -> T {
        (self.closure)(self, expr)
    }
}

