//! The Styx type system.

use prelude::*;
use expr::{Expr};
use lexer::{HasSpan, Span};
use procedures::Proc;
use symbols::{Sym, Symbol, SymbolTree};

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::marker::PhantomData;
use std::sync::RwLock;

use rand::{Rng, SeedableRng, StdRng};


/// Implements `Hash` for the given type.
macro_rules! derive_symbol {
    ( $ty: ident ) => (
        impl<'cx> Symbol for $ty<'cx> {
            fn parts(&self) -> &[u64] {
                self.symbol.parts()
            }
            fn full_name(&self) -> &str {
                self.symbol.full_name()
            }
        }
    )
}

/// Implements `Debug` for the given type, by using the type's implementation
/// of `Display`.
macro_rules! debug_from_display {
    ( $ty: ident ) => (
        impl<'cx> Debug for $ty<'cx> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                Display::fmt(self, f)
            }
        }
    );
}

/// A `SymbolTree` of `Member`s.
pub type MemberTree<'cx> = SymbolTree<Member<'cx>>;

/// A Styx member.
#[derive(Debug, Clone)]
pub enum Member<'cx> {
    /// A [`Ty`].
    Type(&'cx Ty<'cx>),
    /// A [`Fun`].
    Function(&'cx Fun<'cx>)
}

impl<'cx> Member<'cx> {
    /// Returns a reference to the underlying type.
    ///
    /// # Errors
    /// The member is not a type.
    pub fn as_type(&self) -> Option<&'cx Ty<'cx>> {
        match self {
            &Member::Type(ty) => Some(ty),
            _ => None
        }
    }

    /// Returns a reference to the underlying function.
    ///
    /// # Errors
    /// The member is not a function.
    pub fn as_function(&self) -> Option<&'cx Fun<'cx>> {
        match self {
            &Member::Function(fun) => Some(fun),
            _ => None
        }
    }
}

impl<'cx> From<&'cx Ty<'cx>> for Member<'cx> {
    fn from(this: &'cx Ty<'cx>) -> Self { Member::Type(this) }
}

impl<'cx> From<&'cx Fun<'cx>> for Member<'cx> {
    fn from(this: &'cx Fun<'cx>) -> Self { Member::Function(this) }
}

// //==========================================================================//
// // TYPES                                                                    //
// //==========================================================================//

/// Indicates that this object has a Styx representation.
pub trait Typed<'cx> {
    /// Gets the type of this expression.
    fn ty(&self) -> &'cx Ty<'cx>;

    /// Returns a boolean indicating whether this type is empty (it has a null size).
    fn is_empty(&self) -> bool {
        match &self.ty().def {
            &TyDef::Raw(0) => true,
            _ => false
        }
    }
}


/// Represents a generic argument.
pub struct Generic<'cx> {
    name: String,
    concept: &'cx Concept<'cx>
}

impl<'cx> Display for Generic<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", self.concept, self.name)
    }
}

/// Defines the generic parameters whose type definition is known in a certain context.
pub enum TyParameters<'tp, 'cx> {
    /// Two merged type parameter maps.
    Siblings(*const TyParameters<'tp, 'cx>, *const TyParameters<'tp, 'cx>, PhantomData<&'tp ()>),

    /// A single type parameter map.
    Orphan(HashMap<u64, &'cx Ty<'cx>>)
}

unsafe impl<'tp, 'cx> Send for TyParameters<'tp, 'cx> {}
unsafe impl<'tp, 'cx> Sync for TyParameters<'tp, 'cx> {}

impl<'cx> Default for TyParameters<'static, 'cx> {
    fn default() -> Self {
        TyParameters::Orphan(HashMap::default())
    }
}

impl<'tp, 'cx> TyParameters<'tp, 'cx> {
    /// Creates a new type parameter map.
    pub fn new(map: HashMap<u64, &'cx Ty<'cx>>) -> TyParameters<'static, 'cx> {
        TyParameters::Orphan(map)
    }

    /// Merges two type parameter maps together.
    pub fn merged<'a, 'otp>(&'a self, with: &'a TyParameters<'otp, 'cx>) -> TyParameters<'a, 'cx> {
        TyParameters::Siblings(self, with, PhantomData)
    }

    /// Defines the type matching the specified type argument.
    pub fn define(&mut self, gentyid: u64, ty: &'cx Ty<'cx>) {
        if let &mut TyParameters::Orphan(ref mut map) = self {
            map.insert(gentyid, ty);
        } else {
            panic!("Cannot define types in a merged type parameters map.");
        }
    }

    /// Resolves a type, given its type id.
    pub fn resolve(&self, gentyid: u64) -> Option<&'cx Ty<'cx>> {
        match self {
            &TyParameters::Siblings(a, b, _) => unsafe {
                (*a).resolve(gentyid).or_else(|| (*b).resolve(gentyid))
            },
            &TyParameters::Orphan(ref map) => match map.get(&gentyid) {
                Some(ty) => Some(ty),
                None => None
            }
        }
    }
}

impl<'tp, 'cx> Hash for TyParameters<'tp, 'cx> {
    fn hash<H: Hasher>(&self, f: &mut H) {
        match self {
            &TyParameters::Siblings(a, b, _) => unsafe {
                (*a).hash(f);
                (*b).hash(f);
            },
            &TyParameters::Orphan(ref map) => {
                for ty in map.values() {
                    ty.hash(f);
                }
            }
        }
    }
}

impl<'tp, 'cx> Display for TyParameters<'tp, 'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let &TyParameters::Orphan(ref map) = self {
            let mut values = map.values();
            let len = values.len();

            if len == 0 {
                return Ok(())
            }

            f.write_char('<')?;

            for _ in 0..len-1 {
                write!(f, "{}, ", values.next().unwrap())?;
            }

            write!(f, "{}", values.next().unwrap())
        } else {
            Ok(())
        }
    }
}

/// A `Ty`'s definition.
#[derive(Hash)]
pub enum TyDef<'cx> {
    /// A placeholder for a generic type.
    Generic,

    /// A type whose data is implemented as raw bytes.
    Raw(u16),

    /// A tuple built on top of other types.
    Tuple(Vec<&'cx Ty<'cx>>, TyParameters<'cx, 'cx>),

    /// A type whose type parameters are known.
    Refined(&'cx Ty<'cx>, TyParameters<'cx, 'cx>)
}

lazy_static! {
    static ref RAND: RwLock<StdRng> = {
        RwLock::new(StdRng::new().unwrap_or_else(|_| StdRng::from_seed(&[0x42, 0x43, 0x44])))
    };
}

/// The Styx type, which has a name, and inner types.
///
/// Styx types are represented very simply as tuples of inner types,
/// and may take parameters. Those parameters define generic types,
/// whose size changes.
pub struct Ty<'cx> {
    id: u64,
    span: Span,
    symbol: Sym,
    generic_parameters: Vec<Ty<'cx>>,
    def: TyDef<'cx>
}

impl<'cx> PartialEq for Ty<'cx> {
    fn eq(&self, other: &Ty<'cx>) -> bool {
        self.id == other.id
    }
}

impl<'cx> Eq for Ty<'cx> {}

impl<'cx> Ty<'cx> {
    /// Creates a new type, given its symbol and definition.
    pub fn new(symbol: Sym, span: Span, def: TyDef<'cx>) -> Self {
        Ty { symbol, def, span, id: (*RAND.write().unwrap()).next_u64(), generic_parameters: Vec::new() }
    }

    /// Creates a new generic parameter type.
    pub fn generic(symbol: Sym, span: Span) -> Self {
        Self::new(symbol, span, TyDef::Generic)
    }

    /// Creates a new raw type.
    pub fn raw(symbol: Sym, span: Span, size: u16) -> Self {
        Self::new(symbol, span, TyDef::Raw(size))
    }

    /// Creates a new tuple type.
    pub fn tuple(symbol: Sym, span: Span, types: Vec<&'cx Ty<'cx>>) -> Self {
        Self::new(symbol, span, TyDef::Tuple(types, TyParameters::default()))
    }

    /// Creates a new tuple type with type arguments.
    pub fn tuple_with_args(symbol: Sym, span: Span, types: Vec<&'cx Ty<'cx>>, args: TyParameters<'cx, 'cx>) -> Self {
        Self::new(symbol, span, TyDef::Tuple(types, args))
    }

    /// Creates a new refined type.
    pub fn refined(symbol: Sym, span: Span, ty: &'cx Ty<'cx>, parameters: TyParameters<'cx, 'cx>) -> Self {
        Self::new(symbol, span, TyDef::Refined(ty, parameters))
    }

    /// Creates a new refined type with no type parameters given.
    pub fn alias(symbol: Sym, span: Span, ty: &'cx Ty<'cx>) -> Self {
        Self::new(symbol, span, TyDef::Refined(ty, TyParameters::default()))
    }

    /// Returns a type identical to this one, with its generic parameters changed
    /// to the given vector.
    pub fn with_generic_parameters(mut self, params: Vec<Ty<'cx>>) -> Self {
        self.generic_parameters = params;
        self
    }

    /// Returns whether this type represents a known type.
    pub fn is_known(&self) -> bool {
        let a = self as *const _ as *const ();
        let b = Self::unknown() as *const _ as *const ();
        a == b &&
        match &self.def {
            &TyDef::Generic | &TyDef::Raw(_) => true,
            &TyDef::Tuple(ref tys, _)        => tys.iter().all(|t| t.is_known()),
            &TyDef::Refined(ty, _)           => ty.is_known()
        }
    }

    /// Returns a slice containg all generic parameters of this type.
    pub fn generic_parameters(&self) -> &[Ty<'cx>] {
        &self.generic_parameters
    }

    /// Returns the name of the type.
    pub fn name(&self) -> &str {
        self.symbol.as_ref()
    }

    /// Returns the size in bytes occupied by the type.
    pub fn size(&self) -> Option<u16> {
        self.inner_size(&TyParameters::default())
    }

    fn inner_size<'tp>(&self, args: &TyParameters<'tp, 'cx>) -> Option<u16> {
        match &self.def {
            &TyDef::Raw(size) => Some(size),

            &TyDef::Generic => match args.resolve(self.id) {
                Some(param) => param.inner_size(args),
                None        => None
            },
            &TyDef::Tuple(ref tys, ref p) => {
                let mut sum = 0;
                let merged = p.merged(args);

                for ty in tys {
                    sum += match ty.inner_size(&merged) {
                        Some(size) => size,
                        None => return None
                    };
                }

                Some(sum)
            },
            &TyDef::Refined(ty, ref p) => ty.inner_size(&p.merged(args))
        }
    }
}

impl<'cx> Display for Ty<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.symbol)?;

        match &self.def {
            &TyDef::Tuple(_, ref args) => write!(f, "{}", args),
            _ => Ok(())
        }
    }
}

impl<'cx> HasSpan for Ty<'cx> {
    fn span(&self) -> Span { self.span }
}

impl<'cx> Hash for Ty<'cx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
        self.def.hash(state);
    }
}

debug_from_display!(Ty);
derive_symbol!(Ty);

impl<'cx> Ty<'cx> {
    /// Returns the global unknown type, which represents types that could not be resolved.
    pub fn unknown() -> &'cx Ty<'cx> {
        static_type! {
            UNKNOWN_TY, Ty::raw(Sym::from("<Unknown>"), span!(), 0)
        }

        unsafe { &*(&*UNKNOWN_TY as *const _ as *const () as *const _) }
    }

    /// Returns the global void type.
    pub fn void() -> &'cx Ty<'cx> {
        static_type! {
            VOID_TY, Ty::raw(Sym::from("System.Void"), span!(), 0)
        }

        unsafe { &*(&*VOID_TY as *const _ as *const () as *const _) }
    }

    /// Returns the global expression type.
    pub fn expression() -> &'cx Ty<'cx> {
        static_type! {
            EXPR_TY, Ty::raw(Sym::from("Styx.Expression"), span!(), 0)
                        .with_generic_parameters(vec!(Ty::generic(Sym::from("T"), span!())))
        }

        unsafe { &*(&*EXPR_TY as *const _ as *const () as *const _) }
    }

    /// Returns the global quote type.
    pub fn quote() -> &'cx Ty<'cx> {
        static_type! {
            QUOTE_TY, Ty::raw(Sym::from("Styx.Quote"), span!(), 0)
                         .with_generic_parameters(vec!(Ty::generic(Sym::from("T"), span!())))
        }

        unsafe { &*(&*QUOTE_TY as *const _ as *const () as *const _) }
    }

    /// Returns the global pointer type for the given architecture.
    pub fn pointer(arch: Architecture) -> &'cx Ty<'cx> {
        static_type! {
            PTR32_TY, Ty::raw(Sym::from("Styx.Pointer"), span!(), 4)
                         .with_generic_parameters(vec!(Ty::generic(Sym::from("T"), span!())))
        }

        static_type! {
            PTR64_TY, Ty::raw(Sym::from("Styx.Pointer"), span!(), 8)
                         .with_generic_parameters(vec!(Ty::generic(Sym::from("T"), span!())))
        }

        unsafe {
            match arch {
                Architecture::X86_64 => &*(&*PTR64_TY as *const _ as *const () as *const _),
                _ => &*(&*PTR32_TY as *const _ as *const () as *const _),
            }
        }
    }
}

// //==========================================================================//
// // FUNCTIONS                                                                //
// //==========================================================================//

/// Defines a function parameter.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Parameter<'cx> {
    name: String,
    ty: &'cx Ty<'cx>
}

impl<'cx> Parameter<'cx> {
    /// Creates a new parameter, given its name and type constraint.
    pub fn new(name: String, ty: &'cx Ty<'cx>) -> Parameter<'cx> {
        Parameter { ty, name }
    }

    /// Returns the string that corresponds to the parameter's name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl<'cx> Typed<'cx> for Parameter<'cx> {
    #[inline]
    fn ty(&self) -> &'cx Ty<'cx> {
        self.ty
    }
}

impl<'cx> Display for Parameter<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}

debug_from_display!(Parameter);


/// Represents a user-defined function.
pub struct Fun<'cx> {
    symbol: Sym,
    span: Span,
    body: Expr<'cx>,
    generic_parameters: Vec<Ty<'cx>>,
    parameters: Vec<Parameter<'cx>>,
    is_pure: bool
}

impl<'cx> Fun<'cx> {
    /// Creates a new function, given its symbol, parameters, and body.
    pub fn new(symbol: Sym, span: Span, parameters: Vec<Parameter<'cx>>, body: Expr<'cx>) -> Self {
        Fun { symbol, is_pure: Self::compute_is_pure(&body), parameters, body, span, generic_parameters: Vec::new() }
    }

    /// Creates a new anonymous function, given its body.
    pub fn anonymous(body: Expr<'cx>) -> Self {
        Fun { symbol: Sym::default(), is_pure: Self::compute_is_pure(&body), parameters: Vec::new(), span: body.span(), body, generic_parameters: Vec::new() }
    }

    /// Returns a function identical to this one, with its generic parameters changed
    /// to the given vector.
    pub fn with_generic_parameters(mut self, params: Vec<Ty<'cx>>) -> Self {
        self.generic_parameters = params;
        self
    }

    /// Returns a boolean indicating whether the function can be compiled.
    pub fn verify<'a>(&self, diagnostics: Diags, generics: &TyParameters<'a, 'cx>) -> bool {
        use visitor::Verifier;

        let mut verifier = Verifier::new(diagnostics, generics);

        verifier.visit(&self.body);

        // TODO: Verify type parameters; body
        verifier.is_valid()
    }

    /// Returns the name of the function.
    pub fn name(&self) -> &str {
        self.symbol.as_ref()
    }

    /// Returns the slice containing all parameters that this function takes.
    pub fn parameters(&self) -> &[Parameter<'cx>] {
        &self.parameters
    }

    /// Returns the body of the function.
    pub fn body(&self) -> &Expr<'cx> {
        &self.body
    }

    /// Returns a boolean indicating whether the function is pure.
    pub fn is_pure(&self) -> bool {
        self.is_pure
    }

    /// Returns a boolean indicating whether the function represents an expression.
    pub fn is_anonymous(&self) -> bool {
        self.symbol == Sym::default() && self.parameters.is_empty()
    }

    /// Returns the global unknown function, which represents a function that could not be
    /// resolved.
    pub fn unknown() -> &'cx Fun<'cx> {
        unsafe {
            &*(&*UNKNOWN_FUN as *const Fun<'static> as *const () as *const Fun<'cx>)
        }
    }

    /// Returns whether this function represents a known function.
    pub fn is_known(&self) -> bool {
        self as *const _ as *const () != &*UNKNOWN_FUN as *const _ as *const () &&
        self.parameters.iter().all(|param| param.ty().is_known())
    }

    fn compute_is_pure(expr: &Expr<'cx>) -> bool {
        use expr::{BuiltIns, Call, Expression};
        use visitor::{ClosureVisitor, Visitor};

        ClosureVisitor::<bool>::new(&|visitor: &mut ClosureVisitor<bool>, expr: &Expr| -> bool {
            let children: Vec<bool> = visitor.visit_children(expr);

            if children.iter().any(|b| !*b) {
                return false
            }

            if expr.expr_ty() != BuiltIns::call() {
                return true
            }

            match expr.data() {
                &Expression::Native(ref native) => unsafe {
                    let call = Call::from_native(native);

                    call.target().is_pure()
                },
                _ => true
            }
        }).visit(expr)
    }
}

impl<'cx> Display for Fun<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // write name
        write!(f, "{}", self.symbol)?;

        // write parameters
        let paramsc = self.parameters.len();

        f.write_char('(')?;

        if paramsc > 0 {
            for param in &self.parameters[..self.parameters.len() - 1] {
                write!(f, "{}, ", param)?;
            }

            let last = &self.parameters[paramsc - 1];
            write!(f, "{}", last)?;
        }

        f.write_char(')')
    }
}

impl<'cx> Typed<'cx> for Fun<'cx> {
    #[inline]
    fn ty(&self) -> &'cx Ty<'cx> {
        Typed::ty(&self.body)
    }
}

impl<'cx> PartialEq for Fun<'cx> {
    fn eq(&self, other: &Fun<'cx>) -> bool {
        self.name() == other.name() && self.parameters == other.parameters
    }
}

impl<'cx> Eq for Fun<'cx> {}

impl<'cx> HasSpan for Fun<'cx> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'cx> Hash for Fun<'cx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
        self.ty().hash(state);
        self.parameters.hash(state);
    }
}

unsafe impl<'cx> Sync for Fun<'cx> {}
unsafe impl<'cx> Send for Fun<'cx> {}

debug_from_display!(Fun);
derive_symbol!(Fun);

lazy_static! {
    static ref UNKNOWN_FUN: Fun<'static> = {
        Fun::new(Sym::from("<Unknown>"), span!(), Vec::new(), Expr::block(Vec::new(), span!()))
    };
}


// //==========================================================================//
// // CONCEPTS                                                                 //
// //==========================================================================//

/// A fact known or required of an `Expr`.
pub struct Axiom<'cx> {
    repr: String,
    #[allow(dead_code)]
    predicate: Proc<'cx>
}

impl<'cx> Axiom<'cx> {
    /// Creates a new `Axiom`, given its string representation and the predicate
    /// it must satisfy.
    pub fn new<S: ToString>(repr: &S, predicate: Proc<'cx>) -> Self {
        Axiom { repr: repr.to_string(), predicate }
    }

    /// Returns a string representation of the axiom.
    pub fn representation(&self) -> &str {
        self.repr.as_str()
    }

    /// Returns whether the given type verifies the current axiom.
    pub fn verify<'tcx>(&self, _ty: &Ty<'tcx>) -> bool {
        unimplemented!()
    }

    /// Combines multiple axioms into a single axiom.
    pub fn combine(axioms: &[Axiom<'cx>]) -> Self {
        // make representation and expressions
        let mut repr = String::new();

        //let parameters = Vec::new();
        //let mut body = Vec::new();

        for axiom in axioms {
            repr.push_str(axiom.representation());

            //body.push(Call::new(axiom, ));
        }

        // build body
        unimplemented!()
        //let mut builder = Builder::new();

        //Axiom { repr, body }
    }
}

impl<'cx> Display for Axiom<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.representation())
    }
}

debug_from_display!(Axiom);


/// The definition of a type that satisfies one or many `Axiom`s.
pub struct Concept<'cx> {
    name: Sym,
    axiom: Axiom<'cx>
}

impl<'cx> Concept<'cx> {
    /// Creates a new concept, given its name and the axioms it must satisfy.
    pub fn new(name: Sym, axioms: &[Axiom<'cx>]) -> Self {
        Concept { name, axiom: Axiom::combine(axioms) }
    }

    /// Returns whether the given type matches the current concept.
    pub fn verify<'tcx>(&self, ty: &Ty<'tcx>) -> bool {
        self.axiom.verify(ty)
    }
}

impl<'cx> Display for Concept<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.axiom)
    }
}

debug_from_display!(Concept);

