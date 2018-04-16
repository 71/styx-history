//! Types and functions used in Styx to implement the core language.

use binder::Binder;

/// Eases the declaration of a static type.
macro_rules! syntaxty {
    ( $name: ident of size $size: tt: $string: tt ) => (
        /// Static instance of the $string type.
        pub fn $name<'cx>() -> &'cx Ty<'cx> {
            use $crate::symbols::Sym;
            use $crate::typesystem::Ty;

            static_type!(
                Ty::raw(Sym::from($string), span!(), $size)
            )
        }
    );
}


pub mod prelude;
pub mod syntax;

pub(crate) fn import_intrinsics(binder: &mut Binder) {
    use self::prelude;
    use self::syntax;

    binder.import_syntax(prelude::using());
    binder.import_syntax(prelude::call());
    binder.import_syntax(prelude::ternary());
    binder.import_syntax(syntax::path());
    binder.import_syntax(syntax::global());
}

