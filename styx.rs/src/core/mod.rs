//! This module defines the core language implemented in Rust, instead of in Styx directly.

use vm::VirtualMachine;

/// Creates a macro that can be used to generate methods in a specific context.
///
/// # Example
/// ```text
/// // Namespace in which all functions will be declared:
/// let system = Sym::from("Example");
///
/// // Create macro (we assume that a Vm is in scope):
/// mkfun!(fun, vm, system, $);
///
/// // Define the "Example.PlusOne" function, which takes a single argument of type "System.I32",
/// // and returns a value of type "System.I32".
/// fun!("System.I32" "PlusOne", ("System.I32" value), builder => {
///     // Note: The emit! macro is used below, but any interaction with the builder is possible.
///     emit!(builder =>
///          (for X86 | X86_64 =>
///              (4 -> result)
///              (mov (-> result), (-> value))
///              (add (-> result), (= 4 1))
///              (>> result)));
/// });
/// ```
#[macro_export]
macro_rules! mkfun {
    ( $name: ident, $vm: expr, $root: expr, $S: tt ) => (macro_rules! $name {
        ( $S rty: tt $S name: tt, $S ( ($S pty: tt $S pname: ident) ),*, $S builder: ident => $S body: expr ) => {{
            let ctx = $vm.ctx();
            let fn_name = $root.child($S name);
            let fn_type = ctx.get_type(&$crate::symbols::LookupSym::from($S rty)).unwrap();

            let body = $crate::expr::Magic::new(fn_type, |$S builder| {
                let mut _pi = 0;
                $S (
                    let $S pname = $S builder.parameter(_pi).unwrap();
                    _pi += 1;
                )*

                $S body;
            });

            let parameters = vec!(
                $S ({
                    let ty = ctx.get_type(&$crate::symbols::LookupSym::from($S pty)).unwrap();
                    $crate::typesystem::Parameter::new(stringify!($S pname).to_string(), ty)
                }),*
            );

            let fun = $crate::typesystem::Fun::new(
                fn_name,
                span!(),
                parameters,
                $crate::expr::Expr::native(body, span!(), $crate::typesystem::Ty::expression()));
            ctx.add_function(fun);
        }}
    });
}

/// Creates a macro that can be used to generate types in a specific context.
///
/// # Example
/// ```text
/// // Namespace in which all types will be declared:
/// let system = Sym::from("Example");
///
/// // Create macro (we assume that a Vm is in scope):
/// mkty!(ty, vm, system, $);
///
/// // Define the "Example.Foo" and "Example.Bar.Baz" types, which have
/// // respective sizes of 4 and 6.
/// ty!("Foo", 4);
/// ty!("Bar.Baz", 6);
/// ```
#[macro_export]
macro_rules! mkty {
    ( $name: ident, $vm: expr, $root: expr, $S: tt ) => (macro_rules! $name {
        ( $S name: expr, $S size: expr ) => {{
            let ctx = $vm.ctx();
            ctx.add_type($crate::typesystem::Ty::raw($root.child($S name), span!(), $S size)).unwrap();
        }}
    });
}

mod math;
mod meta;
mod primitives;

pub(crate) mod syscore;

/// Imports all built in types in the given virtual machine.
pub(crate) fn initialize(vm: &mut VirtualMachine) {
    use self::primitives;
    use self::meta;
    use self::syscore;

    primitives::import_types(vm);
    primitives::import_functions(vm);
    primitives::import_parsers(vm);

    syscore::import_types(vm);
    syscore::import_functions(vm);
    syscore::import_parsers(vm);

    meta::import_types(vm);
    meta::import_functions(vm);
}

