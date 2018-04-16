//! This module defines the functions used in Styx to interact with the `VirtualMachine`.

use symbols::Sym;
use vm::VirtualMachine;

use std::mem::size_of;

pub fn import_types<'vm>(vm: &mut VirtualMachine<'vm>) {
    let ns = Sym::from("Styx");

    mkty!(ty, vm, ns, $);

    ty!("Binder", size_of::<::binder::Binder>() as _);
    ty!("Parser", size_of::<::parser::Parser>() as _);
}

pub fn import_functions<'vm>(_vm: &mut VirtualMachine<'vm>) {
    // let ns = Sym::from("Styx");
    //
    // mkfun!(fun, vm, ns, $);
    //
    // fun!("System.Void" "DefineBinaryOperator", ("System.String" name), ("System.I32" precedence), ("System.Boolean" right_associative), builder => {
    //
    // });
    //
    // fun!("System.Void" "DefineUnaryOperator", ("System.String" name), ("System.Boolean" right_associative), builder => {
    //
    // });
    //
    // fun!("System.Void" "DefineType", ("System.String" name), ("System.Boolean" right_associative), builder => {
    //
    // });
    //
    // fun!("System.Void" "DefineFunction", ("System.String" name), ("System.Boolean" right_associative), builder => {
    //
    // });
}

