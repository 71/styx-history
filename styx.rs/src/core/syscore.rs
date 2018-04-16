//! This module defines the `System.Core` namespace, imported by default in every binder.

use binder::Binder;
use symbols::Sym;
use vm::{Vm, VirtualMachine};

use std::mem;


lazy_static! {
    static ref SYSCORE_SYMBOL: Sym = {
        Sym::from("System.Prelude")
    };
}


/// Imports all core parsers in the given virtual machine.
pub fn import_parsers(_vm: &mut VirtualMachine) {
}

/// Imports all core types in the given virtual machine.
pub fn import_types(_vm: &mut VirtualMachine) {
}

/// Imports all core functions in the given virtual machine.
pub fn import_functions(vm: &mut VirtualMachine) {
    let core = &*SYSCORE_SYMBOL;

    mkfun!(fun, vm, core, $);

    fun!("System.Void" "__Import", ("System.String" _namespace), _builder => {
        unimplemented!()
    });
}

/// Initializes the given binder, importing the `System.Prelude` namespace.
pub fn initialize<'cx, 'vm: 'cx>(binder: &mut Binder<'cx, 'vm>, vm: Vm<'vm>) {
    let vm = vm.read();
    let ctx = vm.context();

    unsafe {
        binder.import_tree(mem::transmute(ctx.members().lookup(&Sym::from("System")).unwrap()));
        binder.import_tree(mem::transmute(ctx.members().lookup(&*SYSCORE_SYMBOL).unwrap()));
    }
}

