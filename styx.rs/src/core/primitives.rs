//! This modules defines the primitive types in Styx, and their methods.

use diagnostics::DiagnosticBag;
use parser::{Fixity, Precedence};
use symbols::Sym;
use vm::VirtualMachine;

/// Imports all core parsers in the given virtual machine.
pub fn import_parsers(_vm: &mut VirtualMachine) {

}

/// Imports all core types in the given virtual machine.
pub fn import_types(vm: &mut VirtualMachine) {
    let system = Sym::from("System");

    mkty!(ty, vm, system, $);

    ty!("Boolean", 1);

    ty!("Char", 1);
    ty!("I8",   1);
    ty!("I16",  2);
    ty!("I32",  4);
    ty!("I64",  8);
    ty!("I128", 16);
    ty!("U8",   1);
    ty!("U16",  2);
    ty!("U32",  4);
    ty!("U64",  8);
    ty!("U128", 16);
    ty!("F16",  2);
    ty!("F32",  4);
    ty!("F64",  8);
    ty!("F128", 16);

    ty!("String", 16);
}

/// Imports all core functions in the given virtual machine.
#[allow(non_snake_case)]
pub fn import_functions(vm: &mut VirtualMachine) {
    let mut diags = DiagnosticBag::default();
    let system = Sym::from("System");

    let Boolean = "System.Boolean";
    let I8  = "System.I8";
    let I16 = "System.I16";
    let I32 = "System.I32";
    let I64 = "System.I64";

    // Common binary operators
    vm.define_binary_op("+".to_string(),  Precedence::new(2,    Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("-".to_string(),  Precedence::new(2,    Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("*".to_string(),  Precedence::new(5,    Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("/".to_string(),  Precedence::new(5,    Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("%".to_string(),  Precedence::new(5,    Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("=".to_string(),  Precedence::new(1000, Fixity::Right), span!(), &mut diags);
    vm.define_binary_op("^".to_string(),  Precedence::new(10,   Fixity::Right), span!(), &mut diags);
    vm.define_binary_op("&&".to_string(), Precedence::new(10,   Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("||".to_string(), Precedence::new(10,   Fixity::Left),  span!(), &mut diags);
    vm.define_binary_op("==".to_string(), Precedence::new(20,   Fixity::Right), span!(), &mut diags);

    vm.define_unary_op("!".to_string(),   Fixity::Left, span!(), &mut diags);
    vm.define_unary_op("-".to_string(),   Fixity::Left, span!(), &mut diags);
    vm.define_unary_op("not".to_string(), Fixity::Left, span!(), &mut diags);

    mkfun!(fun, vm, system, $);

    macro_rules! bin {
        ( $ty: tt ($left: ident $op: tt $right: ident) => $($body: tt)* ) => (
            fun!($ty $op, ($ty $left), ($ty $right), builder => { emit!(builder => $($body)*); })
        );
    }

    macro_rules! multi_bin {
        ( $([$size: tt $ty: ident])+ ($left: ident $op: tt $right: ident) $result: ident => $( $body: tt )* ) => (
            multi_bin!(@fix ($($ty $size)+), $left, $op, $right, $result ( $($body)* ) )
        );

        (@fix ($($ty: ident $size: tt)+), $left: ident, $op: tt, $right: ident, $result: ident $body: tt ) => {{$(
            multi_bin!(@fixr $ty, $size, $left, $op, $right, $result $body );
        )+}};

        (@fixr $ty: tt, $size: tt, $left: ident, $op: tt, $right: ident, $result: ident ( $($body: tt)* ) ) => (
            bin!($ty ($left $op $right) =>
                (for X86 | X86_64 =>
                    ($size -> $result)
                    (mov (-> $result), (-> $left))
                    $( $body )*
                    (>> $result)));
        );
    }

    // Booleans
    fun!(Boolean "!", (Boolean operand), builder => { emit!(builder =>
        (for X86 | X86_64 =>
            (1 -> result)
            (mov (-> result), (-> operand))
            (xor (-> result), (= 1 1))
            (>> result)));
    });
    fun!(Boolean "not", (Boolean operand), builder => { emit!(builder =>
        (for X86 | X86_64 =>
            (1 -> result)
            (mov (-> result), (-> operand))
            (xor (-> result), (= 1 1))
            (>> result)));
    });

    bin!(Boolean (left "&&" right) =>
        (for X86 | X86_64 =>
            (1 -> result)
            (mov (-> result), (-> left))
            (and (-> result), (-> right))
            (>> result))
    );
    bin!(Boolean (left "||" right) =>
        (for X86 | X86_64 =>
            (1 -> result)
            (mov (-> result), (-> left))
            (or (-> result), (-> right))
            (>> result))
    );
    bin!(Boolean (left "^" right) =>
        (for X86 | X86_64 =>
            (1 -> result)
            (mov (-> result), (-> left))
            (xor (-> result), (-> right))
            (>> result))
    );

    // Integers
    fun!(I32 "-", (I32 operand), builder => { emit!(builder =>
        (for X86 | X86_64 =>
            (1 -> result)
            (mov (-> result), (-> operand))
            (neg (-> result))
            (>> result)));
    });

    multi_bin!( [1 I8] [2 I16] [4 I32] [8 I64] (left "+" right) result =>
        (add (-> result), (-> right))
    );
    multi_bin!( [1 I8] [2 I16] [4 I32] [8 I64] (left "-" right) result =>
        (sub (-> result), (-> right))
    );

    macro_rules! eq {
        ( $([$size: tt $ty: ident])+ ) => ({$(
            fun!(Boolean "==", ($ty left), ($ty right), builder => { emit!(builder =>
                (for X86 | X86_64 =>
                    (1 -> result)
                    (cmp (-> left), (-> right))
                    (sete (-> result))
                    (>> result)));
            });
            )+});
    }

    eq!([1 Boolean] [1 I8] [2 I16] [4 I32] [8 I64]);
}

