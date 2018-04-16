//! This module contains useful macros used throughout the crate.
//!
//! Most notably, it declares the `emit!` macro, which provides
//! a Lisp-like syntax for emitting raw instructions through a `Builder`.
#![macro_use]


/// Easily creates a `Span`.
///
/// # Example
/// ```
/// #[macro_use] extern crate styx;
/// # use styx::lexer::Span;
/// # fn main() {
/// let start = 0;
/// let end   = 9;
///
/// assert_eq!(span!(),             Span::default());
/// assert_eq!(span!(start),        Span::with_start(start));
/// assert_eq!(span!(start => end), Span::with_start_and_end(start, end));
/// # }
/// ```
#[macro_export]
macro_rules! span {
    () => ($crate::lexer::Span::default());
    ($start: expr) => ($crate::lexer::Span::with_start($start));
    ($start: expr => $end: expr) => ($crate::lexer::Span::with_start_and_end($start, $end));
}

/// Easily defines a `Syntax`.
///
/// # Example
/// ```no_run
/// #[macro_use] extern crate styx;
/// # fn main() {
/// use styx::prelude::{Diags, Ty};
/// use styx::parser::{Parser, ParseResult, Syntax};
///
/// fn visibility_type<'cx>() -> &'cx Ty<'cx> {
///     unimplemented!()
/// }
///
/// fn continue_type<'cx>() -> &'cx Ty<'cx> {
///     unimplemented!()
/// }
///
/// struct Visibility;
///
/// syntax! {
///     in visibility_syntax (visibility_type()):
///
///     fn parse(parser: &mut Parser, diags: Diags) -> ParseResult {
///         unimplemented!()
///     }
/// }
///
/// struct Loop;
///
/// syntax! {
///     in continue_syntax (continue_type()):
///
///     fn parse(this: &Loop, parser: &mut Parser, diags: Diags) -> ParseResult {
///         unimplemented!()
///     }
/// }
///
/// let vis: Syntax = visibility_syntax();
/// let cont: Syntax = continue_syntax(Loop);
/// # }
/// ```
#[macro_export]
macro_rules! syntax {
    (
        type $ty: expr =>
        fn $name: ident($parser: ident, $diags: ident) $body: expr
    ) => (
        fn $name<'a, 'p, 'cx, 'vm>(_s: &'a (), $parser: &mut $crate::parser::Parser<'p, 'cx, 'vm>, $diags: $crate::prelude::Diags<'a>)
                                   -> ParseResult<'cx> {
            $body
        }

        $crate::parser::Syntax::new_x($ty, unsafe { ::std::mem::transmute($name as *const ()) })
    );

    (
        type $ty: expr =>
        in $data: expr =>
        fn $name: ident($this: ident, $parser: ident, $diags: ident) $body: expr
    ) => (
        fn $name<'a, 'p, 'cx, 'vm>($this: &'a (), $parser: &mut $crate::parser::Parser<'p, 'cx, 'vm>, $diags: $crate::prelude::Diags<'a>)
                                   -> ParseResult<'cx> {
            $body
        }

        $crate::parser::Syntax::new_with_state_x($ty, $data, unsafe { ::std::mem::transmute(&$name) })
    );

    (
        type $ty: expr =>
        fn $name: ident($parser: ident, $diags: ident) -> ($rty: ty) $body: expr
    ) => (
        fn $name<'a, 'p, 'cx, 'vm>(_s: &'a (), $parser: &mut $crate::parser::Parser<'p, 'cx, 'vm>, $diags: $crate::prelude::Diags<'a>)
                                   -> Result<$rty, $crate::parser::Failure> {
            $body
        }

        $crate::parser::Syntax::new($ty, unsafe { ::std::mem::transmute(&$name) })
    );

    (
        type $ty: expr =>
        in $data: expr =>
        fn $name: ident($this: ident, $parser: ident, $diags: ident) -> ($rty: ty) $body: expr
    ) => (
        fn $name<'a, 'p, 'cx, 'vm>($this: &'a (), $parser: &mut $crate::parser::Parser<'p, 'cx, 'vm>, $diags: $crate::prelude::Diags<'a>)
                                   -> Result<$rty, $crate::parser::Failure> {
            $body
        }

        $crate::parser::Syntax::new_with_state($ty, $data, unsafe { ::std::mem::transmute(&$name) })
    );

    (
        in $name: ident ($ty: expr):

        $(#[$attrs: meta])*
        fn parse ($parser: ident: $parserty: ty, $diags: ident: $diagsty: ty) -> $retty: ty { $body: expr }
    ) => (
        $(#[$attrs])*
        pub fn $name<'cx>() -> $crate::parser::Syntax<'cx> {
            $crate::parser::Syntax::new($ty, |_s, $parser, $diags| -> $retty {
                $body
            })
        }
    );

    (
        in $name: ident ($ty: expr):

        $(#[$attrs: meta])*
        fn parse ($this: ident: &$st: ty, $parser: ident: $parserty: ty, $diags: ident: $diagsty: ty) -> $retty: ty { $body: expr }
    ) => (
        $(#[$attrs])*
        pub fn $name<'cx>(data: $st) -> $crate::parser::Syntax<'cx> {
            use std::mem::size_of;

            unsafe {
                let data = Box::into_raw(Box::new(data));
                let state = Vec::from_raw_parts(data as _, size_of::<$st>(), size_of::<$st>());

                $crate::parser::Syntax::new_with_state($ty, state, |s, $parser, $diags| -> $retty {
                    let $this = unsafe { &*(s as *const () as *const $st) };
                    $body
                })
            }
        }
    );
}

/// Reports an error and returns `Failure::Fatal`.
///
/// # Example
/// ```no_run
/// #[macro_use] extern crate styx;
/// # fn main() {
/// use styx::diagnostics::{Diagnostic, DiagnosticBag};
/// use styx::parser::Failure;
///
/// let mut diagnostics = DiagnosticBag::default();
///
/// fn parse(diagnostics: &mut DiagnosticBag) -> Result<(), Failure> {
///     report!(diagnostics, Diagnostic::error(0, "Not implemented.", span!()));
/// }
/// # }
/// ```
#[macro_export]
macro_rules! report {
    ( $diagnostics: expr, $expr: expr ) => ({
        $diagnostics.report($expr);
        return Err(Failure::Fatal)
    })
}

/// Hashes the given item.
macro_rules! hash {
    ( $expr: expr, $hasher: expr ) => {{
        use std::hash::Hash;
        Hash::hash($expr, $hasher);
        $hasher.finish()
    }};

    ( $expr: expr ) => ( hash!($expr, &mut ::fnv::FnvHasher::default()) );
}

/// Takes the successful result of a `Result`.
#[allow(unused_macros)]
macro_rules! take_ok {
    ( $result: expr ) => (match $result {
        Ok(result) => result,
        _ => unreachable!()
    });
}

/// Takes the successful result of an `Option`.
#[allow(unused_macros)]
macro_rules! take_ok {
    ( $option: expr ) => (match $option {
        Some(result) => result,
        _ => unreachable!()
    });
}

/// Returns the byte representation of the given `usize`.
#[cfg(target_pointer_width = "32")]
#[allow(unused_macros)]
macro_rules! usize_to_bytes {
    ( $v: expr ) => (unsafe { ::std::mem::transmute::<usize, [u8; 4]>($v) });
}

/// Returns the byte representation of the given `usize`.
#[cfg(target_pointer_width = "64")]
#[allow(unused_macros)]
macro_rules! usize_to_bytes {
    ( $v: expr ) => (unsafe { ::std::mem::transmute::<usize, [u8; 8]>($v) });
}

/// Unsafely duplicates a (mutable) reference, deceiving the borrow checker.
macro_rules! dup {
    ( ($explain: tt) $r: expr )     => (dup!($r));
    ( ($explain: tt) mut $r: expr ) => (dup!(mut $r));

    ( ($explain: tt) $r: expr => $t: ty )     => (dup!($r => $t));
    ( ($explain: tt) mut $r: expr => $t: ty ) => (dup!(mut $r => $t));

    ( $r: expr )     => (unsafe { ($r as *const _).as_ref().unwrap() });
    ( mut $r: expr ) => (unsafe { ($r as *mut _).as_mut().unwrap() });

    ( $r: expr => $t: ty )     => (unsafe { ($r as *const $t).as_ref().unwrap() });
    ( mut $r: expr => $t: ty ) => (unsafe { ($r as *mut $t).as_mut().unwrap() });
}

/// Eases the creation of a static type.
macro_rules! static_type {
    ( $name: ident, $ty: expr ) => (lazy_static! {
        static ref $name: $crate::typesystem::Ty<'static> = {
            $ty
        };
    });

    ( $ty: expr ) => ({
        static_type!(TY, $ty);

        unsafe {
            &*(&*TY as *const _ as *const () as *const _)
        }
    });
}


#[doc(hidden)]
#[macro_export]
macro_rules! __register {
    ( $b: expr, eax ) => ( $crate::instr::Register::EAX );
    ( $b: expr, rax ) => ( $crate::instr::Register::RAX );
}

#[doc(hidden)]
#[macro_export]
macro_rules! __operand {
    // End of operands list
    ( $b: expr, ) => ();

    // Builder variable syntax
    ( $b: expr, @ $size: tt $var: ident ) => ( $b.get_variable($size, stringify!($var)) );

    // Memory address syntax
    ( $b: expr, [$size: tt $mem: expr] ) => ( $b.new_value($size, stringify!([$size: $mem]), $crate::builder::ValueKind::Memory($mem)) );

    // Local variable syntax
    ( $b: expr, -> $var: ident ) => ( $var.clone() );

    // Immediate syntax
    ( $b: expr, = $size: tt $imm: expr ) => ( $b.new_value($size, stringify!($imm), $crate::builder::ValueKind::Immediate($imm)) );
}

/// Easily emits instructions in a builder.
///
/// The syntax for this macro is inspired by the Intel Assembly syntax.
///
/// # Example
/// ```text
/// emit!(builder =>                    // specify that the `Builder` in scope named 'builder'
///                                     // will be used
///
///     (for X86 | X86_64 =>            // emit for the X86 | X86_64 architecture
///
///         (4 -> foo)                  // declare a variable named 'foo' of size 4
///         (mov (-> foo), (= 4 42))    // moves the immediate '42' of size 4 to 'foo'
///         (add (-> foo), (@ 4 bar))   // adds the 'bar' variable of size 4 to 'foo'
///         (>> foo))                   // returns 'foo'
///
///     !! binder                       // reports an error explaining that the target arch
///                                     // isn't supported using the binder in scope
/// );
/// ```
#[macro_export]
macro_rules! emit {
    // Does nothing.
    ( $builder: expr => ) => ();

    // Emits an empty instruction.
    ( $builder: expr => ( $opcode: ident ) $($tail: tt)* ) => (
        $builder.emit0(stringify!($opcode));
        emit!($builder => $($tail)*);
    );

    // Defines a variable.
    ( $builder: expr => ( $varsize: tt -> $varname: ident ) $($tail: tt)* ) => (
        let $varname = $builder.define($varsize, stringify!($varname));

        emit!($builder => $($tail)*);
    );

    // Defines a block.
    ( $builder: expr => ( : $name: ident $(($size: tt $parameters: ident)),* ) $($tail: tt)* ) => (
        let parameters = ;

        let $name = $builder.($varsize, Some(stringify!($varname)));

        emit!($builder => $($tail)*);
    );

    // Jumps to block.
    ( $builder: expr => ( @ $name: ident $(($size: tt $parameters: ident)),* ) $($tail: tt)* ) => (
        let parameters = ;

        let $name = $builder.($varsize, Some(stringify!($varname)));

        emit!($builder => $($tail)*);
    );

    // Returns a value.
    ( $builder: expr => ( >> $varname: ident ) $($tail: tt)* ) => (
        $builder.emit_ret($varname);

        emit!($builder => $($tail)*);
    );

    // Calls a method.
    ( $builder: expr => ( ? $fun: ident $($parameters: ident),* ) $($tail: tt)* ) => (
        let $name = $builder.emit_call($fun);

        emit!($builder => $($tail)*);
    );

    // Emits a 1-operand instruction.
    ( $builder: expr => ( $opcode: ident ($($operand1: tt)+) ) $($tail: tt)* ) => (
        let a = __operand!($builder, $($operand1)*);

        $builder.emit1(stringify!($opcode), a);
        emit!($builder => $($tail)*);
    );

    // Emits a 2-operands instruction.
    ( $builder: expr => ( $opcode: ident ($($operand1: tt)+), ($($operand2: tt)+) ) $($tail: tt)* ) => (
        let a = __operand!($builder, $($operand1)*);
        let b = __operand!($builder, $($operand2)*);

        $builder.emit2(stringify!($opcode), a, b);
        emit!($builder => $($tail)*);
    );

    // Emits a 3-operands instruction.
    ( $builder: expr => ( $opcode: ident ($($operand1: tt)+), ($($operand2: tt)+), ($($operand3: tt)+) ) $($tail: tt)* ) => (
        let a = __operand!($builder, $($operand1)*);
        let b = __operand!($builder, $($operand2)*);
        let c = __operand!($builder, $($operand3)*);

        $builder.emit3(stringify!($opcode), a, b, c);
        emit!($builder => $($tail)*);
    );

    // Emits an n-operands instruction.
    ( $builder: expr => ( $opcode: ident $(($($operands: tt)+)),+ ) $($tail: tt)* ) => (
        let mut vec = Vec::new();

        $(
            vec.push(__operand!($builder, $($operands)*));
        )+

        $builder.emit(stringify!($opcode), &vec);
        emit!($builder => $($tail)*);
    );

    // Emits for a specific architecture.
    ( $builder: expr => ( for $($archs: ident)|* => $($exprs: tt)* ) $($tail: tt)* ) => (
        if $( $builder.arch() == $crate::arch::Architecture::$archs )||* {
            emit!($builder => $($exprs)*);
        }

        emit!($builder => $($tail)*);
    );

    // Panics.
    ( $builder: expr => !! $binder: expr ) => (
        $binder.diagnostics().report($crate::diagnostics::Diagnostic::unsupported_arch(span!()));
    );
}

