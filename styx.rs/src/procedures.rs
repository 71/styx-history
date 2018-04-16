//! This module defines the `Proc`, which represents a compiled function in memory.
//! The `Proc` struct contains enough informations for the `Builder` to inline calls to procedures,
//! and to call them efficiently.

use arch::Operand;
use assembler::AssemblyStyle;
use parser::Syntax;
use typesystem::{Fun, Ty, TyParameters};

use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::slice;


/// A procedure compiled by an `Assembler` into machine code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Proc<'cx> {
    /// Reference to the start of the procedure.
    pub ptr: &'cx (),

    /// Length of the procedure's body.
    pub size: u32,

    /// Parameters accepted by the proc.
    pub parameters: Vec<Operand>,

    /// Location of the return value.
    pub return_value: Operand
}

impl<'cx> Proc<'cx> {
    /// Returns a new compiled procedure, given a pointer to the
    /// function it points to, the length of its body, its parameters and
    /// return value.
    pub fn new(ptr: &'cx (), size: u32, parameters: Vec<Operand>, return_value: Operand) -> Self {
        Proc { ptr, size, parameters, return_value }
    }

    /// Returns a slice that represents the body of the compiled function.
    pub fn as_slice(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(self.ptr as *const () as *const u8, self.size as usize)
        }
    }

    /// Writes the content of the compiled function as a string.
    pub fn write_buffer(&self, w: &mut Write) {
        for b in self.as_slice() {
            write!(w, "\\x{:02X}", b).expect("Could not write byte to buffer.")
        }
    }
}

/// A group of procedures related to a same function.
///
/// This type stores both the direct- and continuation-passing-style versions of the [`Proc`]
/// of a single [`Fun`].
#[derive(Debug)]
pub struct Procs<'cx> {
    pub(crate) fun: &'cx Fun<'cx>,
    pub(crate) gh: u64,
    cps_proc: Option<Proc<'cx>>,
    ds_proc: Option<Proc<'cx>>
}

impl<'cx> Eq for Procs<'cx> {}

impl<'cx> PartialEq for Procs<'cx> {
    fn eq(&self, other: &Procs<'cx>) -> bool {
        self.fun == other.fun && self.gh == other.gh
    }
}

impl<'cx> Hash for Procs<'cx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fun.hash(state)
    }
}

impl<'cx> Procs<'cx> {
    /// Returns a new procedures group associated with the specified function,
    /// and for the specified type parameters.
    #[allow(unnecessary_mut_passed)]
    pub fn new(fun: &'cx Fun<'cx>, args: &TyParameters<'static, 'cx>) -> Self {
        Procs { fun, gh: hash!(args), cps_proc: None, ds_proc: None }
    }

    /// Returns the Continuation-Passing Style version of the procedure.
    pub fn cps_proc(&self) -> Option<&Proc<'cx>> {
        self.cps_proc.as_ref()
    }

    /// Returns the Direct Style version of the procedure.
    pub fn ds_proc(&self) -> Option<&Proc<'cx>> {
        self.ds_proc.as_ref()
    }

    /// Returns the specified version of the procedure.
    pub fn get_proc(&self, style: AssemblyStyle) -> Option<&Proc<'cx>> {
        match style {
            AssemblyStyle::CPS => self.cps_proc.as_ref(),
            AssemblyStyle::DS  => self.ds_proc.as_ref()
        }
    }

    /// Sets the Continuation-Passing Style version of the procedure.
    pub fn set_cps_proc(&mut self, procedure: Proc<'cx>) {
        self.cps_proc = Some(procedure)
    }

    /// Sets the Direct Style version of the procedure.
    pub fn set_ds_proc(&mut self, procedure: Proc<'cx>) {
        self.ds_proc = Some(procedure)
    }

    /// Sets the specified version of the procedure.
    pub fn set_proc(&mut self, procedure: Proc<'cx>, style: AssemblyStyle) {
        match style {
            AssemblyStyle::CPS => self.cps_proc = Some(procedure),
            AssemblyStyle::DS  => self.ds_proc  = Some(procedure)
        }
    }

    /// If the CPS procedure already exists, returns it.
    /// Else, creates it using the specified function, inserts it,
    /// and returns it.
    pub fn get_cps_proc_or_insert<F>(&mut self, insertion: F) -> &Proc<'cx> where F: FnOnce() -> Proc<'cx> {
        if let &Some(ref procd) = &dup!(self => Self).cps_proc {
            return procd
        }

        let procd = insertion();

        self.cps_proc = Some(procd);
        self.cps_proc.as_ref().unwrap()
    }

    /// If the DS procedure already exists, returns it.
    /// Else, creates it using the specified function, inserts it,
    /// and returns it.
    pub fn get_ds_proc_or_insert<F>(&mut self, insertion: F) -> &Proc<'cx> where F: FnOnce() -> Proc<'cx> {
        if let &Some(ref procd) = &dup!(self => Self).ds_proc {
            return procd
        }

        let procd = insertion();

        self.ds_proc = Some(procd);
        self.ds_proc.as_ref().unwrap()
    }

    /// Returns a boolean indicating whether the Continuation-Passing Style version of this
    /// procedure has been compiled.
    pub fn has_cps(&self) -> bool {
        self.cps_proc.is_some()
    }

    /// Returns a boolean indicating whether the Direct Style version of this procedure has been
    /// compiled.
    pub fn has_ds(&self) -> bool {
        self.ds_proc.is_some()
    }

    /// Returns the function to which this procedure corresponds.
    pub fn function(&self) -> &'cx Fun<'cx> {
        self.fun
    }

    /// Returns a `Syntax` that represents the current procedure.
    ///
    /// # Errors
    /// The procedure cannot be made into a syntax parser.
    pub fn as_syntax(&self, ty: &'cx Ty<'cx>) -> Option<Syntax<'cx>> {
        if let Some(ref procd) = self.ds_proc {
            Some(Syntax::new::<()>(ty, unsafe { *(procd.ptr as *const () as *const _) }))
        } else {
            None
        }
    }
}

