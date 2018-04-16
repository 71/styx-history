//! The Styx Virtual Machine.
//!
//! This module declares the `VirtualMachine`, which manages binding, compiling, optimizing
//! and encoding all Styx code. It also handles memory, and is the bridge between the type system
//! and the binder.

use arch::*;
use binder::*;
use context::Context;
use diagnostics::*;
use input::Input;
use lexer::Span;
use opt::*;
use parser::*;
use typesystem::{Fun, Ty};

use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::io::Write;
use std::mem;
use std::ptr;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};


/// Represents the Styx Virtual Machine, which can parse, execute and compile Styx code
/// in memory.
#[derive(Debug)]
pub struct VirtualMachine<'vm> {
    target_arch: Architecture,
    arch: Architecture,
    target_opt: Optimization,

    context: Context<'vm>,
    pub(crate) binary_operators: HashMap<String, Precedence>,
    pub(crate) unary_operators: HashMap<String, Fixity>
}

impl<'vm> VirtualMachine<'vm> {
    /// Creates a new virtual machine, given its target architecture and optimization strategy.
    pub fn new(arch: Architecture, opt: Optimization) -> Diagn<RwLock<VirtualMachine<'vm>>> {
        let vm = VirtualMachine {
            arch: match Architecture::current() {
                Some(arch) => arch,
                None => return Err(Diagnostic::unsupported_running_arch(span!()))
            },
            target_arch: arch,
            target_opt: opt,

            context: unsafe { mem::zeroed() },
            binary_operators: HashMap::new(),
            unary_operators: HashMap::new()
        };

        Ok(RwLock::new(vm))
    }

    /// Returns the global `Context` associated with this VM.
    #[inline]
    pub fn context(&self) -> &Context<'vm> {
        &self.context
    }

    /// Returns the global mutable `Context` associated with this VM.
    #[inline]
    pub fn ctx(&mut self) -> &mut Context<'vm> {
        &mut self.context
    }

    /// Returns the `Architecture` used by the JIT compiler.
    #[inline]
    pub fn execution_architecture(&self) -> Architecture {
        self.arch
    }

    /// Returns the `Architecture` used by the executable compiler.
    #[inline]
    pub fn target_architecture(&self) -> Architecture {
        self.target_arch
    }

    /// Returns the `Optimization` strategy used by the compiler.
    #[inline]
    pub fn optimization(&self) -> &Optimization {
        &self.target_opt
    }

    /// Upgrades an item, copying it from its current location to a global
    /// memory region.
    #[inline]
    pub fn upgrade<'i, 'a, T>(&'a mut self, item: &'i T) -> &'vm T {
        self.context.memmnger.store(unsafe { ptr::read(item as *const _) })
    }

    /// Defines a binary operator, given its name and precedence.
    pub fn define_binary_op(&mut self, op: String, precedence: Precedence, span: Span, diagnostics: &mut DiagnosticBag) {
        match self.binary_operators.entry(op) {
            Entry::Occupied(mut entry) => {
                diagnostics.report(Diagnostic::duplicate_bin_op(span));
                entry.insert(precedence);
            },

            Entry::Vacant(entry) => {
                entry.insert(precedence);
            }
        }
    }

    /// Defines an unary operator, given its name and fixity.
    pub fn define_unary_op(&mut self, op: String, fixity: Fixity, span: Span, diagnostics: &mut DiagnosticBag) {
        match self.unary_operators.entry(op) {
            Entry::Occupied(mut entry) => {
                diagnostics.report(Diagnostic::duplicate_unary_op(span));
                entry.insert(fixity);
            },

            Entry::Vacant(entry) => {
                entry.insert(fixity);
            }
        }
    }
}

unsafe impl<'vm> Send for VirtualMachine<'vm> {}
unsafe impl<'vm> Sync for VirtualMachine<'vm> {}

/// A thread-safe reference to a `VirtualMachine` that
/// declares utility methods typically used by the `Binder` and `Assembler`,
/// among others.
#[derive(Clone, Copy, Debug)]
pub struct Vm<'vm> {
    vm: &'vm RwLock<VirtualMachine<'vm>>
}

impl<'vm> Vm<'vm> {
    /// Creates a new `Vm`, given the lock around the `VirtualMachine` it wraps.
    pub fn new(vm: &'vm RwLock<VirtualMachine<'vm>>) -> Self {
        use core::initialize;

        let svm = Vm { vm };
        let mut lock = svm.write();

        mem::forget(mem::replace(&mut lock.context, Context::for_vm(svm)));
        Context::import_all(&mut lock.context);
        initialize(&mut lock);

        svm
    }

    /// Returns an immutable reference to the wrapped `VirtualMachine`.
    pub fn read<'a>(&'a self) -> RwLockReadGuard<'vm, VirtualMachine<'vm>> {
        self.vm.read().unwrap()
    }

    /// Returns a mutable reference to the wrapped `VirtualMachine`.
    pub fn write<'a>(&'a self) -> RwLockWriteGuard<'vm, VirtualMachine<'vm>> {
        self.vm.write().unwrap()
    }

    /// Returns a reference to the `Context` associated with the wrapped `VirtualMachine`.
    pub fn context<'a>(&'a self) -> &'vm Context<'vm> {
        unsafe {
            &*(&self.vm.read().unwrap().context as *const _)
        }
    }

    /// Returns a mutable reference to the `Context` associated with the wrapped `VirtualMachine`.
    pub fn ctx<'a>(&'a self) -> &'vm mut Context<'vm> {
        unsafe {
            &mut *(&mut self.vm.write().unwrap().context as *mut _)
        }
    }
}

impl<'vm> Vm<'vm> {
    /// Returns a `Future` that binds the specified stream of input into a `Bound`
    /// structure, which holds diagnostics and expressions generated by the binding
    /// operation.
    pub fn bind<'cx>(&self, inputs: Input) -> Bind<'vm, 'cx> where 'vm: 'cx {
        Bind::new(*self, inputs)
    }

    /// Compiles the given builder into machine code.
    pub fn compile<'b, 'cx>(&self, fun: &'cx Fun<'cx>, diagnostics: &mut DiagnosticBag) where 'vm: 'cx {
        use assembler::{AssemblyGraph, AssemblyStyle};

        let ctx: &mut Context<'cx> = unsafe { mem::transmute(self.ctx()) };

        let arch = Architecture::current().unwrap();
        let mut graph = AssemblyGraph::new(arch, diagnostics, ctx, dup!(fun)).expect("Could not create graph.");

        if diagnostics.has_error() {
            return
        }

        graph.process(AssemblyStyle::DS, diagnostics, *self);
    }

    /// Calls the given procedure, formatting its output and writing it to the given stream.
    pub fn call<'cx>(&self, procd: *const (), rty: &Ty<'cx>, output: &mut Write) {
        macro_rules! call {
            ( $ty: ty ) => ( write!(output, "{}", (unsafe { ::std::mem::transmute::<_, fn() -> $ty>(procd) })()) );
        }

        match rty.name() {
            "System.Boolean" => call![bool],
            "System.Char" => call![char],
            "System.String" => call![String],
            "System.I32" => call![i32],
            ty => unimplemented!("Cannot format {}.", ty)
        }
        .expect("Could not write output to given buffer.")
    }
}

