//! [`Context`] and [`MemoryManager`] types.

use diagnostics::{Diagn, Diagnostic};
use emit::Emitter;
use procedures::Procs;
use symbols::{LookupSym, Symbol};
use typesystem::{Fun, Member, MemberTree, Ty, TyParameters};
use utils::{Generics, InternedString};
use vm::Vm;

use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;
use std::mem::{self, size_of};
use std::ptr;

use alloc::allocator::{Alloc, AllocErr, CannotReallocInPlace, Excess};
use alloc::heap::{Heap, Layout};
use string_interner::StringInterner;


/// The context of a binder or of the virtual machine, which provides ways to allocate memory in different arenas,
/// and to store certain elements in a certain context.
///
/// # Notes
/// Typically, the context should be written `ctx` for mutable references, and `context` for
/// immutable references.
pub struct Context<'cx> {
    parent: Option<&'cx Context<'cx>>,
    vm: Vm<'cx>,
    pub(crate) interner: StringInterner<usize>,
    pub(crate) memmnger: MemoryManager<'cx>,

    pub(crate) members: MemberTree<'cx>,
    pub(crate) procs: Vec<Procs<'cx>>,
    pub(crate) gentys: Generics<Ty<'cx>>,
    pub(crate) genfns: Generics<Fun<'cx>>
}

impl<'cx> Debug for Context<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Context")
    }
}

impl<'cx> Context<'cx> {
    pub(crate) fn for_vm<'vm: 'cx>(vm: Vm<'vm>) -> Self {
        Context {
            parent: None,
            vm: unsafe { mem::transmute(vm) },
            interner: StringInterner::new(),
            procs: Vec::new(),
            gentys: Generics::default(),
            genfns: Generics::default(),
            members: MemberTree::default(),
            memmnger: MemoryManager::default()
        }
    }

    /// Creates a new `Context`, given the virtual machine to which it is associated.
    pub fn new<'vm: 'cx>(vm: Vm<'vm>) -> Self {
        Context {
            parent: Some(unsafe { mem::transmute(vm.read().context()) }),
            vm: unsafe { mem::transmute(vm) },
            interner: StringInterner::new(),
            procs: Vec::new(),
            gentys: Generics::default(),
            genfns: Generics::default(),
            members: MemberTree::default(),
            memmnger: MemoryManager::default()
        }
    }

    /// Creates a new `Context`, which is associated with the given parent context.
    pub fn child<'pcx>(parent: &'cx Context<'pcx>) -> Self {
        Context {
            vm: unsafe { mem::transmute(parent.vm()) },
            parent: Some(unsafe { mem::transmute(parent) }),
            interner: StringInterner::new(),
            procs: Vec::new(),
            gentys: Generics::default(),
            genfns: Generics::default(),
            members: MemberTree::default(),
            memmnger: MemoryManager::default()
        }
    }

    /// Returns the virtual machine associated with this context.
    #[inline]
    pub fn vm(&self) -> Vm<'cx> {
        self.vm
    }

    /// Gets or interns the given string.
    #[inline]
    pub fn get_or_intern<S: Into<String> + AsRef<str>>(&mut self, string: S) -> InternedString<'cx> {
        InternedString::new(dup!(&self.interner), self.interner.get_or_intern(string))
    }

    /// Gets the string with the given symbol.
    ///
    /// # Safety
    /// This method does not check whether the specified symbol exists,
    /// and implies that it does.
    /// If it doesn't exist, undefined behavior should be expected to arise.
    #[inline]
    pub fn get_string(&self, symbol: usize) -> &str {
        assert!(self.interner.len() > symbol);

        unsafe {
            self.interner.resolve_unchecked(symbol)
        }
    }


    /// Adds the given type if it doesn't override another existing
    /// one, and returns a reference to its position.
    ///
    /// # Safety
    /// The returned reference to the type is guaranteed to be valid for the context's lifetime.
    ///
    /// # Errors
    /// A type with the same name is already defined.
    pub fn add_type<'a>(&'a mut self, ty: Ty<'cx>) -> Diagn<&'cx Ty<'cx>> {
        let sym = LookupSym::from(ty.parts().to_vec());
        let ty = self.memmnger.store(ty);

        match self.members.insert(sym.parts(), Member::Type(ty), false) {
            Ok(_)  => Ok(ty),
            Err(_) => Err(Diagnostic::duplicate_ty(span!()))
        }
    }

    /// Globally adds the given function if it doesn't override another existing
    /// one, and returns a reference to its global position.
    ///
    /// # Safety
    /// The returned reference to the function is guaranteed to be valid for the context's lifetime.
    pub fn add_function<'a>(&'a mut self, fun: Fun<'cx>) -> &'cx Fun<'cx> {
        let sym = LookupSym::from(fun.parts().to_vec());
        let fun = self.memmnger.store(fun);

        self.members.insert(sym.parts(), Member::Function(fun), true).unwrap();
        fun
    }

    /// Adds the specified generic type to the list of generic types
    /// if it doesn't already exist, or returns it if it does.
    pub fn generic_type<'a>(&'a mut self, gen: Ty<'cx>) -> &'cx Ty<'cx> {
        self.gentys.get_or_insert(gen)
    }

    /// Adds the specified generic function to the list of generic functions
    /// if it doesn't already exist, or returns it if it does.
    pub fn generic_function<'a>(&'a mut self, gen: Fun<'cx>) -> &'cx Fun<'cx> {
        self.genfns.get_or_insert(gen)
    }

    /// Gets the type bearing the specified name.
    pub fn get_type<'a>(&'a self, name: &'a Symbol) -> Option<&'cx Ty<'cx>> {
        match self.members.get(name) {
            Some(member) => member.as_type(),
            None => match self.parent {
                Some(parent) => parent.get_type(name),
                None => None
            }
        }
    }

    /// Gets all the functions bearing the specified name.
    pub fn get_functions<'a>(&'a self, name: &'a Symbol) -> Vec<&'cx Fun<'cx>> {
        let name_hash = {
            let parts = name.parts();
            parts[parts.len() - 1]
        };

        match self.members.lookup(name) {
            Some(node) => node.siblings().filter_map(|n|
                if n.hash() == name_hash {
                    n.value().and_then(Member::as_function)
                } else {
                    None
                }
            ).collect(),
            None => match self.parent {
                Some(parent) => parent.get_functions(name),
                None => vec!()
            }
        }
    }

    /// Gets the procedure group associated with the specified function.
    /// If it doesn't exist, it will be created.
    pub fn get_or_insert_procs(&mut self, function: &'cx Fun<'cx>) -> &mut Procs<'cx> {
        for procs in &mut dup!(mut self => Self).procs {
            if procs.fun as *const _ == function as *const _ {
                return procs
            }
        }

        let len = self.procs.len();
        self.procs.push(Procs::new(function, &TyParameters::default()));

        unsafe {
            self.procs.get_unchecked_mut(len)
        }
    }

    /// Gets the procedure group associated with the specified function.
    pub fn get_procs(&self, function: &'cx Fun<'cx>) -> Option<&Procs<'cx>> {
        for procs in &self.procs {
            if procs.fun as *const _ == function as *const _ {
                return Some(procs)
            }
        }
        None
    }

    /// Returns a reference to the tree containing all members declared in this context.
    pub fn members<'a>(&'a self) -> &'cx MemberTree<'cx> {
        // The root tree lives as long as the VM does, and does not move in memory.
        // We can thus extend its lifetime to the context's.
        dup!(&self.members)
    }

    /// Returns a slice of all the procedures in this context.
    pub fn procedures<'a>(&'a self) -> &[Procs<'cx>] {
        &self.procs
    }

    /// Creates and returns an emitter in this context, allocated for the specified size.
    pub fn create_emitter<'a>(&'a mut self, size: usize) -> Emitter<'a, 'cx> {
        let ptr = self.memmnger.allocate(size);

        Emitter::Memory { memory: &mut self.memmnger, cap: size, size: 0, ptr }
    }

    /// Imports all statically-defined functions and types into this context.
    pub(crate) fn import_all(&mut self) {
        macro_rules! import {
            ( ) => ();

            ( type $ty: expr; $($tail: tt)* ) => (
                self.members.insert($ty.parts(), Member::Type($ty), false).expect("Pre-existing type already imported.");
                import!($($tail)*);
            );

            ( fn $fun: expr; $($tail: tt)* ) => (
                self.members.insert($fun.parts(), Member::Function($fun), true).expect("Pre-existing function already imported.");
                import!($($tail)*);
            );
        }

        use expr::BuiltIns;

        import! {
            type Ty::void();
            type Ty::expression();
            type Ty::quote();

            type BuiltIns::block();
            type BuiltIns::unit();
            type BuiltIns::call();
            type BuiltIns::literal();
            type BuiltIns::magic();
            type BuiltIns::switch();
            type BuiltIns::ret();
            type BuiltIns::unary();
            type BuiltIns::binary();
            type BuiltIns::conditional();
        }
    }
}


// //==========================================================================//
// // MEMORY MANAGER                                                           //
// //==========================================================================//

struct MemoryRegion {
    ptr: usize,
    align: usize,
    cap: usize,
    len: usize
}

/// A memory manager used by the [`Context`] to allocate memory both in executable
/// and non-executable regions of memory.
pub struct MemoryManager<'m> {
    rwx_regions: Vec<MemoryRegion>,
    rw_regions: Vec<MemoryRegion>,
    rwx: RwxHeap,
    rw: Heap,

    _phantom: PhantomData<&'m ()>
}

impl<'m> Default for MemoryManager<'m> {
    fn default() -> Self {
        MemoryManager {
            rwx_regions: Vec::new(),
            rw_regions: Vec::new(),
            rwx: RwxHeap::default(),
            rw: Heap::default(),
            _phantom: PhantomData
        }
    }
}

impl<'m> Drop for MemoryManager<'m> {
    fn drop(&mut self) {
        macro_rules! dealloc {
            ( $allocator: expr, $regions: expr ) => (
                for MemoryRegion { ptr, align, cap, .. } in $regions.drain(..) {
                    $allocator.dealloc(ptr as *mut u8, Layout::from_size_align_unchecked(cap, align));
                }
            );
        }

        unsafe {
            dealloc!(self.rwx, self.rwx_regions);
            dealloc!(self.rw,  self.rw_regions);
        }
    }
}

impl<'m> MemoryManager<'m> {
    /// Stores the given value in the read-write memory managed by this
    /// manager, and returns a pointer to it.
    pub fn store<'s, T>(&'s mut self, value: T) -> &'m mut T {
        unsafe {
            let allocated = &mut *(Self::alloc(&mut self.rw_regions, &mut self.rw, size_of::<T>()) as *mut T);
            ptr::write(allocated, value);
            allocated
        }
    }

    /// Allocates the given amount of read-write-execute memory for an item,
    /// and returns a reference to it.
    pub fn allocate<'s>(&'s mut self, size: usize) -> &'m mut u8 {
        unsafe {
            &mut *Self::alloc(&mut self.rwx_regions, &mut self.rwx, size)
        }
    }

    fn alloc<A: Alloc>(regions: &mut Vec<MemoryRegion>, allocator: &mut A, size: usize) -> *mut u8 {
        // try to find an available region
        for &mut MemoryRegion { ptr, ref mut len, cap, .. } in regions.iter_mut() {
            if cap - *len > size {
                let offset = (ptr + *len) as *mut u8;
                *len += size;
                return offset
            }
        }

        // no available region, create one
        unsafe {
            let align = size.next_power_of_two();
            let rsize = if align > 4096 { align } else { 4096 };

            let layout = Layout::from_size_align_unchecked(rsize, align);
            let ptr = allocator.alloc(layout).unwrap();

            regions.push(MemoryRegion { ptr: ptr as usize, align, len: size, cap: rsize });

            ptr
        }
    }
}


// //==========================================================================//
// // RWX HEAP                                                                 //
// //==========================================================================//

/// Represents an allocator that allocates rwx regions on the `Heap`.
pub struct RwxHeap {
    h: Heap,
    pp: i32
}

impl RwxHeap {
    #[cfg(windows)]
    #[inline]
    const fn rwx_protection() -> i32 {
        0x40
    }

    #[cfg(not(windows))]
    #[inline]
    const fn rwx_protection() -> i32 {
        use libc::{PROT_EXEC, PROT_READ, PROT_WRITE};

        PROT_EXEC | PROT_READ | PROT_WRITE
    }

    #[cfg(windows)]
    fn protect(start: *mut u8, layout: &Layout, protection: i32) -> i32 {
        use kernel32;

        unsafe {
            let mut prev: u32 = mem::uninitialized();

            kernel32::VirtualProtect(start as *mut _, layout.size() as _, protection as _, &mut prev);

            prev as _
        }
    }

    #[cfg(not(windows))]
    fn protect(start: *mut u8, layout: &Layout, protection: i32) -> i32 {
        use libc;
        use libc::{PROT_READ, PROT_WRITE};

        unsafe {
            libc::mprotect(start as *mut _, layout.size(), protection);
        }

        PROT_READ | PROT_WRITE
    }
}

impl Default for RwxHeap {
    fn default() -> Self {
        RwxHeap { h: Heap::default(), pp: 0 }
    }
}

unsafe impl Alloc for RwxHeap {
    #[inline]
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        match self.h.alloc(layout.clone()) {
            Ok(ptr) => {
                self.pp = Self::protect(ptr, &layout, Self::rwx_protection());
                Ok(ptr)
            },

            err => err
        }
    }

    #[inline]
    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        Self::protect(ptr, &layout, self.pp);
        self.h.dealloc(ptr, layout)
    }

    #[inline]
    fn oom(&mut self, e: AllocErr) -> ! {
        self.h.oom(e)
    }

    #[inline]
    fn usable_size(&self, layout: &Layout) -> (usize, usize) {
        self.h.usable_size(layout)
    }

    #[inline]
    unsafe fn realloc(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<*mut u8, AllocErr> {
        self.h.realloc(ptr, layout, new_layout)
    }

    #[inline]
    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.h.alloc_zeroed(layout)
    }

    #[inline]
    unsafe fn alloc_excess(&mut self, layout: Layout) -> Result<Excess, AllocErr> {
        self.h.alloc_excess(layout)
    }

    #[inline]
    unsafe fn realloc_excess(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<Excess, AllocErr> {
        self.h.realloc_excess(ptr, layout, new_layout)
    }

    #[inline]
    unsafe fn grow_in_place(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<(), CannotReallocInPlace> {
        self.h.grow_in_place(ptr, layout, new_layout)
    }

    #[inline]
    unsafe fn shrink_in_place(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<(), CannotReallocInPlace> {
        self.h.shrink_in_place(ptr, layout, new_layout)
    }
}

