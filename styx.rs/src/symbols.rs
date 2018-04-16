//! Styx symbols are global identifiers for `Member`s, such as types or functions.
//!
//! This module provides the `Symbol` trait, useful for symbol lookup,
//! the `Sym` struct, its main implementation, and the `SymbolTree`, used to
//! efficiently lookup symbols based on their name.

use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem;


// //==========================================================================//
// // SYMBOL                                                                   //
// //==========================================================================//

/// Defines an entity which has a name that can be decomposed in multiple
/// parts, all of which corresponding to the hash of the symbol.
pub trait Symbol {
    /// Returns the hashes that make up this symbol.
    fn parts(&self) -> &[u64];

    /// Returns the full name of this symbol.
    fn full_name(&self) -> &str;

    /// Returns a `bool` that represents whether the given symbol matches
    /// the end of the current symbol.
    ///
    /// # Example
    /// ```
    /// use styx::symbols::{Sym, Symbol};
    ///
    /// let a = Sym::from("Hello");
    /// let b = Sym::from("System");
    /// let c = Sym::from("Hello.World");
    /// let d = Sym::from("Hell");
    ///
    /// assert!(c.matches(&a));
    /// ```
    fn matches(&self, other: &Symbol) -> bool {
        let a = self.parts();
        let b = other.parts();

        if a.len() < b.len() {
            return false
        }

        for i in (0..b.len()).rev() {
            if a[i] != b[i] {
                return false
            }
        }

        true
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.parts() == other.parts()
    }
}

impl Eq for Symbol {}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.full_name().fmt(f)
    }
}

impl Hash for Symbol {
    default fn hash<H: Hasher>(&self, state: &mut H) {
        self.parts().hash(state);
    }
}

/// Implements a basic `Symbol`.
#[derive(Clone, Debug)]
pub struct Sym {
    parts: Vec<u64>,
    full_name: String
}

impl Symbol for Sym {
    #[inline]
    fn parts(&self) -> &[u64] {
        &self.parts
    }

    #[inline]
    fn full_name(&self) -> &str {
        self.full_name.as_str()
    }
}

impl Hash for Sym {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.parts().hash(state);
    }
}

impl Default for Sym {
    fn default() -> Sym {
        Sym { parts: Vec::new(), full_name: String::new() }
    }
}

impl AsRef<str> for Sym {
    fn as_ref(&self) -> &str {
        self.full_name.as_str()
    }
}

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.full_name.as_str())
    }
}

impl From<String> for Sym {
    fn from(sym: String) -> Sym {
        let parts = Self::parts(sym.as_str());

        Sym { parts, full_name: sym }
    }
}

impl<'a> From<&'a str> for Sym {
    fn from(sym: &'a str) -> Sym {
        let sym = sym.to_string();
        let parts = Self::parts(sym.as_str());

        Sym { parts, full_name: sym }
    }
}

impl Sym {
    /// Creates and returns the `parts` part of a `Symbol`, given a string.
    ///
    /// # Example
    /// ```
    /// use styx::symbols::Sym;
    ///
    /// assert_eq!(Sym::parts("System.+").len(), 2);
    /// ```
    pub fn parts<S: AsRef<str>>(sym: S) -> Vec<u64> {
        use fnv::FnvHasher;

        let mut parts = Vec::new();

        for part in sym.as_ref().split('.') {
            let mut hasher = FnvHasher::default();
            part.hash(&mut hasher);
            parts.push(hasher.finish());
        }

        parts
    }

    /// Creates and returns a new symbol whose full name is the current symbol's name,
    /// plus the specified string.
    ///
    /// # Example
    /// ```
    /// use styx::symbols::Sym;
    ///
    /// assert_eq!(Sym::from("Hello").child("World"), Sym::from("Hello.World"));
    /// assert_ne!(Sym::from("Foo").child("Bar.Baz"), Sym::from("Foo.Bar.Baz"));
    /// ```
    pub fn child<S: Display + Hash>(&self, sym: S) -> Sym {
        use fnv::FnvHasher;

        let mut hasher = FnvHasher::default();
        sym.to_string().hash(&mut hasher);

        let parts = self.parts.iter().chain(&[hasher.finish()]).cloned().collect();

        Sym { parts, full_name: format!("{}.{}", self.full_name, sym) }
    }

    /// Creates and returns a new symbol whose full name is the current symbol's name,
    /// plus the specified string.
    ///
    /// Additionally, the child might have multiple parts itself.
    ///
    /// # Example
    /// ```
    /// use styx::symbols::Sym;
    ///
    /// assert_eq!(Sym::from("Foo").nested_child("Bar.Baz"), Sym::from("Foo.Bar.Baz"));
    /// ```
    #[allow(needless_pass_by_value)]
    pub fn nested_child<S: ToString>(&self, sym: S) -> Sym {
        let sym = sym.to_string();
        let parts = self.parts.iter().cloned().chain(Self::parts(sym.as_str())).collect();

        Sym { parts, full_name: format!("{}.{}", self.full_name, sym) }
    }
}

impl PartialEq for Sym {
    fn eq(&self, other: &Sym) -> bool {
        self.parts == other.parts
    }
}

impl Eq for Sym {}

/// Implements a simple `Symbol` with no name, only used during lookups.
pub struct LookupSym {
    parts: Vec<u64>
}

impl Symbol for LookupSym {
    #[inline]
    fn parts(&self) -> &[u64] {
        &self.parts
    }

    #[inline]
    fn full_name(&self) -> &str {
        panic!("Unable to get full name of lookup symbol.")
    }
}

impl<'s> From<&'s str> for LookupSym {
    #[inline]
    fn from(symbol: &'s str) -> LookupSym {
        LookupSym { parts: Sym::parts(symbol) }
    }
}

impl From<Vec<u64>> for LookupSym {
    #[inline]
    fn from(parts: Vec<u64>) -> LookupSym {
        LookupSym { parts }
    }
}

impl<'a> From<&'a Symbol> for LookupSym {
    #[inline]
    fn from(symbol: &'a Symbol) -> LookupSym {
        LookupSym { parts: Vec::from(symbol.parts()) }
    }
}


// //==========================================================================//
// // TREE                                                                     //
// //==========================================================================//

/// An `Iterator` over siblings of a `SymbolTree`.
pub struct SiblingsIterator<'a, T: 'a> {
    node: Option<&'a SymbolTree<T>>
}

impl<'a, T: 'a> Iterator for SiblingsIterator<'a, T> {
    type Item = &'a SymbolTree<T>;

    fn next(&mut self) -> Option<&'a SymbolTree<T>> {
        match self.node {
            None => None,
            Some(node) => {
                let node = node.sibling();

                mem::replace(&mut self.node, node.map(|s| s as &_))
            }
        }
    }
}

/// An `Iterator` over every node of a `SymbolTree`.
pub enum TreeIterator<'a, T: 'a> {
    /// Iterating over a single child.
    Child(&'a SymbolTree<T>),

    /// Iterating over children of a node.
    Children(&'a SymbolTree<T>, Box<TreeIterator<'a, T>>),

    /// Iterating over a single sibling.
    Sibling(&'a SymbolTree<T>),

    /// Iterating over siblings of a node.
    Siblings(&'a SymbolTree<T>, Box<TreeIterator<'a, T>>),

    /// Done iterating.
    Done
}

impl<'a, T: 'a> Iterator for TreeIterator<'a, T> {
    type Item = &'a SymbolTree<T>;

    fn next(&mut self) -> Option<&'a SymbolTree<T>> {
        use self::TreeIterator::*;

        loop {
            let mut to_return = None;

            *self = match self {
                &mut Done => return None,

                &mut Child(tree) => {
                    to_return = Some(tree as *const _);

                    Children(tree, box tree.iter())
                },

                &mut Sibling(tree) => {
                    to_return = Some(tree as *const _);

                    Siblings(tree, box tree.iter())
                },

                &mut Siblings(_, ref mut iter) => match iter.as_mut().next() {
                    Some(value) => return Some(value),
                    None => Done
                },

                &mut Children(ref mut tree, ref mut iter) => match iter.as_mut().next() {
                    Some(value) => return Some(value),
                    None => match tree.sibling() {
                        Some(sibling) => Siblings(tree, box sibling.iter()),
                        None => Done
                    }
                }
            };

            if let Some(ret) = to_return {
                return Some(unsafe { ret.as_ref().unwrap() })
            }
        }
    }
}


/// A binary tree that holds items of type `T`.
///
/// Items are looked up using each part of a `Symbol`, making the lookup very fast.
/// References are held as raw memory addresses; that way, the whole tree gets destroyed
/// at once when it is dropped, instead of counting references between each node.
pub struct SymbolTree<T> {
    hash: u64,
    value: Option<T>,

    sibling_ptr: usize,
    child_ptr: usize
}

impl<T: ::std::fmt::Debug> ::std::fmt::Debug for SymbolTree<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        let mut debg = f.debug_struct("Tree");

        if let &Some(c) = &self.child() {
            debg.field("child", c);
        }

        if let &Some(s) = &self.sibling() {
            debg.field("sibling", s);
        }

        if let &Some(ref v) = &self.value {
            debg.field("value", v);
        }

        debg.finish()
    }
}

impl<T> Drop for SymbolTree<T> {
    fn drop(&mut self) {
        match self.sibling_ptr {
            0 => (),
            n => unsafe { Box::from_raw(n as *mut T); }
        }

        match self.child_ptr {
            0 => (),
            n => unsafe { Box::from_raw(n as *mut T); }
        }
    }
}

impl<T> SymbolTree<T> {
    /// Returns a new `SymbolTree`, given its hash and value.
    #[inline]
    pub fn new(hash: u64, value: Option<T>) -> Self {
        SymbolTree { hash, value, sibling_ptr: 0, child_ptr: 0 }
    }

    /// Gets the value contained in the node.
    #[inline]
    pub fn value(&self) -> Option<&T> {
        self.value.as_ref()
    }

    /// Gets the hash of the symbol part represented by this node.
    #[inline]
    pub fn hash(&self) -> u64 {
        self.hash
    }

    /// Gets the next sibling of the node.
    #[inline]
    pub fn sibling(&self) -> Option<&SymbolTree<T>> {
        match self.sibling_ptr {
            0 => None,
            n => Some(unsafe { &mut *(n as *mut _) })
        }
    }

    /// Gets the first child of the node.
    #[inline]
    pub fn child(&self) -> Option<&SymbolTree<T>> {
        match self.child_ptr {
            0 => None,
            n => Some(unsafe { &mut *(n as *mut _) })
        }
    }

    /// Gets an iterator that returns all next siblings of this node.
    #[inline]
    pub fn siblings(&self) -> SiblingsIterator<T> {
        SiblingsIterator { node: Some(self) }
    }

    /// Gets an iterator that returns all siblings and children of this node.
    #[inline]
    pub fn iter(&self) -> TreeIterator<T> {
        use self::TreeIterator::*;

        match self.child() {
            Some(child) => Child(child),
            None => match self.sibling() {
                Some(sibling) => Sibling(sibling),
                None => Done
            }
        }
    }

    /// Gets the value associated with the given symbol.
    #[inline]
    pub fn get<'a>(&'a self, symbol: &'a Symbol) -> Option<&'a T> {
        self.lookup(symbol).and_then(Self::value)
    }

    /// Looks up the node associated with the given symbol.
    #[inline]
    pub fn lookup<'a, 'b>(&'a self, symbol: &'b Symbol) -> Option<&'a SymbolTree<T>> {
        if symbol.parts().is_empty() {
            None
        } else {
            self.recursive_lookup(symbol.parts())
        }
    }

    fn recursive_lookup<'a, 'b>(&'a self, parts: &'b [u64]) -> Option<&'a SymbolTree<T>> {
        if parts[0] == self.hash {
            if parts.len() == 1 {
                Some(self)
            } else if let Some(child) = self.child() {
                child.recursive_lookup(&parts[1..])
            } else {
                None
            }
        } else if let Some(sibling) = self.sibling() {
            sibling.recursive_lookup(parts)
        } else {
            None
        }
    }

    /// Inserts the given value, given its symbol.
    #[inline]
    pub fn insert<'a>(&'a mut self, symbol: &'a [u64], value: T, force: bool) -> Result<&'a SymbolTree<T>, T> {
        if symbol.is_empty() {
            Err(value)
        } else {
            self.recursive_insert(symbol, 0, value, force)
        }
    }

    fn recursive_insert<'a>(&'a mut self, sym: &'a [u64], part: usize, value: T, force: bool) -> Result<&'a SymbolTree<T>, T> {
        if part == sym.len() - 1 {
            // Reached the end of the symbol name, insert value as sibling
            self.insert_sibling(sym[part], value, force)
        } else if sym[part] == self.hash {
            // Matching hash, keep going
            let child = unsafe { &mut *match self.child_ptr {
                0 => {
                    let ptr = Box::into_raw(box Self::new(sym[part], None));
                    self.child_ptr = ptr as usize;
                    ptr
                },
                n => n as *mut _
            }};

            child.recursive_insert(sym, part + 1, value, force)
        } else {
            // None matching hash, insert sibling
            let sibling = unsafe { &mut *match self.sibling_ptr {
                0 => {
                    let ptr = Box::into_raw(box Self::new(sym[part], None));
                    self.sibling_ptr = ptr as usize;
                    ptr
                },
                n => n as *mut _
            }};

            sibling.recursive_insert(sym, part, value, force)
        }
    }

    fn insert_sibling(&mut self, hash: u64, value: T, force: bool) -> Result<&SymbolTree<T>, T> {
        if !force && self.hash == hash {
            // Return error if inserting an exact match
            Err(value)
        } else {
            match self.sibling_ptr {
                0 => {
                    let sibling = Box::into_raw(box Self::new(hash, Some(value)));
                    self.sibling_ptr = sibling as usize;

                    Ok(unsafe { &*sibling })
                },
                n => unsafe { &mut *(n as *mut Self) }.insert_sibling(hash, value, force)
            }
        }
    }
}

impl<T> Default for SymbolTree<T> {
    fn default() -> Self {
        SymbolTree { hash: 0, value: None, child_ptr: 0, sibling_ptr: 0 }
    }
}

