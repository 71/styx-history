//! This module defines various utilities that do not belong in other modules.

use std::fmt::{self, Debug, Display, Formatter};
use std::hash::*;
use std::mem;

use alloc::raw_vec::RawVec;
use string_interner::StringInterner;


/// Defines an interned string, which can be cheaply cloned.
pub struct InternedString<'cx> {
    interner: &'cx StringInterner<usize>,
    symbol: usize
}

impl<'cx> InternedString<'cx> {
    /// Creates an interned string, given its interner and symbol.
    pub fn new(interner: &'cx StringInterner<usize>, symbol: usize) -> Self {
        InternedString { interner, symbol }
    }
}

impl<'cx> Clone for InternedString<'cx> {
    fn clone(&self) -> InternedString<'cx> {
        InternedString { interner: dup!(self.interner), symbol: self.symbol }
    }
}

impl<'cx> Display for InternedString<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        unsafe {
            f.write_str(self.interner.resolve_unchecked(self.symbol))
        }
    }
}

impl<'cx> Debug for InternedString<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        unsafe {
            f.write_str(self.interner.resolve_unchecked(self.symbol))
        }
    }
}

/// Defines a data structure that can be interned.
pub trait Internable: Into<String> + AsRef<str> {
    fn intern(self, interner: &mut StringInterner<usize>) -> InternedString {
        let symbol = interner.get_or_intern(self);
        InternedString::new(interner, symbol)
    }
}

/// Defines a structure that can be displayed, provided a `StringInterner`.
pub trait InternedDisplay {
    /// Creates a human-readable representation of the structure, optionally
    /// retrieving data in a `StringInterner`.
    fn fmt(&self, f: &mut Formatter, interner: &StringInterner<usize>) -> fmt::Result;

    /// Returns a string representation of the structure.
    fn display(&self, interner: &StringInterner<usize>) -> String where Self: Sized {
        let fmt = {
            #[allow(dead_code)]
            struct ManualDisplay<'a>(&'a InternedDisplay, &'a StringInterner<usize>);

            impl<'a> Display for ManualDisplay<'a> {
                fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                    self.0.fmt(f, self.1)
                }
            }

            ManualDisplay(self, interner)
        };

        format!("{}", fmt)
    }
}

impl<T: Display> InternedDisplay for T {
    fn fmt(&self, f: &mut Formatter, _: &StringInterner<usize>) -> fmt::Result {
        Display::fmt(self, f)
    }
}


/// A map of generic types.
pub struct Generics<T> {
    vec: RawVec<*const T>,
    len: usize
}

impl<T> Default for Generics<T> {
    fn default() -> Self {
        Generics { vec: RawVec::with_capacity(28), len: 0 }
    }
}

impl<T> Drop for Generics<T> {
    fn drop(&mut self) {
        let size = mem::size_of::<*const T>();
        let ptr = self.vec.ptr() as usize;

        for i in 0..self.len {
            unsafe {
                Box::from_raw((ptr + i * size + 8) as *mut T);
            }
        }
    }
}

impl<T: Hash> Generics<T> {
    /// If the given type already exists in the generic map; returns a reference to it.
    /// Else, inserts it and returns a reference to the inserted value.
    pub fn get_or_insert<'vm>(&mut self, ty: T) -> &'vm T {
        use fnv::FnvHasher;

        let size = mem::size_of::<*const T>() + 8;
        let vec = &mut self.vec;
        let len = self.len;

        let mut addr = vec.ptr() as usize;

        let hash = {
            let mut hasher = FnvHasher::default();
            ty.hash(&mut hasher);
            hasher.finish()
        };

        unsafe {
            for i in 0..len {
                let addr = addr + (size * i);
                let item_hash = *(addr as *mut u64);

                if item_hash == hash {
                    let addr = *((addr + 8) as *const usize) as *const _;
                    return &*addr
                }
            }

            if vec.cap() == len {
                vec.double();
                addr = vec.ptr() as usize;
            }

            let boxed = Box::into_raw(box ty);

            *((addr + len * size) as *mut _) = hash;
            *((addr + len * size + 8) as *mut _) = boxed;

            &*boxed
        }
    }
}

/// A structure similar to a `HashMap`, but that uses references for lookups.
#[derive(Clone, Debug)]
pub struct RefMap<'k, K: 'k, V> {
    keys: Vec<&'k K>,
    values: Vec<V>
}

impl<'k, K, V> Default for RefMap<'k, K, V> {
    fn default() -> Self {
        RefMap { keys: Vec::default(), values: Vec::default() }
    }
}

impl<'k, K, V> RefMap<'k, K, V> {
    /// Creates a new RefMap, given its capacity.
    pub fn with_capacity(cap: usize) -> Self {
        RefMap { keys: Vec::with_capacity(cap), values: Vec::with_capacity(cap) }
    }

    /// Returns the keys of this map.
    pub fn keys(&self) -> &[&'k K] {
        &self.keys
    }

    /// Returns the values of this map.
    pub fn values(&self) -> &[V] {
        &self.values
    }

    /// Gets the value related to the specified reference.
    pub fn get(&self, key: &K) -> Option<&V> {
        let key = key as *const K;

        for (i, k) in self.keys.iter().enumerate() {
            if *k as *const K == key {
                return unsafe {
                    Some(self.values.get_unchecked(i))
                }
            }
        }

        None
    }

    /// Returns a `bool` that represents whether the reference map contains
    /// a value matching the specified key.
    pub fn contains(&self, key: &K) -> bool {
        let key = key as *const K;

        for k in &self.keys {
            if *k as *const K == key {
                return true
            }
        }

        false
    }

    /// Takes the value matching the specified key.
    pub fn take(&mut self, key: &K) -> Option<V> {
        let key = key as *const K;
        let mut i = 0;

        while i < self.keys.len() {
            let is_match = unsafe {
                *self.keys.get_unchecked(i) as *const K == key
            };

            if is_match {
                self.keys.swap_remove(i);

                return Some(self.values.swap_remove(i));
            } else {
                i += 1;
            }
        }

        None
    }

    /// Inserts the given value in the reference map;
    /// if it already exists, its old value will be returned.
    pub fn insert(&mut self, key: &'k K, value: V) -> Option<V> {
        let result = self.take(key);

        self.keys.push(key);
        self.values.push(value);

        result
    }
}

