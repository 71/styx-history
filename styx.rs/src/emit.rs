//! This module defines the `Emitter`, which emits functions and data either to a file,
//! or to memory.

use arch::Operand;
use context::MemoryManager;
use diagnostics::*;

use std::fs::File;
use std::io::{self, Seek, SeekFrom, Write};
use std::ptr;


// //==========================================================================//
// // EMITTER                                                                  //
// //==========================================================================//

/// A struct that emits functions to a memory region,
/// or to a file.
pub enum Emitter<'a, 'cx: 'a> {
    /// Emits to an in-memory rwx region.
    Memory {
        /// [`MemoryManager`] responsible for allocating memory when
        /// the emitter runs out of free memory.
        memory: &'a mut MemoryManager<'cx>,
        /// Capacity of the emitter.
        cap: usize,
        /// Size of the data emitted so far.
        size: usize,
        /// Pointer to the start of the region to which this emitter emits.
        ptr: *mut u8
    },

    /// Emits to a file.
    File {
        /// Size of the data emitted so far.
        size: usize,
        /// File to which the data shall be emitted.
        file: File
    }
}

impl<'a, 'cx> Emitter<'a, 'cx> {
    /// Emits the given byte in an executable context.
    pub fn emit_byte(&mut self, byte: u8) -> Result<(), EmitError> {
        match self {
            &mut Emitter::File { ref mut file, ref mut size } => {
                if let Err(error) = file.write(&[ byte ]) {
                    return Err(EmitError::IO(error.to_string()))
                }

                *size += 1;
            },

            &mut Emitter::Memory { size, cap, .. } if cap == size + 1 => {
                return self.emit_bytes(&[ byte ]);
            },
            &mut Emitter::Memory { ptr, ref mut size, .. } => unsafe {
                ptr::write(ptr.offset(*size as isize), byte);
                *size += 1;
            }
        }

        Ok(())
    }

    /// Emits the given buffer in an executable context.
    pub fn emit_bytes(&mut self, bytes: &[u8]) -> Result<(), EmitError> {
        let length = bytes.len();

        match self {
            &mut Emitter::File { ref mut file, ref mut size } => {
                if let Err(error) = file.write(bytes) {
                    return Err(EmitError::from(error))
                }

                *size += length;
            },

            &mut Emitter::Memory { ref mut ptr, ref mut size, ref mut cap, ref mut memory } => unsafe {
                let endsize = *size + length;

                if endsize > *cap {
                    assert!(*cap * 2 > *size + length);

                    info!("{:X}\tReallocating memory for emitter.", ptr.offset(*size as isize) as usize);

                    // not enough room; allocate new portion
                    let new_ptr = memory.allocate(*cap * 2);

                    ptr::copy(*ptr, new_ptr, *size);

                    *ptr = new_ptr;
                    *cap *= 2;
                }

                ptr::copy(bytes.as_ptr(), ptr.offset(*size as isize), length);

                *size = endsize;
            }
        }

        Ok(())
    }

    /// Emits the given buffer in an executable context, at the specified position.
    pub fn emit_bytes_at(&mut self, offset: usize, bytes: &[u8]) -> Result<(), EmitError> {
        let length = bytes.len();

        match self {
            &mut Emitter::File { ref mut file, size } => {
                let start_pos = size as u64;

                if offset + length >= size {
                    return Err(EmitError::IndexOutOfRange)
                }

                if let Err(error) = file.seek(SeekFrom::Start(offset as u64)) {
                    return Err(EmitError::from(error))
                }

                if let Err(error) = file.write(bytes) {
                    file.seek(SeekFrom::Start(start_pos)).expect("Could not recover from writing error.");
                    return Err(EmitError::from(error))
                }

                if let Err(error) = file.seek(SeekFrom::Start(start_pos)) {
                    return Err(EmitError::from(error))
                }
            },

            &mut Emitter::Memory { size, .. } => unsafe {
                if offset + length >= size {
                    return Err(EmitError::IndexOutOfRange)
                }

                ptr::copy(bytes.as_ptr(), offset as *mut u8, length);
            }
        }

        Ok(())
    }

    /// Places the emitter at the specified offset.
    pub fn goto(&mut self, offset: usize) {
        match self {
            &mut Emitter::File { ref mut file, .. } => { file.seek(SeekFrom::Start(offset as _)).expect("Could not go to offset."); },
            &mut Emitter::Memory { ref mut size, ptr, .. } => *size = offset - ptr as usize
        }
    }

    /// Returns the current offset, which can be later used to apply modifications to emitted code.
    pub fn offset(&self) -> usize {
        match self {
            &Emitter::File { size, .. } => size as usize,
            &Emitter::Memory { ptr, size, .. } => ptr as usize + size
        }
    }

    /// Returns the start of the emitter's target body.
    pub fn start(&self) -> usize {
        match self {
            &Emitter::Memory { ptr, .. } => ptr as usize,
            _ => 0
        }
    }

    /// Returns the length of the emitter.
    pub fn len(&self) -> usize {
        match self {
            &Emitter::File { size, .. } | &Emitter::Memory { size, .. } => size
        }
    }

    /// Returns whether the emitter is empty.
    pub fn is_empty(&self) -> bool {
        // mostly to make clippy happy
        self.len() == 0
    }

    /// Emits the specified data in an executable context.
    pub fn emit(&mut self, data: &Emit) -> EmitResult {
        data.emit(self)
    }
}

impl<'a, 'cx> Write for Emitter<'a, 'cx> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self.emit_bytes(buf) {
            Ok(_) => Ok(buf.len()),
            Err(_) => Err(io::Error::last_os_error())
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}


// //==========================================================================//
// // EMIT                                                                     //
// //==========================================================================//

/// An error encountered when calling `Emit.emit`.
#[derive(Debug)]
pub enum EmitError {
    /// OS-related IO error.
    IO(String),

    /// Index of movement out of range.
    IndexOutOfRange
}

impl From<io::Error> for EmitError {
    fn from(error: io::Error) -> EmitError {
        EmitError::IO(error.to_string())
    }
}

impl From<EmitError> for Diagnostic {
    fn from(error: EmitError) -> Diagnostic {
        use self::EmitError::*;

        match error {
            IO(_)               => Diagnostic::io(span!()),
            IndexOutOfRange     => unimplemented!()
        }
    }
}

/// The result of an emission through the `Emitter`.
pub type EmitResult = Result<Operand, EmitError>;


/// Defines an object that can be emitted by an `Emitter`.
pub trait Emit {
    /// Emits the content of the object into an `Emitter`.
    fn emit(&self, emitter: &mut Emitter) -> EmitResult;

    /// Returns a hint of the size of the object once emitted
    /// in order to pre-allocate memory.
    fn size(&self) -> Option<usize> { None }
}

macro_rules! make_emittable {
    ( $($n: expr),* ) => {
        $(
        impl Emit for [u8; $n] {
            fn emit(&self, emitter: &mut Emitter) -> EmitResult {
                emitter.emit_bytes(self)?;
                Ok(Operand::null())
            }

            fn size(&self) -> Option<usize> {
                Some($n)
            }
        }
        )*
    };

    ( $ty: ty, $s: ident, $e: ident => $body: expr => $size: expr ) => {
        impl Emit for $ty {
            fn emit(&$s, $e: &mut Emitter) -> EmitResult {
                $body;
                Ok(Operand::null())
            }
            fn size(&$s) -> Option<usize> {
                Some($size)
            }
        }
    };
}

make_emittable!(2, 4, 8, 16);
make_emittable!(u8, self, emitter => emitter.emit_byte(*self)?
                                  => 1);
