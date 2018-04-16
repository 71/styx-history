//! This module declares the low-level implentation of an instruction.
//!
//! Unlike `builder::Instr` which is supposed to accomodata all architectures,
//! this module emits platform-specific code.

#![allow(non_upper_case_globals)]

use std::fmt::{self, Display, Formatter};
use std::io::Write;


/// The size of a value.
pub type Size = u16;

/// The description (name, size) of a parameter.
pub type Parameter = (String, Size);


// //==========================================================================//
// // OPERAND                                                                  //
// //==========================================================================//


/// An `Instruction` operand.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operand {
    /// Empty operand.
    None,

    /// Register operand.
    Register(Size),

    /// Stack-allocated operand.
    Stack(usize, Size),

    /// Offset relative to the start of the instruction.
    RelativeOffset(isize, Size),

    /// Offset relative to the start of the procedure.
    AbsoluteOffset(isize, Size)
}


