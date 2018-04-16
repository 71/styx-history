//! The Styx Compiler.
//!
//! A CLI can also be found as a binary.

// unstable syntax / features
#![feature(box_syntax, box_patterns, const_fn, stmt_expr_attributes, plugin, specialization)]

// rust compiler apis
#![feature(libc, rustc_private, allocator_api, alloc)]

// built-in (but unstable) apis
#![feature(io, unicode, drain_filter, i128_type)]

// lints config
#![warn(missing_docs)]

#![cfg_attr(not(feature = "cargo-clippy"), allow(unknown_lints))]
#![cfg_attr(feature = "cargo-clippy", allow(match_ref_pats, cast_lossless))]
#![cfg_attr(feature = "cargo-clippy", warn(filter_map, int_plus_one, if_not_else,
                                           mut_mut, mutex_integer, nonminimal_bool,
                                           option_map_unwrap_or, option_map_unwrap_or_else,
                                           print_stdout, range_plus_one, result_map_unwrap_or_else,
                                           used_underscore_binding, wrong_pub_self_convention))]
#![cfg_attr(feature = "cargo-clippy", deny(mem_forget))]

#![cfg_attr(release, deny(option_unwrap_used, result_unwrap_used, warnings))]

#![cfg_attr(test, plugin(stainless))]
#![cfg_attr(test, allow(unused_functions, dead_code))]


// imported crates
extern crate yansi;
extern crate libc;
extern crate fnv;
extern crate futures;
extern crate futures_cpupool;
extern crate string_interner;
extern crate rand;
extern crate byteorder;
extern crate hyper;
extern crate uuid;
extern crate qstring;

#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;

// imported crates from compiler
extern crate arena;
extern crate alloc;
extern crate glob;
extern crate env_logger;

#[cfg(windows)]
extern crate kernel32;


// modules
pub(crate) mod macros;
pub(crate) mod utils;

pub mod analysis;
pub mod arch;
pub mod assembler;
pub mod binder;
pub mod builder;
pub mod context;
pub mod diagnostics;
pub mod emit;
pub mod expr;
pub mod input;
pub mod intrinsics;
pub mod lexer;
pub mod opt;
pub mod parser;
pub mod prelude;
pub mod procedures;
pub mod server;
pub mod symbols;
pub mod typesystem;
pub mod visitor;
pub mod vm;

mod core;

