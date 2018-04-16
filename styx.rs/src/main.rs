//! This module defines the entry point of the Styx CLI.
//!
//! A REPL is provided, along with a few commands.

#![feature(box_syntax)]
#![cfg_attr(not(feature = "cargo-clippy"), allow(unknown_lints))]

#[macro_use] extern crate styx;
#[macro_use] extern crate clap;
#[macro_use] extern crate log;

extern crate futures;
extern crate futures_cpupool;
extern crate yansi;
extern crate glob;
extern crate env_logger;
extern crate hyper;

use styx::arch::Architecture;
use styx::assembler::{AssemblyGraph, AssemblyStyle};
use styx::binder::{Binder, BinderState, Bound};
use styx::context::Context;
use styx::diagnostics::*;
use styx::expr::{BuiltIns, Expr, Unit};
use styx::input::{Input, InputEntity, InputFile};
use styx::lexer::*;
use styx::opt::{Optimization, OptimizationOptions};
use styx::typesystem::{Fun, Typed};
use styx::vm::{Vm, VirtualMachine};

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::io::{stdin, stdout, stderr, Write};
use std::mem;
use std::sync::RwLock;

use futures::prelude::*;
use futures_cpupool::Builder as PoolBuilder;
use yansi::Color;


// //==========================================================================//
// // MISC UTILS                                                               //
// //==========================================================================//

/// Flushes stdout.
macro_rules! flush {
    () => ( flush!(stdout()) );
    ( $io: expr ) => ( $io.flush().expect("Could not flush to console.") );
}

/// Displays lexed input on stdout.
macro_rules! display_lexed {
    ( $file: expr, $diags: expr, $err: expr ) => {{
        let tokens = Lexer::new($file.source()).tokenize(&mut $diags);

        match tokens {
            Ok(tokens) => {
                // Displays tokens on success
                println!("{} Lexed: {:?}", Color::Blue.paint("->"), tokens);
                println!();

                DiagnosticPrinter::new(&mut stderr(), &$file)
                                    .print_bag(&$diags);

                if tokens.is_empty() {
                    $err
                }
            },

            Err(err) => {
                $diags.report(err);

                DiagnosticPrinter::new(&mut stderr(), &$file)
                                    .print_bag(&$diags);

                $err
            }
        }
    }};
}

/// Displays parsed input on stdout.
macro_rules! display_parsed {
    ( $expr: expr ) => {{
        println!("{} Parsed: {:?}", Color::Blue.paint("->"), $expr);
        println!();
    }};
}

/// Displays built IR on stdout.
macro_rules! display_built {
    ( $expr: expr ) => {{
        println!("{} IR:", Color::Blue.paint("->"));
        println!("{}", $expr);
        println!();
    }};
}

/// Displays produced machine code on stdout.
macro_rules! display_assembly {
    ( $expr: expr ) => {{
        for procd in $expr {
            let fun = procd.function();

            if let Some(procd) = procd.ds_proc() {
                let mut buffer = String::new();
                procd.write_buffer(&mut buffer);

                println!("{} Assembly of {}: {}", Color::Blue.paint("->"), fun, buffer);
                println!();
            }
        }
    }};
}

/// Throws an unrecoverable error encountered in the REPL.
macro_rules! fatal_error {
    ( $err: expr ) => {{
        eprintln!("{}: {}", Color::Red.paint("Error").bold(), Color::Red.paint($err));
        std::process::exit(1)
    }}
}


// //==========================================================================//
// // MAIN FUNCTION & SUBCOMMANDS                                              //
// //==========================================================================//

use std::io::Read;
use std::fs::File;

use clap::ArgMatches;
use glob::glob;

#[inline]
fn get_hash<H: Hash>(h: &H) -> u64 {
    let mut hasher = DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}

/// Entry point of the CLI.
pub fn main() {
    // Initialize logger if needed.
    if let Err(error) = env_logger::init() {
        eprintln!("Couldn't initialize logger: {}.", error);
    }

    debug!("Successfully initialized logger.");

    // Parse args.
    let mut app = clap_app!(
        styx => (@setting DontCollapseArgsInUsage)
                (@setting GlobalVersion)
                (@setting InferSubcommands)
                (@setting SubcommandRequiredElseHelp)

                (version: crate_version!())
                (author: crate_authors!("\n"))
                (about: crate_description!())

                (@arg architecture: -A --arch +takes_value "Target architecture")
                (@arg optimization: -O --opt +takes_value "Optimization level (none, speed or size)")
                (@arg verbose: -v --verbose "Increase verbosity")
                (@arg hide: -hw --("hide-warnings") "Hide warnings")

                (@subcommand run =>
                    (about: "Runs Styx files")
                    (@arg inputs: +required ... "Input globs"))

                (@subcommand deamon =>
                    (about: "Starts the Styx deamon")
                    (@arg address: +required "IP Address on which to listen"))

                (@subcommand execute =>
                    (about: "Executes the specified input")
                    (@arg lexed: --("display-lexed") "Display lexed input")
                    (@arg parsed: --("display-parsed") "Display parsed input")
                    (@arg built: --("display-ir") "Display produced IR")
                    (@arg assembly: --("display-assembly") "Display produced assembly")
                    (@arg inputs: ... "Input strings"))

                (@subcommand repl =>
                    (about: "Starts the Styx REPL")
                    (@arg lexed: --("display-lexed") "Display lexed input")
                    (@arg parsed: --("display-parsed") "Display parsed input")
                    (@arg built: --("display-ir") "Display produced IR")
                    (@arg assembly: --("display-assembly") "Display produced assembly")));

    if cfg!(server) {
        use clap::SubCommand;

        app = app.subcommand(SubCommand::with_name("deamon")
                                        .about("Starts the Styx deamon"));
    }

    let app = app.get_matches();

    let arch = match app.value_of("architecture") {
        Some(arch) => match arch {
            "x86" => Architecture::X86,
            "x64" | "x86-64" | "x86_64" => Architecture::X86_64,
            "default" => Architecture::default(),
            _ => fatal_error!("Unknown architecture.")
        },

        None => Architecture::current().unwrap_or_default()
    };

    let opti = match app.value_of("optimization") {
        Some(optimization) => match optimization {
            "none"  => Optimization::None,
            "speed" => Optimization::Speed(OptimizationOptions::default()),
            "size"  => Optimization::Size(OptimizationOptions::default()),
            _ => fatal_error!("Unknown optimization level.")
        },

        None => Optimization::None
    };

    let is_verbose = app.is_present("verbose");

    // Create virtual machine.
    let vm = match VirtualMachine::new(arch, opti) {
        Ok(vm) => vm,
        Err(err) => fatal_error!(err)
    };
    let vm = unsafe {
        Vm::new(&*(&vm as *const _ as *const () as *const _))
    };

    match app.subcommand() {
        ("run",     Some(args)) => run(vm, is_verbose, args),
        ("execute", Some(args)) => execute(vm, is_verbose, args),
        ("repl",    Some(args)) => repl(vm, is_verbose, args),
        ("deamon",  Some(args)) => deamon(vm, is_verbose, args),

        _ => panic!()
    }
}

#[allow(print_stdout)]
fn deamon<'a, 'vm: 'a>(vm: Vm<'vm>, _is_verbose: bool, args: &'a ArgMatches) {
    use styx::server::*;
    use hyper::server::Http;

    let addr = args.value_of("address").unwrap(); // we know it is there thanks to clap
    let addr = match addr.parse() {
        Ok(addr) => addr,
        Err(_) => fatal_error!("Invalid listening address.")
    };

    let deamon: Deamon<'static> = unsafe { mem::transmute(Deamon::new(vm)) };

    // The server requires a static deamon, which isn't possible. However the server doesn't live
    // longer that the deamon, so we extend its lifetime illegally.
    let server = Http::new().bind(&addr, deamon).unwrap();

    server.run().unwrap();
}

#[allow(print_stdout, option_unwrap_used, filter_map)]
fn run<'a, 'vm: 'a>(vm: Vm<'vm>, is_verbose: bool, args: &'a ArgMatches) {
    let diagnostics = RwLock::new(DiagnosticBag::default());

    let mut entries = args.values_of("inputs").unwrap()
        .flat_map(|input| glob(input).ok())
        .flat_map(|input| input)
        .filter_map(Result::ok)
        .collect::<Vec<_>>();

    entries.dedup();

    let entries = entries.iter()
        .filter_map(|input| match File::open(input.clone()) {
            Ok(file) => {
                Some(InputEntity::File {
                    id: get_hash(input),
                    filename: input.to_string_lossy().to_string(),
                    file: file
                })
            },
            Err(_) => {
                diagnostics.write()
                           .expect("Failed to get lock on diagnostics.")
                           .report(Diagnostic::cant_open_file(span!(), input.display()));
                None
            }
        });

    let mut builder = PoolBuilder::new();

    if is_verbose {
        builder.name_prefix("styx")
               .after_start(|| println!("Started a worker thread."))
               .before_stop(|| println!("Stopped a worker thread."));
    }

    let _pool = builder.create();

    for bound in vm.bind(Input::new(entries)).wait() {
        if is_verbose {
            println!("Bound a file: {:?}.", bound);
        }

        match bound {
            Ok(mut bound) => {
                bound.generate_missing_symbols_diagnostics();
                bound.print_diagnostics(&mut stderr(), true, true, true)
            },
            Err(err) => eprintln!("Could not get bound output: {}.", err)
        }
    }
}

#[allow(print_stdout, option_unwrap_used)]
fn execute<'a, 'vm: 'a>(vm: Vm<'vm>, _is_verbose: bool, args: &'a ArgMatches) {
    let mut ctx = Context::new(vm);

    let lexed = args.is_present("lexed");
    let parsed = args.is_present("parsed");
    let built = args.is_present("built");
    let assembly = args.is_present("assembly");

    // read input strings / stdin
    let mut input = String::new();

    match args.values_of("inputs") {
        Some(inputs) => for i in inputs {
            input.push_str(i);
            input.push('\n');
        },

        None => if stdin().read_to_string(&mut input).is_err() {
            fatal_error!("Could not read string from stdin")
        }
    }

    if input.chars().all(char::is_whitespace) {
        return
    }

    let file = InputFile::new("<execute>".to_string(), input, 0);
    let mut diagnostics = DiagnosticBag::default();

    // execute it
    if lexed {
        display_lexed!(file, diagnostics, return);
    }

    let mut binder = match Binder::new(vm, file, &mut diagnostics) {
        Ok(binder) => binder,
        Err(file) => {
            DiagnosticPrinter::new(&mut stderr(), &file).print_bag(&diagnostics);
            return
        }
    };

    let generate = match binder.process(true, &mut diagnostics) {
        BinderState::Done => false,
        BinderState::Stalled | BinderState::Pending => true
    };

    let mut bound = Bound::new(&mut binder, diagnostics);

    if generate {
        bound.generate_missing_symbols_diagnostics();
    }

    bound.print_diagnostics(&mut stderr(), true, true, true);

    if !bound.succeeded {
        return
    }

    if parsed {
        display_parsed!(bound);
    }

    // Build graph
    {
        let arch = Architecture::current().unwrap();
        let expr = mem::replace(&mut bound.expression, Expr::native(Unit::new(), span!(), BuiltIns::unit()));
        let fun = Fun::anonymous(expr);

        let mut diagnostics = mem::replace(&mut bound.diagnostics, DiagnosticBag::default());
        let mut graph = AssemblyGraph::
             new(arch, &mut diagnostics, &mut ctx, unsafe { mem::transmute(&fun) })
            .expect("Could not create graph.");

        graph.process(AssemblyStyle::DS, &mut diagnostics, vm);

        DiagnosticPrinter::new(&mut stderr(), &bound.file).print_bag(&diagnostics);

        if diagnostics.has_error() {
            return
        }

        if built {
            display_built!(graph.builder());
        }
    }

    // Find compiled main expr
    let proced = ctx.procedures().iter().find(|p| p.function().is_anonymous()).expect("Could not find freshly compiled expression.");
    let ty = proced.function().ty();
    let proced = proced.ds_proc().expect("Could not find freshly compiled expression.");
    let addr = proced.ptr as *const ();

    if assembly {
        display_assembly!(ctx.procedures());
    }

    vm.call(addr, ty, &mut stdout());
    println!();
}

#[allow(print_stdout, option_unwrap_used)]
fn repl<'a, 'vm: 'a>(vm: Vm<'vm>, _is_verbose: bool, args: &'a ArgMatches) {
    let ctx = Context::new(vm);
    let mut binder = Binder::empty(vm);

    let arch = Architecture::current().unwrap();

    let lexed = args.is_present("lexed");
    let parsed = args.is_present("parsed");
    let built = args.is_present("built");
    let assembly = args.is_present("assembly");

    loop {
        // read input
        print!("{} ", Color::Green.paint("~>"));
        flush!();

        let mut input = String::new();

        stdin().read_line(&mut input).expect("Could not read line from stdin.");

        if input.starts_with("exit") || input.starts_with("quit") {
            break
        }
        if input.chars().all(char::is_whitespace) {
            continue
        }

        let file = InputFile::new("<repl>".to_string(), input, 0);
        let mut diagnostics = DiagnosticBag::default();

        // Display lexed content if needed
        if lexed {
            display_lexed!(file, diagnostics, continue);
        }

        // Update binder
        Binder::resume(&mut binder, file, &mut diagnostics).expect("Could not resume binder.");

        // Bind input
        let (expr, file, mut diagnostics) = {
            let success = match binder.process(true, &mut diagnostics) {
                BinderState::Done => true,
                BinderState::Stalled | BinderState::Pending => false
            };

            let mut bound = Bound::new(&mut binder, diagnostics);

            if parsed {
                display_parsed!(bound);
            }

            if !success {
                bound.generate_missing_symbols_diagnostics();
            }

            bound.print_diagnostics(&mut stderr(), true, true, true);

            if !success || bound.diagnostics.has_error() {
                continue
            }

            (bound.expression, bound.file, bound.diagnostics)
        };

        // Compile input in new context
        let mut context = Context::child(&ctx);
        let fun = Fun::anonymous(expr);

        {
            // Function doesn't live long enough for context, but we don't really care, we won't
            // use it anymore
            let mut graph = AssemblyGraph::
                 new(arch, &mut diagnostics, &mut context, unsafe { mem::transmute(&fun) })
                .expect("Could not create graph.");

            graph.process(AssemblyStyle::DS, &mut diagnostics, vm);

            DiagnosticPrinter::new(&mut stderr(), &file)
                              .print_bag(&diagnostics);

            if diagnostics.has_error() {
                continue
            }

            if built {
                display_built!(graph.builder());
            }
        }

        // Compute and display output
        let proced = context.procedures().iter().find(|p| p.function().is_anonymous()).unwrap();
        let ty = proced.function().ty();
        let proced = proced.ds_proc().unwrap();
        let addr = proced.ptr as *const () as *mut ();

        if assembly {
            display_assembly!(context.procedures())
        }

        print!("{} ", Color::Blue.paint("=>"));
        vm.call(addr, ty, &mut stdout());
        println!();
    }
}

