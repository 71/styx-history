Styx
====

Styx is a low-level programming language built in [Rust](https://www.rust-lang.org/), with a strong focus on meta-programming, expressions, and a powerful type system.

> **Disclaimer:** The following features are *planned*, but most of them aren't implemented / fully working yet.

### Expressions
The core principle in Styx is that everything is an expression, and that most expressions are executed as soon as they're parsed by the compiler.  
As such, the program being compiled has a direct access to the compiler, and can thus enhance and modify the compilation pipeline as it sees fit.

Furthermore, all Styx expressions are one of two kinds:
- A `Native` expression, which gets compiled to machine code. It has access to the `Builder`, and can produce IR. The IR is very similar to assembly, with a few additions such as variables and blocks.
- An `Extension` expression, which gets expanded to another expression and can be manipulated.

### Meta-programming
Since most expressions are executed are compile-time, and custom parsers can be provided, meta-programming is extreme. In fact, most of the expressions, including the function declaration syntax, are implemented directly in the Styx standard library.

One of the goals made possible thanks to those meta-programming features, is the ability to provide mini-DSLs into Styx, and for specific purposes.

### Type system
Every expression in Styx satisfies one or more axioms, and every generic parameter can have constraints on those axioms.

### Non-linear parsing
Files are parsed in parallel, and different parts of a file are parsed in any order if that is possible. In practice, this means that forward declarations are not required, and that, in some cases, syntax using a particular parser can be parsed before the parser itself.

### JIT
Since Styx uses no outside library for emitting assembly, it can also generate code during compilation, as long as the compiler is referenced.

# Getting started
> A nightly version of Rust is required.

Get the code,

    git clone https://github.com/6A/styx.git
    cd styx
build Styx, and run it.

    cargo run
    
### Usage
```
USAGE:
    styx [FLAGS] [OPTIONS] <SUBCOMMAND>

FLAGS:
        --help             Prints help information
    -h, --hide-warnings    Hide warnings
    -V, --version          Prints version information
    -v, --verbose          Increase verbosity

OPTIONS:
    -A, --arch <architecture>    Target architecture
    -O, --opt <optimization>     Optimization level (none, speed or size)

SUBCOMMANDS:
    execute    Executes the specified input
    help       Prints this message or the help of the given subcommand(s)
    repl       Starts the Styx REPL
    run        Runs Styx files
```

# Compatibility
Assembly can only be emitted to x86 (and x86-64) right now. ARM support is planned.  
The compiler should work on both Unix and Windows.