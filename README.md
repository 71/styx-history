Styx History
============

The development of the Styx programming language began in 2016, and went through dozens of iterations.  
What was originally supposed to be a C-like programming language with powerful
meta-programming features, created in C# and targeting LLVM and .NET eventually
grew to become many things, targeting many different platforms, and being bootstrapped in many different languages.

This repository contains the (abandoned) code of some of these iterations that I decided to make public as I realized that the time I personally spent on this project
approaches an approximate 2,000 hours spanned over two years, sometimes spending 10 to 18 hours a day every day for several days in a row, working solely on it.

Styx development is ongoing, but its latest iterations are currently closed-source.

## [styx.rs](./styx.rs)
This iteration of Styx was implemented in Rust, and features a working parser, type system, non-linear binder, and x86 compiler.  
It can be interacted with using a CLI, a REPL, and a Jupyter Kernel.

The language itself is supposed to be C-like and meta-programmed, with custom expressions that get translated to a custom IR.  
However, it was given up on when I realized that the type system was not powerful enough for its intended purpose.
Therefore, only a few expressions were implemented (calls, conditionals, and binary).

## [styx.fs](./styx.fs)
This iteration was, this time around, implemented in F#. It also features a parser, but can only be interacted with through a REPL. The type system is much more powerful, with dependent types (for example, the return type of a function can be computed using the type of its parameters) and holes, which allow any input to be bound in *any* order. Another change is that x86 is no longer targeted; instead, CIL code is generated. 

Once again, it was abandoned after realizing that CIL was not the way to go.
