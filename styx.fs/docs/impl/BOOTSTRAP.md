Bootstrapping
=============

Bootstrapping is done by a compiler written in F#. Although it reads the whole
unmodified compiler, it actually doesn't work the exact same way.

Custom syntax, operators, and advanced type system features are not available, although
everything needed by Styx itself is emulated one way or another.

## Parsing
Parsing is done using combinators via the [FParsec](https://github.com/stephan-tolksdorf/fparsec) library.

The syntax is defined in the [Styx/Text/Syntax](../../src/Styx/Text/Syntax) directory.

## Tests
Tests are done with a mix of [FsUnit](https://github.com/fsprojects/FsUnit) and
[NUnit](https://github.com/nunit/nunit).

They are available in the [Styx.Tests/Tests](../../src/Styx.Tests/Tests) directory.
