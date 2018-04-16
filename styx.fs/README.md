Styx
====

The following documents describe the Styx language and compilers.  
Styx is being developed in three different parts.

## 1. [Abstract implementation](./lib)
The compiler shall be implemented first in Styx, even though it cannot be compiled. Its syntax should remain simple, and its usage of external tools minimal.  
This allows a set of required features and capabilities to be determined before anything else. It also creates a set of APIs that must be supported by the Styx VM.

Furthermore, the code shall be self-describing and, in some cases, precisely documented.

## 2. [Design of specifications](./docs/specs/README.md)
The features, capabilities, syntactic elements and available APIs being known, the precise set of specifications for the core compiler can now be created.  
It should feature every part of the compiler, its inner workings, its design choices, etc.

While designing the language, some minor modifications can be added to the abstract implementation.

## 3. [Concrete implementation](./docs/impl/README.md)
The compiler shall finally be implemented in an existing programming language. The compiler shall strictly conform to the aforementioned specifications, and shall not implement any other features.

It shall try to remain minimal.  
Instead of being compiled, it shall be interpreted.
