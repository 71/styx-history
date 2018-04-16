Language
========

- Types are first-class values, and can be manipulated and created just like any other value.
- Function signatures are fully-fledged expressions, which can apply operations on types and their parameters.
- Forward declarations are not required, and mutually recursive functions are **fully** supported, even across files.

### Visibility modifiers
Functions can have their visibility set to `public` (default), `internal` or `private` using the [`namespace` syntax](#namespace-syntax):
- `public` makes the declaration available everywhere in the current project, and importing projects. However, it does not make the declaration available to projects importing the importing projects.
- `internal` makes the declaration available in the current project only.
- `private` makes the declaration available in the current file only.

### Namespaces
Instead of modules, namespaces are used to group scopes. This means that a single
namespace can be defined in multiple files, and that the path of a file has no
consequence on the namespace. Additionally, this completely removes the need for
imports to be in a particular order, allowing mutually recursive declarations over
multiple files. However, this brings some minor problems:
- The file in which every namespace is defined is less obvious (although this isn't a problem if good conventions are used).
- Modularization of the source code isn't enforced.

# Custom syntax
One of Styx's main goals is to easily support complete EDSLs. For this, custom syntax can be used in various places by defining functions that follow some signatures.

### Expressions
```idris
ParseExpr : Syntax Expression
  with
    Syntax a = SyntaxMonad -> Result a
    Result a = Parsed a | Passed Span | Failed
```

### Patterns
```idris
ParsePattern : Pattern
  with
    Pattern = Syntax Patt
```

### Miscellaneous
```idris
ParseVisibility : Syntax Visibility
```