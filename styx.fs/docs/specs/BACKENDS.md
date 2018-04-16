Backends
========

Backends provide a way for Styx to work on many platforms. At the core, a backend simply specifies static properties that can change how the compilation works.

A backend should define the following functions.

```idris
concept (Backend b) where
  --| Returns the identifier (or name) of the backend.
  Name : b -> String
  
  --| Returns the version of the backend, as a (major, minor, patch) triple.
  Version : b -> (Int, Int, Int)
  
  --| Computes an expression, and returns its result.
  Compute : b -> Expression -> Value
```

Furthermore, backends **should** provide ways to emit Styx code to files.

# `x86`
This is the default backend, which serves as a reference to other backends that may be created in the future.