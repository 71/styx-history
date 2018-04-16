Type system
===========

Types in Styx are never really manipulated directly. Instead, they're only used in
function declarations.

It should also be noted that although the syntax is identical, no matter what kind
of type we're using, types have different kinds.

## Disambiguation
- **Types** are unique functions whose return value is `TypeSize`. They are
  represented as functions whose `IsType` property is set to `true`.
- **`TypeSize`s** are unsigned 32-bit integers that represent the size of the type
  (in bits) in memory.
- **Types Instances** are type applications where arguments are either statically
  encoded, or resolved from the environment of the expression. They encode
  the "type instance" of an expression, and its properties. They are individual.

```haskell
   Int32 : TypeSize
   Int32 = 32
-- ^^^^^             Signature of the type.
--         ^^        Value of the type.
-- ^^^^^^^^^^^^^^^^  Full definition of the type.

   type Option a = Some a | None
-- ^^^^                           Indicates a type definition.
--      ^^^^^^^^                  Signature of the type.
--                 ^^^^^^^^^^^^^  Body of the type definition.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  Full definition of the type.
```

```haskell
let a = $("Hello") in a.Type
-- TypeInstance (System.String`0, [], [])

let v = $(Vec.New "Hello") in v.Type
-- TypeInstance (System.Vec`2, [1, System.String], [("Length", Exact(1))])

let s = $(System.String) in s.Type
-- TypeInstance (System.TypeSize, [], [])

let f = $(@System.String) in f.Type
-- TypeInstance (System.Function, [], [])
```

## Primitives
Primitive types are simple types encoded as integers that represent their size (in
bits) in memory.

## Mixes
Types can also be mixed together using various operators.

## Function signatures
At the core, function signatures in Styx are compiled to functions with the following
signature:

```idris
Check : Self -> [TypeInstance] -> Option TypeInstance
```

This function simply checks whether the given types (which are the types of the
arguments of the application) match the signature of the function.  
If so, `Some type` is returned, with `type` the type of the resulting expression.  
Otherwise, `None` is returned.

Furthermore, every function signature has an associated coefficient that defines
how "hard" it is to match against it. The higher the coefficient, the higher the
specialization of a function.

For example, let us imagine the two following functions.

```idris
Foo : a -> String
Foo : Option a -> String
```

When calling `Foo` with `Some "bar"`, both signatures can accept the given argument.
However, the latter has a higher coefficient, and will thus be chosen.  
If, instead, `Foo` had been called with `42`, only the former definition would have
matched, and have been chosen.

### Generation of the `Check` function
The `Check` function is implemented using the built-in operators.

```idris
Add : a -> Vec n a -> Vec (n + 1) a
```

would be roughly translated to

```idris
Check : Self -> [TypeInstance] -> TypeInstance?
Check self types =
  if types.Length <> self.Parameters.Length then
    None
  else
    match MakeMap self types with
    | None -> None -- Invalid type parameters
    | Some map -> Vec ((map.Get "n" as Int) + 1) (map.Get "a" as Type)
```

Finally, here's what the call to `Check` would you like with some applications.

```idris
Add : a -> Vec n a -> Vec (n + 1) a

-- Right application
Add "hello" ["world"]

Check (Add) [ String, (Vec 1 String) ] -- > Some (Vec 2 String)

-- Wrong application
Add 42 ["world"]

Check (Add) [ Int, (Vec 1 String) ] -- > None
```

```idris
Head : (n > 0) => Vec n a -> a

-- Right application
Head ["world"]

Check (Head) [ (Vec 1 String) ] -- > Some (String)

-- Wrong application
Head []

Check (Head) [ (Vec 0 a) ] -- > None
```