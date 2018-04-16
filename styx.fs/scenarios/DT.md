# The `Append` example

We have the type `Vec (n : Int) (a : Type)` with the following operations.

```haskell
New :: a -> Vec 1 a
Empty :: () -> Vec 0 a

Add :: Vec n a -> a -> Vec (n + 1) a
```

For some reason, we want to define `Append`, a function that takes two vectors of same item type, the first one having more elements than the second one, none of them being empty, and returns a new vector with elements from both vectors concatenated.

```haskell
Append :: (n >= m, m > 0) =>
          Vec n a -> Vec m a -> Vec (n + m) a
```

What are the implications of calling `Append`?

## The `Vec` definition
`Vec n a` is a type definition of `Vec`, which has two parameters: an `Int` and a `Type`.

In pseudo-code, this may look like:
```haskell
Vec = { Name = "Vec"
      , Arguments = ["n", "a"]
      , Constraints = [ (n : Int)
                      , (a : Type)
                      ]
      }
```

## The `Append` definition
`Append` takes a few type arguments.

In a trivial form, its definition might look like this:

```haskell
Append = { Name = "Append"
         , Arguments = ["x", "y"]
         , TypeArguments = ["n", "m", "a"]
         , Constraints = [ (x : Vec n a)
                         , (y : Vec m a)
                         , (a : Type)
                         , (n >= m)
                         , (m > 0)
                         ]
         , ReturnType = (Vec (n + m) a)
         }
```

However, this doesn't work. A complete definition would be:

```haskell
Append = { Name = "Append"
         , Arguments = ["x", "y"]
         , TypeArguments = ["n", "m", "a", "b"]
         , Constraints = [ (x : Vec n a)
                         , (y : Vec m b)
                         , (a : Type)
                         , (b == a)
                         , (n >= m)
                         , (m > 0)
                         ]
         , ReturnType = (Vec (n + m) a)
         }
```

In pseudo code, this can also be represented as a function:

```haskell
Append x y =
  if x.Type != Vec then
    None
  else
    let n, a = x.Arguments[0], x.Arguments[1] in
    if y.Type != Vec then
      None
    else
      let m, b = y.Arguments[0], y.Arguments[1] in
      if !(a.Type == Type && b == a && n >= m && m > 0) then
        None
      else
        Some $ TypeInstance { Type = Vec`2
                            , Arguments = [ (n + m)
                                          , a
                                          ]
                            }
```

In a perfect world, however, precise errors would be return in case of invalid parameters. For example:

```haskell
Append :: List TypeInstance
       -> Verifier
       -> Maybe TypeInstance
Append types verifier =
  if not (types.Length == 2) then
    verifier.SignatureError "Invalid number of arguments."
    None
  else
    let (x, y) = types[0], types[1] in
    
    if not (x.Type == Vec) then
      verifier.TypeError "Invalid type at first position."
    if not (y.Type == Vec) then
      verifier.TypeError "Invalid type at second position."
      
    if verifier.HasError then
      None
    else
      let (n, a) = x.Arguments[0], x.Arguments[1] in
      let (m, b) = y.Arguments[0], y.Arguments[1] in
      
      if not (a.Type == Type) then
        verifier.ConstraintError "Unverified constraint (a is Type)."
      if not (b.Type == Type) then
        verifier.ConstraintError "Unverified constraint (b is Type)."
      if not (b == a) then
        verifier.ConstraintError "Unverified constraint (a == b)."
      if not (n >= m) then
        verifier.ConstraintError "Unverified constraint (n >= m)."
      if not (m > 0) then
        verifier.ConstraintError "Unverified constraint (m > 0)."
        
      if verifier.HasError then
        None
      else
        Some $ TypeInstance { Type = Vec`2
                            , Arguments = [ (n + m), a ]
                            }
```

Constraints are comma-separated expressions that'll be compiled directly as such. Parameters are "type expressions", and the return value is a "return type expression", that get treated differently.

### Signature expressions
#### Constraint expression
```haskell
(n > m, m > 0) => n > m && m > 0
```

#### Type expression
```haskell
(x : Vec n a)   => x.Type == Vec
(y : Vec n Int) => y.Type == Vec && y.Arguments[1] == Int
```

#### Return type expression
```haskell
-- 'n' and 'a' are in scope.
(r : Vec n a) => { Type = Vec
                 , Arguments = [ n, a ]
                 }

-- 'n', 'm' and 'a' are in scope.
(r : Vec (n + m) a) => { Type = Vec
                       , Arguments = [ (n + m), a ]
                       }

-- 'n' and 'a' are in scope.
(r : if n == 0 then None else Vec n a)
  => if n == 0 then { Type = Option
                    , Arguments = [ (Vec n a) ]
                    }
     else { Type = Vec
          , Arguments = [ n, a ]
          }
```

## A statically-proven example
```haskell
Main : Vec 3 Int
Main = let a = Vec.Push (Vec.New 1) 2 in 
       let b = Vec.New 3 in
       
       Vec.Append a b
```

Here,
- `a` has the type `Vec 2 Int`.
- `b` has the type `Vec 1 Int`.

When calling `Append`, the following checks are made in order:
- `a` is `Vec vn va` (here, `vn == 2` and `va == Int`)
- `b` is `Vec vm vb` (here, `vm == 1` and `vb == Int`)
- `va == vb` (`Int == Int`)
- `vn >= vm` (`2 >= 1`)
- `vm > 0` (`1 > 0`)

All checks prove to be true statically.

### In-depth
`a` and `b` are type instances whose values read:

```haskell
a = TypeInstance { Type = Vec`2
                 , Arguments = [ (Exact 2)
                               , (Exact Int)
                               ]
                 }

b = TypeInstance { Type = Vec`2
                 , Arguments = [ (Exact 1)
                               , (Exact Int)
                               ]
                 }
```

## A dynamically-proven example
```haskell
Main : Vec n a -> Vec m a -> Vec (0 | n + m) a
Main x y = if m == 0 then
             []
           else if n <= m then
             []
           else
             Vec.Append x y
```

Here,
- `x` has the type `Vec n a`
- `y` has the type `Vec m a`

When calling `Append`, the following checks are made in order:
- `x` is `Vec vn vx` (here, `vn == n` and `vx == a`)
- `y` is `Vec vm vy` (here, `vm == m` and `vy == a`)
- `va == vb` (`a == a`)
- `vn >= vm` (`n >= m`)
- `vm > 0` (`m > 0`)

The first three checks can be statically proven, but the two remaining ones cannot (at least explicitly). The body of the method indicates that `m != 0 && n > m`, but this isn't explicit. Flow analysis must be conducted to arrive to this conclusion.

### In-depth
`x` and `y` are type instances whose values read:

```haskell
a = PropertyValue Any

x = TypeInstance { Type = Vec`2
                 , Arguments = [ Any, a ]
                 }

y = TypeInstance { Type = Vec`2
                 , Arguments = [ Any, a ]
                 }
```