## Vector
Let us consider the `Vec n a` type.

```haskell
let vec =
  if Random > .5 then
    Vec.From ["foo", "bar"]
  else
    Vec.Empty
-- 'vec' should have the type 'Vec (0 | 2) String'

let _ = vec.Head
-- this shouldn't be possible

if vec.IsEmpty then
  exit 0
else
  vec.Head
-- this should be possible, with the result being 'Vec 2 String'

if vec.Length < 1 then
  vec.Push "baz"
else
  vec
-- this should also be possible, with the result being 'Vec (1 | 2) String'

assert vec.Length > 0
-- this call should be removed from compile time
```

## Option
Let us consider the `Option a` type.

```haskell
let o = if Random > .5 then Some 'a' else None
-- 'o' should have the type 'Option Char'

let _ = o.Value
-- this shouldn't be possible

if o.IsSome then
  Console.WriteLine o.Value
  -- this should be possible, with 'o' having the type 'Some 'a''
  
if o.IsNone then exit 0

match o with
| Some s -> True
| None -> False
-- a warning should be emitted here;
-- additionally, this should be replaced by 'True' directly

o ?? 'b'
-- should have the type 'Some ('a' | 'b')'
```

## Int
Let us consider the `Int` type.

```haskell
let a = (Random * 100) as Int
-- 'a' should have the type 'Int'

let _ = 100 / a
-- this shouldn't be possible, because 'a' might be equal to 0

let a = if a == 0 then 1 else a
-- 'a' should now have the type 'Int (!= 0)'

let _ = 100 / a
-- this should be possible
```

## Conditions
Let us consider the `Boolean` and `Int` types.

```haskell
Even n = (n & 1) == 0
Odd  n = (n & 1) == 1

Even 10 -- should have the return type 'True'
Odd 10  -- should have the return type 'False'
Even (Random as Int) -- should have the return type 'Boolean'

Magic : n -> m -> (if Even (n + m) then 50 else 100)
Magic a b = ...

Magic 10 1 -- should have the return type '100'
Magic 10 2 -- should have the return type '50'
Magic (Random as Int) 1 -- should have the return type '50 | 100'

Ex : Int -> Int
Ex 0 = 0
Ex n = if Even n then (Ex (n - 2)) else (Ex n)

Ex 0 -- should have type '0'
Ex 2 -- should have type '0'
Ex 1 -- should have type 'Never'
```