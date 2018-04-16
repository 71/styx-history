Sequences
=========

Sequences are objects that return a stream of elements (finite or not).  
In Styx, they are represented by the `Sequence` type classes, whose definitions
are given below.

```haskell
class Sequence seq (n : UInt) a where
    Next : (n > 0) => seq n a -> a
    Size : seq n a -> UInt
    
    Split : (n > 0) => ByValue (seq n a) -> (a, seq (n - 1) a)
    Split seq = let next = seq.Next in (next, seq)
    
class Sequence seq a where
    Next : seq a -> Option a
    Size : seq a -> UInt
    
    Split : ByValue (seq a) -> (Option a, seq a)
    Split seq = let next = seq.Next in (next, seq)
    
instance (Sequence n a) => Sequence a where
    Next seq = maybe (n > 0) \ -> seq.Next
    Size seq = seq.Size
    
    Split seq = match splitted with 
                | Some (i, s) -> (Some i, s)
                | None -> (None, seq)
      where
        splitted = maybe (n > 0) \ -> seq.Split
```

Styx also accepts a special syntax for sequence types.

```haskell
[a]   -- Same as (Sequence a)
[a;n] -- Same as (Sequence n a)
```

Finally, pattern matching can be done on sequences.

```idris
Sum : (Zero a, a + a) => [a] -> a
Sum [] = 0
Sum (x::xs) = x + (Sum xs)
```

## Built-in sequences

### `Array n a`
Arrays are static, stack-allocated sequences with a fixed size.

### `Vec n a`
Vectors are static, heap-allocated sequences that can grow.

### `List n a`
Lists are chained, heap-allocated sequences that can grow.

### `Slice n a`
Slices are static, immutable views into the data of a vector or array.