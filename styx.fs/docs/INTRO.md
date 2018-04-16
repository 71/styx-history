Styx: an introduction
=====================

> This document contains an introduction to Styx: its philosophy,
> its core features, and some details on its implementation.  
> Prior knowledge on programming is expected; knowledge on functional
> and statically typed programming is recommended.

## Preamble

Here is "Hello world" in Styx.
```haskell
Hello : String
Hello = "Hello, world."
```

Now, here is "Square" in Styx.
```haskell
Square : (* : a -> a) => a -> a
Square x = x * x
```

Finally, here is "Inverse" in Styx.
```haskell
Inverse : (n != 0, / : n -> n) => n -> (1 / n)
Inverse x = 1 / x
```

Alright, so what's going on here?

First, we can see the syntax is very close to Haskell's, with a few minor differences.  
Second, we noticed the weird signature of the function `Inverse`. What does it say?

In human language, it says "for any value n different from 0, which supports the
operation '/', return (1 / n)."

The above snippet shows the core feature of Styx: functions aren't parametrized by
the type of their values, but by what is known about them. `Square` and `Inverse`
do not care about the type of their argument; it might be an `I16`, a `I32`, or even
a `Float`. What matters is that it supports the operation you'll be needing for your
function.

Now, why is this useful? Consider the following example, assuming that `Random` is a
function that returns a random integer between 0 and 10 (included).
```haskell
let a = Random, b = Random in a / b
```

Now, what if `b` is 0?  
In a regular programming language, this would result in a runtime exception. If we
really wanted to avoid exceptions, then `/` could return an `Option` instead.  
Both solutions aren't optimal; so what does Styx do?

It returns a compile-time error. `b` *might* be equal to 0, and is thus invalid. So what
can be done to make this work? Here are a few solutions.

```haskell
-- The quick & easy solution that completely changes the
-- behaviour of the function.
let a = Random, b = Random + 1 in a / b

-- The better behavior of the function that retries until a valid
-- integer is found.
let a = Random, b = notZero in a / b
  where
    notZero = let r = Random in
              if r != 0 then r else notZero
```

## Types are dead, long live the types!

From what we've seen so far, types in Styx aren't quite *ordinary*, so what are they?  

TODO

All this adds a requirement to Styx, though, and all for the better.

## Static computations

TODO