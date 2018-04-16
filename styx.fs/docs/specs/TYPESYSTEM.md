Type system
===========

The type system attempts to be minimal, whilst being extremely powerful. The following sections document how this is achieved, and how this makes Styx *very* different from most programming languages.

> **Note**:
> Before getting into types and how they're represented, a fact shall be made clear:
> Integers are at the core of the Styx language (and programming in general).
> With integers alone, one can represent numerical values, but also strings, vectors,
> characters, and just about any property.  
> Thus, most information about a program can be summarized by a bunch
> of integers and operations thereon.

## Types
Before diving into the type system, maybe we'd like to know what a type is *exactly*.

```idris
Type : ?CompilerType
Type = Distinct UInt32
```

Yes, a type is simply an integer; more precisely, it is a 32-bit unsigned integer that encodes
the length of the type in bits when stored in memory.

This means that types can be added to make bigger structures in memory.  
Furthermore, the fact that their size in *bits* is encoded means that adding multiple types
whose size is not a multiple of eight does not make the resulting structures bigger than needed.

As such, both the following types will be encoded with a single byte:
- `Boolean = 1`.
- `FourBooleans = Boolean + Boolean + Boolean + Boolean`.

Finally, types can also be represented as functions, and thus encode information about its
content statically, but also change its size as desired.

## Functions and components
Now, we know that types are simply integers, and that they're declared using functions.  
But what *is* a function? Well, it depends on the backend. The way you interact with it,
though, is always the same: you give it arguments, and except a result.  
In Styx, these arguments are typed, and functions define what kind of arguments they want
using **components**.

Components are simple expressions that define what kind of value a function accepts.  
Since they're expressions, they can change what is known about a type statically.

For example, let's take the `Vec a n` type. Its first type parameter, `a`, simply encodes the
type of the items stored in the vector. Its second argument, `n`, encodes the size of the
vector. The properties of the components thus allow the following function to be defined:

```idris
Concat : Vec a n -> Vec a m -> Vec a (n + m)
```

Now, why is this useful? Well, consider trying to get the 3rd element of the vector. Using
the previous notation, you can statically make sure that this argument exists, and avoid
the use of both `Option` and runtime exceptions.

What if it's not sure, though? Assertions to the rescue!

## Axioms and assertions
Previously, we said that most information about a value could be represented using
integers and operations thereon. Using this fact, the idea **axioms** in Styx was born.

Axioms are operations on types that return booleans. For example, checking whether
a type `T` has a method `Show` with the signature `a -> String`, or whether multiple
sub-axioms are all satisfied.  
Obviously, axioms can also act on integers and ranges of integers. Thus, type arguments
can satisfy axioms as well, which allows us to write the following function:

```idris
Nth : Vec a n -> (0 .. n) -> a
```

Here, `n` is either a value (such as `0` or `48`), a range (such as `1..50`, or even `0..∞`),
or an operation (such as `not (equal to 42)`).  
The information encoded in the type of the vector can be modified at runtime using
assertions.

#### `assume`: the unsafe way
`assume` takes an axiom as argument, and assumes it is true. The assertion is **not
checked**, which can result on crashes or worse: undefined behavior.

However, this function is required to implement all low-level axioms without a runtime
penalty, which is why it exists.

#### `assert`: the easy way
`assert` also takes an axiom as argument, and checks if it is true. If it isn't, an exception
is thrown.  
This is useful for debugging, but shouldn't be used in a production environment.

#### `satisfies`: the nice way
`satisfies`, like the previous functions, takes an axiom as argument. It also returns a
boolean that represents whether or not the given axiom was true.

#### `maybe`: the functional way
`maybe` takes an axiom and a function as arguments, and returns the result of the
function applied in a context where the axiom is true.  
However, if the axiom actually isn't true, `maybe` simply returns `None`.

## Properties
Before going further, it would be nice to know the relationship between statically
known properties, and runtime-checked properties.

First, you might have noticed that in both cases, they're called "properties." Hence,
let us refer to them as such.

```idris
Length : Vec a n -> Property n
```

## Concepts
Finally, concepts can be used where using types might be limiting.  
Concepts are simply named axioms that can be used in place of types in function
components, whilst statically ensuring that certain values will match certain
conditions.

For example, here is a simplified look at the `Monoid` concept:
```idris
concept Monoid m where
  Empty : m
  Append : m -> m -> m
```

## Type unions
Finally, a very useful feature in Styx is the ability to "merge" types into type unions. This will
result in a single type that implements operations from all merged types.  
This is extremely useful in Styx, where monads can be merged.

## Type classes
Type classes are concepts that must be explicitly implemented.