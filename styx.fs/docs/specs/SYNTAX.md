Syntax
======

### Function signature
Declares the signature of a function that is to be defined.
```idris
FunctionName : ParameterType -> ReturnType
ConstantName : ConstantType
```

### Function declaration
Allows a body of a function to be defined. Many declarations can be provided for a single function, as long as the patterns are different.
```idris
FunctionName parameterName        = body
FunctionName (parameter, pattern) = body
ConstantName = value
```

### Qualified access
Provides access to functions using their qualified name, or using the prefix as the first argument (for UFCS).
```idris
-- Qualified call
System.Console.WriteLine "Hello world"

-- UFCS (Uniform Function Call Syntax)
(10 + 10).Display
-- .. is equivalent to ..
Display (10 + 10)
```

### `namespace` syntax
Indicates the namespace in which all the upcoming functions will be defined.
```idris
namespace Styx
    ...
    
private namespace Styx
    ...
    
override namespace System
    ...
```
The optional visibility modifier sets the default visibility in the body of the expression. The default is `public`, and can be further modified in the body of the expression itself.

### `using` syntax
```idris
-- Import every defined function in "System" in the current scope.
using System

-- Import the "System" namespace as "Sys", and requires it to be
-- explicitely qualified in function calls in the current scope.
using System as Sys

-- Import the "String", "Int" and "Ptr" functions defined in the "System"
-- namespace in the current scope.
using System (String, Int, Ptr)

-- Import every defined function in "System", except "Int" and "Ptr",
-- in the current scope.
using System except (Int, Ptr)
```

> **Note**: It is not required to import symbols defined in the current namespace, or
> "child" namespaces. For example, in the following code, the two first "using"
> expressions are completely optional. The third one, however is required
```idris
namespace Example.Namespace
    using Example
    using Example.Namespace
    using Example.OtherNamespace
```

## Expressions
### Function call
Calls a function or resolves a constant.
```idris
Qualified.Function arguments
Constant
```

### `if ... then ... else ...` expression
```idris
if condition then value else otherValue
```

### `let ... in ...` binding
```idris
let name = value in body
```

### Binary, unary operation
```idris
left + right
left plus right
unary-op operand
```

### Anonymous functions
```idris
\x -> \y -> x + y
\x y -> x + y
x y => x + y
```

### `static ...` expression
Computes its body statically.
```idris
static
    Console.WriteLine "Hello, world"
    
static if Platform == "x86"
    then Console.WriteLine "Hello, x86."
```
In `static` bodies, holes are not reported, since if a hole was encountered, a crash
would have happened.

### Function reference
Some functions take no arguments, which is a problem when trying to access to the
function as a value, instead of calling it. This expression solves this problem.

```haskell
Answer = 42

Answer  -- Byte = 42
@Answer -- Function = Answer : Byte
```

### Record-declaration expression
```idris
Person : Type
Person = { Name : String
           Age  : Int    }
```

### Parse EDSL
```idris
-- Parsing "if" .. "then" .. "else"
expect "if"
commit
condition   <- parse until (\x -> x.IsIdentifier "then")
eat
consequence <- parse until (\x -> x.IsIdentifier "else")
eat
alternative <- parse until (\_ -> False)

-- In real life, we would check that 'condition' is a bool,
-- and 'consequence' and 'alternative' have the same type.
ConditionalExpression condition consequence alternative
```

### Assembly EDSL
```idris
-- Emitting the body of the 'xor' function
assembly result <- 1
         mov result, (argument 0)
         xor result, (argument 1)
         result
```

## Declarations
### Core types
```idris
Pointer : Type
Pointer = 64

Pointer : Type
Pointer = Distinct UInt

type Pointer = 64
type Pointer = UInt
```

### Type aliases
```idris
UnsignedInt : Type
UnsignedInt = UInt

alias UnsignedInt = UInt
```

### Records
```idris
Person : Type
Person = { Name: String ;
           Pets: Int    }
           
type Vec a = { Pointer: Ptr a; Length: Int; Capacity: Int; }
             exporting immutably (Length, Capacity)
```

### Tuples
```idris
Version : Type
Version = (Int, Int, Int)
```

### Algebraic data types
```idris
Option : Type -> Type
Option a = None | Some a
```

### Type classes
```idris
class Monoid m where
    Empty : m
    Append : m -> m -> m 
```

### Instances
```idris
instance Show for (Show a) => BinaryTree where
    show Empty = "<>"
    show (Node value left right) = "<" ++ (show value)
                                ++ "," ++ (show left)
                                ++ "," ++ (show right) ++ ">"
```

### Concepts
```idris
concept List l a where
    Head : l a -> a
    Tail : l a -> l a
```

## Modifiers
### Visibility expressions
Marks the body of the expression as `public`, `internal` or `private`, thence overriding the current default visibility modifier.

```idris
public Hello : Int
       Hello = 0

private
    Add a b = a + b
    Mul a b = a * b
```


### Attributes syntax
Modifies the behavior of the marked function.
```idris
Function : From -> To {- Export -}
```

### State syntax
Imports a state into the function.
```idris
Stateful : Int [ state ::: StateType ]
```

## Patterns

### Direct binding pattern
```idris
Add a b = a + b
```

### Destructuring pattern
```idris
Add (a, b) = a + b
Display (Identifier name span) = name
```

### Catch-all pattern
```idris
IgnoreArguments _ _ = ?ignored
```

### List comprehension pattern
```idris
Sum [] = 0
Sum (x::xs) = x + xs.Sum
```

### Literal matching
```idris
Fib 0 = 0
Fib 1 = 1
Fib n = (Fib (n - 1)) + (Fib (n - 2))
```

### Guards
```idris
Fact n when n < 2 = n
Fact n            = n * (Fact (n - 1))
```

### Recursive patterns
All built-in patterns are recursive, which means that they can be used inside one another.

```idris
SumDoubles [] = 0
SumDoubles ((a, b)::rest) = a + b + rest.SumDoubles

DoubleMaybe None = None
DoubleMaybe (Some None) = None
DoubleMaybe (Some (Some v)) = Some v
```