Free monads can help transform monadic computations into sequence of operations.  
Please read [this article on Free Monads](https://deque.blog/2017/11/13/free-monads-from-basics-up-to-implementing-composable-and-effectful-stream-processing/) before continuing.

Using the previous example, we could imagine a parser monad that would allow this
kind of declarations.
```haskell
ParseConditional : Parser Expr
ParseConditional = do expect "if"
                      cond <- parse expr until "then"
                      expect "then"
                      cons <- parse expr until "else"
                      expect "else"
                      altr <- parse expr until end
                      pure $ Conditional cond cons altr
```

As a free monad, this could lead to this kind of sequence.
```haskell
type ParseOp a = Expect (Token -> Boolean)
               | Eat
               | Parse Type String
               | ParseUntil (Token -> Boolean) String
               | Pure (() -> a)
               | ...

[
  Expect (\tok -> tok.Is "if"),
  ParseUntil (\tok -> tok.Is "then", "cond"),
  Expect (\tok -> tok.Is "then"),
  ParseUntil (\tok -> tok.Is "else", "cons"),
  Expect (\tok -> tok.Is "else"),
  ParseUntil (\tok -> tok == EOF, "altr"),
  
  Pure (\_ -> Conditional (lookup "cond") (lookup "cons") (lookup "altr")) 
]
```

What would be nice would be to directly transform the body of the function
`ParseConditional` into the above sequence at compile-time. This way, there is no
allocation, no closure, and no recurring transformation every time the function is
called.

## Limitations
Above, you might have noticed the fact that the variables `cond`, `cons` and `altr`
were no longer variables, but strings to lookup. This is less than ideal, as it brings the need to capture a lookup table in every callback.

What could be done, instead, would be to rewrite the sequence as an *actual* free
monad.
```haskell
Expect (\tok -> tok.Is "if") \_ ->
  Expect (\tok -> tok.Is "if") \_ ->
    ParseUntil (\tok -> tok.Is "then") \cond ->
      Expect (\tok -> tok.Is "then") \_ ->
        ParseUntil (\tok -> tok.Is "else") \cons ->
          Expect (\tok -> tok.Is "else") \_ ->
            ParseUntil (\tok -> tok == EOF) \altr ->
              Pure (\_ -> Conditional cond cons altr) 
```

However, allocations and closures are back in full force.  
Another way to store variables would be using a virtual stack.
```haskell
[
  Expect (\tok -> tok.Is "if"),
  ParseUntil (\tok -> tok.Is "then"), -- pushes 'cond' to stack
  Expect (\tok -> tok.Is "then"),
  ParseUntil (\tok -> tok.Is "else"), -- pushes 'cons' to stack
  Expect (\tok -> tok.Is "else"),
  ParseUntil (\tok -> tok == EOF),    -- pushes 'altr' to stack
  
  Pure (\_ -> let altr = Pop in
              let cons = Pop in
              let cond = Pop in
              Conditional cond cons altr) 
]
```
