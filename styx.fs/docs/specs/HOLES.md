# Binding & Holes

When binding, it might happen that some symbols cannot be resolved at all, even after everything.

The original idea was to have the ability to "seal" observable streams, which would notify holes that they wouldn't be filled, which would in turn log an error. This would happen once the binding phase is over, all at once.

However, this design is flawed in the sense that sometimes, symbols need to be resolved **during** the binding phase for it to end (ie: `static` expressions).

Hence, we could additionally let holes be logged lazily during the binding, when they are forced to take a value. If they are unbound, they would return a dummy value and report an error, thus not interrupting the whole program, while notifying the user that its execution is unreliable and failed.

## New idea
> A **scope** is an area where symbols live. This might be a function, a namespace, etc

### Parsing
There should be a queue in which all parsers are set. Parsers are created individually for each given file of an input, and are ran on said file. Parsers, after running, return a state:
- `Bound` if everything was parsed correctly (even if there are holes inside);
- `Unbound` if some things could not be parsed, or if a binary operator could not be found. In this case, another parser is returned to keep parsing the rest of the input at a higher scope.
- `Hole` if a fatal error was encountered, such as multiple operators at once, or misc invalid expressions ("if if", "match if", ...).

Parsers that are unbound are added to the top of the queue, along with the given parser that handles the rest of the higher scope.

We keep parsing until the queue only contains unbound or hole parsers, where we stop and log all errors (if any). Finally, we seal the symbol streams and notify everyone that we're done.

**This way of parsing also means that the order in which things are parsed is non-deterministic, and thus cannot be relied upon**.

### Binding
- Binding in static blocks says "as soon as the first statement is bound, it is executed", over and over again until all statements have been executed in order.
- Binding in other blocks shouldn't matter until the binder is sealed.

**Why?** That way, everything that needs to run runs when it can, or doesn't. This is allowed because things are parsed in a non deterministic way. Static blocks are lazy and thus do not force unbound values to choose "bound" or "hole."

Finally, when the input is sealed. Every hole we still have reports an error. An error logged means we can't do jack with the generated code, and executing it anyway is UB.