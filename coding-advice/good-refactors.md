# Scope reduction

There's an automatic tool for this. It takes a few minutes to run.
Here's its [README](../tools/scope-reducer/README.md).

# Refactoring sequential to nested definitions

## a definition of the refactor, or at least a diverse set of examples

I always find myself doing this,
to reduce the scope over which definitions in a function apply.
I intend to ask AIs to do it.

The transformation can come in many flavors. The simplest such change goes from a passage like this:
```
  let x = ..;
  let r = f(x);
```

to a passage like this:
```
  let y = ..;
  let z = ..;
  let r = {
    let x = ..;
    f(x) };
```

But the use of the nestable variable
might not be immediately adjacent to its definition:
That is, I might transform this:
```
  let x = ..;
  ... // a bunch of other code
  let r = f(x);
```

to a passage like this:
```
  ... // a bunch of other code
  let r = {
    let x = ..;
    f(x) };
```

Moreover there might be multiple inputs,
e.g. taking this:
```
  let x = ..;
  let y = ..;
  let z = ..;
  ...
  let r = f(x,y,z);
```

to this:
```
  let x = ..;
  let y = ..;
  let z = ..;
  let r = f(x,y,z);
```

And there might even be multiple *outputs*.
So in the most complex case we transform something like this:
```
  let x = ..;
  let y = ..;
  let z = ..;
  // maybe some intervening code that doesn't use them
  let (r,s) = f(x,y,z);
```

to this:
```
  // the same maybe intervening code
  let (r,s) = {
    let x = ..;
    let y = ..;
    let z = ..;
    f(x,y,z) };
```

Moreover there might be multiple spots in the same function that can be independently refactored this way.

The procedure feels mechanical, yet hard to completely specify.
I wonder if it might be deceptively NP-complete.

## Caveats and nuances

### The "output" could be side effects.

So in the examples above,
the 'let r = ' in 'let r = {' is really optional.

### "Single use" isn't always required.

Even variables used multiple times within one logical block can be nested if all uses are contained.

### The borrow checker can constrain things.

In Rust, you sometimes can't nest be cause you need to drop a borrow before getting a mutable reference.

### Comments should survive.
