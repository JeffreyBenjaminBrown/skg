# No legacy support code

If I ask you to change a name, even if it's part of the API,
don't create 'legacy support' code for the old name.
This repo contains all the relevant code; so far,
there are no other clients or servers to maintain compatibility with.

# Short atomic functions, but long names

Try to provide a collection of atomic, readable functions, none of them too complex. On the other hand, when naming variables and functions, err on the long side -- the more understandable, the better.

# Fully-qualified imports

Sometimes a function is re-exported from a file with a name
shorter than the file that defines it.
All imports should be from the original, longer-named file,
because that one is more informative.
