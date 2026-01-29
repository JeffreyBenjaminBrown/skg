# IDs and titles are not interchangeable.

In the tests, it is convenient to use
nodes whose titles match their IDs.
In production, though, that would be terrible.
If either one is missing,
and you're considering using the other in its place,
throw an error instead.

# If 'typedb server' won't start,
see troubleshooting/.

# If 'typedb server' is not running,
some Rust tests won't pass.

# If 'cargo run --bin skg' is not running,
some Emacs Lisp tests might not pass.

# Beware race conditions across tests.
Tests are executed in parallel,
so they can't use the same db name.
