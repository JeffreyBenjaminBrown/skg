# History is garbage. Numbers on phases are garbage.

Don't say "phase 1: do this; phase 2: do that."
The numbers will go stale; they do nothing but confuse.
Instead just say "phases: do this, then that, ...".

Similarly, when you change something, don't add a comment like,
"We used to do _ but now we do _," or
"Now we use _". They are unhelpful and confusing.
Just describe the logic of the existing code;
don't document its history.

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
