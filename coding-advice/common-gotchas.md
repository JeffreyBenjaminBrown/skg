# History is garbage. Numbers on phases are garbage.
Don't say "phase 1: do this; phase 2: do that."
Such numbers usually go stale, and they illuminate nothing.
Instead just say "in order: do this, then that, ...".

Similarly, when you change something, don't add a comment like,
"We used to _ but now we _," or "Now we use _ instead of _".
Descriptions of historical changes are generally unhelpful,
unless the comment is intended as temporary during a refactor.
Unless that's happening, just describe the logic of the existing code;
don't document its history.
(I'll see the changes in the git diff before committing anyway.)
# IDs and titles are not interchangeable.
In the tests, it is convenient to use
nodes whose titles match their IDs.
In production, though, that would be terrible.
If either one is missing,
and you're considering using the other in its place,
throw an error instead.
# Don't rename 'node' to 'parent' or 'child'
Often there's a 'node' (or 'id', or 'pid') argument to a function, which a single 'origin node' at which the function is called. In those cases, call that thing 'node' throughout the function. Its children, we should call 'children', and its parent, 'parent'. I don't want to call the node 'parent', even if we're dealing with its children, because that's confusing.
# If 'typedb server' won't start,
see troubleshooting/.
# If 'typedb server' is not running,
some Rust tests won't pass.
# If 'cargo run --bin skg' is not running,
some Emacs Lisp tests might not pass.
# Beware race conditions across tests.
Tests are executed in parallel,
so they can't use the same db name.
