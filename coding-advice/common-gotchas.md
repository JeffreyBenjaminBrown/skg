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
# The Tantivy search index is updated in the background, after a save.
A save (the save-buffer pipeline, 'update_graph_minus_merges', etc.)
returns BEFORE its Tantivy search-index update has committed. The index
update is handed to a single background worker
('server/dbs/tantivy/background_writer.rs') so the save can respond
without paying for the Tantivy commit (~60ms) on the critical path. The
filesystem and TypeDB are updated synchronously (before the save
returns); only Tantivy is deferred.

Consequence: if you read the search index right after a save and expect
to see that save's changes, you must first call
'wait_for_tantivy_writes_idle'. That blocks until every enqueued write
has committed. The production text-search handler does this for you, and
'search_index' reloads the reader before searching, so a search reflects
every save that finished before it.

In TESTS this bites you directly: a test that saves and then reads
Tantivy (via 'search_index' or a helper like 'tantivy_source_for_id')
must call 'wait_for_tantivy_writes_idle' between the save and the read.
Otherwise the read races the background commit and the test flakes ---
typically a Tantivy read that returns 'None' or stale data
intermittently, while passing in isolation and failing only in the full
suite. ('test_utils' already drains the worker before per-test cleanup,
so leftover index files don't trip 'remove_dir_all'.)

This does NOT affect the re-rendered view a save returns: that is built
from the in-Rust graph, not from Tantivy. Only direct Tantivy reads are
subject to the delay.
