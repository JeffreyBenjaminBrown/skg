# If 'typedb server' won't start,
see troubleshooting/.

# If 'typedb server' is not running,
some Rust tests won't pass.

# If 'cargo run --bin skg' is not running,
some Emacs Lisp tests might not pass.

# Beware race conditions across tests.
Tests are executed in parallel,
so they can't use the same db name.
