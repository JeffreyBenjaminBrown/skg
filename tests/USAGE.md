# Running Tests

There are three kinds of tests. The authoritative description lives in `coding-advice/claude-to-claude.org`, section "Running Tests".

1. **Emacs Lisp tests**: `bash/emacs-tests.sh`
   - Runs the ERT tests in `tests/elisp/` in Emacs batch mode
   - Some tests require the skg server (`cargo run --bin skg`) to be running

2. **Rust tests**: `cargo nextest run` (plain `cargo test` also works, but nextest gives each test its own process, avoiding a known flake)
   - Requires `typedb server` to be running

3. **Integration tests**: `bash/integration-tests.sh`
   - End-to-end tests in `tests/integration/*/run-test.sh`
   - Requires both `typedb server` and the skg server to be running

Note that `tests/nvim/` holds the Neovim client's tests.

The Docker container includes Emacs and Neovim (see `coding-advice/docker.org`), so all tests can run inside it.
