# Running Tests

There are three kinds of tests.

1. **Emacs Lisp tests**: `bash/emacs-tests.sh`
   - Runs the ERT tests in `tests/elisp/` in Emacs batch mode
   - Some tests require the skg server (`cargo run --bin skg`) to be running

2. **Rust tests**: `cargo nextest run` (plain `cargo test` also works, but nextest gives each test its own process, avoiding a known flake)
   - Runs without any external database service

3. **Integration tests**: `bash/integration-tests.sh`
   - End-to-end tests in `tests/integration/*/run-test.sh`
   - Starts and exercises the Skg server

Note that `tests/nvim/` holds the Neovim client's tests.

The Docker container includes Emacs and Neovim (see `coding-advice/docker.org`), so all tests can run inside it.

## Where a plan run keeps its logs

Agents executing a plan write their logs and one-time scripts under `tests/results/<plan>/sliceN/`, where `<plan>` is the plan's number (`tests/results/06/`). `bash/all-tests.sh` clears `tests/results/` at its start but for the subdirectories whose name is a plan number: those belong to the plan run's agents, and only they delete them.
