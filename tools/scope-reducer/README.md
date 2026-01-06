# Scope Reducer

Automatically reduces visibility of Rust items (`pub` and `pub(super)`) where possible.

## Usage

From the project root:

```bash
cd tools/scope-reducer
cargo run -- ../..
```

Or build once and run:

```bash
cd tools/scope-reducer
cargo build --release
../../target/release/scope-reducer ../..
```

## How it works

Instead of compiling after each individual change (which would be very slow), the tool:

1. Collects all `pub` and `pub(super)` items in the codebase
2. Batch-reduces them all to private at once
3. Compiles and parses error messages to identify which items failed
4. Restores only the failed items to `pub(super)` and tries again
5. Restores remaining failures to `pub`

This minimizes per-file compile cycles
from O(n) (where n = number of definitions in the file)
to roughly O(1).

Compilation does not cover the integration tests,
but it covers the unit tests, via `cargo check --all-targets`.
