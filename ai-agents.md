First check out [the project overview](OVERVIEW.org).

## Don't change anything in git.

That includes my credentials (which, as the git history reveals,
an earlier version of you you once did).
You'll get plenty of credit, don't worry.

## Initialization Instructions

Upon starting each session, automatically:

1. Read all files in `coding-advice/` directory to understand project context.

2. Also read:
   - `api-and-formats.md` - TCP API specification and data formats
   - `schema.tql` - TypeDB schema definition
   - `glossary.md` - Project terminology and abbreviations

3. Retain all this information in context for the entire session to ensure consistent adherence to project conventions and understanding of the codebase architecture

## Key Project Context

This is the **skg** project - a knowledge graph manipulation system with:
- **Rust server** (`rust/`) handling core logic with TypeDB and Tantivy
  - TypeDB is now on version 3, which might be after your training data cutoff date, so bear in mind that your instincts might conflict with the latest syntax.
- **Emacs client** (`elisp/`) providing org-mode interface
- **YAML data storage** in `.skg` files
- **TCP communication** between components:
  - Port 1730 (default): Rust-Emacs API communication (configurable)
  - Port 1729: Rust-TypeDB database connection (TypeDB server)

## Formatting Rules

When writing or modifying code:
- **Type signatures**: Name, each argument, and return type on separate lines
- **Variable definitions**: Type signature required, newline after equals sign
- **Function order**: Callers before callees within same module
- **Whitespace**: Honestly, hard to describe, but try to follow the precedents you find in the code. My (this is Jeff speaking) goal is readability.
- **Bracket grouping**: Adjacent punctuation grouped `(( ))` not `(())`
- **Comments**: Inside code blocks they apply to, preserve existing comments
- **No empty lines** in function definitions unless necessary for comment clarity.
- **Don't reformat existing code** - only apply rules to new/modified code

## Testing Dependencies

- `typedb server` must be running for Rust tests to pass
- `cargo run` must be running for some Emacs Lisp tests, and for all integration tests, to pass.

The script `run-servers.sh` persists those things. Only one TypeDB server should be running at a time, but many Skg servers (`cargo run --bash skg`) can run at the same time. Config files like the one at `data/skgconfig.toml` can be used to change TCP ports, database names and data folders,, so that different instances of Skg don't collide.

## Running Tests

There are three ways to test the code:

1. **Emacs Lisp tests**: `bash/emacs-tests.sh`
   - Runs all automated Emacs tests in `tests/elisp/`
   - Uses Emacs batch mode with ERT (Emacs Lisp Regression Testing)
   - Some tests require `cargo run` to be running

2. **Rust unit/integration tests**: `cargo nextest run`
   - Runs Rust tests in parallel
   - Requires `typedb server` to be running
   - Tests use separate database names to avoid race conditions

3. **Integration tests**: `bash/integration-tests.sh`
   - Runs end-to-end tests that exercise both Rust and Emacs components
   - Tests run in parallel with isolated configurations
   - Requires both `typedb server` and `cargo run` to be running
   - Individual tests located in `tests/integration/*/run-test.sh`

## Disregard Reverted Edits

If you make an edit and then see in a system-reminder that the file has been modified (reverting your changes), this means the user has rejected those changes. **DO NOT re-apply those edits**. The reversion is intentional and indicates the changes were not wanted. Move on without attempting to restore the reverted changes.
