# Claude Code Configuration

## Initialization Instructions

Upon starting each session, automatically:

1. Read all files in `coding-advice/` directory to understand project context:
   - `coding-advice/architecture.md` - Overview of the skg knowledge graph system
   - `coding-advice/common-gotchas.md` - Important testing dependencies
   - `coding-advice/formatting.md` - Specific code formatting conventions
   - `coding-advice/for-ai.md` - Additional coding philosophy (no legacy support, atomic functions)

2. Read the linked reference files:
   - `api-and-formats.md` - TCP API specification and data formats
   - `schema.tql` - TypeDB schema definition
   - `glossary.md` - Project terminology and abbreviations

3. Retain all this information in context for the entire session to ensure consistent adherence to project conventions and understanding of the codebase architecture

## Key Project Context

This is the **skg** project - a knowledge graph manipulation system with:
- **Rust server** (`rust/`) handling core logic with TypeDB and Tantivy
  - TypeDB is now on version 3, which has so far been after the cutoff date for training data for Claude, so bear in mind that your instincts might conflict with the current syntax.
- **Emacs client** (`elisp/`) providing org-mode interface
- **YAML data storage** in `.skg` files
- **TCP communication** between components:
  - Port 1730 (default): Rust-Emacs API communication (configurable)
  - Port 1729: Rust-TypeDB database connection (TypeDB server)

## Critical Formatting Rules

When writing or modifying code:
- **Type signatures**: Name, each argument, and return type on separate lines
- **Variable definitions**: Type signature required, newline after equals sign
- **Function order**: Callers before callees within same module
- **Whitespace**: Around all punctuation `< > ( ) [ ] { } . ; + - * / :: ? &`
- **Bracket grouping**: Adjacent punctuation grouped `(( ))` not `(())`
- **Comments**: Inside code blocks they apply to, preserve existing comments
- **No empty lines** in function definitions unless necessary for comment clarity
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

## Handling Reverted Edits

If you make an edit and then see in a system-reminder that the file has been modified (reverting your changes), this means the user or linter has rejected those changes. **DO NOT re-apply those edits**. The reversion is intentional and indicates the changes were not wanted. Move on without attempting to restore the reverted changes.
