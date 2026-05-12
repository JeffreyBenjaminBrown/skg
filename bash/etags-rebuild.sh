# Build TAGS file for Emacs navigation.
#
# Uses custom regexes for Rust because the built-in etags Rust support
# includes trailing characters (like `(` or `<'a,`) in tag names,
# which breaks M-. lookups.

# Rust regexes - capture just the identifier name
RUST_REGEXES=(
  --regex='/^[ \t]*pub[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub([a-z]+)[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+async[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub([a-z]+)[ \t]+async[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*async[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+struct[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*struct[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+enum[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*enum[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+type[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*type[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+trait[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*trait[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+const[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*const[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+static[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*static[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*mod[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+mod[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^macro_rules![ \t]+\([a-zA-Z0-9_]+\)/\1/'
  # Struct fields and enum variants are not regex-matchable without
  # false-positives on function arguments, so they are emitted by
  # tools/rust-field-tags (a syn-based AST walker) and appended at the
  # end of this script.
)

# Directory names to skip wherever they appear in the tree.
PRUNE_DIRS=(
  .cache
  .claude
  .config
  .cargo
  .codex
  .emacs.d
  .git
  .local
  .npm
  _burying
  abandoned
  data
  example-data
  target
  temp
  tools
)

# Build a find -name ... -o -name ... expression from PRUNE_DIRS.
PRUNE_NAMES=()
for d in "${PRUNE_DIRS[@]}"; do
  [[ ${#PRUNE_NAMES[@]} -gt 0 ]] && PRUNE_NAMES+=(-o)
  PRUNE_NAMES+=(-name "$d")
done

# Process .el files with default elisp support
find . '(' -type d '(' "${PRUNE_NAMES[@]}" ')' -prune ')' -o \
       -type f -name "*.el" -print0 \
  | xargs -0 etags

# Process .rs files with custom regexes (--language=none disables built-in Rust)
find . '(' -type d '(' "${PRUNE_NAMES[@]}" ')' -prune ')' -o \
       -type f -name "*.rs" -print0 \
  | xargs -0 etags --append --language=none "${RUST_REGEXES[@]}"

# Append struct-field / enum-variant entries, produced by a syn-based
# AST walker so function arguments never sneak in as false positives.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cargo build --release -p rust-field-tags --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>/dev/null
FIELD_TAGS="$PROJECT_ROOT/target/release/rust-field-tags"
if [[ -x "$FIELD_TAGS" ]]; then
  find . '(' -type d '(' "${PRUNE_NAMES[@]}" ')' -prune ')' -o \
         -type f -name "*.rs" -print0 \
    | xargs -0 "$FIELD_TAGS" >> TAGS
else
  echo "WARN: rust-field-tags binary not found; struct fields and enum variants will be missing from TAGS." >&2
fi
