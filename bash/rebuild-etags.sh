# Build TAGS file for Emacs navigation.
#
# Uses custom regexes for Rust because the built-in etags Rust support
# includes trailing characters (like `(` or `<'a,`) in tag names,
# which breaks M-. lookups.

# Rust regexes - capture just the identifier name
RUST_REGEXES=(
  --regex='/^[ \t]*pub[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
  --regex='/^[ \t]*pub[ \t]+async[ \t]+fn[ \t]+\([a-zA-Z0-9_]+\)/\1/'
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
)

# Process .el files with default elisp support
find . -type f -name "*.el" -print0 \
  | xargs -0 etags

# Process .rs files with custom regexes (--language=none disables built-in Rust)
find . -type f -name "*.rs" -print0 \
  | xargs -0 etags --append --language=none "${RUST_REGEXES[@]}"
