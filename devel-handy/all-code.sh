# PURPOSE: Write all markdown, Emacs Lisp and Rust files
# to a single org file.

ignore_patterns=(
  ".git"
  "abandoned"
  "index.tantivy"
  "node_modules"
  "target"
)

file_extensions=(
  "el"
  "md"
  "rs"
  "tql"
)

# Build tree ignore string: "pattern1|pattern2|pattern3"
build_tree_ignore() {
  local IFS='|'
  echo "${ignore_patterns[*]}"
}

# Build find command array
build_find_command() {
  local find_cmd=(find .)

  # Add prune conditions
  for pattern in "${ignore_patterns[@]}"; do
    find_cmd+=(-path "*/$pattern" -prune -o -path "*/$pattern/*" -prune -o)
  done

  # Add extension conditions in parentheses
  find_cmd+=('(')
  for i in "${!file_extensions[@]}"; do
    if [[ $i -gt 0 ]]; then
      find_cmd+=(-o)
    fi
    find_cmd+=(-name "*.${file_extensions[$i]}")
  done
  find_cmd+=(')' -type f -print)

  # Bash can't return arrays, so print each element.
  printf '%s\0' "${find_cmd[@]}"
}

process_file() {
  # Add separator and indent content
  local file="$1"
  local separator_herald="$2"

  echo "$separator_herald file: $file"
  cat "$file" | sed 's/^/    /'
  echo ""
}

tree_ignore=$(build_tree_ignore)

# Announces that a line precedes a new kind of content (hence "separator").
# Should come at the start of the line (hence "herald").
separator_herald="****"

{
  echo "$separator_herald" "Files are indented a few spaces in this printout, but not in reality."
  echo "$separator_herald tree -I '$tree_ignore' ."
  tree -I "$tree_ignore" .

  # Build and execute find command
  readarray -d '' find_cmd < <(build_find_command)
  "${find_cmd[@]}" | while read -r f; do
    process_file "$f" "$separator_herald"
  done
} > all-code.org
