#!/bin/bash

# Generate ~781 .skg files for benchmark testing.
# Usage: bash generate-data.sh <output-directory>
#
# Creates a tree with wrapper node "0" containing 5 roots (1-5).
# Each node has 5 children; labels are digit-string prefixes.
# Depth 4 (4-digit leaves). Total: 1 + 5 + 25 + 125 + 625 = 781 nodes.

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
  echo "Usage: $0 <output-directory>"
  exit 1
fi

mkdir -p "$OUTPUT_DIR"

# Write a single .skg file.
# Args: label, child1 child2 ... (space-separated children, or empty)
write_skg_file() {
  local label="$1"
  shift
  local children=("$@")
  local file="$OUTPUT_DIR/${label}.skg"

  if [ ${#children[@]} -eq 0 ]; then
    cat > "$file" << EOF
title: "$label"
ids:
  - "$label"
contains: []
EOF
  else
    {
      echo "title: \"$label\""
      echo "ids:"
      echo "  - \"$label\""
      echo "contains:"
      for child in "${children[@]}"; do
        echo "  - \"$child\""
      done
    } > "$file"
  fi
}

# Generate children labels for a given prefix.
# At max depth (5 digits), nodes are leaves.
generate_tree() {
  local label="$1"
  local depth=${#label}

  if [ "$depth" -ge 4 ]; then
    # Leaf node
    write_skg_file "$label"
    return
  fi

  # Interior node: generate 5 children
  local children=()
  for digit in 1 2 3 4 5; do
    children+=("${label}${digit}")
  done

  write_skg_file "$label" "${children[@]}"

  # Recurse into children
  for child in "${children[@]}"; do
    generate_tree "$child"
  done
}

# Wrapper node "0" containing roots 1-5
write_skg_file "0" "1" "2" "3" "4" "5"

# Generate each root tree
for root in 1 2 3 4 5; do
  generate_tree "$root"
done

# Count generated files
FILE_COUNT=$(ls "$OUTPUT_DIR"/*.skg 2>/dev/null | wc -l)
echo "Generated $FILE_COUNT .skg files in $OUTPUT_DIR"
