# PURPOSE: Write all markdown, Emacs Lisp and Rust files
# to a single org file.


# Announces that a line precedes a new kind of content (hence "separator").
# Should come at the start of the line (hence "herald").
separator_herald="****"

{ echo "$separator_herald" "Files are indented a few spaces in this printout, but not in reality."

  echo "$separator_herald tree -I 'target|index.tantivy|abandoned' ."
  tree -I 'target|index.tantivy|abandoned' .

  find  . -path ./target -prune -o \
        \( -name '*.md' -o -name '*.el' -o -name '*.rs' \) \
        -type f -print | while read -r f;
  do
      echo "$separator_herald file: $f"
      cat "$f" | sed 's/^/    /'
      echo ""
  done
} > temp/as-one-file.org
