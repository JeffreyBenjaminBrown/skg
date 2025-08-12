separator="**** "
{ echo "$separator" "Files are indented a few spaces in this printout, but not in reality."

  echo "$separator " "tree -I 'target|index.tantivy' ."
  tree -I 'target|index.tantivy' .

  find  . -path ./target -prune -o \
        \( -name '*.md' -o -name '*.el' -o -name '*.rs' \) \
        -type f -print | while read -r f;
  do
      echo "$separator file: $f"
      cat "$f" | sed 's/^/    /'
      echo ""
  done
} > temp/as-one-file.org
