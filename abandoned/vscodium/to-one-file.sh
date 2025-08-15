# Announces that a line precedes a new kind of content (hence "separator").
# Should come at the start of the line (hence "herald").
separator_herald="****"

{ echo "$separator_herald" "Files are indented a few spaces in this printout, but not in reality."

  echo "$separator_herald tree -I 'node_modules|out' ."
  tree -a -I 'node_modules|out' .

  find . \
       \( -name node_modules -o -name out \) -prune -o \
       \( -name '*.json' -o -name '*.md' \
       -o -name '*.ts'  -o -name '*.mjs' \) \
       -type f -print | while read -r f;
  do
      echo "$separator_herald file: $f"
      cat "$f" | sed 's/^/    /'
      echo ""
  done
} > as-one-file.org
