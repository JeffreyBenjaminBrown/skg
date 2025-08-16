# PURPOSE: Write all Emacs Lisp files to a single org file.
# USAGE: Run this from the root of the project.


# Announces that a line precedes a new kind of content (hence "separator").
# Should come at the start of the line (hence "herald").
separator_herald="****"

{ echo "$separator_herald" "Files are indented a few spaces in this printout, but not in reality."

  echo "$separator_herald tree -I 'target|index.tantivy' ."
  tree -I 'target|index.tantivy|abandoned' .

  find  . -path ./target -prune -o \
        -name '*.rs' \
        -type f -print | while read -r f;
  do
      echo "$separator_herald file: $f"
      cat "$f" | sed 's/^/    /'
      echo ""
  done
} > temp/as-one-file.org
