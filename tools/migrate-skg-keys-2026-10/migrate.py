#!/usr/bin/env python3
"""One-off migration (October 2026): rename top-level .skg YAML keys.

  misc                         -> flags
  subscribes_to                -> subscribesTo
  hides_from_its_subscriptions -> hidesFromSubs
  overrides_view_of            -> overrides

Only a key at the start of a line (a top-level YAML key, which is how
skg writes them) is renamed, so titles and bodies are never touched.
Idempotent: a migrated file has none of the old keys.

Usage: migrate.py [--dry-run] DIR...
Walks each DIR for *.skg files, skipping .git directories.
"""

import os
import re
import sys

RENAMES = {
  "misc"                         : "flags",
  "subscribes_to"                : "subscribesTo",
  "hides_from_its_subscriptions" : "hidesFromSubs",
  "overrides_view_of"            : "overrides",
}

OLD_KEY = re.compile (
  r"^(" + "|".join (RENAMES) + r"):(?=\s|$)",
  re.MULTILINE )

def migrate_text (text):
  """Return (new_text, number_of_keys_renamed)."""
  return OLD_KEY.subn (
    lambda m: RENAMES [m.group (1)] + ":",
    text )

def skg_files_under (root):
  for dirpath, dirnames, filenames in os.walk (root):
    dirnames [:] = [d for d in dirnames if d != ".git"]
    for name in filenames:
      if name.endswith (".skg"):
        yield os.path.join (dirpath, name)

def main (argv):
  dry_run = "--dry-run" in argv
  roots = [a for a in argv if a != "--dry-run"]
  if not roots:
    print (__doc__)
    return 2
  files_changed = keys_renamed = 0
  for root in roots:
    for path in skg_files_under (root):
      with open (path, encoding = "utf-8", newline = "") as f:
        text = f.read ()
      new_text, n = migrate_text (text)
      if n == 0:
        continue
      files_changed += 1
      keys_renamed  += n
      if not dry_run:
        with open (path, "w", encoding = "utf-8", newline = "") as f:
          f.write (new_text)
  print ( "{}{} keys renamed in {} files".format (
    "(dry run) " if dry_run else "", keys_renamed, files_changed ))
  return 0

if __name__ == "__main__":
  sys.exit (main (sys.argv [1:]))
