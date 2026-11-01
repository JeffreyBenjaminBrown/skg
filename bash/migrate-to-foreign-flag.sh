#!/usr/bin/env bash
# Migrate a config (and its data tree) from ownership-by-folder to
# the per-entry 'foreign' flag. Before: a skgrepo was owned iff its
# path lay under the config's 'owned_folder' (default "owned"). After:
# a skgrepo is owned unless its entry says 'foreign = true', and no
# folder name is special. In order, this script
#   renames DATA_ROOT/OWNED_FOLDER to DATA_ROOT/NEW_FOLDER (if the
#     former exists and the latter does not),
#   rewrites each entry's path under OWNED_FOLDER to lie under
#     NEW_FOLDER instead,
#   adds 'foreign = true' to each entry whose path was not under
#     OWNED_FOLDER, and
#   deletes the 'owned_folder' line.
# An entry with no 'name' is named by its path, so a moved one is
# renamed with it; a 'default_repo_set' naming it follows.
# DATA_ROOT is the config's directory. Several configs sharing one data
# root may be migrated one after another: the folder is renamed once,
# and each config is rewritten.
#
# USAGE:
#   bash/migrate-to-foreign-flag.sh path/to/skgconfig.toml [NEW_FOLDER]
# NEW_FOLDER defaults to "jeff".
#
# Idempotent. A config is taken to need rewriting if it sets
# 'owned_folder', if some entry's path lies under the owned folder, or
# if the owned folder still exists in its data root. Otherwise it is
# left as it is. The one case this cannot decide is a config the
# user owned nothing in, migrated after the folder was renamed; the
# script warns when it may be facing that case.
# See docs/api-and-formats.org, "Skgrepos".

set -euo pipefail
CONFIG="${1:?usage: migrate-to-foreign-flag.sh CONFIG [NEW_FOLDER]}"
NEW_FOLDER="${2:-jeff}"

python3 - "$CONFIG" "$NEW_FOLDER" <<'PYTHON'
import pathlib, re, sys

config_path = pathlib.Path (sys.argv[1])
new_folder  = sys.argv[2] . strip ("/")
data_root   = config_path . absolute () . parent . resolve () # as the server finds it
text        = config_path . read_text ()

blocks = re.split (r'(?m)^(?=\[\[)', text)
header, tables = blocks[0], blocks[1:]

owned_folder_line = re.search (
  r'(?m)^[ \t]*owned_folder[ \t]*=[ \t]*"([^"]+)"[^\n]*\n?', header)
owned_folder = (owned_folder_line . group (1) . strip ("/")
                if owned_folder_line else "owned")
owned_root   = data_root / owned_folder
new_root     = data_root / new_folder

def entry_path (table):
  if not table . startswith ("[[repos]]"):
    return None
  m = re . search (r'(?m)^[ \t]*path[ \t]*=[ \t]*"([^"]+)"', table)
  return m

def under_owned_folder (path_string):
  path = pathlib.Path (path_string)
  absolute = path if path . is_absolute () else data_root / path
  return absolute == owned_root or owned_root in absolute . parents

def moved_path_string (path_string):
  path = pathlib.Path (path_string)
  if path . is_absolute ():
    return str (new_root / path . relative_to (owned_root))
  return str (pathlib.Path (new_folder)
              / path . relative_to (owned_folder))

paths = [ m . group (1) for m in map (entry_path, tables) if m ]
needs_rewrite = ( owned_folder_line is not None
                  or any (map (under_owned_folder, paths))
                  or owned_root . is_dir () )

if owned_folder != new_folder and owned_root . is_dir ():
  if new_root . exists ():
    sys.exit (f"{new_root} exists already; not renaming {owned_root} onto it")
  owned_root . rename (new_root)
  print (f"renamed {owned_root} -> {new_root}")

if not needs_rewrite:
  print (f"{config_path}: already migrated; nothing to do")
  if paths and not re . search (r'(?m)^[ \t]*foreign[ \t]*=', text):
    print ("  note: every entry here is owned, which is right for a "
           "migrated config. If instead this config predates the "
           "'foreign' flag, it owned nothing: add 'foreign = true' "
           "to each entry by hand.")
  sys.exit (0)

def migrated_table (table):
  m = entry_path (table)
  if not m:
    return table
  if under_owned_folder (m . group (1)):
    if owned_folder == new_folder:
      return table
    return table . replace (
      m . group (0), f'path = "{moved_path_string (m . group (1))}"', 1)
  if re . search (r'(?m)^[ \t]*foreign[ \t]*=', table):
    return table
  return table . replace (
    m . group (0), m . group (0) + "\nforeign = true", 1)

new_header = (header . replace (owned_folder_line . group (0), "", 1)
              if owned_folder_line else header)
for table in tables:
  m = entry_path (table)
  if ( m and under_owned_folder (m . group (1))
       and not re . search (r'(?m)^[ \t]*name[ \t]*=', table) ):
    new_header = re . sub (
      r'(?m)^([ \t]*default_repo_set[ \t]*=[ \t]*)"'
      + re . escape (m . group (1)) + '"',
      lambda d: f'{d . group (1)}"{moved_path_string (m . group (1))}"',
      new_header)
new_tables = list (map (migrated_table, tables))
config_path . write_text (new_header + "" . join (new_tables))
print (f"rewrote {config_path}")
kinds = [ bool (re . search (r'(?m)^[ \t]*foreign[ \t]*=[ \t]*true', t))
          for t in new_tables if entry_path (t) ]
if any ( not kinds[i] and kinds[j]
         for i in range (len (kinds))
         for j in range (i + 1, len (kinds)) ):
  print ("  warning: a foreign entry is listed after an owned one. skg "
         "refuses that: every foreign repo must be more public (earlier) "
         "than every owned repo. Reorder the entries.")
PYTHON
