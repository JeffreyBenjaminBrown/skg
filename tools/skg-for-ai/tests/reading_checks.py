"""The checks of 'test_skg_for_ai.py' on the first scratch server: each
view command ('view', 'search', 'links-here', 'contains', 'forest')
against 'test-expected/', handles, 'raw' and 'import', the search
options, a missing ID, the linkstack, the restriction, 'describe'."""

import json
import os
import shutil

import sexp
import heralds

from harness import (
  FIXTURE_FOLDER, HANDLE_PATTERN, SHARED_HERALD_RULES, TOOL_FOLDER,
  VIEWNODE_ID_PATTERN, CaseFailure, hit_titles, read_file, read_sidecar,
  require_equal, require_output, require_tool_run, run_tool )

EXPECTED_FOLDER = os . path . join (TOOL_FOLDER, "test-expected")

# Each run: the files' base name, the tool's arguments, and the view
# ID its sidecar records.
TOOL_RUNS = [
  ( "view_root-id",          [ "view", "root-id" ], "content-view:root-id" ),
  ( "search_Fred",           [ "search", "Fred" ], "search:Fred" ),
  ( "links-here_child-b-id", [ "links-here", "child-b-id" ],
    "content-view:child-b-id" ),
  ( "contains_shared-id",    [ "contains", "shared-id", "--no-summary" ],
    "content-view:shared-id" ) ] + [
  # Each forest kind, from a node of the fixture that shows it
  ( "forest-" + kind + "_" + node_id,
    [ "forest", kind, node_id, "--no-summary" ], "content-view:" + node_id )
  for kind, node_id in [ ( "containers",    "shared-id" ),
                         ( "mentioners",    "child-b-id" ),
                         ( "forks",         "fred-note-id" ),
                         ( "hides",         "fred-aside-id" ),
                         ( "aliases",       "forkee-id" ),
                         ( "flags",         "forker-id" ) ] ]

def check_describe (scratch):
  """'describe' prints the server's sentences for a handle of a view:
  a note's heralds, most informative first, and what a folder is."""
  folder = os . path . join (scratch, "describe")
  require_tool_run ([ "view", "root-id", "--out-dir", folder, "--no-summary" ])
  view_files = os . path . join (folder, "view_root-id.view.json")
  require_output (
    run_tool ([ "describe", view_files, "0a" ]),
    "1•C2: 1 note contains this one; a note of yours, not the one it "
    "stands under, is among them: you keep it elsewhere (•); this note "
    "contains 2 notes.\n"
    "L2: this note links to 2 notes.\n"
    "⌂:main: it lives in your skgrepo 'main'.\n" )
  folder_lines = run_tool ([ "describe", view_files, "0b" ]) . stdout
  if not folder_lines . startswith ("☮ These also contain it.: the other notes"):
    raise CaseFailure ( f"'describe' of a folder printed {folder_lines!r}" )

def failures_of_reading (scratch, data_root, update_expected):
  output_folder = os . path . join (scratch, "out")
  failures = []
  for base_name, tool_arguments, view_id in TOOL_RUNS:
    failures += failures_of_one_run (
      base_name, tool_arguments, view_id, output_folder, update_expected )
  failures += failures_of_raw_and_import (scratch)
  for check in [ check_search_options, check_missing_id, check_linkstack,
                 check_restriction_on_a_command_s_own_connection,
                 check_describe ]:
    try:
      check (scratch)
    except CaseFailure as failure:
      failures . append ( check . __name__ + ": " + str (failure) )
  failures += failures_of_changed_skg_files (data_root)
  return failures

def check_search_options (scratch):
  """'--body' finds a word only a body holds, '--regex' reads a term as
  a regular expression over the lowercased words, and '--operators'
  keeps Tantivy's syntax, which here narrows what the same words find
  without it. A search with no match writes nothing and says so."""
  folder = os . path . join (scratch, "search-options")
  completed = run_tool ([ "search", "unnumbered", "--out-dir", folder ])
  require_output ( completed, "", expected_returncode = 1 )
  require_equal ( "a search with no match, on stderr", completed . stderr,
                  "skg_for_ai: The server returned no view: No matches found.\n" )
  for arguments, base_name, titles in [
      ( [ "unnumbered", "--body" ], "search_unnumbered", [ "First child" ] ),
      ( [ "fre.*", "--regex" ], "search_fre.",
        [ "An aside of Fred's", "Fred's note", "A detail of Fred's" ] ),
      ( [ '"Fred s note" OR aside', "--operators" ],
        "search_Fred-s-note-OR-aside", [ "Fred's note", "An aside of Fred's" ] ),
      ( [ '"Fred s note" OR aside' ], "search_Fred-s-note-OR-aside",
        [ "An aside of Fred's", "Fred's note", "A detail of Fred's" ] ),
      # An apostrophe is a word break, as titles are indexed: "Fred's"
      # finds the words "fred" "s" in order.
      ( [ "Fred's note" ], "search_Fred-s-note",
        [ "An aside of Fred's", "Fred's note", "A detail of Fred's" ] ),
      ( [ "+Fred's +note", "--operators" ], "search_Fred-s-note",
        [ "Fred's note" ] ) ]:
    require_tool_run ([ "search" ] + arguments + [ "--out-dir", folder ])
    # sorted: hits that rank alike come in no fixed order
    require_equal ( "the hits of 'search " + " " . join (arguments) + "'",
                    sorted ( hit_titles (folder, base_name) ), sorted (titles) )
  require_equal ( "the sidecar's request of a search with options",
                  read_sidecar (folder, "search_fre.") [ "request" ],
                  { "terms" : "fre.*", "regex" : True } )

def check_missing_id (scratch):
  """'forest' of an ID no node has, of a kind that saves a folder
  request, ends as 'view' of it does: a plain message, exit 1, nothing
  written."""
  folder = os . path . join (scratch, "missing-id")
  viewed = run_tool ([ "view", "nobody-id", "--out-dir", folder ])
  require_output ( viewed, "", expected_returncode = 1 )
  for kind in [ "mentioners", "aliases", "flags" ]:
    completed = run_tool (
      [ "forest", kind, "nobody-id", "--out-dir", folder ])
    require_output ( completed, "", expected_returncode = 1 )
    require_equal ( "'forest " + kind + "' of an absent ID, on stderr",
                    completed . stderr, viewed . stderr )
  if not viewed . stderr . startswith ("skg_for_ai: The server returned "):
    raise CaseFailure ( f"'view' of an absent ID said {viewed . stderr!r}" )

def check_linkstack (scratch):
  """The linkstack, a file in '--out-dir': pushed IDs take the node's
  title, a pushed link its label; 'paste' prints from the top entry,
  'pop' prints and removes it; an ID the graph lacks is refused."""
  folder = os . path . join (scratch, "linkstack")
  def linkstack (*arguments):
    return run_tool ( [ "linkstack" ] + list (arguments) + [ "--out-dir", folder ] )
  require_output ( linkstack ("show"), "The linkstack is empty.\n" )
  require_output ( linkstack ("push", "shared-id"),
                   "Pushed: 'Contained twice' (shared-id).\n" )
  require_output ( linkstack ("push", "[[id:root-id][the root]]"),
                   "Pushed: 'the root' (root-id).\n" )
  require_output ( linkstack ("show"),
                   "* the root\nroot-id\n* Contained twice\nshared-id\n" )
  require_output ( linkstack ("paste", "link"), "[[id:root-id][the root]]\n" )
  require_output ( linkstack ("paste", "id"), "root-id\n" )
  require_output ( linkstack ("pop", "node"), "the root ⟨node:root-id⟩\n" )
  require_output ( linkstack ("pop", "link"), "[[id:shared-id][Contained twice]]\n" )
  completed = linkstack ("pop", "id")
  require_output ( completed, "", expected_returncode = 1 )
  require_equal ( "popping an empty linkstack, on stderr", completed . stderr,
                  "skg_for_ai: The linkstack is empty.\n" )
  completed = linkstack ("push", "nobody-id")
  require_output ( completed, "", expected_returncode = 1 )
  require_equal ( "pushing an ID the graph lacks, on stderr", completed . stderr,
                  "skg_for_ai: The server gives no title for nobody-id: it "
                  "names no node of the graph.\n" )

def check_restriction_on_a_command_s_own_connection (scratch):
  """'restrict' prints the skgrepo restriction a connection starts with
  and the rungs; setting it needs a daemon, whose connection keeps
  it."""
  require_output ( run_tool ([ "restrict" ]),
                   "Skgrepo restriction: main (no restriction); available: "
                   "main (no restriction).\n" )
  completed = run_tool ([ "restrict", "main" ])
  require_output ( completed, "", expected_returncode = 1 )
  if "start a daemon" not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )

def failures_of_one_run (base_name, tool_arguments, expected_view_id,
                         output_folder, update_expected):
  completed = run_tool ( tool_arguments + [ "--out-dir", output_folder ] )
  if completed . returncode != 0:
    return [ f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" ]
  written = completed . stdout . split ()
  expected_written = [ os . path . join (output_folder, base_name + suffix)
                       for suffix in ( ".full.org", ".compact.org", ".view.json" ) ]
  if written != expected_written:
    return [ f"'{' ' . join (tool_arguments)}' printed {written}" ]
  sidecar = json . loads ( read_file ( expected_written [2] ))
  if sidecar [ "view_id" ] != expected_view_id:
    return [ f"{base_name}: the sidecar's view ID is {sidecar [ 'view_id' ]}" ]
  full_lines = read_file ( expected_written [0] ) . split ("\n")
  compact = read_file ( expected_written [1] )
  failures = failures_of_handles (base_name, full_lines, compact)
  expected_path = os . path . join (EXPECTED_FOLDER, base_name + ".compact.org")
  if update_expected:
    os . makedirs (EXPECTED_FOLDER, exist_ok = True)
    shutil . copyfile ( expected_written [1], expected_path )
  elif compact != read_file (expected_path):
    failures . append (
      f"{base_name}: the compact view differs from {expected_path}; "
      f"got {expected_written [1]}" )
  return failures

def failures_of_raw_and_import (scratch):
  failures = []
  raw = run_tool ([ "raw", '((request . "repos"))' ])
  expected_repos = ( '((response-type repos) (restriction "main") '
                     '(foreign-repos "shown") (repos ('
                     '((name "fred") (path "fred/public") (owned "false") (shown "true")) '
                     '((name "main") (path "me/main") (owned "true") (shown "true")))))' )
  if raw . returncode != 0 or raw . stdout . split ("\n") [1] != expected_repos:
    failures . append ( f"'raw' printed {raw . stdout!r}, {raw . stderr!r}" )
  # The compact views render the served rule table; the herald cases
  # above render the pinned one.
  rules = run_tool ([ "raw", '((request . "herald rules"))' ])
  served = heralds . herald_tables_of_response (
    sexp . parse_sexp ( rules . stdout . split ("\n", 1) [1] ))
  if served . rules != sexp . parse_sexp ( read_file (SHARED_HERALD_RULES) ):
    failures . append ( f"the served herald rules differ from {SHARED_HERALD_RULES}" )
  import_folder = os . path . join (scratch, "import")
  os . makedirs (import_folder)
  with open ( os . path . join (import_folder, "imported.md"), "w",
              encoding = "utf-8" ) as markdown_file:
    markdown_file . write ( "# An imported heading\n\nIts text.\n" )
  imported = run_tool ([ "import", import_folder, "main" ])
  if ( imported . returncode != 0
       or "(response-type import-md-and-org-result) (content \"Imported "
          not in imported . stdout ):
    failures . append (
      f"'import' printed {imported . stdout!r}, {imported . stderr!r}" )
  return failures

def failures_of_handles (base_name, full_lines, compact):
  """The compact tree has one line per full-view line, but the body
  lines of a headline whose handle says 'body=b', which it leaves out,
  and each headline's handle is the viewnode ID of the full view's
  headline on the same line."""
  tree_lines = [ line for line in compact . split ("\n") [ : -1 ]
                 if not line . startswith ("# ") ]
  full_lines = full_lines [ : -1 ] if full_lines [-1] == "" else full_lines
  restated_handles = { handle [-1] for handle in
                       ( HANDLE_PATTERN . findall (line) for line in tree_lines
                         if line . startswith ("*") and " body=b" in line )
                       if handle }
  kept_full_lines = []
  leaving_out = False
  for full_line in full_lines:
    if full_line . startswith ("*"):
      viewnode_id = VIEWNODE_ID_PATTERN . match (full_line)
      leaving_out = bool (viewnode_id) and viewnode_id . group (1) in restated_handles
      kept_full_lines . append (full_line)
    elif not leaving_out:
      kept_full_lines . append (full_line)
  full_lines = kept_full_lines
  failures = []
  if len (tree_lines) != len (full_lines):
    failures . append (
      f"{base_name}: {len (tree_lines)} tree lines for {len (full_lines)} full lines" )
  for line, full_line in zip (tree_lines, full_lines):
    if not line . startswith ("*"):
      continue
    handle = HANDLE_PATTERN . findall (line)
    viewnode_id = VIEWNODE_ID_PATTERN . match (full_line)
    if ( not handle or viewnode_id is None
         or handle [-1] != viewnode_id . group (1) ):
      failures . append (
        f"{base_name}: the handle of {line!r} is not the viewnode ID of "
        f"{full_line!r}" )
  return failures

def failures_of_changed_skg_files (data_root):
  """'links-here' sends a save, and 'import' adds files; neither may
  change a '.skg' file the fixture has."""
  failures = []
  for folder, _, file_names in os . walk (FIXTURE_FOLDER):
    for file_name in file_names:
      if not file_name . endswith (".skg"):
        continue
      fixture_path = os . path . join (folder, file_name)
      scratch_path = os . path . join (
        data_root, os . path . relpath (fixture_path, FIXTURE_FOLDER) )
      if read_file (fixture_path) != read_file (scratch_path):
        failures . append ( f"the runs changed {scratch_path}" )
  return failures
