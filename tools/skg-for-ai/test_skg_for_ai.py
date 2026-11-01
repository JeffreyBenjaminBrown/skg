#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

In order, it:
- checks the title normalizer and the fuzzy match of a new headline's
  title with a removed one's on pairs that must match, pairs that must
  not, and pairs at the thresholds;
- renders every rendering case both clients' tests share
  ('tests/shared/herald-rendering-cases.txt') as they render it, with
  the rule table and label table pinned beside it and each case's
  context, and compares the text;
- copies 'test-fixture/' to a scratch data root, starts a server
  there on port SKG_FOR_AI_TEST_PORT (default 1793), and runs each
  view command of the tool against it, 'forest' with every kind;
- compares each compact view with its file in 'test-expected/', and
  checks that every handle is the viewnode ID of its headline in the
  full view;
- sends a 'repos' and a 'herald rules' request through 'raw' (the
  served rule table is the pinned one), and imports a Markdown file
  through 'import';
- on a second scratch server, edits compact views and saves them:
  a title, a body, a new child, a move, a removal, a deletion, bad
  handles, an edit the server refuses, a stale view the server
  refuses, 'new', one view over several saves, whose handles stay,
  and retyped headlines (an exact retype refused, then saved marked
  '⟨new⟩'; a reworded one asked about, then answered '--keep-ids',
  which keeps its node, or '--all-new', which makes a new one),
  checking each report exactly and the graph after each save;
- on a third scratch server, whose owned skgrepo is a gitrepo with the
  fixture committed: saves a retitle, a new child and a deletion,
  checks the diff view's compact buffer exactly, marks the new child
  ready, restores the deleted node, and commits (refused unconfirmed,
  then confirmed with '--yes'), checking 'git log' and that the diff
  view is then empty; then integrates a change a diff buffer does not
  make editable, and saves an edit of it; a blank commit message is
  refused unsent;
- on a fourth scratch server, also over a gitrepo, runs a daemon:
  commands through it write what they write on their own connections;
  it rewrites a held view after a save through it changes a node the
  view shows, and after a second server over the same data root saves
  one (the first server's watcher integrates the file), logging each,
  but keeps a view with unsaved edits; it logs the warning of a file
  the watcher cannot read; a command for its port writing into another
  folder runs through it, and that view is rewritten after a save; a
  second daemon for the port is refused; 'commit --yes' through it, and
  an interactive 'commit' beside it, each rewrite its held buffer of
  the diff view; 'close' and 'daemon --stop' release its views; a
  socket a killed daemon left is replaced, and SIGTERM stops a daemon.
'--update-expected' rewrites the expected files instead of comparing.
"""

import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time

TOOL_FOLDER = os . path . dirname ( os . path . abspath (__file__) )
REPO_ROOT = os . path . dirname ( os . path . dirname (TOOL_FOLDER) )
sys . dont_write_bytecode = True
sys . path . insert (0, TOOL_FOLDER)
import skg_for_ai

FIXTURE_FOLDER = os . path . join (TOOL_FOLDER, "test-fixture")
EXPECTED_FOLDER = os . path . join (TOOL_FOLDER, "test-expected")
SHARED_HERALD_CASES = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-rendering-cases.txt" )
# The rule table and label table the clients' tests render the cases
# with, as the server serves them.
SHARED_HERALD_RULES = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-rules.sexp" )
SHARED_SKGREPO_LABELS = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-skgrepo-labels.sexp" )
SERVER_BINARY = os . environ . get (
  "SKG_BINARY", os . path . join (REPO_ROOT, "target", "debug", "skg") )
PORT = int ( os . environ . get ( "SKG_FOR_AI_TEST_PORT", "1793" ))
SERVER_READY_TIMEOUT_SECONDS = 60
HANDLE_PATTERN = re . compile (r'⟨([0-9a-z]+)[ ⟩]')
VIEWNODE_ID_PATTERN = re . compile (r'^\*+ \(skg \(vnid ([0-9a-z]+)\)')

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
                         ( "subscribesTo",  "fred-note-id" ),
                         ( "overrides",     "overridden-id" ),
                         ( "hidesFromSubs", "fred-aside-id" ),
                         ( "aliases",       "overridden-id" ),
                         ( "flags",         "overrider-id" ) ] ]

def main (argv):
  update_expected = "--update-expected" in argv
  # The tool's commands find a daemon by a socket in XDG_RUNTIME_DIR,
  # and take a port from SKG_PORT: the test's own folder, and no
  # SKG_PORT, keep its commands and daemons apart from any others.
  os . environ [ "XDG_RUNTIME_DIR" ] = tempfile . mkdtemp (
    prefix = "skg-for-ai-test-runtime-" )
  os . environ . pop ( "SKG_PORT", None )
  failures = []
  failures += failures_of_title_matching ()
  failures += failures_of_shared_herald_cases ()
  failures += failures_against_scratch_servers (update_expected)
  shutil . rmtree ( os . environ [ "XDG_RUNTIME_DIR" ], ignore_errors = True )
  for failure in failures:
    print ( "FAIL: " + failure )
  if failures:
    sys . exit (1)
  print ( "PASS" + ( " (expected files rewritten)" if update_expected else "" ))

# ============================================================
# Retyped headlines: the title normalizer and the fuzzy match
# ============================================================

# (TITLE, its normalized words)
NORMALIZED_TITLES = [
  ( "A save writes the skg files", [ "sav", "writ", "skg", "fil" ] ),
  ( "Saving: it's what the server does!", [ "sav", "server" ] ),
  ( "Fixing the stale-save check", [ "fix", "stal", "sav", "check" ] ),
  ( "Boxes, classes and the class", [ "box", "class", "class" ] ),
  ( "Really quickly used", [ "real", "quick", "used" ] ),
  ( "Is it a thing of theirs?", [ "thing" ] ),
  ( "", [] ) ]

# (NEW TITLE, REMOVED TITLE, whether the fuzzy match asks about them)
TITLE_MATCH_CASES = [
  # Reworded: the same statement
  ( "A save writes the skg files", "Saving writes skg files", True ),
  ( "Foreign repos are removed from the privacy ladder",
    "Foreign repos leave the privacy ladder", True ),
  ( "Fixing the stale save check", "Fix the stale-save check", True ),
  ( "Retyping a headline loses the node ID",
    "A retyped headline loses its ID", True ),
  ( "The handle is the viewnode ID", "Handles are viewnode IDs", True ),
  ( "Watching each repo directory for changes",
    "Watch every repo directory for changes", True ),
  ( "The background worker commits Tantivy",
    "Tantivy commits in the background", True ),
  ( "Ask Jeff about the port", "Ask Jeff about ports", True ),
  # Unrelated
  ( "The diff view shows every change",
    "Foreign repos leave the privacy ladder", False ),
  ( "Sell the car", "Buy milk", False ),
  ( "Tests must not share fixture directories", "Tests run in parallel", False ),
  ( "Tantivy updates after a save", "Tantivy commits in the background", False ),
  ( "Annual budget audit", "Quarterly budget review", False ),
  # Equal titles are the exact case, not the fuzzy one
  ( "Fred likes apples", "Fred likes apples", False ),
  # At the thresholds: Dice 0.6 (ratio 0.70) asks, Dice 0.57 (ratio
  # 0.69) does not; ratio 0.75 (Dice 0) asks, ratio 0.69 does not;
  # Dice 0.5 with ratio 0.61 does not
  ( "one two three six seven", "one two three four five", True ),
  ( "red green yellow purple", "red green blue", False ),
  ( "abcdefxy kmn pqr", "abcdefgh kmo pqs", True ),
  ( "abcdexyz kmn pqr", "abcdefgh kmo pqs", False ),
  ( "Second child", "First child", False ),
  # The ratio alone counts only when each title has three normalized
  # words: misspellings in three words ask (Dice 0.5 and 0.33, ratio
  # 0.93 and 0.9), in two do not (ratio 0.92), nor two words beside
  # three (ratio 0.87), nor the short titles that prompted the rule
  # (Dice 0.5, ratio 0.76), nor one word (ratio 0.75)
  ( "Recieve the quarterly budjet report",
    "Receive the quarterly budget report", True ),
  ( "Recieve the budjet report", "Receive the budget report", True ),
  ( "budjet report", "budget reports", False ),
  ( "Recieve the budjet report", "Receive the budgetreport", False ),
  ( "Budget meeting", "Budget notes", False ),
  ( "abcdefxy", "abcdefgh", False ) ]

def failures_of_title_matching ():
  failures = []
  for title, words in NORMALIZED_TITLES:
    if skg_for_ai . normalized_title_words (title) != words:
      failures . append (
        f"{title!r} normalizes to "
        f"{skg_for_ai . normalized_title_words (title)}, not {words}" )
  for new_title, removed_title, asks in TITLE_MATCH_CASES:
    if skg_for_ai . titles_probably_reworded (new_title, removed_title) != asks:
      failures . append (
        f"{new_title!r} / {removed_title!r}: the fuzzy match "
        + ( "does not ask" if asks else "asks" ) + ", with (Dice, ratio) "
        f"{skg_for_ai . title_similarity (new_title, removed_title)}" )
  dice, ratio = skg_for_ai . title_similarity (
    "one two three six seven", "one two three four five" )
  if dice != 0.6 or ratio >= skg_for_ai . REWORDED_RATIO_THRESHOLD:
    failures . append ( f"the Dice threshold case gives {( dice, ratio )}" )
  dice, ratio = skg_for_ai . title_similarity (
    "abcdefxy kmn pqr", "abcdefgh kmo pqs" )
  if dice != 0 or ratio != 0.75:
    failures . append ( f"the ratio threshold case gives {( dice, ratio )}" )
  return failures + failures_of_reworded_pairing ()

def failures_of_reworded_pairing ():
  """Pairs are one to one, best first: each new headline gets its best
  removed one still unpaired, and names the others it resembles. A
  headline marked '⟨new⟩' is never paired, and carries no other word."""
  original = [ skg_for_ai . OutlineHeadline ( handle, 1, title, [], None )
               for handle, title in [ ( "0a", "Fred likes apples and pears" ),
                                      ( "0b", "Fred likes apples" ),
                                      ( "0c", "Unrelated" ) ] ]
  edited = skg_for_ai . parse_edited_compact_view (
    "* Unrelated ⟨0c⟩\n"
    "* Fred likes apples and plums\n"
    "* Fred likes apples, pears\n"
    "* Fred likes pears and apples ⟨new⟩\n"
    "* Fred likes plums ⟨new delete⟩\n" )
  failures = []
  pairs = [ ( pair . new_index, pair . removed_handle,
              pair . other_removed_handles )
            for pair in skg_for_ai . reworded_headline_pairs (edited, original) ]
  if pairs != [ ( 1, "0b", [ "0a" ] ), ( 2, "0a", [ "0b" ] ) ]:
    failures . append ( f"the reworded pairs are {pairs}" )
  errors = skg_for_ai . handle_error_lines (
    edited [ 1 : ], { "0c" : original [2] } )
  if errors != [ "Error: ⟨new⟩ on line 5 of the compact view carries "
                 "'delete'; it takes no other word." ]:
    failures . append ( f"the '⟨new⟩' errors are {errors}" )
  return failures

# ============================================================
# Heralds: every rendering case both clients' tests share, rendered
# as they render it, with the rule table and label table pinned in
# 'tests/shared/', and each case's context as they supply it.
# ============================================================

def failures_of_shared_herald_cases ():
  herald_tables = skg_for_ai . HeraldTables (
    skg_for_ai . parse_sexp ( read_file (SHARED_HERALD_RULES) ),
    { skg_for_ai . sexp_to_text ( entry [0] ) :
        skg_for_ai . sexp_to_text ( entry [1] )
      for entry in skg_for_ai . parse_sexp (
        read_file (SHARED_SKGREPO_LABELS) ) [ 1 : ] } )
  failures = []
  cases = shared_herald_cases ()
  for name, metadata_text, text, context in cases:
    rendered = skg_for_ai . herald_text (
      skg_for_ai . parse_sexp (metadata_text), herald_tables,
      skg_for_ai . HeraldContext (
        { "folderChange ready" : True,
          "folderChange unready" : False } . get (context),
        context == "pendingDetachment" ))
    if rendered != text:
      failures . append (
        f"herald case '{name}': rendered '{rendered}', expected '{text}'" )
  if len (cases) <= 40:
    failures . append ( f"only {len (cases)} shared herald cases were read" )
  return failures

def shared_herald_cases ():
  """[(NAME, METADATA, TEXT, CONTEXT)] for each case, read as the
  clients' tests read it: a case ends at its styles line, and CONTEXT
  is the text of its '---- context: ' line, or None."""
  cases = []
  name, metadata, text, context = None, None, None, None
  with open (SHARED_HERALD_CASES, encoding = "utf-8") as cases_file:
    for line in cases_file . read () . split ("\n"):
      if line . startswith ("==== "):
        name, metadata, text, context = line [ 5 : ], None, None, None
      elif line . startswith ("---- context: "):
        context = line [ 14 : ]
      elif line . startswith ("---- text: "):
        text = line [ 11 : ]
      elif line . startswith ("---- styles: "):
        cases . append (( name, metadata, text, context ))
        name = None
      elif name and metadata is None:
        metadata = line
  return cases

# ============================================================
# The scratch server
# ============================================================

def failures_against_scratch_servers (update_expected):
  """The reading checks on one scratch server, then the writing
  checks on another, then the reviewing checks on a third, then the
  daemon's on a fourth, the last two with their owned skgrepo a
  gitrepo; each on its own copy of the fixture."""
  if not os . path . exists (SERVER_BINARY):
    return [ f"no server binary at {SERVER_BINARY}; run 'cargo build --bin skg'" ]
  return ( failures_on_a_scratch_server (
             lambda scratch, data_root :
               failures_of_reading (scratch, data_root, update_expected) )
           + failures_on_a_scratch_server (failures_of_writing)
           + failures_on_a_scratch_server (
               failures_of_reviewing, owned_skgrepo_in_git = True )
           + failures_on_a_scratch_server (
               failures_of_daemon, owned_skgrepo_in_git = True ) )

def failures_on_a_scratch_server (check, owned_skgrepo_in_git = False):
  """CHECK (SCRATCH, DATA_ROOT)'s failures, run against a server on a
  fresh copy of the fixture in the scratch folder SCRATCH. With
  OWNED_SKGREPO_IN_GIT, the owned skgrepo is first made a gitrepo
  whose HEAD commit holds the fixture's files, as the diff view
  needs."""
  if port_is_listening (PORT):
    return [ f"port {PORT} is taken; set SKG_FOR_AI_TEST_PORT" ]
  scratch = tempfile . mkdtemp ( prefix = "skg-for-ai-test-" )
  data_root = os . path . join (scratch, "data")
  shutil . copytree (FIXTURE_FOLDER, data_root)
  set_port_in_config ( os . path . join (data_root, "skgconfig.toml"), PORT )
  if owned_skgrepo_in_git:
    commit_fixture_in_new_gitrepo ( os . path . join (data_root, "me", "main") )
  server_log_path = os . path . join (scratch, "server.log")
  with open (server_log_path, "w") as server_log:
    server = subprocess . Popen (
      [ SERVER_BINARY, "skgconfig.toml" ], cwd = data_root,
      stdout = server_log, stderr = subprocess . STDOUT )
  try:
    if not wait_for_server_ready (server_log_path, server):
      return [ f"the scratch server did not become ready; see {server_log_path}" ]
    failures = check (scratch, data_root)
  finally:
    server . terminate ()
    server . wait (timeout = 30)
  if failures:
    print ( f"Scratch files kept in {scratch}" )
  else:
    shutil . rmtree (scratch)
  return failures

def failures_of_reading (scratch, data_root, update_expected):
  output_folder = os . path . join (scratch, "out")
  failures = []
  for base_name, tool_arguments, view_id in TOOL_RUNS:
    failures += failures_of_one_run (
      base_name, tool_arguments, view_id, output_folder, update_expected )
  failures += failures_of_raw_and_import (scratch)
  failures += failures_of_changed_skg_files (data_root)
  return failures

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
  served = skg_for_ai . herald_tables_of_response (
    skg_for_ai . parse_sexp ( rules . stdout . split ("\n", 1) [1] ))
  if served . rules != skg_for_ai . parse_sexp ( read_file (SHARED_HERALD_RULES) ):
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

# ============================================================
# Writing: each case views the root afresh, edits the compact view,
# saves it, and checks the report and the graph. The cases run in
# order on one server, each on the graph the earlier ones left.
# ============================================================

class CaseFailure (Exception):
  pass

ROOT_BASE_NAME = "view_root-id"
REPORT_HEAD = ( "Changes in view_root-id.compact.org, against "
                "view_root-id.full.org (view ID content-view:root-id):\n" )
NOT_SAVED = "Not saved. Run again with '--yes' to save.\n"
DANGLING_LINK_WARNING = (
  "Server warning: Dangling link: node root-id links to absent id "
  "missing-id (label \"a node nobody wrote\").\n" )
UUID_PATTERN = re . compile (
  r'[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' )

def failures_of_writing (scratch, data_root):
  main_repo = os . path . join (data_root, "me", "main")
  context = { "scratch" : scratch, "main_repo" : main_repo }
  for case in [ check_title_edit, check_body_edit, check_new_child,
                check_move, check_removal, check_deletion,
                check_bad_handles, check_server_refusal, check_stale_view,
                check_new_root, check_handles_across_saves,
                check_exact_retype, check_reworded_retype_asked,
                check_reworded_retype_kept, check_reworded_retype_all_new ]:
    try:
      case (context)
    except CaseFailure as failure:
      # Later cases build on this one's graph.
      return [ case . __name__ + ": " + str (failure) ]
  return []

def check_title_edit (context):
  folder = fresh_root_view (context, "title")
  edit_compact ( folder, [ ( "** First child ⟨0d aC1⟩",
                             "** First child, renamed ⟨0d aC1⟩" ) ])
  report = ( REPORT_HEAD +
             "Title: ⟨0d⟩ 'First child' → 'First child, renamed'.\n" )
  require_output ( run_save (folder), report + NOT_SAVED )
  require_equal ( "the title before '--yes'",
                  skg_title (context, "child-a-id"), "First child" )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  require_equal ( "the saved title",
                  skg_title (context, "child-a-id"), "First child, renamed" )
  require_in_compact ( folder, "** First child, renamed ⟨0d aC1⟩\n" )

def check_body_edit (context):
  folder = fresh_root_view (context, "body")
  edit_compact ( folder, [ ( "⟦✗ missing⟧.\n",
                             "⟦✗ missing⟧.\nA new last line.\n" ) ])
  report = REPORT_HEAD + "Bodies changed: 1 (⟨0a⟩).\n"
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (1) + DANGLING_LINK_WARNING
                   + written_paths (folder) )
  require_equal ( "the saved body", skg_body (context, "root-id"),
                  [ "The root links to [[id:child-b-id][its second child]],",
                    "to [[id:fred-note-id][Fred's note]] in the foreign repo,",
                    "and to [[id:missing-id][a node nobody wrote]].",
                    "A new last line." ] )

def check_new_child (context):
  folder = fresh_root_view (context, "new")
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2aC⟩\n",
                             "*** Contained twice ⟨0e 2aC⟩\n"
                             "*** A new grandchild\n" ) ])
  report = ( REPORT_HEAD +
             "New: 'A new grandchild', under ⟨0d⟩ 'First child, renamed'.\n" )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  new_id = id_of_compact_headline ( folder, "*** A new grandchild" )
  context [ "new_id" ] = new_id
  require_equal ( "the new child's container",
                  skg_list (context, "child-a-id", "contains"),
                  [ "shared-id", new_id ] )
  require_equal ( "the new child's title",
                  skg_title (context, new_id), "A new grandchild" )

def check_move (context):
  folder = fresh_root_view (context, "move")
  edit_compact ( folder, [
    ( "*** Contained twice ⟨0e 2aC⟩\n", "" ),
    ( "***** An aside of Fred's ⟨0k ☮ cH ⌂:fred folded⟩\n",
      "***** An aside of Fred's ⟨0k ☮ cH ⌂:fred folded⟩\n"
      "*** Contained twice ⟨0e 2aC⟩\n" ) ])
  report = ( REPORT_HEAD +
             "Moved: ⟨0e⟩ 'Contained twice', from under ⟨0d⟩ 'First child, "
             "renamed' to under ⟨0g⟩ 'Second child, a subscriber'.\n" )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  require_equal ( "the old container's content",
                  skg_list (context, "child-a-id", "contains"),
                  [ context [ "new_id" ] ] )
  require_equal ( "the new container's content",
                  skg_list (context, "child-b-id", "contains"),
                  [ "shared-id" ] )

def check_removal (context):
  folder = fresh_root_view (context, "remove")
  edit_compact ( folder, [ ( "*** Contained twice ⟨0k 2aC⟩\n", "" ) ])
  report = ( REPORT_HEAD +
             "Removed: ⟨0k⟩ 'Contained twice', from under ⟨0f⟩ 'Second "
             "child, a subscriber'.\n" )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  require_equal ( "the container's content after the removal",
                  skg_list (context, "child-b-id", "contains"), [] )
  # The node still exists, and its other container still holds it.
  shared_folder = os . path . join (
    context [ "scratch" ], "write", "remove-shared" )
  require_tool_run ([ "view", "shared-id", "--out-dir", shared_folder,
                      "--no-summary" ])
  require_equal (
    "the removed node's fresh view",
    read_file ( os . path . join ( shared_folder,
                                   "view_shared-id.compact.org" )),
    "* Contained twice ⟨0a 1C ⌂:main⟩\n"
    "** ⟨0b ☮ These contain it.⟩\n"
    "*** A second container, outside the view ⟨0c ☮ Cb⟩\n" )

def check_deletion (context):
  folder = fresh_root_view (context, "delete")
  edit_compact ( folder, [ ( "*** A new grandchild ⟨0e⟩",
                             "*** A new grandchild ⟨0e delete⟩" ) ])
  report = REPORT_HEAD + "Delete: ⟨0e⟩ 'A new grandchild', from the graph.\n"
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  require_equal ( "the deleted node's file",
                  os . path . exists ( skg_path (context, context [ "new_id" ]) ),
                  False )
  require_equal ( "its container's content",
                  skg_list (context, "child-a-id", "contains"), [] )
  if "A new grandchild" in read_compact (folder):
    raise CaseFailure ( "the re-rendered view still shows the deleted node" )

def check_bad_handles (context):
  folder = fresh_root_view (context, "bad-handle")
  edit_compact ( folder, [ ( "⟨0a 1C2 L2 ⌂:main⟩",
                             "⟨0a 1C2 L2 ⌂:main all delete restoreNode⟩" ),
                           ( "** First child, renamed ⟨0d⟩",
                             "** First child, renamed ⟨0z⟩" ),
                           ( "⟨0c ☮ Cb⟩", "⟨zz ☮ Cb⟩" ) ])
  graph_before = skg_files (context)
  full_before = read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") )
  completed = run_save (folder, "--yes")
  require_output (
    completed,
    REPORT_HEAD +
    "Error: ⟨0a⟩ on line 1 of the compact view carries 'delete' and "
    "'restoreNode'; a headline takes one edit request.\n"
    "Error: ⟨0a⟩ on line 1 of the compact view carries 'all' without "
    "'ready' or 'unready'.\n"
    "Error: ⟨zz⟩ on line 7 of the compact view names no headline of the "
    "full view.\n"
    "Error: ⟨0z⟩ on line 8 of the compact view names no headline of the "
    "full view.\n",
    expected_returncode = 1 )
  if "Nothing was saved: some handles are in error" not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  require_equal ( "the graph after a refused save",
                  skg_files (context), graph_before )
  require_equal ( "the full view after a refused save",
                  read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") ),
                  full_before )

def check_server_refusal (context):
  """A write-protected occurrence saves nothing, so the server refuses
  a save that retitles one; the tool prints its errors and leaves the
  files."""
  folder = fresh_root_view (context, "refused")
  edit_compact ( folder, [ ( "*** Above the root ⟨0c ☮ Cb⟩",
                             "*** Above the root, retitled ⟨0c ☮ Cb⟩" ) ])
  graph_before = skg_files (context)
  full_before = read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") )
  completed = run_save (folder, "--yes")
  expected_head = (
    REPORT_HEAD +
    "Title: ⟨0c⟩ 'Above the root' → 'Above the root, retitled'.\n"
    "Server: nothing was saved, 1 error, 0 warnings.\n"
    "Server error: " )
  if ( completed . returncode != 1
       or not completed . stdout . startswith (expected_head)
       or "The server refused the save" not in completed . stderr ):
    raise CaseFailure (
      f"'save' printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "the graph after the server refused",
                  skg_files (context), graph_before )
  require_equal ( "the full view after the server refused",
                  read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") ),
                  full_before )

def check_stale_view (context):
  """View A is rendered, then view B saves a change to a node A shows.
  Saving A would write A's older text of that node, so the server
  refuses the whole save, naming the node, and nothing is written; the
  tool prints the refusal and leaves A's files. A fresh view saves the
  same edit, and B's change stays."""
  folder_a = fresh_root_view (context, "stale-a")
  folder_b = fresh_root_view (context, "stale-b")
  edit_compact ( folder_b, [ ( "** Second child, a subscriber ⟨0e ",
                               "** Second child, renamed in B ⟨0e " ) ])
  require_save_succeeds (folder_b)
  a_edit = [ ( "** First child, renamed ⟨0d⟩",
               "** First child, renamed in A ⟨0d⟩" ) ]
  edit_compact (folder_a, a_edit)
  graph_before = skg_files (context)
  full_before = read_file ( os . path . join (folder_a, ROOT_BASE_NAME + ".full.org") )
  completed = run_save (folder_a, "--yes")
  expected_head = (
    REPORT_HEAD +
    "Title: ⟨0d⟩ 'First child, renamed' → 'First child, renamed in A'.\n"
    "Server: nothing was saved, 1 error, 0 warnings.\n"
    "Server error: NOTHING WAS SAVED" )
  if ( completed . returncode != 1
       or not completed . stdout . startswith (expected_head)
       or "changed after the buffer was rendered" not in completed . stdout
       or "- ID: child-b-id" not in completed . stdout
       or "The server refused the save" not in completed . stderr ):
    raise CaseFailure (
      f"'save' printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "the graph after the stale save",
                  skg_files (context), graph_before )
  require_equal ( "A's full view after the stale save",
                  read_file ( os . path . join (folder_a, ROOT_BASE_NAME + ".full.org") ),
                  full_before )
  folder_fresh = fresh_root_view (context, "stale-a-again")
  edit_compact (folder_fresh, a_edit)
  require_save_succeeds (folder_fresh)
  require_equal ( "A's edit", skg_title (context, "child-a-id"),
                  "First child, renamed in A" )
  require_equal ( "B's title after A's edit",
                  skg_title (context, "child-b-id"), "Second child, renamed in B" )

def check_new_root (context):
  """'new' refuses a foreign skgrepo without saving; in an owned one
  it creates a root with no container, and its view continues the
  edit-and-save loop: a body and a child are saved from it."""
  folder = os . path . join ( context [ "scratch" ], "write", "new-root" )
  files_before = skg_files (context)
  refused = run_tool ([ "new", "A new root", "fred", "--out-dir", folder ])
  if ( refused . returncode != 1 or refused . stdout
       or "'fred' is not an owned skgrepo" not in refused . stderr ):
    raise CaseFailure (
      f"'new' in 'fred' gave exit {refused . returncode}, printed "
      f"{refused . stdout!r}, stderr {refused . stderr!r}" )
  require_equal ( "the owned files after the refused 'new'",
                  skg_files (context), files_before )
  created = require_tool_run (
    [ "new", "A new root", "main", "--out-dir", folder, "--no-summary" ])
  printed = created . stdout . split ("\n")
  require_equal ( "what 'new' printed first", printed [0], saved_line (0) . strip () )
  if not printed [1] . startswith ("New node: "):
    raise CaseFailure ( f"'new' printed {created . stdout!r}" )
  new_id = printed [1] [ len ("New node: ") : ]
  base = os . path . join ( folder, "view_" + new_id )
  require_equal ( "the paths 'new' printed", printed [ 2 : ],
                  [ base + ".full.org", base + ".compact.org",
                    base + ".view.json", "" ] )
  require_equal ( "the new root's title",
                  skg_title (context, new_id), "A new root" )
  compact_path = base + ".compact.org"
  require_equal ( "the new root's compact view",
                  read_file (compact_path), "* A new root ⟨0a ⌂:main⟩\n" )
  with open (compact_path, "w", encoding = "utf-8") as compact_file:
    compact_file . write ( "* A new root ⟨0a⟩\nIts body.\n** Its child\n" )
  require_save_succeeds (folder)
  require_equal ( "the new root's body",
                  skg_body (context, new_id), [ "Its body." ] )
  [ child_id ] = skg_list (context, new_id, "contains")
  require_equal ( "the new root's child's title",
                  skg_title (context, child_id), "Its child" )

def check_handles_across_saves (context):
  """A compact view edited and saved again and again keeps its
  handles: the server, which does not hold the view on the save's
  connection, keeps every viewnode ID the saved buffer carries. A new
  headline gets a new handle. A removal renumbers nothing."""
  folder = fresh_root_view (context, "handles")
  rendered = handles_and_titles (folder)
  first_child = compact_line_starting (folder, "** First child")
  second_child = compact_line_starting (folder, "** Second child")
  edit_compact ( folder, [
    ( first_child, "** Added before the first child\n" + first_child ),
    ( second_child, second_child . replace (
        "Second child, renamed in B", "Second child, retitled" )) ])
  require_save_succeeds (folder)
  after_first_save = handles_and_titles (folder)
  require_kept_handles ( "the first save", rendered, after_first_save,
                         { "Second child, renamed in B" :
                             "Second child, retitled" },
                         [ "Added before the first child" ] )
  added = compact_line_starting (folder, "** Added before the first child")
  containers_folder = compact_line_starting (folder, "** ⟨")
  edit_compact ( folder, [
    ( containers_folder, "** Added at the top\n" + containers_folder ),
    ( added, added . replace ( "Added before the first child",
                               "Added before the first child, retitled" )) ])
  require_save_succeeds (folder)
  after_second_save = handles_and_titles (folder)
  require_kept_handles ( "the second save", after_first_save,
                         after_second_save,
                         { "Added before the first child" :
                             "Added before the first child, retitled" },
                         [ "Added at the top" ] )
  edit_compact ( folder, [ ( compact_line_starting (
    folder, "** Added before the first child, retitled" ) + "\n", "" ) ])
  require_save_succeeds (folder)
  after_removal = handles_and_titles (folder)
  require_kept_handles ( "the save of a removal",
                         { handle : title for handle, title
                           in after_second_save . items ()
                           if title != "Added before the first child, retitled" },
                         after_removal, {}, [] )
  added_at_top = compact_line_starting (folder, "** Added at the top")
  edit_compact ( folder, [ ( added_at_top, added_at_top . replace (
    "Added at the top", "Added at the top, retitled" )
    + "\n** Added after a removal" ) ])
  require_save_succeeds (folder)
  require_kept_handles ( "the save after a removal", after_removal,
                         handles_and_titles (folder),
                         { "Added at the top" : "Added at the top, retitled" },
                         [ "Added after a removal" ] )

def check_exact_retype (context):
  """A headline removed and retyped elsewhere without its handle is
  refused, with or without '--yes'; marked '⟨new⟩', it saves as a new
  node, and the removed one keeps its own."""
  folder = fresh_root_view (context, "retype-exact")
  removed_id = id_of_compact_headline (folder, "** Added after a removal")
  first_child_body = "A body line that stays unnumbered.\n"
  edit_compact ( folder, [
    ( "** Added after a removal ⟨0e⟩\n", "" ),
    ( first_child_body, first_child_body + "*** Added after a removal\n" ) ])
  graph_before = skg_files (context)
  refusal = (
    REPORT_HEAD +
    "Error: the new headline 'Added after a removal' on line 11 of the "
    "compact view has the title of the removed ⟨0e⟩ 'Added after a "
    "removal'. If it is that node, moved or reworded, end it with ⟨0e⟩; "
    "if it is another node, end it with '⟨new⟩'.\n" )
  for flags in [ [], [ "--yes" ] ]:
    completed = run_save (folder, *flags)
    require_output ( completed, refusal, expected_returncode = 1 )
    if "Nothing was saved: some handles are in error" not in completed . stderr:
      raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  require_equal ( "the graph after a refused retype",
                  skg_files (context), graph_before )
  edit_compact ( folder, [ ( "*** Added after a removal\n",
                             "*** Added after a removal ⟨new⟩\n" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Removed: ⟨0e⟩ 'Added after a removal', from under ⟨0a⟩ 'The fixture "
    "root'.\n"
    "New: 'Added after a removal', under ⟨0f⟩ 'First child, renamed in A'.\n"
    + saved_line (1) + DANGLING_LINK_WARNING + written_paths (folder) )
  new_id = id_of_compact_headline (folder, "*** Added after a removal")
  require_equal ( "whether the '⟨new⟩' headline is a new node",
                  new_id != removed_id, True )
  require_equal ( "the new node's container",
                  skg_list (context, "child-a-id", "contains"), [ new_id ] )
  require_equal ( "the removed node's title",
                  skg_title (context, removed_id), "Added after a removal" )
  if removed_id in skg_list (context, "root-id", "contains"):
    raise CaseFailure ( "the root still contains the removed node" )

REWORDED_TITLE = "The headline added at the top, retitled again"
REWORDED_QUESTION = (
  "Question: removed ⟨0d⟩ 'Added at the top, retitled' / new '" +
  REWORDED_TITLE + "' (line 10): probably the same statement reworded; "
  "keep its ID?\n" )
REWORDED_NOT_SAVED = (
  "Not saved. Answer the questions with '--keep-ids' or '--all-new', or on "
  "each new headline (its removed handle, or '⟨new⟩'), and run again with "
  "'--yes' to save.\n" )

def check_reworded_retype_asked (context):
  """A headline removed and retyped elsewhere, reworded, is asked
  about, and '--yes' alone does not answer: nothing is saved."""
  folder = fresh_root_view (context, "retype-reworded")
  context [ "reworded_folder" ] = folder
  context [ "reworded_id" ] = id_of_compact_headline (
    folder, "** Added at the top, retitled" )
  edit_compact ( folder, [
    ( "** Added at the top, retitled ⟨0d⟩\n", "" ),
    ( "*** Added after a removal ⟨",
      "*** " + REWORDED_TITLE + "\n*** Added after a removal ⟨" ) ])
  report = (
    REPORT_HEAD +
    "Removed: ⟨0d⟩ 'Added at the top, retitled', from under ⟨0a⟩ 'The "
    "fixture root'.\n"
    "New: '" + REWORDED_TITLE + "', under ⟨0e⟩ 'First child, renamed in "
    "A'.\n" + REWORDED_QUESTION + REWORDED_NOT_SAVED )
  graph_before = skg_files (context)
  require_output ( run_save (folder), report )
  completed = run_save (folder, "--yes")
  require_output ( completed, report, expected_returncode = 1 )
  if "'--yes' does not answer that" not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  require_equal ( "the graph after an unanswered question",
                  skg_files (context), graph_before )

def check_reworded_retype_kept (context):
  """Answered '--keep-ids', the reworded headline takes the removed
  one's handle: the report shows a move and a retitle, and the save
  keeps the node, its ID and its file, under the new title."""
  folder = context [ "reworded_folder" ]
  node_id = context [ "reworded_id" ]
  report = (
    REPORT_HEAD +
    "Moved: ⟨0d⟩ 'Added at the top, retitled', from under ⟨0a⟩ 'The fixture "
    "root' to under ⟨0e⟩ 'First child, renamed in A'.\n"
    "Title: ⟨0d⟩ 'Added at the top, retitled' → '" + REWORDED_TITLE + "'.\n" )
  require_output ( run_save (folder, "--keep-ids"), report + NOT_SAVED )
  files_before = set ( skg_files (context) )
  require_output ( run_save (folder, "--keep-ids", "--yes"),
                   report + saved_line (1) + DANGLING_LINK_WARNING
                   + written_paths (folder) )
  require_equal ( "the owned files after '--keep-ids'",
                  set ( skg_files (context) ), files_before )
  require_equal ( "the kept node's title",
                  skg_title (context, node_id), REWORDED_TITLE )
  require_equal ( "the kept node's new container",
                  skg_list (context, "child-a-id", "contains") [0], node_id )
  require_equal ( "the kept node in the re-rendered view",
                  id_of_compact_headline (folder, "*** " + REWORDED_TITLE),
                  node_id )

def check_reworded_retype_all_new (context):
  """Answered '--all-new', the reworded headline is a new node, and
  the removed one keeps its ID and title."""
  folder = fresh_root_view (context, "retype-all-new")
  node_id = context [ "reworded_id" ]
  new_title = "The headline added at the top and retitled again"
  edit_compact ( folder, [
    ( compact_line_starting ( folder, "*** " + REWORDED_TITLE ) + "\n", "" ),
    ( "** Second child, retitled ⟨",
      "** " + new_title + "\n** Second child, retitled ⟨" ) ])
  question = (
    "Question: removed ⟨0e⟩ '" + REWORDED_TITLE + "' / new '" + new_title +
    "' (line 11): probably the same statement reworded; keep its ID?\n" )
  if question not in run_save (folder) . stdout:
    raise CaseFailure ( f"no question {question!r}" )
  files_before = set ( skg_files (context) )
  require_output (
    run_save (folder, "--all-new", "--yes"),
    REPORT_HEAD +
    "Removed: ⟨0e⟩ '" + REWORDED_TITLE + "', from under ⟨0d⟩ 'First "
    "child, renamed in A'.\n"
    "New: '" + new_title + "', under ⟨0a⟩ 'The fixture root'.\n"
    + saved_line (1) + DANGLING_LINK_WARNING + written_paths (folder) )
  new_id = id_of_compact_headline (folder, "** " + new_title)
  require_equal ( "the files '--all-new' added",
                  set ( skg_files (context) ) - files_before,
                  { new_id + ".skg" } )
  require_equal ( "the removed node's title",
                  skg_title (context, node_id), REWORDED_TITLE )
  if node_id in skg_list (context, "child-a-id", "contains"):
    raise CaseFailure ( "the first child still contains the removed node" )

def handles_and_titles (folder):
  """{HANDLE: TITLE} of the root view's compact headlines."""
  handles = {}
  for line in read_compact (folder) . split ("\n"):
    match = re . match ( r'^\*+ (.*?) ?⟨([0-9a-z]+)[ ⟩]', line )
    if match:
      handles [ match . group (2) ] = match . group (1)
  return handles

def require_kept_handles (what, before, after, retitled, new_titles):
  """Every handle of BEFORE names the headline it named, under its new
  title where RETITLED maps its old title to one, and the headlines
  NEW_TITLES have handles BEFORE does not use."""
  for handle, title in before . items ():
    require_equal ( f"the title of ⟨{handle}⟩ after {what}",
                    after . get (handle), retitled . get (title, title) )
  for title in new_titles:
    new_handles = [ handle for handle, after_title in after . items ()
                    if after_title == title ]
    if len (new_handles) != 1 or new_handles [0] in before:
      raise CaseFailure (
        f"after {what}, '{title}' has the handles {new_handles}, among {before}" )

def compact_line_starting (folder, start):
  """The one line of the root view's compact file that starts with
  START."""
  lines = [ line for line in read_compact (folder) . split ("\n")
            if line . startswith (start) ]
  if len (lines) != 1:
    raise CaseFailure (
      f"{len (lines)} lines start {start!r}:\n{read_compact (folder)}" )
  return lines [0]

# ============================================================
# Reviewing: on a fixture whose owned skgrepo is a gitrepo, edits are
# saved, the diff view shows them, one is marked ready, a deleted node
# is restored, and everything is committed. The cases run in order on
# one server.
# ============================================================

DIFF_BASE_NAME = "diff_all-changes"
DIFF_VIEW_ID_PATTERN = re . compile (
  r'diff-view:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' )
COMMIT_MESSAGE = "Review the fixture."
NOT_ALL_READY = (
  "Not every change is ready to commit: 1 of 4 changed files are ready.\n" )
EMPTY_DIFF_VIEW = (
  "The diff view is empty: nothing changed since the last commits.\n" )

def failures_of_reviewing (scratch, data_root):
  main_repo = os . path . join (data_root, "me", "main")
  context = { "scratch" : scratch, "main_repo" : main_repo }
  for case in [ check_edits_to_review, check_diff_shows_the_changes,
                check_marking_ready, check_unconfirmed_commit,
                check_restoring_a_deleted_node, check_confirmed_commit,
                check_integrating_unshown_changes,
                check_blank_commit_message ]:
    try:
      case (context)
    except CaseFailure as failure:
      return [ case . __name__ + ": " + str (failure) ]
  return []

def check_edits_to_review (context):
  """A retitle, a new child and a deletion, saved through a content
  view."""
  folder = fresh_root_view (context, "review-edits")
  edit_compact ( folder, [
    ( "** First child ⟨0d aC1⟩", "** First child, renamed ⟨0d aC1⟩" ),
    ( "*** Contained twice ⟨0e 2aC⟩\n",
      "*** Contained twice ⟨0e 2aC delete⟩\n*** A new grandchild\n" ) ])
  require_save_succeeds (folder)
  require_equal ( "the deleted node's file",
                  os . path . exists ( skg_path (context, "shared-id") ),
                  False )

def check_diff_shows_the_changes (context):
  folder = run_diff (context, "diff-shows")
  require_equal ( "the diff view's compact buffer",
                  DIFF_VIEW_ID_PATTERN . sub (
                    "diff-view:ID", read_diff_compact (folder) ),
                  EXPECTED_DIFF_COMPACT )

def check_marking_ready (context):
  """'ready' on the new node marks its existence and its membership;
  a fresh diff view shows both ready. 'unready' there would unmark two
  changes, so the report asks first."""
  folder = run_diff (context, "mark")
  edit_compact (
    folder, [ ( "**** A new grandchild ⟨0j ∃+ ~+⟩",
                "**** A new grandchild ⟨0j ∃+ ~+ ready⟩" ) ],
    DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Ready: ⟨0j⟩ 'A new grandchild', as '(ready node rel)'.\n" +
    saved_line (0) +
    "Readiness: 1 of 4 changed files are ready.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  fresh = run_diff (context, "marked")
  compact = read_diff_compact (fresh)
  for expected in [ "# Readiness: 1 of 4 changed files are ready.\n",
                    "**** A new grandchild ⟨0j ∃+✓ ~+✓⟩\n" ]:
    if expected not in compact:
      raise CaseFailure ( f"the fresh diff view lacks {expected!r}:\n{compact}" )
  edit_compact (
    fresh, [ ( "⟨0j ∃+✓ ~+✓⟩", "⟨0j ∃+✓ ~+✓ unready⟩" ) ], DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (fresh) ]),
    diff_report_head () +
    "Unready: ⟨0j⟩ 'A new grandchild', as '(unready node rel)'.\n"
    "Question: this unmarks 2 changes. Unmarking this node commit-ready "
    "will unmark every contributing decision. You might discard a lot of "
    "hard-won information. Continue? Saving with '--yes' answers yes.\n" +
    NOT_SAVED,
    ignoring_view_ids = True )

def check_unconfirmed_commit (context):
  """Not every change is ready, so the server asks; an answer other
  than 'yes' commits nothing."""
  graph_before = skg_files (context)
  require_output (
    run_tool ([ "commit", COMMIT_MESSAGE ], standard_input = "no\n"),
    NOT_ALL_READY + "Type yes to commit anyway: \nNothing was committed.\n" )
  require_equal ( "the git log after an unconfirmed commit",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  "The fixture\n" )
  require_equal ( "the graph after an unconfirmed commit",
                  skg_files (context), graph_before )

def check_restoring_a_deleted_node (context):
  """'restoreNode' on a ghost writes the node back from HEAD, in both
  of its containers."""
  folder = run_diff (context, "restore")
  edit_compact (
    folder, [ ( "**** Contained twice ⟨0i ᗣ☮ ∃- ~-⟩",
                "**** Contained twice ⟨0i ᗣ☮ ∃- ~- restoreNode⟩" ) ],
    DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Restore node: ⟨0i⟩ 'Contained twice', as HEAD has it.\n" +
    saved_line (0) +
    "Readiness: 1 of 2 changed files are ready.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  require_equal ( "the restored node's title",
                  skg_title (context, "shared-id"), "Contained twice" )
  require_equal ( "its first container's content",
                  skg_list (context, "child-a-id", "contains")
                    [ : 1 ], [ "shared-id" ] )
  require_equal ( "its second container's content",
                  skg_list (context, "elsewhere-id", "contains"),
                  [ "shared-id" ] )

def check_confirmed_commit (context):
  """'--yes' answers the server's question; the commit carries the
  message, and leaves the diff view empty."""
  completed = run_tool ([ "commit", COMMIT_MESSAGE, "--yes" ])
  head = git ( context [ "main_repo" ], [ "rev-parse", "HEAD" ] ) . strip ()
  require_output (
    completed,
    "Not every change is ready to commit: 1 of 2 changed files are ready.\n"
    f"Committed in main ({head}).\n" )
  require_equal ( "the git log after the commit",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  COMMIT_MESSAGE + "\nThe fixture\n" )
  require_equal ( "the uncommitted '.skg' files",
                  [ line for line in git ( context [ "main_repo" ],
                                           [ "status", "--porcelain" ] )
                                     . split ("\n")
                    if line . endswith (".skg") ], [] )
  require_output (
    run_tool ([ "diff", "--out-dir",
                os . path . join ( context [ "scratch" ], "review", "after" ) ]),
    EMPTY_DIFF_VIEW )

def check_integrating_unshown_changes (context):
  """A buffer of the diff view shows a retitled root and, write-protected
  beneath it, a child whose own child another view then retitles. The
  buffer, as it stands, does not make that change editable; 'integrate'
  makes the child editable, which draws the retitled grandchild,
  editable too, and a save of the buffer then retitles it again."""
  root_view = fresh_root_view (context, "integrate-root")
  edit_compact ( root_view, [ ( "* The fixture root ⟨0a ",
                                "* The fixture root, retitled ⟨0a " ) ])
  require_save_succeeds (root_view)
  folder = run_diff (context, "integrate")
  require_in_diff_compact (folder, "*** First child, renamed ⟨0d ☮B aC2⟩\n")
  require_equal ( "the write-protection of the first child's headline",
                  "writeProtected" in diff_full_line (folder, 7), True )
  grandchild_view = fresh_root_view (context, "integrate-grandchild")
  edit_compact ( grandchild_view, [ ( "*** A new grandchild ⟨",
                                      "*** A grandchild, retitled ⟨" ) ])
  require_save_succeeds (grandchild_view)
  require_output (
    run_tool ([ "integrate", sidecar_of_diff (folder) ]),
    "Before integrating: 0 of 2 changed files are ready, 1 changes are "
    "not editable in this buffer.\n"
    "Integrated: 0 of 2 changed files are ready.\n" +
    diff_written_paths (folder) )
  require_in_diff_compact (
    folder, "*** First child, renamed ⟨0d aC2⟩\n"
    "A body line that stays unnumbered.\n"
    "**** Contained twice ⟨0f ☮ 2aC⟩\n"
    "**** A grandchild, retitled ⟨0g M⟩\n"
    "***** A new grandchild ⟨0h old title⟩\n" )
  for line_number in [ 7, 10 ]:
    require_equal ( f"the write-protection of line {line_number}",
                    "writeProtected" in diff_full_line (folder, line_number),
                    False )
  grandchild_id = UUID_PATTERN . search ( diff_full_line (folder, 10) ) . group (0)
  edit_compact (
    folder, [ ( "**** A grandchild, retitled ⟨0g M⟩",
                "**** A grandchild, retitled twice ⟨0g M⟩" ) ],
    DIFF_BASE_NAME )
  require_save_succeeds (folder)
  require_equal ( "the grandchild's title",
                  skg_title (context, grandchild_id),
                  "A grandchild, retitled twice" )

def check_blank_commit_message (context):
  """A blank message is refused before anything is sent: no server
  answers on port 1."""
  completed = run_tool ([ "commit", " " ], port = 1)
  require_output ( completed, "", expected_returncode = 1 )
  if "A commit message is required; nothing was sent." not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )

# ------------------------------------------------------------
# Helpers of the reviewing cases
# ------------------------------------------------------------

def run_diff (context, name):
  """Run 'diff' into a fresh folder; require that it wrote only the
  all-changes buffer."""
  folder = os . path . join ( context [ "scratch" ], "review", name )
  completed = require_tool_run ([ "diff", "--out-dir", folder ])
  require_equal ( "the files 'diff' wrote", completed . stdout,
                  diff_written_paths (folder) )
  return folder

def require_in_diff_compact (folder, text):
  compact = read_diff_compact (folder)
  if text not in compact:
    raise CaseFailure ( f"the diff view's compact buffer lacks {text!r}:\n{compact}" )

def diff_full_line (folder, line_number):
  """Line LINE_NUMBER (1-based) of the diff view's full buffer."""
  return read_file ( os . path . join ( folder, DIFF_BASE_NAME + ".full.org" )
                   ) . split ("\n") [ line_number - 1 ]

def sidecar_of_diff (folder):
  return os . path . join (folder, DIFF_BASE_NAME + ".view.json")

def read_diff_compact (folder):
  return read_file ( os . path . join (folder, DIFF_BASE_NAME + ".compact.org") )

def diff_report_head ():
  return ( "Changes in " + DIFF_BASE_NAME + ".compact.org, against " +
           DIFF_BASE_NAME + ".full.org (view ID diff-view:ID):\n" )

def diff_written_paths (folder):
  return "" . join ( os . path . join (folder, DIFF_BASE_NAME + suffix) + "\n"
                     for suffix in ( ".full.org", ".compact.org", ".view.json" ))

EXPECTED_DIFF_COMPACT = (
  "# Readiness: 0 of 4 changed files are ready.\n"
  "# Headlines with change facts 9, ready 0, not ready 9.\n"
  "# skg-for-ai compact view of diff_all-changes.full.org (view ID diff-view:ID)\n"
  "# Each headline ends in its handle ⟨ID HERALDS⟩: ID its viewnode ID,\n"
  "# the '(vnid ID)' of its metadata in the full view, and HERALDS what a\n"
  "# client shows for it, but a bare 'aC' (only the parent contains it);\n"
  "# 'folded' marks a headline a client shows collapsed.\n"
  "# Headlines 15, node occurrences 9, distinct nodes 6, changesetMarker "
  "content 2, folders 1, ghost 2, oldText title 1.\n"
  "# Skgrepos: main (owned) 9.\n"
  "# Foreign 0, write-protected 4, folded 0.\n"
  "# Contained elsewhere too 2, subscribed to 0, subscribing 1.\n"
  "# Links 3, distinct targets 3, resolved 2, unresolved 1 (missing 1).\n"
  "# Server errors 0, warnings 0.\n"
  # The deleted node's other container, a root of its own
  "* A second container, outside the view ⟨0a ⌂:main M⟩\n"
  "** ⟨0b its content changed⟩\n"
  "** Contained twice ⟨0c ᗣ☮ ∃- ~-⟩\n"
  "* Above the root ⟨0d C1 ⌂:main⟩\n"
  "** The fixture root ⟨0e aC2 L2⟩\n"
  "The root links to [[id:child-b-id][its second child]]⟦→ Second child, "
  "a subscriber ⌂:main⟧,\n"
  "to [[id:fred-note-id][Fred's note]]⟦→ Fred's note ⌂:fred⟧ in the "
  "foreign repo,\n"
  "and to [[id:missing-id][a node nobody wrote]]⟦✗ missing⟧.\n"
  "*** First child, renamed ⟨0f aC1 M⟩\n"
  "A body line that stays unnumbered.\n"
  "**** First child ⟨0g old title⟩\n"
  "**** ⟨0h its content changed⟩\n"
  "**** Contained twice ⟨0i ᗣ☮ ∃- ~-⟩\n"
  "**** A new grandchild ⟨0j ∃+ ~+⟩\n"
  # The new node's containers changed: it had none at HEAD. Its chain
  # climbs back through the nodes above it, each a cycle.
  "***** ⟨0k ☮ its containers changed (other files)⟩\n"
  "****** First child, renamed ⟨0l ☮B 1Cb ⟳ M ~+⟩\n"
  "******* The fixture root ⟨0m ☮B 1C2a L2 ⟳⟩\n"
  "******** Above the root ⟨0n ☮ Ca ⟳⟩\n"
  "*** Second child, a subscriber ⟨0o ☮ a/L S1 H2⟩\n" )

# ============================================================
# The daemon: each case runs on the graph and the daemon the earlier
# ones left. A second server, on the same data root with its own port
# and search index, stands for a teammate's: its saves reach the
# daemon's server only through the files on disk.
# ============================================================

DAEMON_READY_TIMEOUT_SECONDS = 30
# How long a case waits for the watcher to integrate a change on disk
# and the daemon to log it.
DAEMON_LOG_TIMEOUT_SECONDS = 20
SECOND_SERVER_PORT = PORT + 1
# What a daemon prints once its socket answers.
DAEMON_READY_LINE = "The daemon listens on"
INTEGRATED_ONE_MAIN_FILE = "changed on disk: repo main: 1 file integrated"

def failures_of_daemon (scratch, data_root):
  context = { "scratch"       : scratch,
              "data_root"     : data_root,
              "main_repo"     : os . path . join (data_root, "me", "main"),
              "daemon_folder" : os . path . join (scratch, "daemon"),
              "processes"     : [] }
  try:
    for case in [ check_daemon_starts, check_daemon_writes_what_a_command_writes,
                  check_daemon_rewrites_a_collateral_rerender,
                  check_daemon_rewrites_after_a_change_on_disk,
                  check_daemon_keeps_unsaved_edits,
                  check_daemon_logs_an_integration_warning,
                  check_daemon_takes_a_command_for_its_port,
                  check_second_daemon_on_the_port_refused,
                  check_daemon_commits_and_rewrites_a_diffView_buffer,
                  check_interactive_commit_tells_the_daemon,
                  check_daemon_closes_a_view, check_daemon_stops,
                  check_daemon_replaces_a_stale_socket ]:
      try:
        case (context)
      except CaseFailure as failure:
        return [ case . __name__ + ": " + str (failure) +
                 "\nThe daemon's log:\n" + daemon_log (context) ]
    return []
  finally:
    for process in context [ "processes" ]:
      if process . poll () is None:
        process . kill ()
        process . wait (timeout = 30)

def check_daemon_starts (context):
  start_daemon (context)
  require_log_line (context, "started: process ")

def check_daemon_writes_what_a_command_writes (context):
  """Each command, run once through the daemon (with no '--port': it
  talks to the daemon's server) and once on its own connection (with
  no daemon's socket to find), writes
  the same full and compact views; the sidecars differ only in
  whether the view is still open, which the daemon's is, and in the
  generation it was rendered at, which the save of 'links-here'
  advances."""
  plain_folder = os . path . join ( context [ "scratch" ], "daemon-plain" )
  for tool_arguments, base_name in [
      ( [ "view", "root-id" ], "view_root-id" ),
      ( [ "links-here", "child-b-id" ], "links-here_child-b-id" ),
      ( [ "search", "Fred" ], "search_Fred" ) ]:
    require_routed_run ( context, tool_arguments )
    require_tool_run ( tool_arguments + [ "--out-dir", plain_folder ],
                       bypassing_daemons = True )
    for suffix in ( ".full.org", ".compact.org" ):
      require_equal (
        base_name + suffix + " through the daemon",
        read_file ( os . path . join ( context [ "daemon_folder" ], base_name + suffix )),
        read_file ( os . path . join ( plain_folder, base_name + suffix )) )
    daemon_sidecar = read_sidecar ( context [ "daemon_folder" ], base_name )
    plain_sidecar = read_sidecar (plain_folder, base_name)
    require_equal ( base_name + "'s sidecars, open or not",
                    ( daemon_sidecar . pop ("closed"), plain_sidecar . pop ("closed") ),
                    ( False, True ) )
    for sidecar in ( daemon_sidecar, plain_sidecar ):
      int ( sidecar . pop ("rendered_at") )
    require_equal ( base_name + "'s sidecar through the daemon",
                    daemon_sidecar, plain_sidecar )
  require_log_line ( context, "holding view_root-id (view ID content-view:root-id)" )

def check_daemon_rewrites_a_collateral_rerender (context):
  """A save through the daemon of one view retitles a node another
  held view shows: the server rerenders that view as collateral, and
  the daemon rewrites its files, before the save returns, and logs
  why."""
  folder = context [ "daemon_folder" ]
  require_routed_run ( context, [ "view", "child-a-id" ] )
  edit_compact ( folder, [ ( "* First child ⟨0a ",
                             "* First child, renamed through the daemon ⟨0a " ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_in_file ( folder, "view_root-id",
                    "** First child, renamed through the daemon ⟨0d aC1⟩\n" )
  require_log_line ( context, "saved view_child-a-id" )
  require_log_line (
    context,
    "re-rendered view_root-id because of 'save view_child-a-id.view.json --yes' "
    "on the daemon's connection; 'First child, renamed through the daemon' "
    "changed: title, was 'First child'" )

def check_daemon_rewrites_after_a_change_on_disk (context):
  """A save on the second server writes a file the daemon's server did
  not write; its watcher integrates it and tells the daemon's
  connection, naming the views that show the node; the daemon asks
  for their rerender, rewrites their files and logs why."""
  start_second_server (context)
  second_folder = save_through_second_server (
    context, "view_root-id",
    ( "** Second child, a subscriber ⟨0f ",
      "** Second child, renamed on the second server ⟨0f " ))
  require_log_line ( context, INTEGRATED_ONE_MAIN_FILE )
  require_log_line (
    context,
    "re-rendered view_root-id because files changed on disk (repo main: 1 "
    "file integrated); 'Second child, renamed "
    "on the second server' changed: title, was 'Second child, a subscriber'" )
  require_log_line ( context, "re-rendered links-here_child-b-id because files changed on disk" )
  require_in_file ( context [ "daemon_folder" ], "view_root-id",
                    "** Second child, renamed on the second server ⟨0f " )
  require_in_file ( context [ "daemon_folder" ], "links-here_child-b-id",
                    "* Second child, renamed on the second server ⟨0a " )
  context [ "second_folder" ] = second_folder

def check_daemon_keeps_unsaved_edits (context):
  """A held view whose compact view has unsaved edits is not
  rerendered when a change on disk touches it: its edits stay, and
  the log says so."""
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** First child, renamed through the daemon ⟨0d ",
                             "** First child, my unsaved edit ⟨0d " ) ])
  save_through_second_server (
    context, "view_child-a-id",
    ( "* First child, renamed through the daemon ⟨0a ",
      "* First child, renamed on the second server ⟨0a " ))
  require_log_line ( context, "kept view_root-id: files changed on disk change "
                              "a node it shows, but its compact view has unsaved edits" )
  require_log_line ( context, "re-rendered view_child-a-id because files changed on disk" )
  require_in_file ( folder, "view_root-id", "** First child, my unsaved edit ⟨0d " )

def check_daemon_logs_an_integration_warning (context):
  """A file the watcher cannot read is a warning in the log."""
  bad_path = os . path . join ( context [ "main_repo" ], "bad-id.skg" )
  with open (bad_path, "w", encoding = "utf-8") as bad_file:
    bad_file . write ( "title: [unclosed\n" )
  require_log_line ( context, "warning: Node bad-id changed on disk but could "
                              "not be read, so the server keeps serving its "
                              "previous version" )
  os . remove (bad_path)

def check_daemon_takes_a_command_for_its_port (context):
  """A command for the daemon's port that writes into another folder
  runs through the daemon, which writes its files there and holds its
  view: a later save through the daemon of a node that view shows
  rewrites it."""
  other_folder = os . path . join ( context [ "scratch" ], "daemon-other" )
  context [ "other_folder" ] = other_folder
  completed = require_tool_run ([ "view", "shared-id", "--out-dir", other_folder ])
  require_equal ( "the files the routed command wrote", completed . stdout,
                  "" . join ( os . path . join ( other_folder, "view_shared-id" + suffix )
                              + "\n"
                              for suffix in ( ".full.org", ".compact.org",
                                              ".view.json" )))
  require_equal ( "the routed sidecar's 'closed'",
                  read_sidecar (other_folder, "view_shared-id") [ "closed" ], False )
  other_stem = os . path . join ( os . path . realpath (other_folder), "view_shared-id" )
  require_log_line ( context, f"holding {other_stem} (view ID content-view:shared-id)" )
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice ⟨",
                             "** Contained twice, retitled through the daemon ⟨" ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_log_line (
    context,
    f"re-rendered {other_stem} because of 'save view_child-a-id.view.json --yes' "
    "on the daemon's connection; 'Contained twice, retitled through the daemon' "
    "changed: title, was 'Contained twice'" )
  require_in_file ( other_folder, "view_shared-id",
                    "* Contained twice, retitled through the daemon ⟨" )

def check_second_daemon_on_the_port_refused (context):
  """A second daemon for the same port, in another folder, refuses to
  start, naming the first one's process; the first keeps its
  sockets."""
  second_folder = os . path . join ( context [ "scratch" ], "daemon-second" )
  completed = run_tool ([ "daemon", "--out-dir", second_folder ])
  expected = ( f"A daemon already runs for port {PORT}: process "
               f"{context [ 'daemon' ] . pid}, started with '--out-dir "
               f"{os . path . realpath ( context [ 'daemon_folder' ] )} --port {PORT}'" )
  if completed . returncode == 0 or expected not in completed . stderr:
    raise CaseFailure ( f"the second daemon exited {completed . returncode}, "
                        f"printed {completed . stdout!r}, {completed . stderr!r}" )
  for socket_path in ( os . path . join ( context [ "daemon_folder" ], "skg-for-ai.sock" ),
                       port_socket_path () ):
    require_equal ( "whether " + socket_path + " remains",
                    os . path . lexists (socket_path), True )
  require_equal ( "whether the second daemon left a socket",
                  os . path . lexists ( os . path . join (second_folder, "skg-for-ai.sock") ),
                  False )

# What the summary of a buffer of the diff view says once a git-commit
# leaves it showing no change.
NO_CHANGE_FACTS = "# Headlines with change facts 0, "

def check_daemon_commits_and_rewrites_a_diffView_buffer (context):
  """'commit --yes' runs through the daemon, so the server rerenders
  the buffer of the diff view the daemon holds, and the daemon
  rewrites it: after the git-commit the buffer's headlines carry no
  change facts."""
  diff_folder = os . path . join ( context [ "scratch" ], "daemon-diff" )
  context [ "diff_folder" ] = diff_folder
  require_tool_run ([ "diff", "--out-dir", diff_folder ])
  require_in_diff_compact ( diff_folder, "Contained twice, retitled through the daemon" )
  require_not_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )
  require_equal ( "the held diff buffer's 'closed'",
                  read_sidecar (diff_folder, DIFF_BASE_NAME) [ "closed" ], False )
  completed = require_tool_run ([ "commit", "Commit through the daemon.", "--yes" ])
  if "Committed in main (" not in completed . stdout:
    raise CaseFailure ( f"the commit printed {completed . stdout!r}" )
  require_equal ( "the git log after the commit through the daemon",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  "Commit through the daemon.\nThe fixture\n" )
  diff_stem = os . path . join ( os . path . realpath (diff_folder), DIFF_BASE_NAME )
  require_log_line (
    context, f"re-rendered {diff_stem} because of 'commit Commit through the "
             f"daemon. --yes --port {PORT}' on the daemon's connection" )
  require_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )

def check_interactive_commit_tells_the_daemon (context):
  """A 'commit' that waits for a typed "yes" runs on its own
  connection; when it has committed, it asks the daemon to rerender
  its buffers of the diff view, which the daemon rewrites. (The new
  'diff' writes over the held buffer's files, which closes the view
  ID they showed.)"""
  diff_folder = context [ "diff_folder" ]
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice, retitled through the daemon ⟨",
                             "** Contained twice, committed interactively ⟨" ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_tool_run ([ "diff", "--out-dir", diff_folder ])
  require_in_diff_compact ( diff_folder, "Contained twice, committed interactively" )
  require_not_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )
  completed = run_tool ( [ "commit", "Commit interactively." ],
                         standard_input = "yes\n" )
  if completed . returncode != 0 or "Committed in main (" not in completed . stdout:
    raise CaseFailure ( f"the commit printed {completed . stdout!r}, {completed . stderr!r}" )
  require_log_line (
    context, f"re-rendered {os . path . join ( os . path . realpath (diff_folder), DIFF_BASE_NAME )} "
             "because of a git-commit on another connection" )
  require_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )

def check_daemon_closes_a_view (context):
  folder = context [ "daemon_folder" ]
  sidecar_path = os . path . join (folder, "view_child-a-id.view.json")
  require_output ( run_tool ( [ "close", sidecar_path ], port = None ),
                   sidecar_path + "\n" )
  require_equal ( "the closed sidecar's 'closed'",
                  read_sidecar (folder, "view_child-a-id") [ "closed" ], True )
  require_log_line ( context, "closed view_child-a-id" )

def check_daemon_stops (context):
  """'daemon --stop' releases every view (each sidecar says closed),
  removes the socket and ends the daemon. A command then runs on its
  own connection: a save of a view the daemon held is an ordinary
  unheld save, which keeps its handles."""
  folder = context [ "daemon_folder" ]
  require_output (
    run_tool ( [ "daemon", "--stop", "--out-dir", folder ], port = None ),
    f"Stopped the daemon in {os . path . realpath (folder)}.\n" )
  require_equal ( "the daemon's exit code",
                  context [ "daemon" ] . wait (timeout = 30), 0 )
  require_equal ( "whether the socket remains",
                  os . path . lexists ( os . path . join (folder, "skg-for-ai.sock") ),
                  False )
  for held_folder in ( folder, context [ "other_folder" ], context [ "diff_folder" ] ):
    for name in os . listdir (held_folder):
      if name . endswith (".view.json"):
        require_equal ( name + "'s 'closed'",
                        read_sidecar ( held_folder,
                                       name [ : - len (".view.json") ] ) [ "closed" ],
                        True )
  require_equal ( "whether the port's socket remains",
                  os . path . lexists ( port_socket_path () ), False )
  require_log_line ( context, "stopped: released 5 views" )
  edit_compact ( folder, [ ( "* Second child, renamed on the second server ⟨0a ",
                             "* Second child, saved after the daemon stopped ⟨0a " ) ],
                 "links-here_child-b-id" )
  completed = run_tool ([ "save", os . path . join (folder, "links-here_child-b-id.view.json"),
                          "--yes" ])
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_in_file ( folder, "links-here_child-b-id",
                    "* Second child, saved after the daemon stopped ⟨0a " )
  require_equal ( "child-b's title", skg_title (context, "child-b-id"),
                  "Second child, saved after the daemon stopped" )

def check_daemon_replaces_a_stale_socket (context):
  """A daemon killed outright leaves its socket. A command then runs
  on its own connection, and a new daemon replaces the socket; a
  signal stops it as '--stop' does."""
  folder = context [ "daemon_folder" ]
  start_daemon (context) . kill ()
  context [ "daemon" ] . wait (timeout = 30)
  socket_path = os . path . join (folder, "skg-for-ai.sock")
  require_equal ( "whether the dead daemon's socket remains",
                  os . path . lexists (socket_path), True )
  require_tool_run ([ "view", "shared-id", "--out-dir", folder ])
  require_equal ( "the sidecar written beside a dead daemon's socket, 'closed'",
                  read_sidecar (folder, "view_shared-id") [ "closed" ], True )
  start_daemon (context)
  require_log_line ( context, "replaced the socket a dead daemon left" )
  require_routed_run ( context, [ "view", "shared-id" ] )
  require_equal ( "the sidecar written through the new daemon, 'closed'",
                  read_sidecar (folder, "view_shared-id") [ "closed" ], False )
  context [ "daemon" ] . terminate ()
  require_equal ( "the exit code after SIGTERM",
                  context [ "daemon" ] . wait (timeout = 30), 0 )
  require_equal ( "whether the socket remains after SIGTERM",
                  os . path . lexists (socket_path), False )
  require_log_line ( context, "stopped: released 1 view" )

# ------------------------------------------------------------
# Helpers of the daemon cases
# ------------------------------------------------------------

def start_daemon (context):
  """Start a daemon for the daemon folder, on the scratch server, and
  wait until its socket answers; return its process."""
  folder = context [ "daemon_folder" ]
  output_path = os . path . join ( context [ "scratch" ], "daemon-output.txt" )
  ready_lines_before = ( read_file (output_path) . count (DAEMON_READY_LINE)
                         if os . path . exists (output_path) else 0 )
  with open (output_path, "a") as output:
    process = subprocess . Popen (
      [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py"),
        "daemon", "--out-dir", folder, "--port", str (PORT) ],
      stdout = output, stderr = subprocess . STDOUT )
  context [ "processes" ] . append (process)
  context [ "daemon" ] = process
  deadline = time . time () + DAEMON_READY_TIMEOUT_SECONDS
  while time . time () < deadline:
    if process . poll () is not None:
      raise CaseFailure ( f"the daemon exited: {read_file (output_path)}" )
    if read_file (output_path) . count (DAEMON_READY_LINE) > ready_lines_before:
      return process
    time . sleep (0.1)
  raise CaseFailure ( f"the daemon did not start: {read_file (output_path)}" )

def start_second_server (context):
  """A second server over the scratch data root, with its own port and
  search index, as a teammate's server reads the same skgrepos."""
  if port_is_listening (SECOND_SERVER_PORT):
    raise CaseFailure ( f"port {SECOND_SERVER_PORT} is taken" )
  config_name = "skgconfig-second.toml"
  config_text = read_file ( os . path . join ( context [ "data_root" ], "skgconfig.toml" ))
  with open ( os . path . join ( context [ "data_root" ], config_name ), "w",
              encoding = "utf-8" ) as config_file:
    config_file . write (
      re . sub ( r'(?m)^tantivy_folder = .*$',
                 'tantivy_folder = ".index-second.tantivy"',
                 re . sub ( r'(?m)^port = [0-9]+', f"port = {SECOND_SERVER_PORT}",
                            config_text )))
  log_path = os . path . join ( context [ "scratch" ], "second-server.log" )
  with open (log_path, "w") as server_log:
    server = subprocess . Popen (
      [ SERVER_BINARY, config_name ], cwd = context [ "data_root" ],
      stdout = server_log, stderr = subprocess . STDOUT )
  context [ "processes" ] . append (server)
  if not wait_for_server_ready (log_path, server):
    raise CaseFailure ( f"the second server did not become ready; see {log_path}" )

def save_through_second_server (context, base_name, replacement):
  """View the node BASE_NAME names afresh on the second server, make
  the one REPLACEMENT in its compact view, and save it there; return
  the folder."""
  folder = os . path . join ( context [ "scratch" ], "second-server", base_name )
  node_id = base_name . split ("_", 1) [1]
  require_tool_run_on ( [ "view", node_id, "--out-dir", folder ], SECOND_SERVER_PORT )
  edit_compact ( folder, [ replacement ], "view_" + node_id )
  completed = run_tool ( [ "save", os . path . join ( folder, "view_" + node_id + ".view.json" ),
                           "--yes" ], port = SECOND_SERVER_PORT )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the second server's save printed {completed . stdout!r}, "
                        f"{completed . stderr!r}" )
  return folder

def port_socket_path ():
  return os . path . join ( os . environ [ "XDG_RUNTIME_DIR" ],
                            f"skg-for-ai-{PORT}.sock" )

def require_not_in_diff_compact (folder, text):
  compact = read_diff_compact (folder)
  if text in compact:
    raise CaseFailure ( f"the diff view's compact buffer still has {text!r}:\n{compact}" )

def require_routed_run (context, tool_arguments):
  """Run the command with the daemon folder as its '--out-dir' and no
  '--port', so that it runs through the daemon."""
  completed = run_tool (
    tool_arguments + [ "--out-dir", context [ "daemon_folder" ] ], port = None )
  if completed . returncode != 0:
    raise CaseFailure (
      f"'{' ' . join (tool_arguments)}' through the daemon failed: "
      f"{completed . stdout}{completed . stderr}" )
  return completed

def require_tool_run_on (tool_arguments, port):
  completed = run_tool (tool_arguments, port = port)
  if completed . returncode != 0:
    raise CaseFailure (
      f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" )
  return completed

def require_log_line (context, text):
  """Wait until a line of the daemon's log contains TEXT."""
  deadline = time . time () + DAEMON_LOG_TIMEOUT_SECONDS
  while time . time () < deadline:
    if text in daemon_log (context):
      return
    time . sleep (0.1)
  raise CaseFailure ( f"the daemon's log has no line with {text!r}" )

def daemon_log (context):
  path = os . path . join ( context [ "daemon_folder" ], "skg-for-ai.log" )
  return read_file (path) if os . path . exists (path) else ""

def require_in_file (folder, base_name, text):
  compact = read_file ( os . path . join (folder, base_name + ".compact.org") )
  if text not in compact:
    raise CaseFailure ( f"{base_name}.compact.org lacks {text!r}:\n{compact}" )

def read_sidecar (folder, base_name):
  return json . loads ( read_file ( os . path . join (folder, base_name + ".view.json") ))

# ------------------------------------------------------------
# Helpers of the writing cases
# ------------------------------------------------------------

def fresh_root_view (context, name):
  folder = os . path . join ( context [ "scratch" ], "write", name )
  require_tool_run ([ "view", "root-id", "--out-dir", folder, "--no-summary" ])
  return folder

def require_tool_run (tool_arguments, bypassing_daemons = False):
  completed = run_tool ( tool_arguments, bypassing_daemons = bypassing_daemons )
  if completed . returncode != 0:
    raise CaseFailure (
      f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" )
  return completed

def run_save (folder, *flags):
  return run_tool ( [ "save", folder ] + list (flags) )

def require_save_succeeds (folder):
  completed = run_save (folder, "--yes")
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure (
      f"saving {folder} printed {completed . stdout!r}, {completed . stderr!r}" )
  return completed

def edit_compact (folder, replacements, base_name = ROOT_BASE_NAME):
  """Replace, in the compact view BASE_NAME in FOLDER, each OLD of
  REPLACEMENTS by its NEW, requiring that OLD occur once."""
  path = os . path . join (folder, base_name + ".compact.org")
  text = read_file (path)
  for old, new in replacements:
    if text . count (old) != 1:
      raise CaseFailure (
        f"{old!r} occurs {text . count (old)} times in {path}:\n{text}" )
    text = text . replace (old, new)
  with open (path, "w", encoding = "utf-8") as compact_file:
    compact_file . write (text)

def read_compact (folder):
  return read_file ( os . path . join (folder, ROOT_BASE_NAME + ".compact.org") )

def require_in_compact (folder, text):
  if text not in read_compact (folder):
    raise CaseFailure (
      f"the re-rendered compact view lacks {text!r}:\n{read_compact (folder)}" )

def id_of_compact_headline (folder, headline_start):
  """The ID in the metadata of the full-view headline that the handle
  of the compact headline starting HEADLINE_START names."""
  for line in read_compact (folder) . split ("\n"):
    if line . startswith (headline_start + " ⟨"):
      handle = HANDLE_PATTERN . search (line) . group (1)
      for full_line in read_file (
          os . path . join (folder, ROOT_BASE_NAME + ".full.org") ) . split ("\n"):
        viewnode_id = VIEWNODE_ID_PATTERN . match (full_line)
        if viewnode_id and viewnode_id . group (1) == handle:
          match = UUID_PATTERN . search (full_line)
          if match is None:
            raise CaseFailure (
              f"the full view's headline ⟨{handle}⟩ has no UUID: {full_line!r}" )
          return match . group (0)
      raise CaseFailure ( f"the full view has no headline ⟨{handle}⟩" )
  raise CaseFailure (
    f"no headline {headline_start!r} in:\n{read_compact (folder)}" )

def require_output (completed, expected_stdout, expected_returncode = 0,
                    ignoring_view_ids = False):
  """IGNORING_VIEW_IDS: each diff view's view ID, a fresh UUID, is
  read as 'diff-view:ID'."""
  stdout = ( DIFF_VIEW_ID_PATTERN . sub ( "diff-view:ID", completed . stdout )
             if ignoring_view_ids else completed . stdout )
  if ( stdout != expected_stdout
       or completed . returncode != expected_returncode ):
    raise CaseFailure (
      f"exit {completed . returncode}, printed:\n{completed . stdout}"
      f"stderr: {completed . stderr}\nexpected exit {expected_returncode}, "
      f"printed:\n{expected_stdout}" )

def require_equal (what, actual, expected):
  if actual != expected:
    raise CaseFailure ( f"{what} is {actual!r}, not {expected!r}" )

def saved_line (warning_count):
  return ( f"Server: saved, 0 errors, {warning_count} warning"
           f"{'' if warning_count == 1 else 's'}.\n" )

def written_paths (folder):
  return "" . join ( os . path . join (folder, ROOT_BASE_NAME + suffix) + "\n"
                     for suffix in ( ".full.org", ".compact.org", ".view.json" ))

def skg_path (context, node_id):
  return os . path . join ( context [ "main_repo" ], node_id + ".skg" )

def skg_lines (context, node_id):
  return read_file ( skg_path (context, node_id) ) . split ("\n")

def skg_title (context, node_id):
  for line in skg_lines (context, node_id):
    if line . startswith ("title: "):
      return line [ len ("title: ") : ]
  return None

def skg_body (context, node_id):
  """The lines of a block-scalar body."""
  lines = skg_lines (context, node_id)
  for index, line in enumerate (lines):
    if line . startswith ("body: |"):
      body = []
      for body_line in lines [ index + 1 : ]:
        if not body_line . startswith ("  "):
          break
        body . append ( body_line [ 2 : ] )
      return body
  return None

def skg_list (context, node_id, key):
  lines = skg_lines (context, node_id)
  if key + ":" not in lines:
    return []
  members = []
  for line in lines [ lines . index (key + ":") + 1 : ]:
    if not line . startswith ("- "):
      break
    members . append ( line [ 2 : ] )
  return members

def skg_files (context):
  """{FILE NAME: TEXT} of the owned skgrepo."""
  return { name : read_file ( os . path . join ( context [ "main_repo" ], name ))
           for name in sorted ( os . listdir ( context [ "main_repo" ] ))
           if name . endswith (".skg") }

def run_tool (tool_arguments, standard_input = "", port = PORT,
              bypassing_daemons = False):
  """Run the tool; with PORT None, without '--port'. BYPASSING_DAEMONS:
  where it finds no daemon for its port."""
  environment = dict (os . environ)
  if bypassing_daemons:
    environment [ "XDG_RUNTIME_DIR" ] = tempfile . mkdtemp (
      prefix = "skg-for-ai-test-no-daemon-" )
  try:
    return subprocess . run (
      [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py") ]
      + tool_arguments + ( [ "--port", str (port) ] if port is not None else [] ),
      input = standard_input, capture_output = True, text = True,
      env = environment )
  finally:
    if bypassing_daemons:
      os . rmdir ( environment [ "XDG_RUNTIME_DIR" ] )

def commit_fixture_in_new_gitrepo (folder):
  """Make FOLDER a gitrepo, with a local author, whose one commit
  holds its files."""
  for git_arguments in [ [ "init", "--quiet" ],
                         [ "config", "user.name", "Fixture Author" ],
                         [ "config", "user.email", "fixture@example.com" ],
                         [ "add", "--all" ],
                         [ "commit", "--quiet", "--message", "The fixture" ] ]:
    git (folder, git_arguments)

def git (folder, git_arguments):
  """Git's standard output, run in FOLDER; raises when git fails."""
  return subprocess . run (
    [ "git", "-C", folder ] + git_arguments,
    capture_output = True, text = True, check = True ) . stdout

def failures_of_handles (base_name, full_lines, compact):
  """The compact tree has one line per full-view line, and each
  headline's handle is the viewnode ID of the full view's headline on
  the same line."""
  tree_lines = [ line for line in compact . split ("\n") [ : -1 ]
                 if not line . startswith ("# ") ]
  full_lines = full_lines [ : -1 ] if full_lines [-1] == "" else full_lines
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

def set_port_in_config (config_path, port):
  text = read_file (config_path)
  with open (config_path, "w", encoding = "utf-8") as config_file:
    config_file . write (
      re . sub ( r'(?m)^port = [0-9]+', f"port = {port}", text ))

def wait_for_server_ready (server_log_path, server):
  deadline = time . time () + SERVER_READY_TIMEOUT_SECONDS
  while time . time () < deadline:
    if server . poll () is not None:
      return False
    if "Server ready." in read_file (server_log_path):
      return True
    time . sleep (0.2)
  return False

def port_is_listening (port):
  with socket . socket () as probe:
    return probe . connect_ex (( "127.0.0.1", port )) == 0

def read_file (path):
  with open (path, encoding = "utf-8") as input_file:
    return input_file . read ()

if __name__ == "__main__":
  main ( sys . argv [ 1 : ] )
