#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

In order, it:
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
  refuses, 'new', and one view over several saves, whose handles
  stay, checking each report exactly and the graph after each save;
- on a third scratch server, whose owned skgrepo is a gitrepo with the
  fixture committed: saves a retitle, a new child and a deletion,
  checks the diff view's compact buffer exactly, marks the new child
  ready, restores the deleted node, and commits (refused unconfirmed,
  then confirmed with '--yes'), checking 'git log' and that the diff
  view is then empty; then integrates a change a diff buffer does not
  make editable, and saves an edit of it; a blank commit message is
  refused unsent.
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
  failures = []
  failures += failures_of_shared_herald_cases ()
  failures += failures_against_scratch_servers (update_expected)
  for failure in failures:
    print ( "FAIL: " + failure )
  if failures:
    sys . exit (1)
  print ( "PASS" + ( " (expected files rewritten)" if update_expected else "" ))

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
  checks on another, then the reviewing checks on a third, whose owned
  skgrepo is a gitrepo; each on its own copy of the fixture."""
  if not os . path . exists (SERVER_BINARY):
    return [ f"no server binary at {SERVER_BINARY}; run 'cargo build --bin skg'" ]
  return ( failures_on_a_scratch_server (
             lambda scratch, data_root :
               failures_of_reading (scratch, data_root, update_expected) )
           + failures_on_a_scratch_server (failures_of_writing)
           + failures_on_a_scratch_server (
               failures_of_reviewing, owned_skgrepo_in_git = True ))

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
                check_new_root, check_handles_across_saves ]:
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
  """A compact view edited and saved twice keeps its handles: each
  save opens the view again on its connection, and the server keeps
  the viewnode IDs of the view it holds. A new headline gets a new
  handle. After a removal the view the server opens again holds fewer
  headlines than the saved one, so a save may renumber the highest
  handles, and the report names them first."""
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
  saved = require_save_succeeds (folder)
  require_equal ( "a renumbering in the second save",
                  "Handles the save may change" in saved . stdout, False )
  require_kept_handles ( "the second save", after_first_save,
                         handles_and_titles (folder),
                         { "Added before the first child" :
                             "Added before the first child, retitled" },
                         [ "Added at the top" ] )
  edit_compact ( folder, [ ( compact_line_starting (
    folder, "** Added before the first child, retitled" ) + "\n", "" ) ])
  require_save_succeeds (folder)
  added_at_top = compact_line_starting (folder, "** Added at the top")
  top_handle = HANDLE_PATTERN . search (added_at_top) . group (1)
  edit_compact ( folder, [ ( added_at_top, added_at_top . replace (
    "Added at the top", "Added at the top, retitled" )) ])
  saved = require_save_succeeds (folder)
  require_equal (
    "the renumbering the save after a removal reports",
    [ line for line in saved . stdout . split ("\n")
      if line . startswith ("Handles the save may change") ],
    [ f"Handles the save may change: ⟨{top_handle}⟩ 'Added at the top'. "
      "The view the server opened again does not hold these viewnode "
      "IDs, so it gives each headline a new one." ] )

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
    "**** Contained twice ⟨0i ☮ 2aC⟩\n"
    "**** A grandchild, retitled ⟨0j M⟩\n"
    "***** A new grandchild ⟨0k old title⟩\n" )
  for line_number in [ 7, 10 ]:
    require_equal ( f"the write-protection of line {line_number}",
                    "writeProtected" in diff_full_line (folder, line_number),
                    False )
  grandchild_id = UUID_PATTERN . search ( diff_full_line (folder, 10) ) . group (0)
  edit_compact (
    folder, [ ( "**** A grandchild, retitled ⟨0j M⟩",
                "**** A grandchild, retitled twice ⟨0j M⟩" ) ],
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

# ------------------------------------------------------------
# Helpers of the writing cases
# ------------------------------------------------------------

def fresh_root_view (context, name):
  folder = os . path . join ( context [ "scratch" ], "write", name )
  require_tool_run ([ "view", "root-id", "--out-dir", folder, "--no-summary" ])
  return folder

def require_tool_run (tool_arguments):
  completed = run_tool (tool_arguments)
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

def run_tool (tool_arguments, standard_input = "", port = PORT):
  return subprocess . run (
    [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py") ]
    + tool_arguments + [ "--port", str (port) ],
    input = standard_input, capture_output = True, text = True )

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
