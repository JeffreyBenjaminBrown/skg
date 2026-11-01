#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

In order, it:
- checks the relationship heralds against the rendering cases both
  clients' tests share ('tests/shared/herald-rendering-cases.txt'),
  those whose text is nothing but relationship and property tokens;
- copies 'test-fixture/' to a scratch data root, starts a server
  there on port SKG_FOR_AI_TEST_PORT (default 1793), and runs each
  command of the tool against it;
- compares each compact view with its file in 'test-expected/', and
  checks that every handle names a headline line of the full view;
- sends a 'repos' request through 'raw', and imports a Markdown file
  through 'import';
- on a second scratch server, edits compact views and saves them:
  a title, a body, a new child, a move, a removal, a deletion, a bad
  handle, an edit the server refuses, and a stale view, checking
  each report exactly and the graph after each save.
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
SERVER_BINARY = os . environ . get (
  "SKG_BINARY", os . path . join (REPO_ROOT, "target", "debug", "skg") )
PORT = int ( os . environ . get ( "SKG_FOR_AI_TEST_PORT", "1793" ))
SERVER_READY_TIMEOUT_SECONDS = 60
HANDLE_PATTERN = re . compile (r'⟨([0-9]+)[ ⟩]')

# Each run: the files' base name, then the tool's arguments.
TOOL_RUNS = [
  ( "view_root-id",          [ "view", "root-id" ] ),
  ( "search_Fred",           [ "search", "Fred" ] ),
  ( "links-here_child-b-id", [ "links-here", "child-b-id" ] ),
  ( "contains_shared-id",    [ "contains", "shared-id", "--no-summary" ] ) ]

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
# Relationship heralds
# ============================================================

def failures_of_shared_herald_cases ():
  failures = []
  checked = 0
  for name, metadata_text, text in shared_herald_cases ():
    if not relationship_tokens_only (metadata_text, text):
      continue
    checked += 1
    rendered = skg_for_ai . relationship_heralds_text (
      skg_for_ai . parse_sexp (metadata_text) )
    if rendered != text:
      failures . append (
        f"herald case '{name}': rendered '{rendered}', expected '{text}'" )
  if checked < 30:
    failures . append (
      f"only {checked} shared herald cases were relationship-only" )
  return failures

def shared_herald_cases ():
  """[(NAME, METADATA, TEXT)] for each case with a text line."""
  cases = []
  name, metadata = None, None
  with open (SHARED_HERALD_CASES, encoding = "utf-8") as cases_file:
    for line in cases_file . read () . split ("\n"):
      if line . startswith ("==== "):
        name, metadata = line [ 5 : ], None
      elif line . startswith ("---- text: ") and metadata:
        cases . append (( name, metadata, line [ 11 : ] ))
      elif name and metadata is None and line . startswith ("(skg"):
        metadata = line
  return cases

def relationship_tokens_only (metadata_text, text):
  """Whether the case's whole herald text is relationship and property
  tokens: its metadata has relationship facts, and its text no other
  glyph."""
  return ( ( "(rels" in metadata_text or "(props" in metadata_text )
           and all ( re . fullmatch (
                       r'[0-9a-z{}/]*[CLSOHAIF]ĥ?[0-9a-z{}/]*', token )
                     for token in text . split (" ") ))

# ============================================================
# The scratch server
# ============================================================

def failures_against_scratch_servers (update_expected):
  """The reading checks on one scratch server, then the writing
  checks on another, each on its own copy of the fixture."""
  if not os . path . exists (SERVER_BINARY):
    return [ f"no server binary at {SERVER_BINARY}; run 'cargo build --bin skg'" ]
  return ( failures_on_a_scratch_server (
             lambda scratch, data_root :
               failures_of_reading (scratch, data_root, update_expected) )
           + failures_on_a_scratch_server (failures_of_writing) )

def failures_on_a_scratch_server (check):
  """CHECK (SCRATCH, DATA_ROOT)'s failures, run against a server on a
  fresh copy of the fixture in the scratch folder SCRATCH."""
  if port_is_listening (PORT):
    return [ f"port {PORT} is taken; set SKG_FOR_AI_TEST_PORT" ]
  scratch = tempfile . mkdtemp ( prefix = "skg-for-ai-test-" )
  data_root = os . path . join (scratch, "data")
  shutil . copytree (FIXTURE_FOLDER, data_root)
  set_port_in_config ( os . path . join (data_root, "skgconfig.toml"), PORT )
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
  for base_name, tool_arguments in TOOL_RUNS:
    failures += failures_of_one_run (
      base_name, tool_arguments, output_folder, update_expected )
  failures += failures_of_raw_and_import (scratch)
  failures += failures_of_changed_skg_files (data_root)
  return failures

def failures_of_one_run (base_name, tool_arguments, output_folder,
                         update_expected):
  completed = run_tool ( tool_arguments + [ "--out-dir", output_folder ] )
  if completed . returncode != 0:
    return [ f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" ]
  written = completed . stdout . split ()
  expected_written = [ os . path . join (output_folder, base_name + suffix)
                       for suffix in ( ".full.org", ".compact.org", ".view.json" ) ]
  if written != expected_written:
    return [ f"'{' ' . join (tool_arguments)}' printed {written}" ]
  sidecar = json . loads ( read_file ( expected_written [2] ))
  expected_view_id = ( "search:" + tool_arguments [1]
                       if tool_arguments [0] == "search"
                       else "content-view:" + tool_arguments [1] )
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
  expected_repos = ( '((response-type repos) (restriction "main") (repos ('
                     '((name "fred") (path "fred/public") (owned "false")) '
                     '((name "main") (path "me/main") (owned "true")))))' )
  if raw . returncode != 0 or raw . stdout . split ("\n") [1] != expected_repos:
    failures . append ( f"'raw' printed {raw . stdout!r}, {raw . stderr!r}" )
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
REMOVAL_NOTE = (
  "  (A removed headline leaves its parent's list, where the parent "
  "reads claims; the node itself is not deleted. To delete a node, "
  "add 'delete' to its handle.)\n" )
UUID_PATTERN = re . compile (
  r'[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' )

def failures_of_writing (scratch, data_root):
  main_repo = os . path . join (data_root, "me", "main")
  context = { "scratch" : scratch, "main_repo" : main_repo }
  for case in [ check_title_edit, check_body_edit, check_new_child,
                check_move, check_removal, check_deletion,
                check_bad_handles, check_server_refusal, check_stale_view ]:
    try:
      case (context)
    except CaseFailure as failure:
      # Later cases build on this one's graph.
      return [ case . __name__ + ": " + str (failure) ]
  return []

def check_title_edit (context):
  folder = fresh_root_view (context, "title")
  edit_compact ( folder, [ ( "** First child ⟨7 aC1⟩",
                             "** First child, renamed ⟨7 aC1⟩" ) ])
  report = ( REPORT_HEAD +
             "Title: ⟨7⟩ 'First child' → 'First child, renamed'.\n" )
  require_output ( run_save (folder), report + NOT_SAVED )
  require_equal ( "the title before '--yes'",
                  skg_title (context, "child-a-id"), "First child" )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  require_equal ( "the saved title",
                  skg_title (context, "child-a-id"), "First child, renamed" )
  require_in_compact ( folder, "** First child, renamed ⟨7 aC1⟩\n" )

def check_body_edit (context):
  folder = fresh_root_view (context, "body")
  edit_compact ( folder, [ ( "⟦✗ missing⟧.\n",
                             "⟦✗ missing⟧.\nA new last line.\n" ) ])
  report = REPORT_HEAD + "Bodies changed: 1 (⟨1⟩).\n"
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
  edit_compact ( folder, [ ( "*** Contained twice ⟨10 2aC⟩\n",
                             "*** Contained twice ⟨10 2aC⟩\n"
                             "*** A new grandchild\n" ) ])
  report = ( REPORT_HEAD +
             "New: 'A new grandchild', under ⟨8⟩ 'First child, renamed'.\n" )
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
    ( "*** Contained twice ⟨10 2aC⟩\n", "" ),
    ( "***** An aside of Fred's ⟨16 cH folded⟩\n",
      "***** An aside of Fred's ⟨16 cH folded⟩\n"
      "*** Contained twice ⟨10 2aC⟩\n" ) ])
  report = ( REPORT_HEAD +
             "Moved: ⟨10⟩ 'Contained twice', from under ⟨8⟩ 'First child, "
             "renamed' to under ⟨12⟩ 'Second child, a subscriber'.\n" )
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
  edit_compact ( folder, [ ( "*** Contained twice ⟨16 2aC⟩\n", "" ) ])
  report = ( REPORT_HEAD +
             "Removed: ⟨16⟩ 'Contained twice', from under ⟨11⟩ 'Second "
             "child, a subscriber'.\n" + REMOVAL_NOTE )
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
    "* Contained twice ⟨1 1C⟩\n"
    "** ⟨2 containerFolder⟩\n"
    "*** A second container, outside the view ⟨3 Cb⟩\n" )

def check_deletion (context):
  folder = fresh_root_view (context, "delete")
  edit_compact ( folder, [ ( "*** A new grandchild ⟨10⟩",
                             "*** A new grandchild ⟨10 delete⟩" ) ])
  report = REPORT_HEAD + "Delete: ⟨10⟩ 'A new grandchild', from the graph.\n"
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
  edit_compact ( folder, [ ( "** First child, renamed ⟨8⟩",
                             "** First child, renamed ⟨3⟩" ),
                           ( "⟨7 Cb⟩", "⟨99 Cb⟩" ) ])
  graph_before = skg_files (context)
  full_before = read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") )
  completed = run_save (folder, "--yes")
  require_output (
    completed,
    REPORT_HEAD +
    "Error: ⟨99⟩ on line 7 of the compact view names no line; the full "
    "view has 14 lines.\n"
    "Error: ⟨3⟩ on line 8 of the compact view names line 3 of the full "
    "view, a body line, not a headline.\n",
    expected_returncode = 1 )
  if "Nothing was saved: a handle names no headline" not in completed . stderr:
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
  edit_compact ( folder, [ ( "*** Above the root ⟨7 Cb⟩",
                             "*** Above the root, retitled ⟨7 Cb⟩" ) ])
  graph_before = skg_files (context)
  full_before = read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") )
  completed = run_save (folder, "--yes")
  expected_head = (
    REPORT_HEAD +
    "Title: ⟨7⟩ 'Above the root' → 'Above the root, retitled'.\n"
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
  The server keeps no record of what A was rendered from: saving A
  writes A's text of every node it shows, the untouched stale one
  included. So the tool renders the view again before saving, and
  refuses while a node it would write differs; with
  '--overwrite-stale' it saves, and shows what the server answered."""
  folder_a = fresh_root_view (context, "stale-a")
  folder_b = fresh_root_view (context, "stale-b")
  edit_compact ( folder_b, [ ( "** Second child, a subscriber ⟨10 ",
                               "** Second child, renamed in B ⟨10 " ) ])
  require_save_succeeds (folder_b)
  edit_compact ( folder_a, [ ( "** First child, renamed ⟨8⟩",
                               "** First child, renamed in A ⟨8⟩" ) ])
  full_before = read_file ( os . path . join (folder_a, ROOT_BASE_NAME + ".full.org") )
  report = (
    REPORT_HEAD +
    "Stale: ⟨10⟩ 'Second child, a subscriber': its title changed on the "
    "server since the view was rendered.\n"
    "Title: ⟨8⟩ 'First child, renamed' → 'First child, renamed in A'.\n" )
  completed = run_save (folder_a, "--yes")
  require_output ( completed, report, expected_returncode = 1 )
  if "'--overwrite-stale'" not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  require_equal ( "B's title after the refused save",
                  skg_title (context, "child-b-id"), "Second child, renamed in B" )
  require_equal ( "A's full view after the refused save",
                  read_file ( os . path . join (folder_a, ROOT_BASE_NAME + ".full.org") ),
                  full_before )
  require_output ( run_save (folder_a, "--yes", "--overwrite-stale"),
                   report + saved_line (0) + written_paths (folder_a) )
  require_equal ( "A's edit", skg_title (context, "child-a-id"),
                  "First child, renamed in A" )
  # What the server did with the stale node: A's text replaced B's.
  require_equal ( "B's title after A overwrote it",
                  skg_title (context, "child-b-id"), "Second child, a subscriber" )

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

def edit_compact (folder, replacements):
  path = os . path . join (folder, ROOT_BASE_NAME + ".compact.org")
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
  """The ID in the metadata of the full-view line that the handle of
  the compact headline starting HEADLINE_START names."""
  for line in read_compact (folder) . split ("\n"):
    if line . startswith (headline_start + " ⟨"):
      line_number = int ( HANDLE_PATTERN . search (line) . group (1) )
      full_lines = read_file (
        os . path . join (folder, ROOT_BASE_NAME + ".full.org") ) . split ("\n")
      match = UUID_PATTERN . search ( full_lines [ line_number - 1 ] )
      if match is None:
        raise CaseFailure (
          f"line {line_number} of the full view has no UUID: "
          f"{full_lines [ line_number - 1 ]!r}" )
      return match . group (0)
  raise CaseFailure (
    f"no headline {headline_start!r} in:\n{read_compact (folder)}" )

def require_output (completed, expected_stdout, expected_returncode = 0):
  if ( completed . stdout != expected_stdout
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

def run_tool (tool_arguments):
  return subprocess . run (
    [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py") ]
    + tool_arguments + [ "--port", str (PORT) ],
    capture_output = True, text = True )

def failures_of_handles (base_name, full_lines, compact):
  """Each handle names a headline of the full view, every headline is
  named once, and the compact tree has one line per full-view line."""
  tree_lines = [ line for line in compact . split ("\n") [ : -1 ]
                 if not line . startswith ("# ") ]
  full_lines = full_lines [ : -1 ] if full_lines [-1] == "" else full_lines
  failures = []
  if len (tree_lines) != len (full_lines):
    failures . append (
      f"{base_name}: {len (tree_lines)} tree lines for {len (full_lines)} full lines" )
  for index, line in enumerate (tree_lines):
    match = HANDLE_PATTERN . search (line)
    if line . startswith ("*"):
      if not match or int ( match . group (1) ) != index + 1:
        failures . append ( f"{base_name}: line {index + 1} has handle {line!r}" )
      elif not full_lines [index] . startswith ("*"):
        failures . append ( f"{base_name}: handle {index + 1} is no headline" )
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
