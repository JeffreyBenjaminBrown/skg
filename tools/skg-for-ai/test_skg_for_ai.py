#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

In order, it:
- checks that a save's envelope carries the saved buffer's baseline;
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
  through 'import'; searches with each search option; pushes, pastes
  and pops the linkstack; prints the restriction, and refuses to set
  it without a daemon;
- on a second scratch server, edits compact views and saves them:
  a title, a body, a new child, a move, a removal, a deletion, bad
  handles, an edit the server refuses, a stale view the server
  refuses, 'new', one view over several saves, whose handles stay,
  and retyped headlines (an exact retype refused, then saved marked
  '⟨new⟩'; a reworded one asked about, then answered '--keep-ids',
  which keeps its node, or '--all-new', which makes a new one),
  checking each report exactly and the graph after each save;
- on a third scratch server, places existing nodes with '⟨node:ID⟩'
  (an owned and a foreign one, each then editable with its real title;
  one beside an editable occurrence, and one placed twice, the later
  placed write-protected; refused placements), refuses an edit of a
  write-protected foreign node, makes it editable with 'editable',
  forks a placed foreign node by adding a child to it (declined
  without '--yes', approved with it: the clone subscribes to and
  overrides it), forks an owned node with 'fork' (refused beside
  another edit, then approved), and subscribes and hides by editing a
  subscribee folder;
- on a fourth scratch server, writes the words of a handle:
  'detach' and 'attach', 'write-protect' (and 'subtree'),
  'noSearchMatching' and 'searchMatching' (and 'subtree', skipping
  foreign nodes), 'merge:ID', 'delete subtree', and words refused where
  they stand;
- on a fifth scratch server, whose owned skgrepo is a gitrepo with the
  fixture committed: saves a retitle, a new child and a deletion,
  checks the diff view's compact buffer exactly, marks the new child
  ready, restores the deleted node, and commits (refused unconfirmed,
  then confirmed with '--yes'), checking 'git log' and that the diff
  view is then empty; then extends a diff buffer to a change it does not
  make editable, and saves an edit of it; marks and unmarks a subtree's
  changes with 'ready subtree' and 'unready subtree'; a blank commit
  message is refused unsent;
- on a sixth scratch server, also over a gitrepo, runs a daemon:
  commands through it write what they write on their own connections;
  it rewrites a held view after a save through it changes a node the
  view shows, and after a second server over the same data root saves
  one (the first server's watcher reloads the file), logging each,
  but keeps a view with unsaved edits; it logs the warning of a file
  the watcher cannot read; a command for its port writing into another
  folder runs through it, and that view is rewritten after a save; a
  second daemon for the port is refused; 'commit --yes' through it, and
  an interactive 'commit' beside it, each rewrite its held buffer of
  the diff view; 'close' and 'daemon --stop' release its views; a
  socket a killed daemon left is replaced, and SIGTERM stops a daemon;
- on a seventh scratch server, whose fixture gains a second owned
  skgrepo, 'private' ('add_private_skgrepo'), both gitrepos, through a
  daemon: restricts the connection, and under it views, searches and
  saves with overPrivateText text withheld and then released, deletes
  a node a private record names (delete-from), and hides and shows the
  foreign skgrepos; then Hoists a telescope's title home, sets
  relRepos, moves nodes between the owned skgrepos (the strandeds
  question; '⟨new repo:NAME⟩'; 'repo:NAME subtree'), marks like moves
  ready, and removes the references to an absent node.
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
  failures += failures_of_save_envelope ()
  failures += failures_of_shared_herald_cases ()
  failures += failures_against_scratch_servers (update_expected)
  shutil . rmtree ( os . environ [ "XDG_RUNTIME_DIR" ], ignore_errors = True )
  for failure in failures:
    print ( "FAIL: " + failure )
  if failures:
    sys . exit (1)
  print ( "PASS" + ( " (expected files rewritten)" if update_expected else "" ))

# ============================================================
# The save envelope
# ============================================================

def failures_of_save_envelope ():
  """A save's envelope carries the saved buffer, its baseline (the
  text the server last drew for it), and the other views, each string
  quoted."""
  envelope = skg_for_ai . save_envelope_text (
    '* edited "λ"\n', '* drawn "λ"\n', "()" )
  expected = ( '((saved-buffer "* edited \\"λ\\"\n") '
               '(baseline "* drawn \\"λ\\"\n") (other-views ()))' )
  return ( [] if envelope == expected
           else [ f"the save envelope is {envelope!r}, not {expected!r}" ] )

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
                 "'delete'; it takes no other word but one 'repo:NAME'." ]:
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
  checks on another, then the placing and forking checks on a third,
  then the other words' on a fourth, then the reviewing checks on a
  fifth, then the daemon's on a sixth, then those of two owned
  skgrepos on a seventh, the last three with their owned skgrepos
  gitrepos; each on its own copy of the fixture."""
  if not os . path . exists (SERVER_BINARY):
    return [ f"no server binary at {SERVER_BINARY}; run 'cargo build --bin skg'" ]
  return ( failures_on_a_scratch_server (
             lambda scratch, data_root :
               failures_of_reading (scratch, data_root, update_expected) )
           + failures_on_a_scratch_server (failures_of_writing)
           + failures_on_a_scratch_server (failures_of_placing_and_forking)
           + failures_on_a_scratch_server (failures_of_more_words)
           + failures_on_a_scratch_server (
               failures_of_reviewing, owned_skgrepo_in_git = True )
           + failures_on_a_scratch_server (
               failures_of_daemon, owned_skgrepo_in_git = True )
           + failures_on_a_scratch_server (
               failures_of_two_owned_skgrepos, owned_skgrepo_in_git = True,
               with_private_skgrepo = True ) )

def failures_on_a_scratch_server (check, owned_skgrepo_in_git = False,
                                  with_private_skgrepo = False):
  """CHECK (SCRATCH, DATA_ROOT)'s failures, run against a server on a
  fresh copy of the fixture in the scratch folder SCRATCH. With
  WITH_PRIVATE_SKGREPO, the copy gains a second owned skgrepo
  ('add_private_skgrepo'). With OWNED_SKGREPO_IN_GIT, each owned
  skgrepo is first made a gitrepo whose HEAD commit holds its files,
  as the diff view needs."""
  if port_is_listening (PORT):
    return [ f"port {PORT} is taken; set SKG_FOR_AI_TEST_PORT" ]
  scratch = tempfile . mkdtemp ( prefix = "skg-for-ai-test-" )
  data_root = os . path . join (scratch, "data")
  shutil . copytree (FIXTURE_FOLDER, data_root)
  set_port_in_config ( os . path . join (data_root, "skgconfig.toml"), PORT )
  if with_private_skgrepo:
    add_private_skgrepo (data_root)
  if owned_skgrepo_in_git:
    for owned in [ "main" ] + ( [ "private" ] if with_private_skgrepo else [] ):
      commit_fixture_in_new_gitrepo ( os . path . join (data_root, "me", owned) )
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
  for check in [ check_search_options, check_linkstack,
                 check_restriction_on_a_command_s_own_connection ]:
    try:
      check (scratch)
    except CaseFailure as failure:
      failures . append ( check . __name__ + ": " + str (failure) )
  failures += failures_of_changed_skg_files (data_root)
  return failures

def hit_titles (folder, base_name):
  """The titles of the top-level headlines of a search buffer's compact
  view: its hits."""
  return [ line [ 2 : line . index (" ⟨") ]
           for line in read_file ( os . path . join (
             folder, base_name + ".compact.org" )) . split ("\n")
           if line . startswith ("* ") ]

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
        [ "An aside of Fred's", "Fred's note", "A detail of Fred's" ] ) ]:
    require_tool_run ([ "search" ] + arguments + [ "--out-dir", folder ])
    # sorted: hits that rank alike come in no fixed order
    require_equal ( "the hits of 'search " + " " . join (arguments) + "'",
                    sorted ( hit_titles (folder, base_name) ), sorted (titles) )
  require_equal ( "the sidecar's request of a search with options",
                  read_sidecar (folder, "search_fre.") [ "request" ],
                  { "terms" : "fre.*", "regex" : True } )

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
                             "** First child, renamed ⟨0s⟩" ),
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
    "Error: ⟨0s⟩ on line 8 of the compact view names no headline of the "
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

def compact_line_starting (folder, start, base_name = ROOT_BASE_NAME):
  """The one line of the compact file BASE_NAME (by default the root
  view's) that starts with START."""
  compact = read_file ( os . path . join (folder, base_name + ".compact.org") )
  lines = [ line for line in compact . split ("\n") if line . startswith (start) ]
  if len (lines) != 1:
    raise CaseFailure ( f"{len (lines)} lines start {start!r}:\n{compact}" )
  return lines [0]

# ============================================================
# Placing and forking: on a fresh fixture, existing nodes are placed
# under others with '⟨node:ID⟩', a placed foreign node is forked by an
# edit (declined, then approved), a write-protected foreign node is
# made editable with 'editable', and an owned node is forked with
# 'fork'. The cases run in order on one server.
# ============================================================

FORK_INTO_MAIN = (
  "Fork into: 'main', the one skgrepo this server owns, unless the edit "
  "settles another. The server asks before forking; '--yes' approves.\n" )

def failures_of_placing_and_forking (scratch, data_root):
  context = { "scratch"   : scratch,
              "main_repo" : os . path . join (data_root, "me", "main"),
              "fred_repo" : os . path . join (data_root, "fred", "public") }
  for case in [ check_placing_nodes, check_placing_beside_an_editable_occurrence,
                check_placing_a_node_twice, check_placement_errors,
                check_write_protected_foreign_edit_refused,
                check_making_a_foreign_node_editable, check_implicit_fork_declined,
                check_implicit_fork_approved, check_explicit_fork,
                check_subscribing_and_hiding_by_placing ]:
    try:
      case (context)
    except CaseFailure as failure:
      return [ case . __name__ + ": " + str (failure) ]
  return []

def check_placing_nodes (context):
  """Two existing nodes, one owned and one foreign, placed under the
  first child with titles the server replaces: each is saved as a
  pasted occurrence that asks for an editable view, so the re-rendered
  view shows each editable, with its real title and its content."""
  folder = fresh_root_view (context, "place")
  edit_compact ( folder, [ (
    "A body line that stays unnumbered.\n",
    "A body line that stays unnumbered.\n"
    "*** any words at all ⟨node:elsewhere-id⟩\n"
    "*** Fred's ⟨node:fred-aside-id⟩\n" ) ])
  report = (
    REPORT_HEAD +
    "Placed: ⟨node:elsewhere-id⟩ 'A second container, outside the view', "
    "under ⟨0d⟩ 'First child'.\n"
    "Placed: ⟨node:fred-aside-id⟩ 'An aside of Fred's', under ⟨0d⟩ 'First "
    "child'.\n" )
  require_output ( run_save (folder), report + NOT_SAVED )
  require_equal ( "the container's content before '--yes'",
                  skg_list (context, "child-a-id", "contains"), [ "shared-id" ] )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + written_paths (folder) )
  require_equal ( "the container's content",
                  skg_list (context, "child-a-id", "contains"),
                  [ "elsewhere-id", "fred-aside-id", "shared-id" ] )
  require_in_compact (
    folder,
    "** First child ⟨0d aC3⟩\n"
    "A body line that stays unnumbered.\n"
    "*** A second container, outside the view ⟨0k aC1⟩\n"
    "**** Contained twice ⟨0l ☮ 2aC⟩\n"
    "*** An aside of Fred's ⟨0m 1H ⌂:fred⟩\n"
    "*** Contained twice ⟨0e 2aC⟩\n" )
  if "writeProtected (viewRequests" in read_file (
      os . path . join (folder, ROOT_BASE_NAME + ".full.org") ):
    raise CaseFailure ( "the re-rendered view still carries a paste's request" )

def check_placing_beside_an_editable_occurrence (context):
  """A node placed where the view already holds an editable
  occurrence of it is placed write-protected, without the view
  request, and the report says so."""
  folder = fresh_root_view (context, "place-again")
  with open ( os . path . join ( folder, ROOT_BASE_NAME + ".compact.org" ),
              "a", encoding = "utf-8" ) as compact_file:
    compact_file . write ( "*** Contained twice ⟨node:shared-id⟩\n" )
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Placed: ⟨node:shared-id⟩ 'Contained twice', under ⟨0i⟩ 'Second child, "
    "a subscriber', write-protected, since the view already holds an "
    "editable occurrence of it.\n" + saved_line (0) + written_paths (folder) )
  require_equal ( "the second container's content",
                  skg_list (context, "child-b-id", "contains"), [ "shared-id" ] )
  if not read_compact (folder) . endswith ( "*** Contained twice ⟨0q ☮ 3aC⟩\n" ):
    raise CaseFailure (
      f"the placed occurrence is not last and write-protected:\n{read_compact (folder)}" )

def check_placing_a_node_twice (context):
  """A foreign node placed twice in one save: the first placement asks
  for the editable view, the second, finding it, is placed
  write-protected."""
  folder = fresh_root_view (context, "place-twice")
  edit_compact ( folder, [ (
    "A body line that stays unnumbered.\n",
    "A body line that stays unnumbered.\n"
    "*** Fred's detail ⟨node:fred-detail-id⟩\n" ) ])
  with open ( os . path . join ( folder, ROOT_BASE_NAME + ".compact.org" ),
              "a", encoding = "utf-8" ) as compact_file:
    compact_file . write ( "*** Fred's detail again ⟨node:fred-detail-id⟩\n" )
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Placed: ⟨node:fred-detail-id⟩ 'A detail of Fred's', under ⟨0d⟩ 'First "
    "child'.\n"
    "Placed: ⟨node:fred-detail-id⟩ 'A detail of Fred's', under ⟨0i⟩ 'Second "
    "child, a subscriber', write-protected, since the view already holds an "
    "editable occurrence of it.\n" + saved_line (0) + written_paths (folder) )
  require_equal ( "the first child's content after placing twice",
                  skg_list (context, "child-a-id", "contains"),
                  [ "fred-detail-id", "elsewhere-id", "fred-aside-id", "shared-id" ] )
  require_equal ( "the second child's content after placing twice",
                  skg_list (context, "child-b-id", "contains"),
                  [ "shared-id", "fred-detail-id" ] )

def check_placement_errors (context):
  """A placement of an ID the graph lacks, one with a body, one with a
  child and one carrying another word are refused before any save."""
  folder = fresh_root_view (context, "place-errors")
  edit_compact ( folder, [ (
    "A body line that stays unnumbered.\n",
    "A body line that stays unnumbered.\n"
    "*** Nobody ⟨node:nobody-id⟩\n"
    "*** With a body ⟨node:top-id⟩\n"
    "Its body line.\n"
    "*** With a child ⟨node:overridden-id⟩\n"
    "**** Its child\n"
    "*** With a word ⟨node:overrider-id fork⟩\n" ) ])
  graph_before = skg_files (context)
  completed = run_save (folder, "--yes")
  require_output (
    completed,
    REPORT_HEAD +
    "Error: ⟨node:top-id⟩ on line 10 of the compact view has a body; a placed "
    "node is saved write-protected, which saves no body. Save the placement "
    "first, then edit its body.\n"
    "Error: ⟨node:overridden-id⟩ on line 12 of the compact view has children; "
    "a placed node is saved write-protected, under which nothing claims "
    "membership. Save the placement first, then add them.\n"
    "Error: ⟨node:overrider-id⟩ on line 14 of the compact view carries "
    "'fork'; it takes no other word.\n",
    expected_returncode = 1 )
  edit_compact ( folder, [ ( "Its body line.\n", "" ), ( "**** Its child\n", "" ),
                           ( "⟨node:overrider-id fork⟩", "⟨node:overrider-id⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨node:nobody-id⟩ on line 9 of the compact view names no node of "
    "the graph.\n",
    expected_returncode = 1 )
  require_equal ( "the graph after refused placements",
                  skg_files (context), graph_before )

def check_write_protected_foreign_edit_refused (context):
  """An edit of a write-protected occurrence of a foreign node, in
  content, forks nothing, so it is refused before any save, saying how
  to fork it."""
  folder = fresh_root_view (context, "fork-write-protected")
  detail = compact_line_starting (folder, "*** A detail of Fred's ⟨0s ")
  edit_compact ( folder, [ ( detail, detail . replace (
    "A detail of Fred's ⟨", "A detail of Fred's, mine ⟨" )) ])
  graph_before = skg_files (context)
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨0s⟩ 'A detail of Fred's', of the foreign skgrepo 'fred', is "
    "write-protected, and its title changed. A write-protected occurrence "
    "saves nothing of its own, so this would fork nothing. To fork it, undo "
    "the edit, write 'editable' in its handle and save; then edit it and "
    "save again.\n",
    expected_returncode = 1 )
  require_equal ( "the graph after a refused edit",
                  skg_files (context), graph_before )

def check_making_a_foreign_node_editable (context):
  """'editable' in the handle of that write-protected occurrence asks
  for an editable view, as the clients' 'C-c s d' does: the save
  writes nothing, and the re-rendered view shows it editable, and the
  occurrence that was editable write-protected."""
  folder = fresh_root_view (context, "editable")
  detail = compact_line_starting (folder, "*** A detail of Fred's ⟨0s ")
  edit_compact ( folder, [ ( detail, detail [ : -1 ] + " editable⟩" ) ])
  graph_before = skg_files (context)
  fred_before = fred_files (context)
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Editable: ⟨0s⟩ 'A detail of Fred's', as '(viewRequests editableView)': "
    "the save makes it this view's editable occurrence of its node.\n"
    + saved_line (0) + written_paths (folder) )
  require_equal ( "the owned files after 'editable'", skg_files (context), graph_before )
  require_equal ( "the foreign files after 'editable'", fred_files (context), fred_before )
  if "☮" in compact_line_starting (folder, "*** A detail of Fred's ⟨0s "):
    raise CaseFailure ( f"the occurrence is still write-protected:\n{read_compact (folder)}" )
  if "☮" not in compact_line_starting (folder, "*** A detail of Fred's ⟨0e "):
    raise CaseFailure ( f"the other occurrence is still editable:\n{read_compact (folder)}" )

def check_implicit_fork_declined (context):
  """A child added under the editable occurrence of a foreign node
  forks it: the report says so, and names the clone's skgrepo. Without
  '--yes' nothing is sent; a '--fork-into' naming no owned skgrepo is
  refused."""
  folder = fresh_root_view (context, "fork")
  context [ "fork_folder" ] = folder
  aside = compact_line_starting (folder, "*** An aside of Fred's ⟨")
  edit_compact ( folder, [ ( aside + "\n", aside + "\n**** My note on it\n" ) ])
  graph_before = skg_files (context)
  fred_before = fred_files (context)
  require_output (
    run_save (folder),
    REPORT_HEAD +
    "New: 'My note on it', under ⟨0h⟩ 'An aside of Fred's'.\n"
    "Fork: ⟨0h⟩ 'An aside of Fred's', of the foreign skgrepo 'fred', whose "
    "children changed: the edit goes to a new clone of it, which subscribes "
    "to it and overrides it; the original stays as it is.\n"
    + FORK_INTO_MAIN + NOT_SAVED )
  require_output (
    run_save (folder, "--yes", "--fork-into", "fred"),
    REPORT_HEAD +
    "Error: '--fork-into fred' names no owned skgrepo of this server; its "
    "owned ones are: main.\n",
    expected_returncode = 1 )
  require_equal ( "the owned files after a declined fork",
                  skg_files (context), graph_before )
  require_equal ( "the foreign files after a declined fork",
                  fred_files (context), fred_before )

def check_implicit_fork_approved (context):
  """With '--yes' the server's fork confirmation is printed and
  approved: a clone in 'main' holds the edit and subscribes to and
  overrides the original, which is unchanged, and the re-rendered view
  shows the clone in the original's place."""
  folder = context [ "fork_folder" ]
  fred_before = fred_files (context)
  files_before = set ( skg_files (context) )
  completed = run_save (folder, "--yes")
  require_output (
    completed,
    REPORT_HEAD +
    "New: 'My note on it', under ⟨0h⟩ 'An aside of Fred's'.\n"
    "Fork: ⟨0h⟩ 'An aside of Fred's', of the foreign skgrepo 'fred', whose "
    "children changed: the edit goes to a new clone of it, which subscribes "
    "to it and overrides it; the original stays as it is.\n"
    + FORK_INTO_MAIN +
    "Server: fork confirmation: the save forks 1 node, and writes nothing "
    "until that is approved.\n"
    "Clone-to-be: 'An aside of Fred's', in 'main' (the server suggests "
    "'main').\n"
    "  It forks 'An aside of Fred's' (fred-aside-id), of the skgrepo 'fred'.\n"
    "Approved: the save is sent again with the forks approved.\n"
    + saved_line (0) + written_paths (folder) )
  new_files = set ( skg_files (context) ) - files_before
  clone_id = id_of_compact_headline (folder, "*** An aside of Fred's")
  child_id = id_of_compact_headline (folder, "**** My note on it")
  require_equal ( "the files the fork added", new_files,
                  { clone_id + ".skg", child_id + ".skg" } )
  require_equal ( "the clone's title", skg_title (context, clone_id),
                  "An aside of Fred's" )
  require_equal ( "the clone's content", skg_list (context, clone_id, "contains"),
                  [ child_id ] )
  require_equal ( "what the clone subscribes to",
                  skg_list (context, clone_id, "subscribesTo"), [ "fred-aside-id" ] )
  require_equal ( "what the clone overrides",
                  skg_list (context, clone_id, "overrides"), [ "fred-aside-id" ] )
  require_equal ( "the foreign files after the fork",
                  fred_files (context), fred_before )
  require_equal ( "the container's content after the fork",
                  skg_list (context, "child-a-id", "contains"),
                  [ "fred-detail-id", "elsewhere-id", "fred-aside-id", "shared-id" ] )
  require_in_compact ( folder, "*** An aside of Fred's ⟨0h C1 S1 Oĥ1⟩\n"
                               "**** My note on it ⟨0t⟩\n" )

def check_explicit_fork (context):
  """'fork' on an owned node, as the clients' 'skg-fork-node' marks
  it: refused beside other edits, then approved with '--yes', making a
  clone in 'main' that subscribes to and overrides it."""
  folder = fresh_root_view (context, "fork-explicit")
  shared = compact_line_starting (folder, "*** Contained twice ⟨0o ")
  edit_compact ( folder, [ ( shared, shared [ : -1 ] + " fork⟩" ) ])
  first_child = compact_line_starting (folder, "** First child ⟨")
  edit_compact ( folder, [ ( first_child, first_child . replace (
    "First child", "First child, renamed" )) ])
  graph_before = skg_files (context)
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨0o⟩ 'Contained twice' carries 'fork', but the compact view has "
    "other edits; save them first, as a client forks only from a saved "
    "buffer, so that the clone is the node as it is saved.\n",
    expected_returncode = 1 )
  require_equal ( "the graph after a refused fork",
                  skg_files (context), graph_before )
  first_child = compact_line_starting (folder, "** First child, renamed ⟨")
  edit_compact ( folder, [ ( first_child, first_child . replace (
    "First child, renamed", "First child" )) ])
  files_before = set ( skg_files (context) )
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Fork: ⟨0o⟩ 'Contained twice', by request: a new clone of it subscribes "
    "to it and overrides it.\n" + FORK_INTO_MAIN +
    "Server: fork confirmation: the save forks 1 node, and writes nothing "
    "until that is approved.\n"
    "Clone-to-be: 'Contained twice', in 'main' (the server suggests "
    "'main').\n"
    "  It forks 'Contained twice' (shared-id), of the skgrepo 'main'.\n"
    "Approved: the save is sent again with the forks approved.\n"
    + saved_line (0) + written_paths (folder) )
  [ clone_file ] = set ( skg_files (context) ) - files_before
  clone_id = clone_file [ : - len (".skg") ]
  require_equal ( "the explicit clone's title", skg_title (context, clone_id),
                  "Contained twice" )
  require_equal ( "what the explicit clone subscribes to",
                  skg_list (context, clone_id, "subscribesTo"), [ "shared-id" ] )
  require_equal ( "what the explicit clone overrides",
                  skg_list (context, clone_id, "overrides"), [ "shared-id" ] )
  require_equal ( "the forked node's title", skg_title (context, "shared-id"),
                  "Contained twice" )

def check_subscribing_and_hiding_by_placing (context):
  """In the editable subscribee folder 'forest subscribesTo' draws, a
  placed foreign node is subscribed to; its unintegrated child, then
  removed from under it, is hidden from the subscription. Neither
  forks."""
  folder = os . path . join ( context [ "scratch" ], "write", "subscribe" )
  require_tool_run ([ "forest", "subscribesTo", "elsewhere-id", "--out-dir",
                      folder, "--no-summary" ])
  base_name = "forest-subscribesTo_elsewhere-id"
  compact_path = os . path . join ( folder, base_name + ".compact.org" )
  compact = read_file (compact_path)
  folder_line = next ( line for line in compact . split ("\n")
                       if "It subscribes to these." in line )
  edit_compact ( folder, [ ( folder_line + "\n", folder_line +
                             "\n*** Fred's ⟨node:fred-note-id⟩\n" ) ], base_name )
  if "Server: saved," not in run_save (folder, "--yes") . stdout:
    raise CaseFailure ( "the subscribing save failed" )
  require_equal ( "what the node subscribes to",
                  skg_list (context, "elsewhere-id", "subscribesTo"),
                  [ "fred-note-id" ] )
  detail = next ( line for line in read_file (compact_path) . split ("\n")
                  if line . startswith ("**** A detail of Fred's ⟨") )
  edit_compact ( folder, [ ( detail + "\n", "" ) ], base_name )
  completed = run_save (folder, "--yes")
  if "Fork:" in completed . stdout or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the hiding save printed {completed . stdout!r}" )
  require_equal ( "what the node hides from its subscriptions",
                  skg_list (context, "elsewhere-id", "hidesFromSubs"),
                  [ "fred-detail-id" ] )

# ============================================================
# More words: each case views a node afresh, writes words in handles,
# saves, and checks the report and the graph, on the graph the earlier
# ones left.
# ============================================================

def failures_of_more_words (scratch, data_root):
  context = { "scratch"   : scratch,
              "main_repo" : os . path . join (data_root, "me", "main") }
  for case in [ check_detaching_and_attaching, check_write_protecting,
                check_search_matching, check_merging, check_deleting_a_subtree,
                check_misplaced_words ]:
    try:
      case (context)
    except CaseFailure as failure:
      return [ case . __name__ + ": " + str (failure) ]
  return []

def check_detaching_and_attaching (context):
  """'detach' takes a child out of its parent's list and keeps it in
  the view, unwarranted ('⊥'); 'attach' puts it back. Either is
  refused where the parent reads no claims, and where the headline
  already claims (or does not) what it asks."""
  folder = fresh_root_view (context, "detach")
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2aC⟩",
                             "*** Contained twice ⟨0e 2aC detach⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Detach: ⟨0e⟩ 'Contained twice', from ⟨0d⟩ 'First child', as 'noClaim': "
    "the save takes it out of its parent's list, and the view still shows "
    "it.\n" + saved_line (0) + written_paths (folder) )
  require_equal ( "the parent's content after 'detach'",
                  skg_list (context, "child-a-id", "contains"), [] )
  require_in_compact ( folder, "** First child ⟨0d⟩\n"
                               "A body line that stays unnumbered.\n"
                               "*** Contained twice ⟨0e ⊥ 1C⟩\n" )
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e ⊥ 1C⟩",
                             "*** Contained twice ⟨0e ⊥ 1C attach⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Attach: ⟨0e⟩ 'Contained twice', to ⟨0d⟩ 'First child': the save puts it "
    "in its parent's list.\n" + saved_line (0) + written_paths (folder) )
  require_equal ( "the parent's content after 'attach'",
                  skg_list (context, "child-a-id", "contains"), [ "shared-id" ] )
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2aC⟩",
                             "*** Contained twice ⟨0e 2aC attach⟩" ),
                           ( "*** Above the root ⟨0c ☮ Cb⟩",
                             "*** Above the root ⟨0c ☮ Cb detach⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨0c⟩ on line 6 of the compact view carries 'detach', but its "
    "parent reads no claims (it is write-protected, or no editable node), so "
    "no child claims membership of it.\n"
    "Error: ⟨0e⟩ on line 9 of the compact view carries 'attach', but claims "
    "membership already.\n",
    expected_returncode = 1 )

def check_write_protecting (context):
  """'write-protect' makes an occurrence write-protected, keeping its
  children's editability; 'write-protect subtree' makes its content
  subtree write-protected too. Neither writes a file. 'write-protect'
  beside 'editable', or on a folder, is refused."""
  folder = fresh_root_view (context, "write-protect")
  graph_before = skg_files (context)
  edit_compact ( folder, [ ( "** First child ⟨0d aC1⟩",
                             "** First child ⟨0d aC1 write-protect⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Write-protect: ⟨0d⟩ 'First child', as '(viewRequests "
    "writeProtectedView)': after saving its edits, the save makes it "
    "write-protected.\n" + saved_line (0) + written_paths (folder) )
  require_in_compact ( folder, "** First child ⟨0d ☮B aC1⟩\n"
                               "*** Contained twice ⟨0e 2aC⟩\n" )
  folder = fresh_root_view (context, "write-protect-subtree")
  edit_compact ( folder, [ ( "** First child ⟨0d aC1⟩",
                             "** First child ⟨0d aC1 write-protect subtree⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Write-protect: ⟨0d⟩ 'First child', as '(viewRequests "
    "writeProtectedSubtreeView)': after saving the edits, the save makes it "
    "and every occurrence of its content subtree write-protected.\n"
    + saved_line (0) + written_paths (folder) )
  require_in_compact ( folder, "** First child ⟨0d ☮B aC1⟩\n"
                               "*** Contained twice ⟨0e ☮ 2aC⟩\n" )
  require_equal ( "the graph after write-protecting", skg_files (context),
                  graph_before )
  folder = fresh_root_view (context, "write-protect-refused")
  edit_compact ( folder, [ ( "** First child ⟨0d aC1⟩",
                             "** First child ⟨0d aC1 write-protect editable⟩" ),
                           ( "** ⟨0b ☮ These contain it.⟩",
                             "** ⟨0b ☮ These contain it. write-protect⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨0b⟩ on line 5 of the compact view carries 'write-protect', but "
    "is not a node occurrence; only a node takes it.\n"
    "Error: ⟨0d⟩ on line 7 of the compact view carries both 'write-protect' "
    "and 'editable'.\n",
    expected_returncode = 1 )

def check_search_matching (context):
  """'noSearchMatching' sets the node's flag, 'searchMatching' clears
  it. 'noSearchMatching subtree' sets it on the node and every member
  occurrence below, skipping the foreign ones, which it names."""
  folder = fresh_root_view (context, "search-matching")
  edit_compact ( folder, [ ( "** First child ⟨0d aC1⟩",
                             "** First child ⟨0d aC1 noSearchMatching⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Search matching: ⟨0d⟩ 'First child', as '(flag noSearchMatching true)': "
    "the node no longer matches searches.\n"
    + saved_line (0) + written_paths (folder) )
  require_equal ( "the flags after 'noSearchMatching'",
                  skg_list (context, "child-a-id", "flags"), [ "NoSearchMatching" ] )
  edit_compact ( folder, [ ( "** First child ⟨0d aC1 F1⟩",
                             "** First child ⟨0d aC1 F1 searchMatching⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Search matching: ⟨0d⟩ 'First child', as '(flag noSearchMatching false)': "
    "the node matches searches again.\n"
    + saved_line (0) + written_paths (folder) )
  require_equal ( "the flags after 'searchMatching'",
                  skg_list (context, "child-a-id", "flags"), [] )
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2aC⟩\n",
                             "*** Contained twice ⟨0e 2aC⟩\n"
                             "*** Fred's ⟨node:fred-note-id⟩\n" ) ])
  require_save_succeeds (folder)
  root = compact_line_starting (folder, "* The fixture root ⟨0a ")
  edit_compact ( folder, [ ( root, root [ : -1 ] + " noSearchMatching subtree⟩" ) ])
  completed = run_save (folder, "--yes")
  require_equal ( "the report of 'noSearchMatching subtree'",
                  [ line for line in completed . stdout . split ("\n")
                    if line . startswith ( ( "Search matching:", "Skipped:" ) ) ],
                  [ "Search matching: ⟨0a⟩ 'The fixture root', as '(flag "
                    "noSearchMatching true)': the node no longer matches searches.",
                    "Search matching: ⟨0d⟩ 'First child', as '(flag "
                    "noSearchMatching true)': the node no longer matches searches.",
                    "Search matching: ⟨0e⟩ 'Contained twice', as '(flag "
                    "noSearchMatching true)': the node no longer matches searches.",
                    "Search matching: ⟨0f⟩ 'Second child, a subscriber', as "
                    "'(flag noSearchMatching true)': the node no longer matches "
                    "searches.",
                    "Skipped: 'noSearchMatching subtree' on ⟨0a⟩ leaves ⟨0k⟩ "
                    "'Fred's note' (foreign), ⟨0l⟩ 'A detail of Fred's' "
                    "(foreign)." ] )
  for node_id in [ "root-id", "child-a-id", "shared-id", "child-b-id" ]:
    require_equal ( node_id + "'s flags after 'noSearchMatching subtree'",
                    skg_list (context, node_id, "flags"), [ "NoSearchMatching" ] )

def check_merging (context):
  """'merge:ID' makes the node acquire the node ID by a nodeMerge: the
  acquiree's file goes, and its ID becomes one of the acquirer's. An ID
  the graph lacks is refused before any save."""
  folder = os . path . join ( context [ "scratch" ], "write", "merge" )
  require_tool_run ([ "view", "elsewhere-id", "--out-dir", folder, "--no-summary" ])
  base_name = "view_elsewhere-id"
  head = ( "Changes in view_elsewhere-id.compact.org, against "
           "view_elsewhere-id.full.org (view ID content-view:elsewhere-id):\n" )
  root = compact_line_starting (folder, "* A second container", base_name)
  edit_compact ( folder, [ ( root, root [ : -1 ] + " merge:nobody-id⟩" ) ], base_name )
  require_output (
    run_save (folder, "--yes"),
    head + "Error: ⟨0a⟩ on line 1 of the compact view carries "
    "'merge:nobody-id', which names no node of the graph.\n",
    expected_returncode = 1 )
  edit_compact ( folder, [ ( "merge:nobody-id", "merge:overridden-id" ) ], base_name )
  require_output (
    run_save (folder, "--yes"),
    head + "Merge: ⟨0a⟩ 'A second container, outside the view' acquires "
    "⟨node:overridden-id⟩ 'An overridden node, outside the view', as '(merge "
    "overridden-id)': a nodeMerge makes the two one node, this one, with the "
    "relationships of both.\n" + saved_line (0) +
    "" . join ( os . path . join (folder, base_name + suffix) + "\n"
                for suffix in ( ".full.org", ".compact.org", ".view.json" )))
  require_equal ( "whether the acquiree's file remains",
                  os . path . exists ( skg_path (context, "overridden-id") ), False )
  require_equal ( "the acquirer's extra IDs",
                  skg_list (context, "elsewhere-id", "extra_ids"),
                  [ "overridden-id" ] )

def check_deleting_a_subtree (context):
  """'delete subtree' deletes the node and every node occurrence below
  it, as 'skg-delete-recursive' marks them."""
  folder = fresh_root_view (context, "delete-subtree")
  edit_compact ( folder, [ ( "A body line that stays unnumbered.\n",
                             "A body line that stays unnumbered.\n"
                             "*** A branch to delete\n**** Its leaf\n" ) ])
  require_save_succeeds (folder)
  branch_id = id_of_compact_headline (folder, "*** A branch to delete")
  leaf_id = id_of_compact_headline (folder, "**** Its leaf")
  branch = compact_line_starting (folder, "*** A branch to delete ⟨")
  leaf = compact_line_starting (folder, "**** Its leaf ⟨")
  edit_compact ( folder, [ ( branch, branch [ : -1 ] + " delete subtree⟩" ) ])
  completed = run_save (folder, "--yes")
  require_equal ( "the report of 'delete subtree'",
                  [ line for line in completed . stdout . split ("\n")
                    if line . startswith ("Delete:") ],
                  [ f"Delete: ⟨{HANDLE_PATTERN . search (branch) . group (1)}⟩ "
                    "'A branch to delete', from the graph.",
                    f"Delete: ⟨{HANDLE_PATTERN . search (leaf) . group (1)}⟩ "
                    "'Its leaf', from the graph." ] )
  for node_id in [ branch_id, leaf_id ]:
    require_equal ( "whether a deleted node's file remains",
                    os . path . exists ( skg_path (context, node_id) ), False )
  require_equal ( "the first child's content after 'delete subtree'",
                  skg_list (context, "child-a-id", "contains"),
                  [ "shared-id", "fred-note-id" ] )

def check_misplaced_words (context):
  """Words that cannot stand where they are written are refused before
  any save: 'subtree' alone, 'relRepo:NAME' on a root, 'repo:NAME'
  naming a foreign skgrepo or the node's own."""
  folder = fresh_root_view (context, "misplaced")
  def append_word (start, word):
    line = compact_line_starting (folder, start)
    edit_compact ( folder, [ ( line, line [ : -1 ] + " " + word + "⟩" ) ])
    return ( HANDLE_PATTERN . search (line) . group (1),
             read_compact (folder) . split ("\n") . index (
               line [ : -1 ] + " " + word + "⟩" ) + 1 )
  first_child = append_word ("** First child ⟨", "subtree")
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    f"Error: ⟨{first_child [0]}⟩ on line {first_child [1]} of the compact view "
    "carries 'subtree' with no word it applies to; 'subtree' applies to "
    "'delete', 'ready', 'unready', 'write-protect', 'repo:NAME', "
    "'relRepo:NAME', 'noSearchMatching' and 'searchMatching'.\n",
    expected_returncode = 1 )
  edit_compact ( folder, [ ( " subtree⟩", "⟩" ) ])
  with open ( os . path . join ( folder, ROOT_BASE_NAME + ".compact.org" ),
              "a", encoding = "utf-8" ) as compact_file:
    compact_file . write ( "** Made in a foreign skgrepo ⟨new repo:fred⟩\n" )
  new_line = len ( read_compact (folder) . split ("\n") ) - 1
  root = append_word ("* The fixture root ⟨", "relRepo:main")
  contained = append_word ("*** Contained twice ⟨", "repo:fred")
  second = append_word ("** Second child, a subscriber ⟨", "repo:main")
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    f"Error: ⟨{root [0]}⟩ on line {root [1]} of the compact view carries "
    "'relRepo:main', but is a root: there is no relationship here to set.\n"
    f"Error: ⟨{contained [0]}⟩ on line {contained [1]} of the compact view "
    "carries 'repo:fred', but 'fred' is no owned skgrepo of this server; its "
    "owned ones are: main.\n"
    f"Error: ⟨{second [0]}⟩ on line {second [1]} of the compact view carries "
    "'repo:main', but its home skgrepo is 'main' already.\n"
    f"Error: ⟨new⟩ on line {new_line} of the compact view carries 'repo:fred', "
    "but 'fred' is no owned skgrepo of this server; its owned ones are: main.\n",
    expected_returncode = 1 )

def fred_files (context):
  """{FILE NAME: TEXT} of the foreign skgrepo."""
  return { name : read_file ( os . path . join ( context [ "fred_repo" ], name ))
           for name in sorted ( os . listdir ( context [ "fred_repo" ] ))
           if name . endswith (".skg") }

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
                check_extending_the_diff_view, check_marking_a_subtree_ready,
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

def check_extending_the_diff_view (context):
  """A buffer of the diff view shows a retitled root and, write-protected
  beneath it, a child whose own child another view then retitles. The
  buffer, as it stands, does not make that change editable; 'extend'
  makes the child editable, which draws the retitled grandchild,
  editable too, and a save of the buffer then retitles it again."""
  root_view = fresh_root_view (context, "extend-root")
  edit_compact ( root_view, [ ( "* The fixture root ⟨0a ",
                                "* The fixture root, retitled ⟨0a " ) ])
  require_save_succeeds (root_view)
  folder = run_diff (context, "extend")
  require_in_diff_compact (folder, "*** First child, renamed ⟨0d ☮B aC2⟩\n")
  require_equal ( "the write-protection of the first child's headline",
                  "writeProtected" in diff_full_line (folder, 7), True )
  grandchild_view = fresh_root_view (context, "extend-grandchild")
  edit_compact ( grandchild_view, [ ( "*** A new grandchild ⟨",
                                      "*** A grandchild, retitled ⟨" ) ])
  require_save_succeeds (grandchild_view)
  require_output (
    run_tool ([ "extend", sidecar_of_diff (folder) ]),
    "Before extending: 0 of 2 changed files are ready, 1 changes are "
    "not editable in this buffer.\n"
    "Extended: 0 of 2 changed files are ready.\n" +
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

def check_marking_a_subtree_ready (context):
  """'ready subtree' marks every change in the headline's subtree, as
  'C-u C-c r r' does, those below it without 'all'; 'unready subtree'
  unmarks them, asking first."""
  folder = run_diff (context, "subtree-ready")
  edit_compact ( folder, [ ( "** The fixture root, retitled ⟨0b aC2 L2 M⟩",
                             "** The fixture root, retitled ⟨0b aC2 L2 M ready "
                             "subtree⟩" ) ], DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Ready: ⟨0b⟩ 'The fixture root, retitled', as '(ready all)'.\n"
    "Ready: ⟨0c⟩ 'The fixture root', as '(ready)'.\n"
    "Ready: ⟨0g⟩ 'A new grandchild', as '(ready)'.\n" +
    saved_line (0) + "Readiness: Commit ready. All 2 of 2 changed files are ready.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  edit_compact ( folder, [ ( "** The fixture root, retitled ⟨0b aC2 L2 M✓⟩",
                             "** The fixture root, retitled ⟨0b aC2 L2 M✓ "
                             "unready subtree⟩" ) ], DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Unready: ⟨0b⟩ 'The fixture root, retitled', as '(unready all)'.\n"
    "Unready: ⟨0c⟩ 'The fixture root', as '(unready)'.\n"
    "Unready: ⟨0g⟩ 'A new grandchild', as '(unready)'.\n"
    "Question: this unmarks 3 changes. Unmarking this node commit-ready will "
    "unmark every contributing decision. You might discard a lot of hard-won "
    "information. Continue? Saving with '--yes' answers yes.\n" +
    saved_line (0) + "Readiness: 0 of 2 changed files are ready.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )

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
# How long a case waits for the watcher to reload a change on disk
# and the daemon to log it.
DAEMON_LOG_TIMEOUT_SECONDS = 20
SECOND_SERVER_PORT = PORT + 1
# What a daemon prints once its socket answers.
DAEMON_READY_LINE = "The daemon listens on"
RELOADED_ONE_MAIN_FILE = "changed on disk: reloaded 1 file from repo main"

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
                  check_daemon_logs_a_reload_warning,
                  check_daemon_declares_its_unsaved_views,
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
  not write; its watcher reloads it and tells the daemon's
  connection, naming the views that show the node; the daemon asks
  for their rerender, rewrites their files and logs why."""
  start_second_server (context)
  second_folder = save_through_second_server (
    context, "view_root-id",
    ( "** Second child, a subscriber ⟨0f ",
      "** Second child, renamed on the second server ⟨0f " ))
  require_log_line ( context, RELOADED_ONE_MAIN_FILE )
  require_log_line (
    context,
    "re-rendered view_root-id because files changed on disk (reloaded 1 "
    "file from repo main); 'Second child, renamed "
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

def check_daemon_logs_a_reload_warning (context):
  """A file the watcher cannot read is a warning in the log."""
  bad_path = os . path . join ( context [ "main_repo" ], "bad-id.skg" )
  with open (bad_path, "w", encoding = "utf-8") as bad_file:
    bad_file . write ( "title: [unclosed\n" )
  require_log_line ( context, "warning: Node bad-id changed on disk but could "
                              "not be read, so the server keeps serving its "
                              "previous version" )
  os . remove (bad_path)

def check_daemon_declares_its_unsaved_views (context):
  """A save through the daemon names, in its envelope's 'other-views',
  every other view the daemon holds, with the edited text of those
  whose compact views have unsaved edits, as the clients name their
  other buffers: the server then refuses a save that changes a node
  such a view shows, naming the view, and writes nothing. Undoing the
  unsaved edit lets the next case's save through."""
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice ⟨",
                             "** Contained twice, retitled through the daemon ⟨" ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if ( completed . returncode == 0
       or "this save conflicts with unsaved dependencies in content-view:root-id"
          not in completed . stdout ):
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "shared-id's title after the refused save",
                  skg_title (context, "shared-id"), "Contained twice" )
  edit_compact ( folder, [ ( "** Contained twice, retitled through the daemon ⟨",
                             "** Contained twice ⟨" ) ], "view_child-a-id" )
  edit_compact ( folder, [ ( "** First child, my unsaved edit ⟨0d ",
                             "** First child, renamed through the daemon ⟨0d " ) ])

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

# ============================================================
# Two owned skgrepos: the fixture gains 'private', more private than
# 'main' ('add_private_skgrepo'), both gitrepos, and a daemon holds the
# skgrepo restriction. The cases run in order, on the graph and the
# restriction the earlier ones left.
# ============================================================

def failures_of_two_owned_skgrepos (scratch, data_root):
  context = { "scratch"       : scratch,
              "data_root"     : data_root,
              "main_repo"     : os . path . join (data_root, "me", "main"),
              "private_repo"  : os . path . join (data_root, "me", "private"),
              "daemon_folder" : os . path . join (scratch, "daemon"),
              "processes"     : [] }
  try:
    for case in [ check_restricting, check_viewing_overPrivateText,
                  check_searching_under_a_restriction,
                  check_releasing_a_save_s_rerender,
                  check_deleting_under_a_restriction,
                  check_hiding_the_foreign_skgrepos, check_hoisting,
                  check_setting_relRepos, check_moving_to_another_skgrepo,
                  check_marking_like_moves_ready, check_deleting_references ]:
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

def private_lines (context, node_id):
  """The lines of NODE_ID's section in 'private', or None."""
  path = os . path . join ( context [ "private_repo" ], node_id + ".skg" )
  return read_file (path) . split ("\n") if os . path . exists (path) else None

def require_failure (completed, stdout_lines, stderr_text):
  """COMPLETED exited 1, printing every one of STDOUT_LINES (in order,
  among others) and STDERR_TEXT on standard error."""
  lines = completed . stdout . split ("\n")
  if ( completed . returncode != 1 or stderr_text not in completed . stderr
       or [ line for line in lines if line in stdout_lines ] != stdout_lines ):
    raise CaseFailure (
      f"exit {completed . returncode}, printed:\n{completed . stdout}stderr: "
      f"{completed . stderr}\nexpected exit 1, the lines {stdout_lines!r} and "
      f"on stderr {stderr_text!r}" )

def check_restricting (context):
  """'restrict' through a daemon sets its connection's skgrepo
  restriction, which every command run through it then has."""
  start_daemon (context)
  require_output ( run_tool ([ "restrict" ]),
                   "Skgrepo restriction: private (no restriction); available: "
                   "main, private (no restriction).\n" )
  require_output ( run_tool ([ "restrict", "main" ]),
                   "Skgrepo restriction: main\n" )
  require_output ( run_tool ([ "restrict" ]),
                   "Skgrepo restriction: main; available: main, private (no "
                   "restriction).\n" )
  require_log_line ( context, "Skgrepo restriction: main" )
  require_output ( run_tool ([ "restrict", "fred" ]), "", expected_returncode = 1 )

OVERPRIVATETEXT_PROMPT_OF_A_VIEW = (
  "Server: overPrivateText confirmation: the skgrepo restriction withholds "
  "text from overPrivateText telescopes (tele-id) until approved. single "
  "root view would reveal title or body text selected below its node's home "
  "repo for PID tele-id. Include that text?\n" )

def check_viewing_overPrivateText (context):
  """Under the restriction, viewing a telescope whose title sits below
  its home asks first: refused without '--show-overPrivateText',
  released with it."""
  folder = context [ "daemon_folder" ]
  completed = run_tool ([ "view", "tele-id", "--out-dir", folder ])
  require_output ( completed, "", expected_returncode = 1 )
  require_equal (
    "the refused view's stderr", completed . stderr,
    OVERPRIVATETEXT_PROMPT_OF_A_VIEW +
    "skg_for_ai: Nothing was viewed: the server withholds text from "
    "overPrivateText telescopes under the skgrepo restriction; run again with "
    "'--show-overPrivateText' to release it.\n" )
  completed = require_tool_run ([ "view", "tele-id", "--out-dir", folder,
                                  "--show-overPrivateText" ])
  require_equal ( "the released view's stderr", completed . stderr,
                  OVERPRIVATETEXT_PROMPT_OF_A_VIEW +
                  "Approved: the request is sent again, releasing that text.\n" )
  require_in_file ( folder, "view_tele-id",
                    "* A title kept private ⟨0a C1 ⌂:main⟩\n"
                    "** Contained twice ⟨0b 3aC⟩\n" )

def check_searching_under_a_restriction (context):
  """Under the restriction a search asks whether to include the
  overPrivateText telescopes: excluded by default, included with
  '--show-overPrivateText'."""
  folder = os . path . join ( context [ "scratch" ], "restricted-search" )
  completed = run_tool ([ "search", "kept", "--out-dir", folder ])
  require_output ( completed, "", expected_returncode = 1 )
  if ( "The search excludes them; '--show-overPrivateText' includes them."
       not in completed . stderr or "No matches found." not in completed . stderr ):
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  require_tool_run ([ "search", "kept", "--out-dir", folder,
                      "--show-overPrivateText" ])
  require_equal ( "the hits including overPrivateText telescopes",
                  hit_titles (folder, "search_kept"), [ "A title kept private" ] )

def check_releasing_a_save_s_rerender (context):
  """Under the restriction, a save whose re-rendered view shows an
  overPrivateText telescope's text happens, but the view is withheld:
  '--yes' alone leaves the files stale. With '--show-overPrivateText'
  the tool asks for the withheld rerender, approving it, rather than
  sending the save again, which the server would refuse as stale once
  the save had made a node; the files then show the new node."""
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice ⟨0b 3aC⟩",
                             "** Contained twice, retitled ⟨0b 3aC⟩" ) ],
                 "view_tele-id" )
  sidecar = os . path . join (folder, "view_tele-id.view.json")
  release_lines = [
    "Server: overPrivateText confirmation: the save succeeded, but the "
    "skgrepo restriction withholds text from overPrivateText telescopes "
    "(tele-id) until approved. save rerender would reveal title or body text "
    "selected below its node's home repo for PID tele-id. Include that "
    "text?" ]
  require_failure (
    run_tool ([ "save", sidecar, "--yes" ]), release_lines,
    "skg_for_ai: The save succeeded, but its re-rendered view is withheld; "
    "the files are out of date. View the node again with "
    "'--show-overPrivateText'.\n" )
  require_equal ( "the title saved before the release",
                  skg_title (context, "shared-id"), "Contained twice, retitled" )
  require_tool_run ([ "view", "tele-id", "--out-dir", folder,
                      "--show-overPrivateText", "--no-summary" ])
  edit_compact ( folder, [ ( "** Contained twice, retitled ⟨0b 3aC⟩\n",
                             "** Contained twice, retitled ⟨0b 3aC⟩\n"
                             "*** Added under the telescope\n" ) ],
                 "view_tele-id" )
  completed = require_tool_run ([ "save", sidecar, "--yes",
                                  "--show-overPrivateText" ])
  if not completed . stdout . endswith (
      release_lines [0] + "\n"
      "Approved: the withheld rerender is asked for, releasing that text.\n"
      "Server: saved, 0 errors, 1 warning.\n"
      "Server warning: rerender views includes title or body text selected "
      "below its node's home repo for PID tele-id.\n" +
      "" . join ( os . path . join (folder, "view_tele-id" + suffix) + "\n"
                  for suffix in ( ".full.org", ".compact.org", ".view.json" ))):
    raise CaseFailure ( f"the approving save printed {completed . stdout!r}" )
  new_id = id_of_compact_headline (
    folder, "*** Added under the telescope", "view_tele-id" )
  require_equal ( "the telescope's child's content after the release",
                  skg_list (context, "shared-id", "contains"), [ new_id ] )
  require_in_file ( folder, "view_tele-id",
                    "** Contained twice, retitled ⟨0b 3aC1⟩\n"
                    "*** Added under the telescope ⟨0c⟩\n" )

def check_deleting_under_a_restriction (context):
  """Under the restriction, deleting a node a record of 'private' names
  asks where to strip its ID: '--yes' alone cancels, writing nothing;
  '--delete-from visible' strips it from the visible records only."""
  folder = os . path . join ( context [ "scratch" ], "delete-from" )
  require_tool_run ([ "view", "doomed-id", "--out-dir", folder, "--no-summary" ])
  edit_compact ( folder, [ ( "* A node to delete ⟨0a",
                             "* A node to delete ⟨0a delete" ) ],
                 "view_doomed-id" )
  question = [
    "Server: delete-from confirmation: 1 record(s) the skgrepo restriction "
    "keeps out name a node this save deletes; nothing was written. 1 record "
    "in skgrepos the skgrepo restriction keeps out names a graphnode this "
    "save deletes. Nothing was saved." ]
  require_failure (
    run_tool ([ "save", folder, "--yes" ]), question,
    "skg_for_ai: Nothing was saved (cancelled): save again with "
    "'--delete-from visible' to strip the deleted IDs from the records the "
    "restriction shows only, or '--delete-from all' to strip them from every "
    "owned record.\n" )
  require_equal ( "whether the node's file remains after the cancel",
                  os . path . exists ( skg_path (context, "doomed-id") ), True )
  completed = run_tool ([ "save", folder, "--yes", "--delete-from", "visible" ])
  if ( "Delete from: the visible records only; the save is sent again.\n"
       "Server: saved, 0 errors, 0 warnings.\n" not in completed . stdout ):
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "whether the node's file remains after the delete",
                  os . path . exists ( skg_path (context, "doomed-id") ), False )
  require_equal ( "the private record that names it",
                  "- doomed-id" in private_lines (context, "secret-id"), True )

def check_hiding_the_foreign_skgrepos (context):
  """'restrict --foreign hidden' hides the foreign skgrepos: the held
  views are rerendered under it, one showing overPrivateText text only
  with '--show-overPrivateText', and a held view of a foreign node's
  subscriber no longer shows it. The switch is refused while a held
  view has unsaved edits. 'restrict private --foreign shown' lifts the
  restriction."""
  folder = context [ "daemon_folder" ]
  require_tool_run ([ "view", "child-b-id", "--out-dir", folder ])
  require_in_file ( folder, "view_child-b-id", "*** Fred's note ⟨" )
  edit_compact ( folder, [ ( "* Second child, a subscriber ⟨",
                             "* Second child, unsaved ⟨" ) ], "view_child-b-id" )
  completed = run_tool ([ "restrict", "--foreign", "hidden" ])
  require_output ( completed, "", expected_returncode = 1 )
  if "view_child-b-id.compact.org has unsaved edits" not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  edit_compact ( folder, [ ( "* Second child, unsaved ⟨",
                             "* Second child, a subscriber ⟨" ) ], "view_child-b-id" )
  require_failure (
    run_tool ([ "restrict", "--foreign", "hidden" ]), [],
    "skg_for_ai: The skgrepo restriction is as it was: run again with "
    "'--show-overPrivateText' to release that text.\n" )
  require_output (
    run_tool ([ "restrict", "--foreign", "hidden", "--show-overPrivateText" ]),
    "Server: overPrivateText confirmation: the skgrepo restriction withholds "
    "text from overPrivateText telescopes (tele-id) until approved. repo set "
    "switch rerender would reveal title or body text selected below its node's "
    "home repo for PID tele-id. Include that text?\n"
    "Approved: the request is sent again, releasing that text.\n"
    "Skgrepo restriction: main; foreign repos hidden\n" )
  require_log_line ( context, "re-rendered view_child-b-id because of 'restrict "
                              "--foreign hidden --show-overPrivateText" )
  if "Fred's note" in read_file ( os . path . join ( folder, "view_child-b-id.compact.org" )):
    raise CaseFailure ( "the held view still shows the foreign node" )
  require_output ( run_tool ([ "restrict", "private", "--foreign", "shown" ]),
                   "Skgrepo restriction: private (no restriction)\n" )
  require_in_file ( folder, "view_child-b-id", "*** Fred's note ⟨" )

def check_hoisting (context):
  """A save of a telescope whose title sits below its home asks Hoist
  or Abort: '--yes' alone aborts, writing nothing; '--hoist' moves the
  title home and saves."""
  folder = os . path . join ( context [ "scratch" ], "hoist" )
  require_tool_run ([ "view", "tele-id", "--out-dir", folder, "--no-summary" ])
  edit_compact ( folder, [ ( "* A title kept private ⟨0a C1 ⌂:main⟩\n",
                             "* A title kept private ⟨0a C1 ⌂:main⟩\nA body.\n" ) ],
                 "view_tele-id" )
  question = [
    "Server: Hoist confirmation: Saving would publish title or body text "
    "selected below home for 1 telescope. Hoist writes the selected text at "
    "home and removes lower title/body copies while preserving lower "
    "relationships and aliases. Abort writes nothing; manual repair requires "
    "editing the .skg files. Hoist?",
    "  tele-id, whose home is 'main'." ]
  require_failure (
    run_tool ([ "save", folder, "--yes" ]), question,
    "skg_for_ai: Nothing was saved (Abort). Save again with '--hoist' to "
    "Hoist, or repair the '.skg' files by hand.\n" )
  require_equal ( "the home section's title after Abort",
                  skg_title (context, "tele-id"), None )
  completed = run_tool ([ "save", folder, "--yes", "--hoist" ])
  if ( "Hoist: approved; the save is sent again.\nServer: saved, 0 errors, "
       not in completed . stdout ):
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "the home section's title after Hoist",
                  skg_title (context, "tele-id"), "A title kept private" )
  require_equal ( "the home section's body after Hoist",
                  skg_body (context, "tele-id"), [ "A body." ] )

def check_setting_relRepos (context):
  """'relRepo:private' records a membership in 'private'; 'relRepo:NAME
  subtree' on a node does so for every content relationship of its
  subtree, its own too."""
  folder = fresh_root_view (context, "relRepo")
  contained = compact_line_starting (folder, "*** Contained twice, retitled ⟨")
  edit_compact ( folder, [ ( contained, contained [ : -1 ] + " relRepo:private⟩" ) ])
  completed = require_save_succeeds (folder)
  require_equal ( "the report of 'relRepo:private'",
                  [ line for line in completed . stdout . split ("\n")
                    if line . startswith ("relRepo:") ],
                  [ f"relRepo: ⟨{HANDLE_PATTERN . search (contained) . group (1)}⟩ "
                    "'Contained twice, retitled', as '(relRepo private)': its "
                    "relationship to ⟨0d⟩ 'First child' is recorded in "
                    "'private'." ] )
  require_equal ( "the public section's content", skg_list (context, "child-a-id",
                                                            "contains"), [] )
  require_equal ( "whether the private section of the first child records it",
                  "- shared-id" in private_lines (context, "child-a-id"), True )
  second = compact_line_starting (folder, "** Second child, a subscriber ⟨")
  edit_compact ( folder, [ ( second + "\n",
                             second + "\n*** A child kept private\n" ) ])
  require_save_succeeds (folder)
  second = compact_line_starting (folder, "** Second child, a subscriber ⟨")
  child = compact_line_starting (folder, "*** A child kept private ⟨")
  edit_compact ( folder, [ ( second, second [ : -1 ] + " relRepo:private subtree⟩" ) ])
  completed = require_save_succeeds (folder)
  require_equal ( "the report of 'relRepo:private subtree'",
                  [ line [ : line . index (",") ]
                    for line in completed . stdout . split ("\n")
                    if line . startswith ("relRepo:") ],
                  [ f"relRepo: ⟨{HANDLE_PATTERN . search (second) . group (1)}⟩ "
                    "'Second child",
                    f"relRepo: ⟨{HANDLE_PATTERN . search (child) . group (1)}⟩ "
                    "'A child kept private'" ] )
  require_equal ( "whether the private section of the second child records its child",
                  "- " + id_of_compact_headline ( folder, "*** A child kept private" )
                  in private_lines (context, "child-b-id"), True )
  require_equal ( "whether the private section of the root records the second child",
                  "- child-b-id" in private_lines (context, "root-id"), True )

def check_moving_to_another_skgrepo (context):
  """'repo:main' moves a private node home to 'main'; the relationships
  left in 'private' are the strandeds question, which '--yes' alone
  does not answer and '--strandeds keep' answers by leaving them. A
  new headline written '⟨new repo:private⟩' is made in 'private', and
  'repo:main subtree' moves it and its child."""
  folder = os . path . join ( context [ "scratch" ], "move" )
  require_tool_run ([ "view", "secret-id", "--out-dir", folder, "--no-summary" ])
  root = compact_line_starting (folder, "* A private note ⟨", "view_secret-id")
  edit_compact ( folder, [ ( root, root [ : -1 ] + " repo:main⟩" ) ], "view_secret-id" )
  question = [
    "Server: strandeds confirmation: the save's home skgrepo moves would "
    "leave 3 relationships in their old skgrepos; nothing was written.",
    "  child-a-id contains secret-id: stays at private; its default is now main.",
    "  secret-id contains doomed-id: stays at private; its default is now main.",
    "  secret-id contains shared-id: stays at private; its default is now main." ]
  require_failure (
    run_tool ([ "save", folder, "--yes" ]), question,
    "skg_for_ai: Nothing was saved: save again with '--strandeds reset' to move "
    "every listed relationship to its new default, or '--strandeds keep' to "
    "leave them where they are.\n" )
  require_equal ( "the home of the node after the unanswered question",
                  os . path . exists ( skg_path (context, "secret-id") ), False )
  completed = run_tool ([ "save", folder, "--yes", "--strandeds", "keep" ])
  if ( "Strandeds: each stays where it is; the save is sent again.\n"
       "Server: saved, 0 errors, 0 warnings.\n" not in completed . stdout ):
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "the moved node's title at home", skg_title (context, "secret-id"),
                  "A private note" )
  require_equal ( "the private section it left",
                  private_lines (context, "secret-id") [ 1 : ],
                  [ "contains:", "- shared-id", "- doomed-id", "" ] )
  folder = fresh_root_view (context, "move-new")
  second = compact_line_starting (folder, "** Second child, a subscriber ⟨")
  edit_compact ( folder, [ ( second + "\n", second +
                             "\n*** Made private ⟨new repo:private⟩\n" ) ])
  completed = require_save_succeeds (folder)
  require_equal ( "the title of the new headline's private home",
                  private_lines (context, id_of_compact_headline (
                    folder, "*** Made private" )) [ 0 ], "title: Made private" )
  folder = os . path . join ( context [ "scratch" ], "move-subtree" )
  require_tool_run ([ "view", "branch-id", "--out-dir", folder, "--no-summary" ])
  root = compact_line_starting (folder, "* A private branch ⟨", "view_branch-id")
  edit_compact ( folder, [ ( root, root [ : -1 ] + " repo:main subtree⟩" ) ],
                 "view_branch-id" )
  completed = run_tool ([ "save", folder, "--yes", "--strandeds", "reset" ])
  require_equal ( "the report of 'repo:main subtree'",
                  [ line for line in completed . stdout . split ("\n")
                    if line . startswith ("Repo:") ],
                  [ "Repo: ⟨0a⟩ 'A private branch', from 'private' to 'main': "
                    "the node's home skgrepo moves.",
                    "Repo: ⟨0b⟩ 'Its private leaf', from 'private' to 'main': "
                    "the node's home skgrepo moves." ] )
  if "Server: saved, 0 errors" not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  for node_id, title in [ ( "branch-id", "A private branch" ),
                          ( "leaf-id", "Its private leaf" ) ]:
    require_equal ( "a moved node's title at home", skg_title (context, node_id), title )

def check_marking_like_moves_ready (context):
  """In the diff view, 'ready subtree' on a home skgrepo move marks
  every move between the same two skgrepos in the moved node's
  subtree, as 'C-u C-c r h' does: the branch's and its leaf's, and
  not the move of the node above them."""
  folder = os . path . join ( context [ "scratch" ], "review", "like-moves" )
  require_tool_run ([ "diff", "--out-dir", folder ])
  require_in_diff_compact ( folder, "* A private branch ⟨0a C1 ⌂:main M⟩\n"
                                    "** ⟨0b ⌂ -private +main⟩\n" )
  edit_compact ( folder, [ ( "** ⟨0b ⌂ -private +main⟩",
                             "** ⟨0b ⌂ -private +main ready subtree⟩" ) ],
                 DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Ready: ⟨0b⟩ (homeSkgrepoMove (old private) (new main)), as '(ready)'.\n"
    "Ready: ⟨0e⟩ (homeSkgrepoMove (old private) (new main)), as '(ready)'.\n"
    + saved_line (0) + "Readiness: 2 of 17 changed files are ready.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  require_in_diff_compact ( folder, "** ⟨0b ⌂ -private +main✓⟩\n" )
  require_in_diff_compact ( folder, "** Its private leaf ⟨0d M✓⟩\n"
                                    "*** ⟨0e ⌂ -private +main✓⟩\n" )
  require_in_diff_compact ( folder, "***** ⟨1c ⌂ -private +main⟩\n" )

def check_deleting_references (context):
  """'delete-references ID' sends nothing without '--yes'; with it the
  server warns that a body links to the ID, and '--leave-links'
  approves: the membership goes, the link stays, and the daemon
  rewrites the view of the node that named it."""
  folder = context [ "daemon_folder" ]
  require_tool_run ([ "view", "refs-id", "--out-dir", folder ])
  require_output (
    run_tool ([ "delete-references", "absent-id" ]),
    "Not sent. This removes every owned structured reference to the absent "
    "node absent-id (in 'contains', 'subscribesTo', 'hidesFromSubs' and "
    "'overrides' lists); run again with '--yes'.\n" )
  warning = [ "Server: delete-references confirmation: titles or bodies link to "
              "absent-id; the cleanup leaves those links, dangling. Remove the "
              "structured references?",
              "- refs-id / contains / absent-id (repo main)" ]
  require_failure (
    run_tool ([ "delete-references", "absent-id", "--yes" ]), warning,
    "skg_for_ai: Nothing was removed: run again with '--yes --leave-links' to "
    "remove the structured references and leave the links.\n" )
  require_equal ( "the membership before the approval",
                  skg_list (context, "refs-id", "contains"), [ "absent-id" ] )
  completed = require_tool_run ([ "delete-references", "absent-id", "--yes",
                                  "--leave-links" ])
  if ( "Server: removed the references to absent-id: 1 membership(s) in 1 "
       "node(s).\n" not in completed . stdout ):
    raise CaseFailure ( f"it printed {completed . stdout!r}" )
  require_equal ( "the membership after the approval",
                  skg_list (context, "refs-id", "contains"), [] )
  require_equal ( "the body after the approval", skg_body (context, "refs-id"),
                  [ "See [[id:absent-id][the absent one]]." ] )
  require_log_line ( context, "re-rendered view_refs-id because of "
                              "'delete-references absent-id --yes --leave-links" )

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

def read_compact (folder, base_name = ROOT_BASE_NAME):
  return read_file ( os . path . join (folder, base_name + ".compact.org") )

def require_in_compact (folder, text):
  if text not in read_compact (folder):
    raise CaseFailure (
      f"the re-rendered compact view lacks {text!r}:\n{read_compact (folder)}" )

def id_of_compact_headline (folder, headline_start, base_name = ROOT_BASE_NAME):
  """The ID in the metadata of the full-view headline that the handle
  of the compact headline starting HEADLINE_START names, in the view
  BASE_NAME."""
  for line in read_compact (folder, base_name) . split ("\n"):
    if line . startswith (headline_start + " ⟨"):
      handle = HANDLE_PATTERN . search (line) . group (1)
      for full_line in read_file (
          os . path . join (folder, base_name + ".full.org") ) . split ("\n"):
        viewnode_id = VIEWNODE_ID_PATTERN . match (full_line)
        if viewnode_id and viewnode_id . group (1) == handle:
          match = UUID_PATTERN . search (full_line)
          if match is None:
            raise CaseFailure (
              f"the full view's headline ⟨{handle}⟩ has no UUID: {full_line!r}" )
          return match . group (0)
      raise CaseFailure ( f"the full view has no headline ⟨{handle}⟩" )
  raise CaseFailure (
    f"no headline {headline_start!r} in:\n{read_compact (folder, base_name)}" )

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

# The files a second owned skgrepo, 'private', adds to the fixture,
# and those it adds to 'main' beside it ('add_private_skgrepo').
PRIVATE_SKGREPO_FILES = {
  # A telescope whose title sits below its home, 'main': overPrivateText.
  "me/main/tele-id.skg"      : "pid: tele-id\ncontains:\n- shared-id\n",
  "me/private/tele-id.skg"   : "pid: tele-id\ntitle: A title kept private\n",
  # A private node, contained by a private section of a public one, and
  # naming a public node that a save deletes.
  "me/private/secret-id.skg" : ( "pid: secret-id\ntitle: A private note\n"
                                 "contains:\n- shared-id\n- doomed-id\n" ),
  "me/private/child-a-id.skg" : "pid: child-a-id\ncontains:\n- secret-id\n",
  "me/main/doomed-id.skg"    : "pid: doomed-id\ntitle: A node to delete\n",
  # A private branch and its leaf, which move to 'main' together.
  "me/private/branch-id.skg" : ( "pid: branch-id\ntitle: A private branch\n"
                                 "contains:\n- leaf-id\n" ),
  "me/private/leaf-id.skg"   : "pid: leaf-id\ntitle: Its private leaf\n",
  # A node naming an ID nobody wrote, by membership and by a link.
  "me/main/refs-id.skg"      : ( "pid: refs-id\ntitle: Names an absent node\n"
                                 "body: |-\n  See [[id:absent-id][the absent one]].\n"
                                 "contains:\n- absent-id\n" ) }

def add_private_skgrepo (data_root):
  """Give the fixture at DATA_ROOT a second owned skgrepo, 'private',
  more private than 'main', and the files PRIVATE_SKGREPO_FILES
  lists."""
  with open ( os . path . join (data_root, "skgconfig.toml"), "a",
              encoding = "utf-8" ) as config_file:
    config_file . write ( '\n[[repos]]\nname = "private"\npath = "me/private"\n' )
  for relative_path, text in PRIVATE_SKGREPO_FILES . items ():
    path = os . path . join (data_root, relative_path)
    os . makedirs ( os . path . dirname (path), exist_ok = True )
    with open (path, "w", encoding = "utf-8") as skg_file:
      skg_file . write (text)

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
