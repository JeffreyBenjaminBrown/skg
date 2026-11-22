#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

In order, it:
- checks that a save's envelope carries the saved buffer's baseline;
- checks that a command needing a server, given no port, no sidecar
  and no daemon, is refused before it connects, there being no
  default port, while 'linkstack show' runs without one;
- checks the title normalizer and the fuzzy match of a new headline's
  title with a removed one's on pairs that must match, pairs that must
  not, and pairs at the thresholds;
- renders every rendering case both clients' tests share
  ('tests/shared/herald-rendering-cases.txt') as they render it, with
  the rule table and label table pinned beside it and each case's
  context, and compares the text;
- copies 'test-fixture/' to a scratch data root, starts a server
  there on port SKG_FOR_AI_TEST_PORT (default 1793), and runs each
  view command of the tool against it, 'forest' with every kind
  ('forest' of an absent ID refused as 'view' of it is);
- compares each compact view with its file in 'test-expected/', and
  checks that every handle is the viewnode ID of its headline in the
  full view;
- sends a 'repos' and a 'herald rules' request through 'raw' (the
  served rule table is the pinned one), and imports a Markdown file
  through 'import'; searches with each search option, and for a word
  with an apostrophe; pushes, pastes
  and pops the linkstack; prints the restriction, and refuses to set
  it without a daemon;
- on a second scratch server, edits compact views and saves them:
  a title, a body, a new child, a move, a removal, a deletion, bad
  handles, titles given a handle's bracket, a view saved by naming its
  compact or full view, an edit the server refuses, a stale view the server
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
  without '--yes', approved with it: the clone forks it, and records
  its attribution, which 'stripAttribution' removes), forks an owned node with 'fork' (refused beside
  another edit, then approved), and forks and hides by editing a
  forks folder;
- on a fourth scratch server, writes the words of a handle:
  'detach' and 'attach', 'write-protect' (and 'subtree'),
  'noSearchMatching' and 'searchMatching' (and 'subtree', skipping
  foreign nodes), 'merge:ID', 'delete subtree', and words refused where
  they stand;
- on a fifth scratch server, whose owned skgrepo is a gitrepo with the
  fixture committed: saves a retitle, a new child and a deletion,
  checks the diff view's compact buffer exactly, marks the new child
  processed and commits it alone ('commit'), then finds nothing
  processed to commit, restores the deleted node, and commits the rest
  ('commit --all' refused unconfirmed, then confirmed with '--yes'),
  checking 'git log' and that the diff
  view is then empty; then extends a diff buffer to a change it does not
  make editable, and saves an edit of it; marks and unmarks a subtree's
  changes with 'processed both subtree' and 'unprocessed both subtree'; a blank commit
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
  an interactive 'commit --all' beside it, each rewrite its held buffer of
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
  processed, and removes the references to an absent node;
- on an eighth scratch server, over the data of the perspective
  integration test: switches a view of a foreign root to the user's
  perspective and back, on a command's own connection and through
  a daemon, refuses the switch for an edited view and for a view whose
  roots are all owned, and checks that a save, a daemon's rewrite and
  the daemon's stop keep the choice.
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
                         ( "forks",         "fred-note-id" ),
                         ( "hides",         "fred-aside-id" ),
                         ( "aliases",       "forkee-id" ),
                         ( "flags",         "forker-id" ) ] ]

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
  failures += failures_of_commit_result_text ()
  failures += failures_of_preview_support ()
  failures += failures_of_no_default_port ()
  failures += failures_of_link_annotation_round_trip ()
  failures += failures_of_link_annotations ()
  failures += failures_of_shared_herald_cases ()
  failures += failures_of_forks_folder_claims ()
  failures += failures_of_chains ()
  failures += failures_of_describe_requests ()
  failures += failures_of_perspective_names ()
  failures += failures_against_scratch_servers (update_expected)
  shutil . rmtree ( os . environ [ "XDG_RUNTIME_DIR" ], ignore_errors = True )
  for failure in failures:
    print ( "FAIL: " + failure )
  if failures:
    sys . exit (1)
  print ( "PASS" + ( " (expected files rewritten)" if update_expected else "" ))

# ============================================================
# The perspective's names
# ============================================================

def failures_of_perspective_names ():
  """A sidecar's perspective is read by the server's names: 'skgrepo
  NAME' as it is; none, 'root homes', and the older forms ('user',
  'owner of NAME', 'root authors'), as the root homes. The command
  line's words are joined, and none means 'next'."""
  failures = []
  for recorded, read in [ ( "skgrepo main", "skgrepo main" ),
                          ( "root homes", "root homes" ),
                          ( "user", "root homes" ),
                          ( "owner of tracy", "root homes" ),
                          ( "root authors", "root homes" ),
                          ( None, "root homes" ) ]:
    sidecar = {} if recorded is None else { "perspective" : recorded }
    if skg_for_ai . perspective_of_sidecar (sidecar) != read:
      failures . append ( f"a sidecar recording {recorded!r} is read as "
                          f"{skg_for_ai . perspective_of_sidecar (sidecar)!r}, not {read!r}" )
  for words, name in [ ( [], "next" ), ( [ "next" ], "next" ),
                       ( [ "root", "homes" ], "root homes" ),
                       ( [ "skgrepo main" ], "skgrepo main" ),
                       ( [ "skgrepo", "main" ], "skgrepo main" ) ]:
    if skg_for_ai . perspective_of_argument (words) != name:
      failures . append ( f"the words {words!r} name "
                          f"{skg_for_ai . perspective_of_argument (words)!r}, not {name!r}" )
  return failures

# ============================================================
# The forks folder's claims
# ============================================================

def failures_of_forks_folder_claims ():
  """In a forks folder a member claims exactly when the folder's
  viewparent forks it. One claiming nothing is a pending detachment
  only when the viewparent still forks it (a detached forkee or
  mutual, whose '(rels (forks (in ...)))' names ancestor generation
  2), not a forker the viewparent does not fork; and 'relRepo:NAME' is
  refused on it, since such a forker records its fork in its own
  file, while a claiming member takes it."""
  full = ( "* (skg (vnid 0a) node (gnid n) (skgRepos (nodeRepo main))) n\n"
           "** (skg (vnid 0b) forksFolder)\n"
           "*** (skg (vnid 0c) node (gnid f) (skgRepos (nodeRepo main)) noClaim writeProtected "
           "(view (rels (forks (out 1 (ancestors 2)))) (birth (forks out 2)))) f\n"
           "*** (skg (vnid 0d) node (gnid e) (skgRepos (nodeRepo main)) noClaim writeProtected "
           "(view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) e\n"
           "*** (skg (vnid 0e) node (gnid m) (skgRepos (nodeRepo main)) noClaim writeProtected "
           "(view (rels (forks (in 1 (ancestors 2)) (out 1 (ancestors 2)))) "
           "(birth (forks in 2) (forks out 2)))) m\n"
           "*** (skg (vnid 0g) node (gnid k) (skgRepos (nodeRepo main)) writeProtected "
           "(view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) k\n" )
  compact = ( "* n ⟨0a⟩\n** ⟨0b⟩\n*** f ⟨0c relRepo:main⟩\n"
              "*** e ⟨0d⟩\n*** m ⟨0e⟩\n*** k ⟨0g relRepo:main⟩\n" )
  headline_of_handle = skg_for_ai . headlines_by_viewnode_id (
    skg_for_ai . parse_full_view (full) )
  edited = skg_for_ai . parse_edited_compact_view (compact)
  def metadata (handle):
    return headline_of_handle [handle] . metadata
  failures = []
  for handle, pending in [ ( "0c", False ), ( "0d", True ), ( "0e", True ) ]:
    if skg_for_ai . is_pending_detachment (
        metadata (handle), metadata ("0b"), metadata ("0a") ) != pending:
      failures . append ( f"the forks folder's member {handle} should "
                          f"{'' if pending else 'not '}be a pending detachment" )
  refusal = skg_for_ai . relRepo_problems (
    edited, 2, headline_of_handle, { "main" : True } )
  if not ( len (refusal) == 1 and "claims nothing" in refusal [0] ):
    failures . append ( f"'relRepo:main' on a forker claiming nothing gave {refusal!r}" )
  allowed = skg_for_ai . relRepo_problems (
    edited, 5, headline_of_handle, { "main" : True } )
  if allowed:
    failures . append ( f"'relRepo:main' on a claiming forkee gave {allowed!r}" )
  # The folder's label asks for nothing: of its handle's words, only
  # those typed after it do.
  for text, asked in [
      ( "** ⟨0b Its mirrors are these.⟩", ( [], [], [], False ) ),
      ( "** ⟨0b Its mirrors are these. relRepo:main subtree⟩",
        ( [], [], [ "relRepo:main" ], True ) ) ]:
    folder = skg_for_ai . parse_edited_compact_view (text) [0]
    found = ( folder . repo_words (), folder . view_request_words (),
              folder . edit_request_words (), folder . asks_subtree () )
    if found != asked:
      failures . append ( f"the handle of {text!r} asks {found!r}, not {asked!r}" )
  return failures

# ============================================================
# Chains ('chain VIEW HANDLE')
# ============================================================

# Jeff's example: a contains b, whose forks folder holds c, which b
# forks; a mentioner folder's member, born of its link though it also
# contains the note; and a mirror joined to its folder's note by no
# edge.
CHAIN_VIEW = """* (skg (vnid 0a) node (gnid a)) a
** (skg (vnid 0b) node (gnid b) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
*** (skg (vnid 0c) forksFolder)
**** (skg (vnid 0d) node (gnid c) (view (rels (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) c
**** (skg (vnid 0e) node (gnid m) (view (rels (forks (in 1))))) m
*** (skg (vnid 0f) mentionerFolder)
**** (skg (vnid 0g) node (gnid r) (view (rels (contains (out 1 (ancestors 2))) (linksTo (out 1 (ancestors 2)))) (birth (linksTo out 2)))) r
***** (skg (vnid 0h) node (gnid h) (view (rels (hides (out 1 (ancestors 1)))))) h
"""

def failures_of_chains ():
  """A chain joins each note to the one above it by the edge its
  metadata records, folders skipped, and refuses a step no edge
  joins; 'chain' prints it."""
  headlines = [ line for line in skg_for_ai . parse_full_view (CHAIN_VIEW)
                if line . is_headline ]
  def chain_of (handle):
    index = next ( i for i, line in enumerate (headlines)
                   if skg_for_ai . headline_viewnode_id (line . metadata) == handle )
    try:
      return skg_for_ai . chain_text (headlines, index)
    except skg_for_ai . SkgToolError as error:
      return "refused: " + str (error)
  failures = []
  for handle, expected in [
      ( "0a", "[[id:a][a]]" ),
      ( "0d", "[[id:a][a]] / [[id:b][b]] ~ [[id:c][c]]" ),
      ( "0g", "[[id:a][a]] / [[id:b][b]] <- [[id:r][r]]" ),
      ( "0h", "[[id:a][a]] / [[id:b][b]] <- [[id:r][r]] is hidden by [[id:h][h]]" ),
      ( "0e", "refused: No edge of the graph joins 'm' to 'b', the note above "
              "it in this view, so no chain runs through them." ) ]:
    if chain_of (handle) != expected:
      failures . append ( f"the chain of {handle} is {chain_of (handle)!r}, not {expected!r}" )
  folder = tempfile . mkdtemp ( prefix = "skg-for-ai-chain-" )
  for name, text in [ ( "v.full.org", CHAIN_VIEW ), ( "v.compact.org", "" ),
                      ( "v.view.json", json . dumps ({ "full_view" : "v.full.org",
                                                       "compact_view" : "v.compact.org" }) ) ]:
    with open ( os . path . join (folder, name), "w", encoding = "utf-8" ) as file:
      file . write (text)
  printed = run_tool ([ "chain", os . path . join (folder, "v.full.org"), "0d" ])
  if printed . stdout . strip () != "[[id:a][a]] / [[id:b][b]] ~ [[id:c][c]]":
    failures . append ( f"'chain' printed {printed . stdout!r} {printed . stderr!r}" )
  shutil . rmtree (folder, ignore_errors = True)
  return failures

# ============================================================
# What heralds mean ('describe VIEW HANDLE')
# ============================================================

def failures_of_describe_requests ():
  """'describe' sends the headline's metadata as the full view holds
  it, with 'pendingDetachment' on a pending detachment."""
  view = skg_for_ai . parse_full_view (
    CHAIN_VIEW + "** (skg (vnid 0i) node (gnid d) noClaim) d\n" )
  failures = []
  for handle, expected in [
      ( "0d", '((request . "describe heralds") (metadata . "(skg (vnid 0d) '
              'node (gnid c) (view (rels (forks (in 1 (ancestors 2)))) '
              '(birth (forks in 2))))"))' ),
      ( "0i", '((request . "describe heralds") (metadata . "(skg (vnid 0i) '
              'node (gnid d) noClaim pendingDetachment)"))' ) ]:
    actual = skg_for_ai . describe_request_line (view, handle)
    if actual != expected:
      failures . append ( f"the describe request of {handle} is {actual!r}, not {expected!r}" )
  return failures

def check_describe (scratch):
  """'describe' prints the server's sentences for a handle of a view:
  a note's heralds, most informative first, and what a folder is."""
  folder = os . path . join (scratch, "describe")
  require_tool_run ([ "view", "root-id", "--out-dir", folder, "--no-summary" ])
  view_files = os . path . join (folder, "view_root-id.view.json")
  require_output (
    run_tool ([ "describe", view_files, "0a" ]),
    "1C2: 1 note contains this one; this note contains 2 notes.\n"
    "L2: this note links to 2 notes.\n"
    "⌂:main: it lives in your skgrepo 'main'.\n" )
  folder_lines = run_tool ([ "describe", view_files, "0b" ]) . stdout
  if not folder_lines . startswith ("☮ These also contain it.: the other notes"):
    raise CaseFailure ( f"'describe' of a folder printed {folder_lines!r}" )

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

def failures_of_commit_result_text ():
  """A commit's line names each node left because it changed in a way
  no mark covers, with how, and says what can commit it."""
  response = skg_for_ai . parse_sexp (
    '((commits (("main" "9c1e40d2"))) (committed-nodes 1) (left-nodes 2)'
    ' (waiting ()) (unmarkable (("h" "H" ("its file changed in a way the diff view does not show (formatting, say)"))'
    ' ("t" "T" ("its file changed in a way the diff view does not show (formatting, say)"))))'
    ' (errors ()) (warnings ()))' )
  text = skg_for_ai . commit_result_text (response)
  expected = ( "Committed 1 node, in main (9c1e40d2). 2 changed nodes are left;"
               " 2 of them changed in ways no mark covers, which 'commit --all'"
               " or Git can commit: 'H' (its file changed in a way the diff view does not show (formatting, say));"
               " 'T' (its file changed in a way the diff view does not show (formatting, say))." )
  return ( [] if text == expected
           else [ f"the commit's line is {text!r}, not {expected!r}" ] )

def failures_of_preview_support ():
  """A dry run previews a save only on a server whose answer to "verify
  connection" names "save preview": an older one ignores the preview
  field, and would save."""
  class Answering:
    def __init__ (self, answer):
      self . answer = answer
    def request (self, line):
      return skg_for_ai . parse_sexp (self . answer)
  old = Answering ( '((response-type verify-connection) (content "This is '
                    'the skg server verifying the connection."))' )
  new = Answering ( '((response-type verify-connection) (content "x") '
                    '(supports ("save preview")))' )
  failures = []
  if skg_for_ai . server_supports (old, "save preview"):
    failures . append ( "an older server was taken to support the preview" )
  if not skg_for_ai . server_supports (new, "save preview"):
    failures . append ( "a server naming the preview was taken not to support it" )
  return failures

# Commands that need a server, each run with no port, no sidecar and
# no daemon: 'commit --all --yes' wrote the user's unfinished changes
# to his own server when a default port named it.
PORTLESS_SERVER_COMMANDS = [
  [ "commit", "A message", "--all", "--yes" ],
  [ "view", "root-id" ],
  [ "raw", "(repos)" ],
  [ "daemon" ],
  [ "linkstack", "push", "root-id" ] ]

def failures_of_no_default_port ():
  """There is no default port. A command that needs a server, given no
  port by '--port' or SKG_PORT, no sidecar and no daemon in its
  folder, is refused, exit 1, with one line on stderr, before it
  connects to anything or writes anything; 'linkstack show', which
  needs no server, still runs."""
  failures = []
  expected_stderr = "skg_for_ai: " + skg_for_ai . NO_PORT_MESSAGE + "\n"
  connections = []
  def refuse_to_connect (*arguments, **keywords):
    connections . append (arguments)
    raise ConnectionRefusedError ( "the test forbids connecting" )
  original_create_connection = socket . create_connection
  original_connect = socket . socket . connect
  socket . create_connection = refuse_to_connect
  socket . socket . connect = refuse_to_connect
  folder = tempfile . mkdtemp ( prefix = "skg-for-ai-test-no-port-" )
  try:
    for command in PORTLESS_SERVER_COMMANDS:
      argv = command + ( [] if command [0] in ( "commit", "raw" )
                         else [ "--out-dir", folder ] )
      described = "'" + " " . join (argv) + "'"
      del connections [ : ]
      try:
        skg_for_ai . main (argv)
        failures . append ( f"{described} without a port was not refused" )
      except skg_for_ai . SkgToolError as error:
        if str (error) != skg_for_ai . NO_PORT_MESSAGE:
          failures . append ( f"{described} without a port said {error}" )
      if connections:
        failures . append ( f"{described} without a port tried to connect" )
      completed = run_tool (argv, port = None, bypassing_daemons = True)
      if ( completed . returncode != 1 or completed . stdout != ""
           or completed . stderr != expected_stderr ):
        failures . append (
          f"{described} without a port: exit {completed . returncode}, "
          f"stdout {completed . stdout!r}, stderr {completed . stderr!r}" )
    if os . listdir (folder):
      failures . append ( f"commands without a port wrote {os . listdir (folder)}" )
    completed = run_tool ( [ "linkstack", "show", "--out-dir", folder ],
                           port = None, bypassing_daemons = True )
    if ( completed . returncode != 0
         or completed . stdout != "The linkstack is empty.\n" ):
      failures . append (
        f"'linkstack show' without a port: exit {completed . returncode}, "
        f"stdout {completed . stdout!r}, stderr {completed . stderr!r}" )
  finally:
    socket . create_connection = original_create_connection
    socket . socket . connect = original_connect
    shutil . rmtree (folder, ignore_errors = True)
  return failures

def failures_of_link_annotation_round_trip ():
  """A save strips exactly the link annotations a view wrote, even one
  quoting a target title that holds '⟦', '⟧', '→', '✗' and '⌂', so an
  unedited body or title saves unchanged."""
  title = ( 'The AI reader writes "⟦not held here⟧" where it now writes '
            '"⟦✗ missing⟧", for a link whose target this server holds no '
            'node of. ⟦→ x ⌂:y⟧' )
  statuses = {
    "odd-id"  : { "status" : "resolved", "pid" : "odd-id",
                  "label" : "cl⟧eo", "title" : title },
    "gone-id" : { "status" : "missing" } }
  failures = []
  for text in [ "See [[id:odd-id][the note]] and [[id:gone-id][gone]].",
                "[[id:odd-id]] then [[id:odd-id][again ⟦→ typed⟧]]⟦→ typed⟧" ]:
    annotated = skg_for_ai . annotate_links (text, statuses)
    stripped = skg_for_ai . strip_link_annotations (annotated)
    if stripped != text:
      failures . append (
        f"annotating then stripping {text!r} gives {stripped!r}" )
  return failures

# (the server's entry for a link's target, the annotation it gets)
LINK_ANNOTATION_CASES = [
  ( { "status" : "resolved", "pid" : "t", "label" : "main",
      "title" : "The target" }, "⟦→ The target ⌂:main⟧" ),
  ( { "status" : "missing" }, "⟦not held here⟧" ),
  ( { "status" : "restricted" }, "⟦✗ restricted⟧" ),
  ( None, "⟦✗ lookup failed⟧" ) ]

def failures_of_link_annotations ():
  """Each status gets its annotation, and a save strips each of them,
  and the '⟦✗ missing⟧' of views written before '⟦not held here⟧'."""
  failures = []
  link = "[[id:t][a label]]"
  for entry, annotation in LINK_ANNOTATION_CASES:
    if skg_for_ai . link_annotation (entry) != annotation:
      failures . append (
        f"the link annotation of {entry} is "
        f"{skg_for_ai . link_annotation (entry)!r}, not {annotation!r}" )
  for annotation in ( [ annotation for _, annotation in LINK_ANNOTATION_CASES ]
                      + [ "⟦✗ missing⟧" ] ):
    text = "see " + link + annotation + ", then more"
    stripped = skg_for_ai . strip_link_annotations (text)
    if stripped != "see " + link + ", then more":
      failures . append ( f"{annotation!r} after a link strips to "
                          f"{stripped!r}" )
  if skg_for_ai . strip_link_annotations ("⟦not held here⟧ alone") \
     != "⟦not held here⟧ alone":
    failures . append ( "'⟦not held here⟧' after no link is stripped" )
  return failures

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
        { "folderChange processed" : True,
          "folderChange unprocessed" : False } . get (context),
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
  skgrepos on a seventh, those three with their owned skgrepos
  gitrepos; each on its own copy of the fixture. Then the checks of
  a view's perspective, on an eighth, over a copy of the
  perspective integration test's data. Last, the receipts', on a
  ninth, over mail ('RECEIPT_FIXTURE_FILES')."""
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
               with_private_skgrepo = True )
           + failures_on_a_scratch_server (
               failures_of_perspective,
               make_data_root = copy_perspective_fixture )
           + failures_on_a_scratch_server (
               failures_of_receipts,
               make_data_root = write_receipt_fixture ) )

def failures_on_a_scratch_server (check, owned_skgrepo_in_git = False,
                                  with_private_skgrepo = False,
                                  make_data_root = None):
  """CHECK (SCRATCH, DATA_ROOT)'s failures, run against a server on a
  fresh copy of the fixture in the scratch folder SCRATCH, or on the
  data root MAKE_DATA_ROOT (DATA_ROOT) writes. With
  WITH_PRIVATE_SKGREPO, the copy gains a second owned skgrepo
  ('add_private_skgrepo'). With OWNED_SKGREPO_IN_GIT, each owned
  skgrepo is first made a gitrepo whose HEAD commit holds its files,
  as the diff view needs."""
  if port_is_listening (PORT):
    return [ f"port {PORT} is taken; set SKG_FOR_AI_TEST_PORT" ]
  scratch = tempfile . mkdtemp ( prefix = "skg-for-ai-test-" )
  data_root = os . path . join (scratch, "data")
  ( make_data_root or ( lambda root : shutil . copytree (FIXTURE_FOLDER, root) )
  ) (data_root)
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
  for check in [ check_search_options, check_missing_id, check_linkstack,
                 check_restriction_on_a_command_s_own_connection,
                 check_describe ]:
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
                check_bad_handles, check_handle_brackets_in_titles,
                check_save_given_an_org_file,
                check_server_refusal, check_stale_view,
                check_new_root, check_handles_across_saves,
                check_exact_retype, check_reworded_retype_asked,
                check_reworded_retype_kept, check_reworded_retype_all_new,
                check_restated_member ]:
    try:
      case (context)
    except CaseFailure as failure:
      # Later cases build on this one's graph.
      return [ case . __name__ + ": " + str (failure) ]
  return []

def check_restated_member (context):
  """A member of a forks folder whose body equals its note's carries
  'body=b' in its handle, and the compact view leaves its body out
  (README, "Restated titles and bodies"). Saved so, nothing changes;
  text written under it is its new body, and its note's stays."""
  folder = os . path . join ( context [ "scratch" ], "write", "restated" )
  require_tool_run ([ "view", "forker-id", "--out-dir", folder, "--no-summary" ])
  base_name = "view_forker-id"
  member = compact_line_starting (folder, "*** A forkee, outside the view ⟨", base_name)
  compact = read_file ( os . path . join (folder, base_name + ".compact.org") )
  if " body=b⟩" not in member or member + "\n" not in compact or (
      compact . endswith ( member + "\n" ) is False
      and "A body the forker and the forkee share." in compact . split ( member ) [1] ):
    raise CaseFailure ( f"the forkee's restated body is not left out:\n{compact}" )
  files_before = skg_files (context)
  completed = run_save (folder, "--yes")
  if completed . returncode != 0 or "No changes." not in completed . stdout:
    raise CaseFailure ( f"the unchanged save: {completed . stdout}{completed . stderr}" )
  require_equal ( "the files after an unchanged save", skg_files (context), files_before )
  edit_compact ( folder, [ ( member + "\n", member + "\nThe forkee's own body now.\n" ) ],
                 base_name )
  completed = run_save (folder, "--yes")
  if completed . returncode != 0:
    raise CaseFailure ( f"the save failed: {completed . stdout}{completed . stderr}" )
  require_equal ( "the forkee's body", skg_body (context, "forkee-id"),
                  [ "The forkee's own body now." ] )
  require_equal ( "the forker's body",
                  [ line for line in skg_lines (context, "forker-id") if line . startswith ("body:") ],
                  [ "body: A body the forker and the forkee share." ] )

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
  # The annotation becomes the one views written before
  # '⟦not held here⟧' hold, which the save strips too.
  edit_compact ( folder, [ ( "⟦not held here⟧.\n",
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
    ( "***** An aside of Fred's ⟨0n ☮ cH ⌂:fred folded⟩\n",
      "***** An aside of Fred's ⟨0n ☮ cH ⌂:fred folded⟩\n"
      "*** Contained twice ⟨0e 2aC⟩\n" ) ])
  report = ( REPORT_HEAD +
             "Moved: ⟨0e⟩ 'Contained twice', from under ⟨0d⟩ 'First child, "
             "renamed' to under ⟨0g⟩ 'Second child, a forker'.\n" )
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
  edit_compact ( folder, [ ( "*** Contained twice ⟨0n 2aC⟩\n", "" ) ])
  report = ( REPORT_HEAD +
             "Removed: ⟨0n⟩ 'Contained twice', from under ⟨0f⟩ 'Second "
             "child, a forker'.\n" )
  # Its container forks Fred's note, which does not contain
  # it, so the removal hides nothing, and the save says why.
  hides_nothing = ( "Hides nothing: 'Contained twice' left 'Second child, a "
                    "forker', but it is no content of 'Fred's note', nor "
                    "a mirror of any.\n" )
  require_output ( run_save (folder, "--yes"),
                   report + saved_line (0) + hides_nothing +
                   written_paths (folder) )
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
    "** ⟨0b ☮ These also contain it.⟩\n"
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
                             "⟨0a 1C2 L2 ⌂:main both delete restoreNode⟩" ),
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
    "Error: ⟨0a⟩ on line 1 of the compact view carries 'both' without "
    "'processed' or 'unprocessed'.\n"
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

def check_handle_brackets_in_titles (context):
  """A title edited to hold a handle's bracket, and a new headline whose
  title holds one, are refused before anything is sent, each named;
  nothing is saved."""
  folder = fresh_root_view (context, "handle-brackets")
  edit_compact ( folder, [ ( "** First child, renamed ⟨0d⟩",
                             "** First child ⟨renamed⟩ ⟨0d⟩" ),
                           ( "A body line that stays unnumbered.\n",
                             "A body line that stays unnumbered.\n"
                             "*** A ⟨ new child\n" ) ])
  graph_before = skg_files (context)
  completed = run_save (folder, "--yes")
  advice = ( " of a handle, and the compact view can misread a title "
             "holding one. Write the title without '⟨' and '⟩' ('〈' and "
             "'〉', or '<' and '>', look alike).\n" )
  require_output (
    completed,
    REPORT_HEAD +
    "Error: ⟨0d⟩ on line 8 of the compact view has its title edited to "
    "'First child ⟨renamed⟩': '⟨' and '⟩' are the brackets" + advice +
    "Error: the new headline on line 10 of the compact view has the title "
    "'A ⟨ new child': '⟨' is a bracket" + advice,
    expected_returncode = 1 )
  if ( "Nothing was saved: some titles hold a handle's bracket"
       not in completed . stderr ):
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  require_equal ( "the graph after a refused save",
                  skg_files (context), graph_before )

def check_save_given_an_org_file (context):
  """'save' given the compact or the full view saves the view whose
  sidecar lies beside it; given an Org file with none beside it, it
  says what it wants."""
  folder = fresh_root_view (context, "org-file")
  edit_compact ( folder, [ ( "** First child, renamed ⟨0d⟩",
                             "** First child, retitled ⟨0d⟩" ) ])
  report = ( REPORT_HEAD + "Title: ⟨0d⟩ 'First child, renamed' → "
             "'First child, retitled'.\n" )
  for suffix in ( ".compact.org", ".full.org" ):
    require_output (
      run_tool ([ "save", os . path . join (folder, ROOT_BASE_NAME + suffix) ]),
      report + NOT_SAVED )
  lonely = os . path . join (folder, "lonely.compact.org")
  shutil . copyfile (
    os . path . join (folder, ROOT_BASE_NAME + ".compact.org"), lonely )
  completed = run_tool ([ "save", lonely ])
  require_output ( completed, "", expected_returncode = 1 )
  require_equal (
    "saving an Org file with no sidecar beside it, on stderr",
    completed . stderr,
    f"skg_for_ai: {lonely} has no lonely.view.json beside it, which records "
    "the view it shows; name a view's BASE.view.json (or its "
    "BASE.compact.org or BASE.full.org), or a directory holding only one "
    "view.\n" )
  require_equal ( "the title after saves without '--yes'",
                  skg_title (context, "child-a-id"), "First child, renamed" )

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
  """Views A and E are rendered, then view B saves a retitle of a node
  both show, which makes that node stale in A and E. E edits it too;
  A edits another node only and leaves it unedited."""
  folder_a = fresh_root_view (context, "stale-a")
  folder_e = fresh_root_view (context, "stale-e")
  folder_b = fresh_root_view (context, "stale-b")
  edit_compact ( folder_b, [ ( "** Second child, a forker ⟨0e ",
                               "** Second child, renamed in B ⟨0e " ) ])
  require_save_succeeds (folder_b)
  require_edited_stale_save_refused (context, folder_e)
  require_unedited_stale_save_warned (context, folder_a)

def require_edited_stale_save_refused (context, folder):
  """Writing FOLDER's copy of the node B retitled would discard B's
  change, so the server refuses the whole save, naming the node, and
  nothing is written; the tool prints the refusal and leaves the
  view's files."""
  edit_compact ( folder, [ ( "** Second child, a forker ⟨0e ",
                             "** Second child, renamed in E ⟨0e " ) ])
  graph_before = skg_files (context)
  full_before = read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") )
  completed = run_save (folder, "--yes")
  expected_head = (
    REPORT_HEAD +
    "Title: ⟨0e⟩ 'Second child, a forker' → 'Second child, renamed in E'.\n"
    "Server: nothing was saved, 1 error, 0 warnings.\n"
    "Server error: NOTHING WAS SAVED" )
  if ( completed . returncode != 1
       or not completed . stdout . startswith (expected_head)
       or "changed after the buffer was rendered" not in completed . stdout
       or "- ID: child-b-id" not in completed . stdout
       or "The server refused the save" not in completed . stderr ):
    raise CaseFailure (
      f"'save' printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "the graph after the edited stale save",
                  skg_files (context), graph_before )
  require_equal ( "the full view after the edited stale save",
                  read_file ( os . path . join (folder, ROOT_BASE_NAME + ".full.org") ),
                  full_before )

def require_unedited_stale_save_warned (context, folder):
  """FOLDER's view leaves the node B retitled unedited, so the server
  writes the view's edit, leaves that node as the graph has it, and
  warns of it; B's title stays, and the redraw shows it."""
  edit_compact ( folder, [ ( "** First child, renamed ⟨0d⟩",
                             "** First child, renamed in A ⟨0d⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Title: ⟨0d⟩ 'First child, renamed' → 'First child, renamed in A'.\n" +
    saved_line (1) +
    "Server warning: Left as the graph has it: Second child, a forker "
    "(child-b-id), which changed after this buffer was rendered and which "
    "this buffer did not edit.\n" +
    written_paths (folder) )
  require_equal ( "the unedited stale save's edit",
                  skg_title (context, "child-a-id"), "First child, renamed in A" )
  require_equal ( "B's title after the unedited stale save",
                  skg_title (context, "child-b-id"), "Second child, renamed in B" )
  require_in_compact ( folder, "** Second child, renamed in B ⟨0e " )

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
    ( "\n** Second child, retitled ⟨",
      "\n** " + new_title + "\n** Second child, retitled ⟨" ) ])
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
# 'fork', and a fork's attribution is stripped with 'stripAttribution'.
# The cases run in order on one server.
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
                check_implicit_fork_approved, check_stripping_an_attribution,
                check_explicit_fork,
                check_forking_and_hiding_by_placing ]:
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
    "*** A second container, outside the view ⟨0n aC1⟩\n"
    "**** Contained twice ⟨0o ☮ 2aC⟩\n"
    "*** An aside of Fred's ⟨0p 1H ⌂:fred⟩\n"
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
    "a forker', write-protected, since the view already holds an "
    "editable occurrence of it.\n" + saved_line (0) + written_paths (folder) )
  require_equal ( "the second container's content",
                  skg_list (context, "child-b-id", "contains"), [ "shared-id" ] )
  if not read_compact (folder) . endswith ( "*** Contained twice ⟨0t ☮ 3aC⟩\n" ):
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
    "child, a forker', write-protected, since the view already holds an "
    "editable occurrence of it.\n" + saved_line (0) +
    # The second child forks Fred's note and hid the detail.
    "Stops hiding 'A detail of Fred's' from 'Second child, a forker', as "
    "content of 'Fred's note'.\n" + written_paths (folder) )
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
    "*** With a child ⟨node:forkee-id⟩\n"
    "**** Its child\n"
    "*** With a word ⟨node:forker-id fork⟩\n" ) ])
  graph_before = skg_files (context)
  completed = run_save (folder, "--yes")
  require_output (
    completed,
    REPORT_HEAD +
    "Error: ⟨node:top-id⟩ on line 10 of the compact view has a body; a placed "
    "node is saved write-protected, which saves no body. Save the placement "
    "first, then edit its body.\n"
    "Error: ⟨node:forkee-id⟩ on line 12 of the compact view has children; "
    "a placed node is saved write-protected, under which nothing claims "
    "membership. Save the placement first, then add them.\n"
    "Error: ⟨node:forker-id⟩ on line 14 of the compact view carries "
    "'fork'; it takes no other word.\n",
    expected_returncode = 1 )
  edit_compact ( folder, [ ( "Its body line.\n", "" ), ( "**** Its child\n", "" ),
                           ( "⟨node:forker-id fork⟩", "⟨node:forker-id⟩" ) ])
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
  detail = compact_line_starting (folder, "*** A detail of Fred's ⟨0u ")
  edit_compact ( folder, [ ( detail, detail . replace (
    "A detail of Fred's ⟨", "A detail of Fred's, mine ⟨" )) ])
  graph_before = skg_files (context)
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨0u⟩ 'A detail of Fred's', of the foreign skgrepo 'fred', is "
    "write-protected, and its title changed. A write-protected occurrence "
    "saves nothing of its own, so this would fork nothing. To fork it, undo "
    "the edit, write 'editable' in its handle and save; then edit it and "
    "save again.\n",
    expected_returncode = 1 )
  require_equal ( "the graph after a refused edit",
                  skg_files (context), graph_before )

def check_making_a_foreign_node_editable (context):
  """'editable' in the handle of that write-protected occurrence asks
  for an editable view, as the clients' 'C-c s e' does: the save
  writes nothing, and the re-rendered view shows it editable, and the
  occurrence that was editable write-protected."""
  folder = fresh_root_view (context, "editable")
  detail = compact_line_starting (folder, "*** A detail of Fred's ⟨0u ")
  edit_compact ( folder, [ ( detail, detail [ : -1 ] + " editable⟩" ) ])
  graph_before = skg_files (context)
  fred_before = fred_files (context)
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Editable: ⟨0u⟩ 'A detail of Fred's', as '(viewRequests editableView)': "
    "the save makes it this view's editable occurrence of its node.\n"
    + saved_line (0) + written_paths (folder) )
  require_equal ( "the owned files after 'editable'", skg_files (context), graph_before )
  require_equal ( "the foreign files after 'editable'", fred_files (context), fred_before )
  if "☮" in compact_line_starting (folder, "*** A detail of Fred's ⟨0u "):
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
    "children changed: the edit goes to a new clone of it, which forks it; "
    "the original stays as it is.\n"
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
  approved: a clone in 'main' holds the edit and forks the original,
  which is unchanged, and the re-rendered view
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
    "children changed: the edit goes to a new clone of it, which forks it; "
    "the original stays as it is.\n"
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
  require_equal ( "what the clone forks",
                  skg_list (context, clone_id, "forks"), [ "fred-aside-id" ] )
  require_equal ( "the foreign files after the fork",
                  fred_files (context), fred_before )
  require_equal ( "the container's content after the fork",
                  skg_list (context, "child-a-id", "contains"),
                  [ "fred-detail-id", "elsewhere-id", "fred-aside-id", "shared-id" ] )
  # The reply, in main with no attribution, speaks in another voice
  # than its parent, the attributed clone: it says its home.
  require_in_compact ( folder, "*** An aside of Fred's ⟨0h C1 Kĥ1 ⌂:<main/fred>⟩\n"
                               "**** My note on it ⟨0v ⌂:main⟩\n" )
  clone_text = read_file ( os . path . join ( context [ "main_repo" ], clone_id + ".skg" ))
  if "attribution:\n  original: fred-aside-id\n  skgrepo: fred\n" not in clone_text:
    raise CaseFailure ( f"the clone records no attribution to Fred's note:\n{clone_text}" )
  context [ "clone_id" ] = clone_id

def check_stripping_an_attribution (context):
  """'stripAttribution' in the clone's handle, as the clients'
  'skg-strip-attribution' writes it, removes the clone's record of
  where it was forked from: its file loses the attribution, and its
  herald, standing under a note of its own skgrepo, goes."""
  folder = context [ "fork_folder" ]
  clone = compact_line_starting (folder, "*** An aside of Fred's ⟨0h ")
  edit_compact ( folder, [ ( clone, clone . replace ( "⟩", " stripAttribution⟩" )) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Strip attribution: ⟨0h⟩ 'An aside of Fred's': it no longer records "
    "the note it was forked from.\n" + saved_line (0) + written_paths (folder) )
  clone_text = read_file ( os . path . join (
    context [ "main_repo" ], context [ "clone_id" ] + ".skg" ))
  if "attribution:" in clone_text:
    raise CaseFailure ( f"the clone still records an attribution:\n{clone_text}" )
  require_in_compact ( folder, "*** An aside of Fred's ⟨0h C1 Kĥ1⟩\n" )

def check_explicit_fork (context):
  """'fork' on an owned node, as the clients' 'skg-fork-node' marks
  it: refused beside other edits, then approved with '--yes', making a
  clone in 'main' that forks it. The original, in 'main' too, stands
  as itself there, so the save warns that the view goes on showing it,
  and that no perspective shows the clone in its place."""
  folder = fresh_root_view (context, "fork-explicit")
  shared = compact_line_starting (folder, "*** Contained twice ⟨0l ")
  edit_compact ( folder, [ ( shared, shared [ : -1 ] + " fork⟩" ) ])
  first_child = compact_line_starting (folder, "** First child ⟨")
  edit_compact ( folder, [ ( first_child, first_child . replace (
    "First child", "First child, renamed" )) ])
  graph_before = skg_files (context)
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Error: ⟨0l⟩ 'Contained twice' carries 'fork', but the compact view has "
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
    "Fork: ⟨0l⟩ 'Contained twice', by request: a new clone of it forks "
    "it.\n" + FORK_INTO_MAIN +
    "Server: fork confirmation: the save forks 1 node, and writes nothing "
    "until that is approved.\n"
    "Clone-to-be: 'Contained twice', in 'main' (the server suggests "
    "'main').\n"
    "  It forks 'Contained twice' (shared-id), of the skgrepo 'main'.\n"
    "Approved: the save is sent again with the forks approved.\n"
    + saved_line (1) +
    "Server warning: Your edit of 'Contained twice' forked it into your "
    "skgrepo 'main', but this view is drawn there from the perspective of "
    "main, so it goes on showing the original there, without your edit. "
    "No perspective shows your fork there: its skgrepo holds another of "
    "the mirrors, which stands there. It stands in the forks folder of "
    "what is shown.\n" + written_paths (folder) )
  [ clone_file ] = set ( skg_files (context) ) - files_before
  clone_id = clone_file [ : - len (".skg") ]
  require_equal ( "the explicit clone's title", skg_title (context, clone_id),
                  "Contained twice" )
  require_equal ( "what the explicit clone forks",
                  skg_list (context, clone_id, "forks"), [ "shared-id" ] )
  require_equal ( "the forked node's title", skg_title (context, "shared-id"),
                  "Contained twice" )

def check_forking_and_hiding_by_placing (context):
  """First Fred's detail is taken out of the first child, so that of
  the owned notes only the second child keeps it. In the editable
  forks folder 'forest forks' draws, a placed foreign node is forked;
  it shows the detail in its folder of what the user keeps elsewhere,
  and the second child, which forks the same note and so stands in
  the folder too, an indirect member, shows it as its content, no
  other owned note keeping it. Cut from under the second child, the
  detail is hidden from the fork, which the report says before the
  save and the save says after. The cut from the indirect member makes
  the node fork it as well ('Fork:' reports none: no clone is made).
  Then the report says that removing a child the first child keeps
  hides nothing, naming the notes that keep it."""
  root = fresh_root_view (context, "free-the-detail")
  edit_compact ( root, [ (
    compact_line_starting (root, "*** A detail of Fred's ⟨0e ") + "\n", "" ) ])
  if "Server: saved," not in run_save (root, "--yes") . stdout:
    raise CaseFailure ( "the save taking the detail out of the first child failed" )
  require_equal ( "the first child's content without the detail",
                  skg_list (context, "child-a-id", "contains"),
                  [ "elsewhere-id", "fred-aside-id", "shared-id" ] )
  folder = os . path . join ( context [ "scratch" ], "write", "fork-by-placing" )
  require_tool_run ([ "forest", "forks", "elsewhere-id", "--out-dir",
                      folder, "--no-summary" ])
  base_name = "forest-forks_elsewhere-id"
  compact_path = os . path . join ( folder, base_name + ".compact.org" )
  compact = read_file (compact_path)
  folder_line = next ( line for line in compact . split ("\n")
                       if "Its mirrors are these." in line )
  edit_compact ( folder, [ ( folder_line + "\n", folder_line +
                             "\n*** Fred's ⟨node:fred-note-id⟩\n" ) ], base_name )
  if "Server: saved," not in run_save (folder, "--yes") . stdout:
    raise CaseFailure ( "the forking save failed" )
  require_equal ( "what the node forks",
                  skg_list (context, "elsewhere-id", "forks"),
                  [ "fred-note-id" ] )
  forked = read_file (compact_path)
  if "It contains these, and you keep them elsewhere." not in forked:
    raise CaseFailure ( f"the forked note shows no folder of what is kept elsewhere:\n{forked}" )
  details = [ line for line in forked . split ("\n")
              if line . startswith ("**** A detail of Fred's ⟨") ]
  if len (details) != 1:
    raise CaseFailure ( f"the second child alone shows the detail as content:\n{forked}" )
  edit_compact ( folder, [ ( detail + "\n", "" ) for detail in details ], base_name )
  hide_line = ( "Hides 'A detail of Fred's' from 'A second container, "
                "outside the view', as content of 'Fred's note' and "
                "'Second child, a forker'.\n" )
  reported = run_save (folder) . stdout
  if hide_line not in reported:
    raise CaseFailure ( f"the report before the hiding save printed {reported!r}" )
  require_equal ( "what the node hides before the save",
                  skg_list (context, "elsewhere-id", "hides"), [] )
  completed = run_save (folder, "--yes")
  if ( "Fork:" in completed . stdout or "Server: saved," not in completed . stdout
       or completed . stdout . count (hide_line) != 1 ):
    raise CaseFailure ( f"the hiding save printed {completed . stdout!r}" )
  require_equal ( "what the node hides from its forks",
                  skg_list (context, "elsewhere-id", "hides"),
                  [ "fred-detail-id" ] )
  require_equal ( "what the node forks after the cut",
                  skg_list (context, "elsewhere-id", "forks"),
                  [ "fred-note-id", "child-b-id" ] )
  lines = read_file (compact_path) . split ("\n")
  start = next ( index for index, line in enumerate (lines)
                 if line . startswith ("** Contained twice ⟨") )
  end = next ( ( index for index in range (start + 1, len (lines))
                 if re . match ( r'\*{1,2} ', lines [index] )),
               len (lines) - 1 ) # the text ends in a newline
  edit_compact ( folder, [ ( "\n" . join ( lines [start : end] ) + "\n", "" ) ],
                 base_name )
  reported = run_save (folder) . stdout
  if ( "Hides nothing: 'Contained twice' left 'A second container, outside "
       "the view', but 'First child' and 'Second child, a forker' keep "
       "it.\n" ) not in reported:
    raise CaseFailure ( f"the report of a removal printed {reported!r}" )

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
                           ( "** ⟨0b ☮ These also contain it.⟩",
                             "** ⟨0b ☮ These also contain it. write-protect⟩" ) ])
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
  occurrence below, skipping the foreign and the write-protected ones,
  which it names. (Fred's note, typed into 'Contained twice', is drawn
  there as 'Second child', the user's fork of it, which is editable
  elsewhere in the view and so write-protected there.)"""
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
                    "Search matching: ⟨0f⟩ 'Second child, a forker', as "
                    "'(flag noSearchMatching true)': the node no longer matches "
                    "searches.",
                    "Skipped: 'noSearchMatching subtree' on ⟨0a⟩ leaves ⟨0n⟩ "
                    "'Second child, a forker' (write-protected)." ] )
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
  edit_compact ( folder, [ ( "merge:nobody-id", "merge:forkee-id" ) ], base_name )
  require_output (
    run_save (folder, "--yes"),
    head + "Merge: ⟨0a⟩ 'A second container, outside the view' acquires "
    "⟨node:forkee-id⟩ 'A forkee, outside the view', as '(merge "
    "forkee-id)': a nodeMerge makes the two one node, this one, with the "
    "relationships of both.\n" + saved_line (0) +
    "" . join ( os . path . join (folder, base_name + suffix) + "\n"
                for suffix in ( ".full.org", ".compact.org", ".view.json" )))
  require_equal ( "whether the acquiree's file remains",
                  os . path . exists ( skg_path (context, "forkee-id") ), False )
  require_equal ( "the acquirer's extra IDs",
                  skg_list (context, "elsewhere-id", "extra_ids"),
                  [ "forkee-id" ] )

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
    "'delete', 'processed', 'unprocessed', 'write-protect', 'repo:NAME', "
    "'relRepo:NAME', 'noSearchMatching' and 'searchMatching'.\n",
    expected_returncode = 1 )
  edit_compact ( folder, [ ( " subtree⟩", "⟩" ) ])
  with open ( os . path . join ( folder, ROOT_BASE_NAME + ".compact.org" ),
              "a", encoding = "utf-8" ) as compact_file:
    compact_file . write ( "** Made in a foreign skgrepo ⟨new repo:fred⟩\n" )
  new_line = len ( read_compact (folder) . split ("\n") ) - 1
  root = append_word ("* The fixture root ⟨", "relRepo:main")
  contained = append_word ("*** Contained twice ⟨", "repo:fred")
  second = append_word ("** Second child, a forker ⟨", "repo:main")
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
# saved, the diff view shows them, one is marked processed, a deleted node
# is restored, and everything is committed. The cases run in order on
# one server.
# ============================================================

DIFF_BASE_NAME = "diff_all-changes"
DIFF_VIEW_ID_PATTERN = re . compile (
  r'diff-view:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' )
COMMIT_MESSAGE = "Review the fixture."
NOT_ALL_PROCESSED = (
  "Not every change is processed: 0 of 3 changed nodes are processed.\n" )
EMPTY_DIFF_VIEW = (
  "The diff view is empty: nothing changed since the last commits.\n" )

def failures_of_reviewing (scratch, data_root):
  main_repo = os . path . join (data_root, "me", "main")
  context = { "scratch" : scratch, "main_repo" : main_repo }
  for case in [ check_edits_to_review, check_diff_shows_the_changes,
                check_marking_processed, check_commit_of_the_processed,
                check_unconfirmed_commit,
                check_restoring_a_deleted_node, check_confirmed_commit,
                check_extending_the_diff_view, check_marking_a_subtree_processed,
                check_marking_a_reorder, check_blank_commit_message ]:
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

def check_marking_processed (context):
  """'processed both outward' on the new node marks its existence and
  its membership, and hands the outward walk to the server, as 'C-u
  C-u C-c m b p' does; a fresh diff view shows both processed.
  'unprocessed both' there would unmark two changes, so the report
  asks first; 'unprocessed rel' unmarks one, and asks nothing."""
  folder = run_diff (context, "mark")
  edit_compact (
    folder, [ ( "**** A new grandchild ⟨0j ∃+ ~+⟩",
                "**** A new grandchild ⟨0j ∃+ ~+ processed both outward⟩" ) ],
    DIFF_BASE_NAME )
  # An unsaved processed word is an edit, which 'extend' refuses.
  completed = run_tool ([ "extend", sidecar_of_diff (folder) ])
  if ( completed . returncode != 1
       or "the compact view has edits" not in completed . stderr ):
    raise CaseFailure ( f"'extend' over an unsaved 'processed' printed "
                        f"{completed . stdout!r}, {completed . stderr!r}" )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Processed: ⟨0j⟩ 'A new grandchild', as '(processed node rel newOutward)'.\n" +
    saved_line (0) +
    "Processed: 1 of 4 changed nodes are processed.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  fresh = run_diff (context, "marked")
  compact = read_diff_compact (fresh)
  for expected in [ "# Processed: 1 of 4 changed nodes are processed.\n",
                    "**** A new grandchild ⟨0j ∃+✓ ~+✓⟩\n" ]:
    if expected not in compact:
      raise CaseFailure ( f"the fresh diff view lacks {expected!r}:\n{compact}" )
  edit_compact (
    fresh, [ ( "⟨0j ∃+✓ ~+✓⟩", "⟨0j ∃+✓ ~+✓ unprocessed both⟩" ) ], DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (fresh) ]),
    diff_report_head () +
    "Unprocessed: ⟨0j⟩ 'A new grandchild', as '(unprocessed node rel)'.\n"
    "Question: this unmarks 2 changes. Unmarking this node processed "
    "will unmark every contributing decision. You might discard a lot of "
    "hard-won information. Continue? Saving with '--yes' answers yes.\n" +
    NOT_SAVED,
    ignoring_view_ids = True )
  edit_compact (
    fresh, [ ( "⟨0j ∃+✓ ~+✓ unprocessed both⟩", "⟨0j ∃+✓ ~+✓ unprocessed rel⟩" ) ],
    DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (fresh) ]),
    diff_report_head () +
    "Unprocessed: ⟨0j⟩ 'A new grandchild', as '(unprocessed rel)'.\n" +
    NOT_SAVED,
    ignoring_view_ids = True )

PROCESSED_COMMIT_MESSAGE = "Commit the new grandchild."

def check_commit_of_the_processed (context):
  """'commit' commits the processed nodes and asks nothing: the new
  grandchild alone, whose container, not processed, is left with its
  mark on the grandchild's membership. Run again, it finds nothing
  processed, and says that 'commit --all' commits everything."""
  completed = run_tool ([ "commit", PROCESSED_COMMIT_MESSAGE ], standard_input = "" )
  head = git ( context [ "main_repo" ], [ "rev-parse", "HEAD" ] ) . strip ()
  require_output (
    completed,
    f"Committed 1 node, in main ({head}). 3 changed nodes are left.\n" )
  require_equal ( "the git log after the commit of the processed",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  PROCESSED_COMMIT_MESSAGE + "\nThe fixture\n" )
  require_equal ( "the files that commit took",
                  git ( context [ "main_repo" ],
                        [ "show", "--name-status", "--format=", "HEAD" ] )
                  . count ("\n"), 1 )
  compact = read_diff_compact ( run_diff (context, "after-processed") )
  for expected in [ "# Processed: 0 of 3 changed nodes are processed.\n",
                    "A new grandchild ⟨0" ]:
    if expected not in compact:
      raise CaseFailure ( f"the diff view after the commit lacks {expected!r}:\n{compact}" )
  if "~+✓" not in compact:
    raise CaseFailure ( f"the container's mark on the grandchild was lost:\n{compact}" )
  require_output (
    run_tool ([ "commit", PROCESSED_COMMIT_MESSAGE ]),
    "Nothing is processed, so nothing was committed; "
    "'commit --all' commits everything.\n" )
  require_equal ( "the git log after a commit with nothing processed",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  PROCESSED_COMMIT_MESSAGE + "\nThe fixture\n" )

def check_unconfirmed_commit (context):
  """'commit --all' while not every change is processed: the server
  asks, and an answer other than 'yes' commits nothing."""
  graph_before = skg_files (context)
  require_output (
    run_tool ([ "commit", COMMIT_MESSAGE, "--all" ], standard_input = "no\n"),
    NOT_ALL_PROCESSED + "Type yes to commit anyway: \nNothing was committed.\n" )
  require_equal ( "the git log after an unconfirmed commit",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  PROCESSED_COMMIT_MESSAGE + "\nThe fixture\n" )
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
    "Processed: 0 of 1 changed nodes are processed.\n" +
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
  """'--yes' alone commits everything, as '--all' does, and answers the
  server's question; the commit carries the message, and leaves the
  diff view empty. It takes 3 nodes where the summary counted 1
  changed: the restore rewrote the two others' files in another field
  order, which the parser does not read, and such files go in with
  any commit, counted ('docs/diff-view.org', "The unit is the
  node")."""
  completed = run_tool ([ "commit", COMMIT_MESSAGE, "--yes" ])
  head = git ( context [ "main_repo" ], [ "rev-parse", "HEAD" ] ) . strip ()
  require_output (
    completed,
    "Not every change is processed: 0 of 1 changed nodes are processed.\n"
    f"Committed 3 nodes, in main ({head}).\n" )
  require_equal ( "the git log after the commit",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  COMMIT_MESSAGE + "\n" + PROCESSED_COMMIT_MESSAGE + "\nThe fixture\n" )
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
    "Before extending: 0 of 2 changed nodes are processed, 1 changes are "
    "not editable in this buffer.\n"
    "Extended: 0 of 2 changed nodes are processed.\n" +
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

def check_marking_a_subtree_processed (context):
  """'processed both subtree' marks every change in the headline's
  subtree, as 'C-u C-c m b p' does; 'unprocessed both subtree' unmarks
  them, asking first. 'processed rel subtree', where no relationship
  there changed, names nothing, and is refused, as the clients refuse it."""
  folder = run_diff (context, "subtree-processed")
  edit_compact ( folder, [ ( "** The fixture root, retitled ⟨0b aC2 L2 M⟩",
                             "** The fixture root, retitled ⟨0b aC2 L2 M processed "
                             "rel subtree⟩" ) ], DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Error: ⟨0b⟩ on line 16 of the compact view carries 'processed rel', but "
    "its relationship to its viewparent has no change to mark here.\n",
    expected_returncode = 1, ignoring_view_ids = True )
  edit_compact ( folder, [ ( "M processed rel subtree⟩", "M processed both subtree⟩" ) ],
                 DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Processed: ⟨0b⟩ 'The fixture root, retitled', as '(processed node)'.\n"
    "Processed: ⟨0c⟩ 'The fixture root', as '(processed)'.\n"
    "Processed: ⟨0f⟩ 'A grandchild, retitled twice', as '(processed node)'.\n"
    "Processed: ⟨0g⟩ 'A new grandchild', as '(processed)'.\n" +
    saved_line (0) + "Processed: Commit ready. All 2 of 2 changed nodes are processed.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  edit_compact ( folder, [ ( "** The fixture root, retitled ⟨0b aC2 L2 M✓⟩",
                             "** The fixture root, retitled ⟨0b aC2 L2 M✓ "
                             "unprocessed both subtree⟩" ) ], DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Unprocessed: ⟨0b⟩ 'The fixture root, retitled', as '(unprocessed node)'.\n"
    "Unprocessed: ⟨0c⟩ 'The fixture root', as '(unprocessed)'.\n"
    "Unprocessed: ⟨0f⟩ 'A grandchild, retitled twice', as '(unprocessed node)'.\n"
    "Unprocessed: ⟨0g⟩ 'A new grandchild', as '(unprocessed)'.\n"
    "Question: this unmarks 4 changes. Unmarking this node processed will "
    "unmark every contributing decision. You might discard a lot of hard-won "
    "information. Continue? Saving with '--yes' answers yes.\n" +
    saved_line (0) + "Processed: 0 of 2 changed nodes are processed.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )

REORDER_COMMIT_MESSAGE = "Put the second child first."

def check_marking_a_reorder (context):
  """A reorder of the root's content, saved through a content view, is
  the root's other changes: the diff view shows it as 'its content was
  reordered', with the old order against the new in its body, and
  writes the order-only buffer too. 'processed node' marks it, and
  'commit' takes the root."""
  require_tool_run ([ "commit", "Before the reorder.", "--all", "--yes" ])
  view = fresh_root_view (context, "reorder")
  compact_path = os . path . join (view, "view_root-id.compact.org")
  lines = read_file (compact_path) . split ("\n")
  first = next ( i for i, line in enumerate (lines)
                 if line . startswith ("** First child, renamed ⟨") )
  second = next ( i for i, line in enumerate (lines)
                  if line . startswith ("** Second child, a forker ⟨") )
  end = len (lines) - 1 if lines [-1] == "" else len (lines)
  lines = lines [ : first ] + lines [ second : end ] + lines [ first : second ] + lines [ end : ]
  with open (compact_path, "w") as f:
    f . write ( "\n" . join (lines) )
  require_save_succeeds (view)
  folder = os . path . join ( context [ "scratch" ], "review", "reorder" )
  completed = require_tool_run ([ "diff", "--out-dir", folder ])
  if "diff_order-only-changes.compact.org" not in completed . stdout:
    raise CaseFailure ( f"'diff' wrote no order-only buffer: {completed . stdout!r}" )
  shown = ( "its content was reordered⟩\n"
            "The order of its content (- at HEAD, + now):\n"
            "+Second child, a forker\n"
            " First child, renamed\n"
            "-Second child, a forker\n" )
  require_in_diff_compact (folder, shown)
  edit_compact ( folder, [ ( "its content was reordered⟩",
                             "its content was reordered processed node⟩" ) ],
                 DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Processed: ⟨0c⟩ (otherChanges contentOrder), as '(processed)'.\n" +
    saved_line (0) +
    "Processed: Commit ready. All 1 of 1 changed nodes are processed.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  require_in_diff_compact ( folder, "its content was reordered✓⟩\n" )
  completed = run_tool ([ "commit", REORDER_COMMIT_MESSAGE ], standard_input = "" )
  head = git ( context [ "main_repo" ], [ "rev-parse", "HEAD" ] ) . strip ()
  require_output ( completed, f"Committed 1 node, in main ({head}).\n" )
  require_equal ( "the files the reorder's commit took",
                  git ( context [ "main_repo" ],
                        [ "show", "--name-only", "--format=", "HEAD" ] ),
                  "root-id.skg\n" )

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
  "# Processed: 0 of 4 changed nodes are processed.\n"
  "# Headlines with change facts 7, processed 0, unprocessed 7.\n"
  "# skg-for-ai compact view of diff_all-changes.full.org (view ID diff-view:ID)\n"
  "# Each headline ends in its handle ⟨ID HERALDS⟩: ID its viewnode ID,\n"
  "# the '(vnid ID)' of its metadata in the full view, and HERALDS what a\n"
  "# client shows for it, but a bare 'aC' (only the parent contains it);\n"
  "# 'folded' marks a headline a client shows collapsed.\n"
  "# What the heralds mean: \"Start here\" in tools/skg-for-ai/README.org.\n"
  "# Headlines 11, node occurrences 6, distinct nodes 6, changesetMarker "
  "content 2, ghost 2, oldText title 1.\n"
  "# Skgrepos: main (owned) 6.\n"
  "# Foreign 0, write-protected 1, folded 0.\n"
  "# Contained elsewhere too 0, forked 0, forking 1.\n"
  "# Links 3, distinct targets 3, resolved 2, unresolved 1 (missing 1).\n"
  "# Server errors 0, warnings 0.\n"
  # The deleted node's other container, a root of its own
  "* A second container, outside the view ⟨0a ⌂:main⟩\n"
  "** ⟨0b its content changed⟩\n"
  "** Contained twice ⟨0c ᗣ☮ ∃- ~-⟩\n"
  "* Above the root ⟨0d C1 ⌂:main⟩\n"
  "** The fixture root ⟨0e aC2 L2⟩\n"
  "The root links to [[id:child-b-id][its second child]]⟦→ Second child, "
  "a forker ⌂:main⟧,\n"
  "to [[id:fred-note-id][Fred's note]]⟦→ Fred's note ⌂:fred⟧ in the "
  "foreign repo,\n"
  "and to [[id:missing-id][a node nobody wrote]]⟦not held here⟧.\n"
  "*** First child, renamed ⟨0f aC1 M⟩\n"
  "A body line that stays unnumbered.\n"
  "**** First child ⟨0g old title⟩\n"
  "**** ⟨0h its content changed⟩\n"
  "**** Contained twice ⟨0i ᗣ☮ ∃- ~-⟩\n"
  # The new node's containers changed: it had none at HEAD. Its one
  # container stands above it, so no containers folder is drawn; its
  # '~+' says the change.
  "**** A new grandchild ⟨0j ∃+ ~+⟩\n"
  "*** Second child, a forker ⟨0k ☮ a/L K1 H2⟩\n" )

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
  whether the view is still open, which the daemon's is."""
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
    ( "** Second child, a forker ⟨0f ",
      "** Second child, renamed on the second server ⟨0f " ))
  require_log_line ( context, RELOADED_ONE_MAIN_FILE )
  require_log_line (
    context,
    "re-rendered view_root-id because files changed on disk (reloaded 1 "
    "file from repo main); 'Second child, renamed "
    "on the second server' changed: title, was 'Second child, a forker'" )
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
  if "Committed 3 nodes, in main (" not in completed . stdout:
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
  """A 'commit --all' that waits for a typed "yes" runs on its own
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
  completed = run_tool ( [ "commit", "Commit interactively.", "--all" ],
                         standard_input = "yes\n" )
  if completed . returncode != 0 or "Committed 1 node, in main (" not in completed . stdout:
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
                  check_marking_like_moves_processed, check_deleting_references ]:
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
  sending the save again, which the server would refuse once the save
  had made a node, whose container would then be an edited stale
  graphnode; the files then show the new node."""
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
  forker no longer shows it. The switch is refused while a held
  view has unsaved edits. 'restrict private --foreign shown' lifts the
  restriction."""
  folder = context [ "daemon_folder" ]
  require_tool_run ([ "view", "child-b-id", "--out-dir", folder ])
  require_in_file ( folder, "view_child-b-id", "*** Fred's note ⟨" )
  edit_compact ( folder, [ ( "* Second child, a forker ⟨0a ",
                             "* Second child, unsaved ⟨0a " ) ], "view_child-b-id" )
  completed = run_tool ([ "restrict", "--foreign", "hidden" ])
  require_output ( completed, "", expected_returncode = 1 )
  if "view_child-b-id.compact.org has unsaved edits" not in completed . stderr:
    raise CaseFailure ( f"stderr was {completed . stderr!r}" )
  edit_compact ( folder, [ ( "* Second child, unsaved ⟨0a ",
                             "* Second child, a forker ⟨0a " ) ], "view_child-b-id" )
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
  second = compact_line_starting (folder, "** Second child, a forker ⟨")
  edit_compact ( folder, [ ( second + "\n",
                             second + "\n*** A child kept private\n" ) ])
  require_save_succeeds (folder)
  second = compact_line_starting (folder, "** Second child, a forker ⟨")
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
  second = compact_line_starting (folder, "** Second child, a forker ⟨")
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

def check_marking_like_moves_processed (context):
  """In the diff view, 'processed node subtree' on a home skgrepo move
  marks every move between the same two skgrepos in the moved node's
  subtree, as 'C-u C-c m h p' does: the branch's and its leaf's, and
  not the move of the node above them."""
  folder = os . path . join ( context [ "scratch" ], "review", "like-moves" )
  require_tool_run ([ "diff", "--out-dir", folder ])
  require_in_diff_compact ( folder, "* A private branch ⟨0a C1 ⌂:main M⟩\n"
                                    "** ⟨0b ⌂ -private +main⟩\n" )
  edit_compact ( folder, [ ( "** ⟨0b ⌂ -private +main⟩",
                             "** ⟨0b ⌂ -private +main processed node subtree⟩" ) ],
                 DIFF_BASE_NAME )
  require_output (
    run_tool ([ "save", sidecar_of_diff (folder), "--yes" ]),
    diff_report_head () +
    "Processed: ⟨0b⟩ (homeSkgrepoMove (old private) (new main)), as '(processed)'.\n"
    "Processed: ⟨0e⟩ (homeSkgrepoMove (old private) (new main)), as '(processed)'.\n"
    + saved_line (0) + "Processed: 1 of 12 changed nodes are processed.\n" +
    diff_written_paths (folder),
    ignoring_view_ids = True )
  require_in_diff_compact ( folder, "** ⟨0b ⌂ -private +main✓⟩\n" )
  require_in_diff_compact ( folder, "** Its private leaf ⟨0d M✓⟩\n"
                                    "*** ⟨0e ⌂ -private +main✓⟩\n" )
  require_in_diff_compact ( folder, "***** ⟨0u ⌂ -private +main⟩\n" )

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
    "node absent-id (in 'contains', 'forks' and 'hides' lists); run again "
    "with '--yes'.\n" )
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

# ============================================================
# A view's perspective: the data of the perspective integration
# test, where R, in foreign skgrepo "foreign", contains M, in foreign
# skgrepo "origin", and K; A, in "foreign", U, in owned "main", and B,
# in foreign "third", each fork M; and P, in "main", contains M. A
# view of R is drawn from its root's home, "foreign", A in M's place;
# from main's, U; from third's, B; from origin's, M itself. The cases
# run in order.
# ============================================================

PERSPECTIVE_DATA = os . path . join (
  REPO_ROOT, "tests", "integration", "perspective", "data" )
FOLLOWS_MAINS_LINE = "Showing the perspective of main.\n"
FOLLOWS_THIRDS_LINE = "Showing the perspective of third.\n"
FOLLOWS_ORIGINS_LINE = "Showing the perspective of origin.\n"
FOLLOWS_ROOT_HOMES_LINE = (
  "Showing the perspective of each root's home skgrepo (foreign).\n" )
FOLLOWS_MAINS_SUMMARY = (
  "# Showing the perspective of main (each root's home skgrepo is the default).\n" )
FOLLOWS_THIRDS_SUMMARY = (
  "# Showing the perspective of third (each root's home skgrepo is the default).\n" )
R_SHOWING_A = "* R ⟨0a C2 ⌂:foreign⟩\n** A ⟨0b Kĥ1⟩\n*** ⟨Its mirrors are these.⟩\n**** M ⟨2C 3bK ⌂:origin⟩\n***** ⟨☮ These also contain it.⟩\n****** P ⟨☮ Cb ⌂:main folded⟩\n**** U ⟨K1 ⌂:main⟩\n**** B ⟨K1 ⌂:third⟩\n** K ⟨0i⟩\n"
R_SHOWING_U = "* R ⟨0a C2 ⌂:foreign⟩\n** U ⟨0b Kĥ1 ⌂:main⟩\n*** ⟨Its mirrors are these.⟩\n**** M ⟨2C 3bK ⌂:origin⟩\n***** ⟨☮ These also contain it.⟩\n****** P ⟨☮ Cb ⌂:main folded⟩\n**** A ⟨K1 ⌂:foreign⟩\n**** B ⟨K1 ⌂:third⟩\n** K ⟨0i⟩\n"
R_SHOWING_B = "* R ⟨0a C2 ⌂:foreign⟩\n** B ⟨0b Kĥ1 ⌂:third⟩\n*** ⟨Its mirrors are these.⟩\n**** M ⟨2C 3bK ⌂:origin⟩\n***** ⟨☮ These also contain it.⟩\n****** P ⟨☮ Cb ⌂:main folded⟩\n**** A ⟨K1 ⌂:foreign⟩\n**** U ⟨K1 ⌂:main⟩\n** K ⟨0i⟩\n"

def copy_perspective_fixture (data_root):
  """The perspective data, with a config naming its three foreign
  skgrepos and its owned one."""
  for folder in ( "foreign", "owned", "third", "origin" ):
    shutil . copytree ( os . path . join (PERSPECTIVE_DATA, folder),
                        os . path . join (data_root, folder) )
  with open ( os . path . join (data_root, "skgconfig.toml"), "w",
              encoding = "utf-8" ) as config_file:
    config_file . write (
      'tantivy_folder = ".index.tantivy"\nport = 0\ntiming_log = false\n'
      'beep_when_server_becomes_available = false\n\n'
      '[[repos]]\nname = "foreign"\npath = "foreign"\nforeign = true\n\n'
      '[[repos]]\nname = "main"\npath = "owned/main"\n\n'
      '[[repos]]\nname = "third"\npath = "third"\nforeign = true\n\n'
      '[[repos]]\nname = "origin"\npath = "origin"\nforeign = true\n' )

# ============================================================
# Receipts: fred's mailboxes 'box' (holding 'letter' and 'aside') and
# 'box2' (holding 'aside'), which the reader's 'boxfork' and
# 'boxfork2', in the owned 'mine', fork; 'loner' is in no mailbox.
# The reader also owns 'private'.
# ============================================================

RECEIPT_FIXTURE_FILES = {
  "skgconfig.toml" :
    'tantivy_folder = ".index.tantivy"\nport = 0\ntiming_log = false\n'
    'beep_when_server_becomes_available = false\n\n'
    '[[repos]]\nname = "fred"\npath = "fred/public"\nforeign = true\n\n'
    '[[repos]]\nname = "mine"\npath = "me/mine"\n\n'
    '[[repos]]\nname = "private"\npath = "me/private"\n',
  "fred/public/box.skg" :
    "pid: box\ntitle: the box\ncontains:\n- letter\n- aside\n",
  "fred/public/box2.skg" : "pid: box2\ntitle: the other box\ncontains:\n- aside\n",
  "fred/public/letter.skg" : "pid: letter\ntitle: a letter\n",
  "fred/public/aside.skg" : "pid: aside\ntitle: an aside\n",
  "fred/public/loner.skg" : "pid: loner\ntitle: a loner\n",
  "me/mine/boxfork.skg" : "pid: boxfork\ntitle: my box\nforks:\n- box\n",
  "me/mine/boxfork2.skg" : "pid: boxfork2\ntitle: my other box\nforks:\n- box2\n",
  "me/private/secret.skg" : "pid: secret\ntitle: a secret\n" }

def write_receipt_fixture (data_root):
  for relative_path, text in RECEIPT_FIXTURE_FILES . items ():
    path = os . path . join (data_root, relative_path)
    os . makedirs ( os . path . dirname (path), exist_ok = True )
    with open (path, "w", encoding = "utf-8") as fixture_file:
      fixture_file . write (text)

def failures_of_receipts (scratch, data_root):
  """'receipt ITEM TEXT' without '--yes' prints what it would do and
  writes nothing; with it, forks the item, writes the receipt under
  the fork and hides the item in the fork of its mailbox. A second
  receipt for it goes under the same fork, hiding nothing more. An
  item in no mailbox asks for '--fork-into'; one in two asks for
  '--mailbox'."""
  context = { "scratch" : scratch, "data_root" : data_root,
              "main_repo" : os . path . join (data_root, "me", "mine") }
  try:
    check_receipts (context)
  except CaseFailure as failure:
    return [ "check_receipts: " + str (failure) ]
  return []

def require_lines_in (completed, lines, returncode = 0, stderr_text = ""):
  """COMPLETED exited RETURNCODE, printing each of LINES, and STDERR_TEXT
  on standard error."""
  printed = completed . stdout . split ("\n")
  if ( completed . returncode != returncode
       or stderr_text not in completed . stderr
       or any ( line not in printed for line in lines ) ):
    raise CaseFailure (
      f"exit {completed . returncode}, printed:\n{completed . stdout}stderr: "
      f"{completed . stderr}\nexpected exit {returncode}, the lines {lines!r} "
      f"and on stderr {stderr_text!r}" )

def check_receipts (context):
  forks = ( "Forks 'a letter' (letter, of 'fred') into 'mine', and writes the "
            "receipt 'Read.' as the first child of the fork." )
  hides = "Hides 'a letter' from 'my box', as content of 'the box'."
  require_lines_in ( run_tool ([ "receipt", "letter", "Done." ]),
    [ "Not written. The receipt would do this; run again with '--yes':",
      forks, hides ] )
  require_equal ( "my box's hides after the preview",
                  skg_list (context, "boxfork", "hides"), [] )
  completed = run_tool ([ "receipt", "letter", "Done.", "--yes" ])
  require_lines_in ( completed, [ forks, hides ] )
  if "Server: wrote the receipt " not in completed . stdout:
    raise CaseFailure ( f"it printed {completed . stdout!r}" )
  require_equal ( "my box's hides", skg_list (context, "boxfork", "hides"),
                  [ "letter" ] )
  again = run_tool ([ "receipt", "letter", "Answered.", "--title", "Answered.",
                      "--yes" ])
  require_lines_in ( again, [ "Hides nothing: 'my box' already hides 'a letter'." ] )
  if "your fork of it, in 'mine'." not in again . stdout:
    raise CaseFailure ( f"the second receipt printed {again . stdout!r}" )
  require_lines_in (
    run_tool ([ "receipt", "loner", "Read.", "--yes" ]), [], 1,
    "Run again with '--fork-into' one of: mine, private." )
  require_lines_in (
    run_tool ([ "receipt", "loner", "Read.", "--fork-into", "private", "--yes" ]),
    [ "Forks 'a loner' (loner, of 'fred') into 'private', and writes the "
      "receipt 'Read.' as the first child of the fork.",
      "Hides nothing: no note of another's that holds 'a loner' is forked by "
      "a note of yours, so it is in no mailbox you read." ] )
  require_lines_in (
    run_tool ([ "receipt", "aside", "", "--yes" ]), [], 1,
    "Run again with '--mailbox' one of: box ('the box'), box2 ('the other box')." )
  require_lines_in (
    run_tool ([ "receipt", "aside", "", "--mailbox", "boxfork2", "--yes" ]),
    [ "Hides 'an aside' from 'my other box', as content of 'the other box'." ] )
  require_equal ( "my other box's hides", skg_list (context, "boxfork2", "hides"),
                  [ "aside" ] )

def failures_of_perspective (scratch, data_root):
  context = { "scratch"       : scratch,
              "data_root"     : data_root,
              "main_repo"     : os . path . join (data_root, "owned", "main"),
              "daemon_folder" : os . path . join (scratch, "daemon"),
              "processes"     : [] }
  try:
    for case in [ check_switching_perspective_on_a_command_s_own_connection,
                  check_saving_a_view_that_follows_mains,
                  check_following_the_root_homes_again,
                  check_the_root_too,
                  check_switching_perspective_through_a_daemon,
                  check_viewing_a_root_a_perspective_put_in_place ]:
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

def r_tree (folder):
  """The headlines of the view of R in FOLDER, and the summary's line
  naming its perspective, if it has one. A headline three or more
  levels deep loses its viewnode ID: those under R's child stand in
  the forks folder of the mirror shown there, drawn afresh, with new
  viewnode IDs, whenever a switch re-points that child."""
  return "" . join ( without_deep_vnid (line) + "\n"
                     for line in read_compact (folder, "view_R") . split ("\n")
                     if line . startswith ("*") or line . startswith ("# Showing") )

def without_deep_vnid (line):
  """LINE without its handle's viewnode ID, if it is a headline three
  or more levels deep."""
  if not line . startswith ("***"):
    return line
  return re . sub ( r"⟨[0-9a-z]+( |⟩)", lambda m: "⟨" + ( "" if m . group (1) == " " else "⟩" ),
                    line, count = 1 )

def check_switching_perspective_on_a_command_s_own_connection (context):
  """With no daemon, 'perspective' registers the view on its
  own connection (by saving it back unchanged) and switches it to the
  next perspective, main's: U stands in M's place, the summary says
  so, and the sidecar records it. A view of P, in "main", is drawn
  from main's already, and the server refuses a switch that names
  it."""
  folder = os . path . join ( context [ "scratch" ], "own" )
  context [ "own_folder" ] = folder
  require_tool_run ([ "view", "R", "--out-dir", folder ])
  require_equal ( "R's view", r_tree (folder), R_SHOWING_A )
  require_equal ( "a fresh sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ],
                  "root homes" )
  sidecar_path = os . path . join (folder, "view_R.view.json")
  require_output (
    run_tool ([ "perspective", sidecar_path ]),
    FOLLOWS_MAINS_LINE + "" . join (
      os . path . join (folder, "view_R" + suffix) + "\n"
      for suffix in ( ".full.org", ".compact.org", ".view.json" )))
  require_equal ( "R's view, following main's", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U )
  require_equal ( "the sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ], "skgrepo main" )
  require_tool_run ([ "view", "P", "--out-dir", folder ])
  require_failure (
    run_tool ([ "perspective", os . path . join (folder, "view_P.view.json"),
                "skgrepo", "main" ]),
    [], "The server refused: The perspective of main would show this buffer "
        "as the perspective of each root's home skgrepo does, so it is not "
        "one of its perspectives." )

def check_saving_a_view_that_follows_mains (context):
  """An edited view is not switched. Its save names the choice: the
  server checks U's '(inPlaceOf M)' and rerenders the view from
  main's perspective, and the rerendered view still shows U, its
  sidecar still recording the choice."""
  folder = context [ "own_folder" ]
  edit_compact ( folder, [ ( "** U ⟨0b Kĥ1 ⌂:main⟩\n",
                             "** U ⟨0b Kĥ1 ⌂:main⟩\nU's new body.\n" ) ], "view_R" )
  require_failure (
    run_tool ([ "perspective", os . path . join (folder, "view_R.view.json"),
                "root homes" ]),
    [], "Nothing was switched: the compact view has edits" )
  require_save_succeeds ( os . path . join (folder, "view_R.view.json") )
  require_equal ( "R's view, saved", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U )
  require_in_file ( folder, "view_R", "** U ⟨0b Kĥ1 ⌂:main⟩\nU's new body.\n" )
  require_equal ( "U's body", skg_body (context, "U"), [ "U's new body." ] )
  require_equal ( "the saved sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ], "skgrepo main" )

def check_following_the_root_homes_again (context):
  """Named, third's perspective shows B in M's place; the next after
  it is origin's, M's own home, which shows M itself; the next after
  that, the last, is the root homes again."""
  folder = context [ "own_folder" ]
  completed = run_tool ([ "perspective", os . path . join (folder, "view_R.view.json"),
                          "skgrepo third" ])
  if completed . returncode != 0 or not completed . stdout . startswith (
      FOLLOWS_THIRDS_LINE ):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's view, following third's", r_tree (folder),
                  FOLLOWS_THIRDS_SUMMARY + R_SHOWING_B )
  require_equal ( "the sidecar's perspective",
                  { key : read_sidecar (folder, "view_R") [key]
                    for key in ( "perspective", "perspective_name" ) },
                  { "perspective" : "skgrepo third",
                    "perspective_name" : "the perspective of third" } )
  completed = run_tool ([ "perspective", os . path . join (folder, "view_R.view.json") ])
  if completed . returncode != 0 or not completed . stdout . startswith (
      FOLLOWS_ORIGINS_LINE ):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's child, from origin's perspective",
                  r_tree (folder) . split ("\n") [0 : 2],
                  [ "# Showing the perspective of origin (each root's home skgrepo is the default).",
                    "* R ⟨0a C2 ⌂:foreign⟩" ] )
  child = r_tree (folder) . split ("\n") [2]
  if not ( child . startswith ("** M ⟨") and "ĥ" not in child ):
    raise CaseFailure ( f"from origin's, M stands as itself:\n{r_tree (folder)}" )
  completed = run_tool ([ "perspective", os . path . join (folder, "view_R.view.json") ])
  if completed . returncode != 0 or not completed . stdout . startswith (
      FOLLOWS_ROOT_HOMES_LINE ):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's view, following its root's home", r_tree (folder),
                  # A is re-pointed where M was reinstated, so keeps
                  # the viewnode ID that occurrence was given.
                  R_SHOWING_A . replace ("** A ⟨0b ", "** A ⟨0v ") )
  require_equal ( "the sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ],
                  "root homes" )

def check_the_root_too (context):
  """A view of M, which U, in "main", forks, shows U as its root from
  main's perspective, standing for M ('ĥ' after its 'K'), and M again
  from the root homes."""
  folder = context [ "own_folder" ]
  require_tool_run ([ "view", "M", "--out-dir", folder ])
  root = lambda: read_compact (folder, "view_M") . split ("\n* ") [1] . split ("\n") [0]
  fresh = root ()
  require_equal ( "M's fresh root", ( fresh . startswith ("M ⟨0a "), "ĥ" in fresh ), ( True, False ) )
  require_tool_run ([ "perspective", os . path . join (folder, "view_M.view.json"),
                      "skgrepo main" ])
  switched = root ()
  require_equal ( "M's root from main's perspective",
                  ( switched . startswith ("U ⟨0a K"), "ĥ" in switched ), ( True, True ) )
  require_tool_run ([ "perspective", os . path . join (folder, "view_M.view.json"),
                      "root homes" ])
  require_equal ( "M's root from the root homes again", root (), fresh )

def check_switching_perspective_through_a_daemon (context):
  """Through a daemon, which holds the view, the switch needs no
  registering save, and is logged. The daemon's rewrite of the view,
  when another view's save retitles U, keeps the choice; so does
  stopping the daemon, after which a save on the command's own
  connection names it, and the switch back registers it again."""
  folder = context [ "daemon_folder" ]
  start_daemon (context)
  require_routed_run ( context, [ "view", "R" ] )
  sidecar_path = os . path . join (folder, "view_R.view.json")
  completed = run_tool ( [ "perspective", sidecar_path ], port = None )
  if completed . returncode != 0 or not completed . stdout . startswith (FOLLOWS_MAINS_LINE):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's view, following main's", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U )
  require_equal ( "the held sidecar", { key : read_sidecar (folder, "view_R") [key]
                                        for key in ( "perspective", "closed" ) },
                  { "perspective" : "skgrepo main", "closed" : False } )
  require_log_line ( context, "perspective of view_R: the perspective of main" )
  require_routed_run ( context, [ "view", "U" ] )
  edit_compact ( folder, [ ( "* U ⟨0a ", "* U, retitled ⟨0a " ) ], "view_U" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_U.view.json"), "--yes" ],
                         port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_log_line ( context, "re-rendered view_R because of 'save view_U.view.json --yes'" )
  require_equal ( "R's view, rewritten by the daemon", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U . replace ("** U ", "** U, retitled ") )
  require_equal ( "the rewritten sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ], "skgrepo main" )
  require_tool_run ([ "daemon", "--stop", "--out-dir", folder ])
  context [ "daemon" ] . wait (timeout = 30)
  require_equal ( "the released sidecar", { key : read_sidecar (folder, "view_R") [key]
                                            for key in ( "perspective", "closed" ) },
                  { "perspective" : "skgrepo main", "closed" : True } )
  edit_compact ( folder, [ ( "\n** U, retitled ⟨", "\n** U, retitled again ⟨" ) ], "view_R" )
  require_save_succeeds (sidecar_path)
  require_equal ( "R's view, saved after the daemon stopped", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U . replace ("** U ", "** U, retitled again ") )
  require_tool_run ([ "perspective", sidecar_path, "root homes" ])
  require_equal ( "R's view, following its root's home", r_tree (folder),
                  R_SHOWING_A . replace (
                    "U ⟨K1", "U, retitled again ⟨K1" ) )

def check_viewing_a_root_a_perspective_put_in_place (context):
  """Through a daemon, a view of M switched to main's perspective
  has U as its root, so the server answers a view of U on that
  connection by naming M's view, as it lands a client there: 'view U'
  prints the files of M's view and says why. Switched back, 'view U'
  draws U by itself."""
  folder = context [ "daemon_folder" ]
  start_daemon (context)
  require_routed_run ( context, [ "view", "M" ] )
  sidecar_path = os . path . join (folder, "view_M.view.json")
  for perspective in ( "skgrepo main", "root homes" ):
    completed = run_tool ( [ "perspective", sidecar_path, perspective ],
                           port = None )
    if completed . returncode != 0:
      raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
    viewed = require_routed_run ( context, [ "view", "U" ] )
    if perspective == "skgrepo main":
      require_equal ( "what 'view U' printed, U being the root of M's view",
                      viewed . stdout . split (),
                      [ os . path . join ( os . path . realpath (folder), "view_M" + suffix )
                        for suffix in ( ".full.org", ".compact.org", ".view.json" ) ] )
      if "U is already a root of the view content-view:M" not in viewed . stderr:
        raise CaseFailure ( f"'view U' said {viewed . stderr!r}" )
    else:
      require_equal ( "what 'view U' wrote, M's view showing M again",
                      [ os . path . basename (path) for path in viewed . stdout . split () ],
                      [ "view_U.full.org", "view_U.compact.org", "view_U.view.json" ] )
  require_tool_run ([ "daemon", "--stop", "--out-dir", folder ])
  context [ "daemon" ] . wait (timeout = 30)

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
