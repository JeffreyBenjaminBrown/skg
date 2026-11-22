#!/usr/bin/env python3
"""PURPOSE: test 'skg_for_ai.py'.

Run from anywhere: 'python3 tools/skg-for-ai/test_skg_for_ai.py'.
It needs a built server, 'target/debug/skg' ('cargo build --bin skg'),
or the binary SKG_BINARY names.

Its checks are in 'tests/', a module per group, which 'main' runs in
this order:
- 'no_server_checks': the checks that need no server.
- 'reading_checks': the view commands, on the first scratch server.
- 'writing_checks': 'save', on the second.
- 'placing_and_forking_checks': '⟨node:ID⟩', 'editable' and forks,
  on the third.
- 'more_words_checks': the words of a handle, on the fourth.
- 'reviewing_checks': 'diff', 'extend' and 'commit', on the fifth.
- 'daemon_checks': 'daemon', on the sixth.
- 'two_owned_skgrepos_checks': the restriction, 'private', moves
  between skgrepos, on the seventh.
- 'perspective_checks': 'perspective', on the eighth.
- 'receipts_checks': 'receipt', on the ninth.
'tests/harness.py' holds what they share: the scratch servers,
'run_tool', the 'require_*' checks, starting a daemon.

In order, it:
- checks that the tool runs under 'python3 -I', from another folder;
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
  there on port SKG_FOR_AI_TEST_PORT (default 1793, or in a linked
  git worktree a port derived from its path), and runs each
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
  the daemon's stop keep the choice;
- on a ninth scratch server, over mail, writes read receipts with
  'receipt'.
'--update-expected' rewrites the expected files instead of comparing.
Given the names of groups ('daemon_checks reviewing_checks'), it runs
only those, in the order above.
"""

import os
import shutil
import sys
import tempfile

TOOL_FOLDER = os . path . dirname ( os . path . abspath (__file__) )
sys . dont_write_bytecode = True
sys . path . insert (0, TOOL_FOLDER)
sys . path . insert ( 0, os . path . join (TOOL_FOLDER, "tests") )

from harness import (
  SERVER_BINARY, failures_on_a_scratch_server )
from daemon_checks import failures_of_daemon
from more_words_checks import failures_of_more_words
from no_server_checks import (
  failures_of_chains, failures_of_commit_result_text,
  failures_of_describe_requests, failures_of_forks_folder_claims,
  failures_of_isolated_run, failures_of_link_annotation_round_trip,
  failures_of_link_annotations, failures_of_no_default_port,
  failures_of_perspective_names, failures_of_preview_support,
  failures_of_save_envelope, failures_of_shared_herald_cases,
  failures_of_title_matching )
from perspective_checks import (
  copy_perspective_fixture, failures_of_perspective )
from placing_and_forking_checks import failures_of_placing_and_forking
from reading_checks import failures_of_reading
from receipts_checks import (
  failures_of_receipts, write_receipt_fixture )
from reviewing_checks import failures_of_reviewing
from two_owned_skgrepos_checks import failures_of_two_owned_skgrepos
from writing_checks import failures_of_writing

def main (argv):
  update_expected = "--update-expected" in argv
  groups = [ argument for argument in argv if argument != "--update-expected" ]
  unknown = [ group for group in groups if group not in GROUP_NAMES ]
  if unknown:
    print ( "Unknown group(s): " + ", " . join (unknown) + ". The groups: "
            + ", " . join (GROUP_NAMES) + ".", file = sys . stderr )
    sys . exit (2)
  # The tool's commands find a daemon by a socket in XDG_RUNTIME_DIR,
  # and take a port from SKG_PORT: the test's own folder, and no
  # SKG_PORT, keep its commands and daemons apart from any others.
  os . environ [ "XDG_RUNTIME_DIR" ] = tempfile . mkdtemp (
    prefix = "skg-for-ai-test-runtime-" )
  os . environ . pop ( "SKG_PORT", None )
  failures = []
  if not groups or "no_server_checks" in groups:
    failures += failures_of_checks_needing_no_server ()
  failures += failures_against_scratch_servers (
    update_expected,
    [ group for group in SCRATCH_SERVER_GROUPS
      if not groups or group [0] in groups ] )
  shutil . rmtree ( os . environ [ "XDG_RUNTIME_DIR" ], ignore_errors = True )
  for failure in failures:
    print ( "FAIL: " + failure )
  if failures:
    sys . exit (1)
  print ( "PASS" + ( " (expected files rewritten)" if update_expected else "" ))

def failures_of_checks_needing_no_server ():
  return ( failures_of_isolated_run ()
           + failures_of_title_matching ()
           + failures_of_save_envelope ()
           + failures_of_commit_result_text ()
           + failures_of_preview_support ()
           + failures_of_no_default_port ()
           + failures_of_link_annotation_round_trip ()
           + failures_of_link_annotations ()
           + failures_of_shared_herald_cases ()
           + failures_of_forks_folder_claims ()
           + failures_of_chains ()
           + failures_of_describe_requests ()
           + failures_of_perspective_names () )

# ============================================================
# The scratch server
# ============================================================

# Each group that needs a scratch server: its module's name, and its
# failures given UPDATE_EXPECTED. The reading checks on one scratch
# server, then the writing checks on another, then the placing and
# forking checks on a third, then the other words' on a fourth, then
# the reviewing checks on a fifth, then the daemon's on a sixth, then
# those of two owned skgrepos on a seventh, those three with their
# owned skgrepos gitrepos; each on its own copy of the fixture. Then
# the checks of a view's perspective, on an eighth, over a copy of
# the perspective integration test's data. Last, the receipts', on a
# ninth, over mail ('RECEIPT_FIXTURE_FILES').
SCRATCH_SERVER_GROUPS = [
  ( "reading_checks",
    lambda update_expected : failures_on_a_scratch_server (
      lambda scratch, data_root :
        failures_of_reading (scratch, data_root, update_expected) )),
  ( "writing_checks",
    lambda update_expected : failures_on_a_scratch_server (failures_of_writing) ),
  ( "placing_and_forking_checks",
    lambda update_expected :
      failures_on_a_scratch_server (failures_of_placing_and_forking) ),
  ( "more_words_checks",
    lambda update_expected : failures_on_a_scratch_server (failures_of_more_words) ),
  ( "reviewing_checks",
    lambda update_expected : failures_on_a_scratch_server (
      failures_of_reviewing, owned_skgrepo_in_git = True )),
  ( "daemon_checks",
    lambda update_expected : failures_on_a_scratch_server (
      failures_of_daemon, owned_skgrepo_in_git = True )),
  ( "two_owned_skgrepos_checks",
    lambda update_expected : failures_on_a_scratch_server (
      failures_of_two_owned_skgrepos, owned_skgrepo_in_git = True,
      with_private_skgrepo = True )),
  ( "perspective_checks",
    lambda update_expected : failures_on_a_scratch_server (
      failures_of_perspective, make_data_root = copy_perspective_fixture )),
  ( "receipts_checks",
    lambda update_expected : failures_on_a_scratch_server (
      failures_of_receipts, make_data_root = write_receipt_fixture )) ]
GROUP_NAMES = ( [ "no_server_checks" ]
                + [ name for name, _ in SCRATCH_SERVER_GROUPS ] )

def failures_against_scratch_servers (update_expected, groups):
  """The failures of GROUPS, each (NAME, FAILURES) of
  'SCRATCH_SERVER_GROUPS', in order."""
  if groups and not os . path . exists (SERVER_BINARY):
    return [ f"no server binary at {SERVER_BINARY}; run 'cargo build --bin skg'" ]
  return [ failure
           for _, failures_of_group in groups
           for failure in failures_of_group (update_expected) ]

if __name__ == "__main__":
  main ( sys . argv [ 1 : ] )
