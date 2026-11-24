"""The checks of 'test_skg_for_ai.py' on the fifth scratch server, a
gitrepo: 'diff', processed marks, 'commit', restoring a deleted node,
'extend'."""

import os

from harness import (
  DIFF_BASE_NAME, DIFF_VIEW_ID_PATTERN, NOT_SAVED, UUID_PATTERN,
  CaseFailure, diff_report_head, diff_written_paths, edit_compact,
  fresh_root_view, git, read_diff_compact, read_file, require_equal,
  require_in_diff_compact, require_output, require_save_succeeds,
  require_tool_run, run_tool, saved_line, sidecar_of_diff, skg_files,
  skg_list, skg_path, skg_title )

# ============================================================
# Reviewing: on a fixture whose owned skgrepo is a gitrepo, edits are
# saved, the diff view shows them, one is marked processed, a deleted node
# is restored, and everything is committed. The cases run in order on
# one server.
# ============================================================

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
    ( "*** Contained twice ⟨0e 2a•C⟩\n",
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
    "**** Contained twice ⟨0f ☮ 2a•C⟩\n"
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

def diff_full_line (folder, line_number):
  """Line LINE_NUMBER (1-based) of the diff view's full buffer."""
  return read_file ( os . path . join ( folder, DIFF_BASE_NAME + ".full.org" )
                   ) . split ("\n") [ line_number - 1 ]

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
