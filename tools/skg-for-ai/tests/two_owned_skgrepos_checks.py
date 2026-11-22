"""The checks of 'test_skg_for_ai.py' on the seventh scratch server,
with a second owned skgrepo 'private': the restriction and
overPrivateText, deleting under it, hiding the foreign skgrepos,
'--hoist', 'relRepo:NAME', moving between skgrepos ('repo:NAME'),
'delete-references'."""

import os

from harness import (
  DIFF_BASE_NAME, HANDLE_PATTERN, CaseFailure, compact_line_starting,
  daemon_log, diff_report_head, diff_written_paths, edit_compact,
  fresh_root_view, hit_titles, id_of_compact_headline, read_file,
  require_equal, require_failure, require_in_diff_compact, require_in_file,
  require_log_line, require_output, require_save_succeeds,
  require_tool_run, run_tool, saved_line, sidecar_of_diff, skg_body,
  skg_list, skg_path, skg_title, start_daemon )

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
