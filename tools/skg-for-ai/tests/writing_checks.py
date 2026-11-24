"""The checks of 'test_skg_for_ai.py' on the second scratch server:
'save' of edited compact views (titles, bodies, new children, moves,
removals, deletions, bad handles, stale views, 'new', retyped
headlines)."""

import os
import re
import shutil

from harness import (
  DANGLING_LINK_WARNING, NOT_SAVED, REPORT_HEAD, ROOT_BASE_NAME,
  CaseFailure, compact_line_starting, edit_compact, fresh_root_view,
  id_of_compact_headline, read_compact, read_file, require_equal,
  require_in_compact, require_output, require_save_succeeds,
  require_tool_run, run_save, run_tool, saved_line, skg_body, skg_files,
  skg_lines, skg_list, skg_path, skg_title, written_paths )

# ============================================================
# Writing: each case views the root afresh, edits the compact view,
# saves it, and checks the report and the graph. The cases run in
# order on one server, each on the graph the earlier ones left.
# ============================================================

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
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2a•C⟩\n",
                             "*** Contained twice ⟨0e 2a•C⟩\n"
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
    ( "*** Contained twice ⟨0e 2a•C⟩\n", "" ),
    ( "***** An aside of Fred's ⟨0n ☮ c•H ⌂:fred folded⟩\n",
      "***** An aside of Fred's ⟨0n ☮ c•H ⌂:fred folded⟩\n"
      "*** Contained twice ⟨0e 2a•C⟩\n" ) ])
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
  edit_compact ( folder, [ ( "*** Contained twice ⟨0n 2a•C⟩\n", "" ) ])
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
    "* Contained twice ⟨0a 1•C ⌂:main⟩\n"
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
  edit_compact ( folder, [ ( "⟨0a 1•C2 L2 ⌂:main⟩",
                             "⟨0a 1•C2 L2 ⌂:main both delete restoreNode⟩" ),
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
