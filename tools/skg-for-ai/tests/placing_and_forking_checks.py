"""The checks of 'test_skg_for_ai.py' on the third scratch server:
placing existing nodes with '⟨node:ID⟩', 'editable', forks by an
edit and by 'fork', 'stripAttribution', and a forks folder."""

import os
import re

from harness import (
  NOT_SAVED, REPORT_HEAD, ROOT_BASE_NAME, CaseFailure,
  compact_line_starting, edit_compact, fresh_root_view,
  id_of_compact_headline, read_compact, read_file, require_equal,
  require_in_compact, require_output, require_tool_run, run_save,
  saved_line, skg_files, skg_list, skg_title, written_paths )

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

def fred_files (context):
  """{FILE NAME: TEXT} of the foreign skgrepo."""
  return { name : read_file ( os . path . join ( context [ "fred_repo" ], name ))
           for name in sorted ( os . listdir ( context [ "fred_repo" ] ))
           if name . endswith (".skg") }
