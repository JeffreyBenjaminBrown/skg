"""The checks of 'test_skg_for_ai.py' on the fourth scratch server: the
words of a handle ('detach', 'attach', 'write-protect',
'searchMatching', 'merge:ID', 'delete subtree') and words refused
where they stand."""

import os

from harness import (
  HANDLE_PATTERN, REPORT_HEAD, ROOT_BASE_NAME, CaseFailure,
  compact_line_starting, edit_compact, fresh_root_view,
  id_of_compact_headline, read_compact, require_equal, require_in_compact,
  require_output, require_save_succeeds, require_tool_run, run_save,
  saved_line, skg_files, skg_list, skg_path, written_paths )

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
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2a•C⟩",
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
                               "*** Contained twice ⟨0e ⊥ 1•C⟩\n" )
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e ⊥ 1•C⟩",
                             "*** Contained twice ⟨0e ⊥ 1C attach⟩" ) ])
  require_output (
    run_save (folder, "--yes"),
    REPORT_HEAD +
    "Attach: ⟨0e⟩ 'Contained twice', to ⟨0d⟩ 'First child': the save puts it "
    "in its parent's list.\n" + saved_line (0) + written_paths (folder) )
  require_equal ( "the parent's content after 'attach'",
                  skg_list (context, "child-a-id", "contains"), [ "shared-id" ] )
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2a•C⟩",
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
                               "*** Contained twice ⟨0e 2a•C⟩\n" )
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
                               "*** Contained twice ⟨0e ☮ 2a•C⟩\n" )
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
  edit_compact ( folder, [ ( "*** Contained twice ⟨0e 2a•C⟩\n",
                             "*** Contained twice ⟨0e 2a•C⟩\n"
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
