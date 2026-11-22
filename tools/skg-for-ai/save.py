"""'save': 'command_save' and what it does with an edited outline.
It rebuilds the full buffer, reports the changes it would make,
places existing nodes, predicts forks, stops on retyped headlines,
answers the server's questions and writes the view the server
rerenders. Two modules beside it hold parts of that:
'retyped_headlines' (a new headline whose title is, or resembles, a
removed one's) and 'save_dialogue' (the save sent, the server's
questions answered, its rerender written). Also what other commands
need of a save:
'register_view_by_saving_it_unchanged', 'compact_view_has_edits',
'refuse_while_held_views_have_edits'."""

import os
import re

from sexp import Symbol, render_sexp, top_level_elements_with_offsets
from server_connection import (
  SkgConnection, SkgToolError, connect_to_server, response_field,
  response_type )
from full_view import (
  METADATA_PARTS, headline_gnid, headline_kind, headline_node_repo,
  metadata_has_atom )
from compact_view import fetch_link_statuses_of_ids, fetch_repo_ownership
from views import (
  SavedView, load_saved_view, metadata_text_with_part,
  perspective_of_sidecar, require_port_of_sidecar )
from edited_compact_view import (
  MERGE_WORD_PREFIX, NEW_NODE_HANDLE_WORD, RELREPO_WORD_PREFIX,
  REPO_WORD_PREFIX, VIEW_REQUEST_TOKENS, WRITE_PROTECT_SUBTREE_REQUEST,
  compact_handle_text, edit_request_part_of_word, edited_outline,
  handle_bracket_title_error_lines, handle_error_lines,
  outline_of_full_view, word_parameter )
from outline_words import (
  OutlineWordCheck, descendant_indices, described_handle, edit_request_text,
  outline_word_check, processed_request_text, unmarked_leaf_count )
from retyped_headlines import (
  give_new_headlines_their_removed_handles, retyped_headline_error_lines,
  reworded_headline_pairs, reworded_question_lines )
from save_dialogue import (
  print_save_preview, send_save_and_write_its_rerender, server_supports )

# What a client asks before a processed gesture that unmarks more
# than one leaf ('skg--processed-wide-unmark-question' in
# 'elisp/skg-processed.el').
WIDE_UNMARK_QUESTION = (
  "Unmarking this node processed will unmark every contributing "
  "decision. You might discard a lot of hard-won information. Continue?" )

def command_save (arguments):
  """Rebuild the full buffer from the edited compact view and print the
  changes it makes. With '--yes', and when the report finds no bad
  handle, no retyped headline and no unanswered question whether a new
  headline rewords a removed one (see "Retyped headlines" in the
  README), send the save (naming the buffer of the diff view, for
  one), print the server's answer and write its re-rendered view over
  the three files. The save carries the full view as its baseline,
  so the server refuses it if a node it would write changed since
  the view was drawn. A refused save leaves the files as they were.
  Without '--yes', the report ends with what the server says the save
  would hide and stop hiding, which it infers and no edit shows: the
  save is sent as a preview, which writes nothing ('print_save_preview').
  A placed node ('⟨node:ID⟩') takes its title from the server. When
  the save forks, the report says so; the server then answers with a
  fork confirmation, which the save prints and approves, as the
  clients' approval does, if it forks only what the report said. The
  server's other questions are printed, and answered as the command
  line says ('answer_save_question'): each answer sends the same save
  again, carrying it and the earlier ones. A save whose rerender the
  server withholds, as it does under a skgrepo restriction when that
  would show text from overPrivateText telescopes, has been written:
  '--show-overPrivateText' releases the rerender by asking for it, as
  the clients do ('release_withheld_save_rerender'), and never sends
  the save again."""
  saved_view = load_saved_view (arguments . view_files)
  require_port_of_sidecar (arguments . port, saved_view . sidecar)
  if NEW_NODE_HANDLE_WORD in saved_view . headline_of_handle:
    raise SkgToolError (
      f"The full view has a headline whose viewnode ID is "
      f"'{NEW_NODE_HANDLE_WORD}', which the compact view reads as the mark "
      "of a new node; view a smaller part of the graph." )
  edited = edited_outline (saved_view)
  original = outline_of_full_view (saved_view . parsed_view)
  report_head = (
    "Changes in " + os . path . basename (saved_view . compact_path) +
    ", against " + os . path . basename (saved_view . full_path) +
    " (view ID " + saved_view . sidecar [ "view_id" ] + "):" )
  title_errors = handle_bracket_title_error_lines (
    edited, saved_view . headline_of_handle )
  if title_errors:
    # Refused before connecting: such a title may already have been
    # misread, so nothing else in the report could be trusted.
    print (report_head)
    for line in title_errors:
      print (line)
    raise SkgToolError (
      "Nothing was saved: some titles hold a handle's bracket; see above." )
  connection = connection_for_saved_view (saved_view)
  try:
    report = save_report (connection, saved_view, edited, original, arguments)
    print (report_head)
    for line in report . error_lines + (
        report . change_lines + report . note_lines if report . change_lines
        else [] if report . error_lines
        else report . note_lines + [ "No changes." ] ):
      print (line)
    if report . error_lines:
      raise SkgToolError (
        "Nothing was saved: some handles are in error; see above." )
    if not report . change_lines:
      print ( "Nothing to save." )
      return
    if report . question_lines:
      print ( "Not saved. Answer the questions with '--keep-ids' or "
              "'--all-new', or on each new headline (its removed handle, or "
              "'⟨new⟩'), and run again with '--yes' to save." )
      if arguments . yes:
        raise SkgToolError (
          "Nothing was saved: the report asks whether new headlines reword "
          "removed ones; '--yes' does not answer that." )
      return
    buffer_text = rebuilt_full_buffer (
      edited, saved_view . headline_of_handle, saved_view . full_text )
    if not arguments . yes:
      if server_supports (connection, "save preview"):
        print_save_preview ( connection . save_buffer (
          saved_view . sidecar [ "view_id" ], buffer_text,
          saved_view . full_text,
          saved_view . sidecar [ "request" ] . get ("buffer"), None,
          perspective_of_sidecar (saved_view . sidecar),
          preview = True ) [-1] )
      else:
        print ( "The server is too old to say what the save would hide; "
                "the save's answer will say what it hid." )
      print ( "Not saved. Run again with '--yes' to save." )
      return
    written_paths = send_save_and_write_its_rerender (
      connection, saved_view, buffer_text, arguments, report . forks )
  finally:
    connection . close ()
  for path in written_paths:
    print (path)

class SaveReport:
  """What 'save_report' finds in an edited compact view. 'error_lines'
  stop the save. 'change_lines' are the changes, then the forks
  ('forks', a 'PredictedForks') and the questions; none when there are
  errors. 'question_lines' ask whether new headlines reword removed
  ones, and stop the save until answered. 'note_lines' say what the
  handles' words leave out."""
  def __init__ (self, error_lines, change_lines, question_lines,
                note_lines, forks):
    self . error_lines = error_lines
    self . change_lines = change_lines
    self . question_lines = question_lines
    self . note_lines = note_lines
    self . forks = forks

def save_report (connection, saved_view, edited, original, arguments):
  """Read EDITED, the edited compact view's outline, against ORIGINAL,
  the full view's: its errors, its changes, the forks it would make and
  the questions it raises, as a 'SaveReport'. It completes EDITED as
  the save needs it: a placed node's title and request, and with
  '--keep-ids' the removed handles of new headlines that reword
  removed ones. A placed node and a merge's acquiree are looked up
  on CONNECTION."""
  ownership = fetch_repo_ownership (connection)
  errors = handle_error_lines (edited, saved_view . headline_of_handle)
  word_check = ( OutlineWordCheck () if errors else outline_word_check (
    edited, saved_view . headline_of_handle, ownership ))
  errors = ( errors or word_check . error_lines
             or resolve_placed_nodes (connection, edited)
             or resolve_merge_acquirees (connection, edited)
             or retyped_headline_error_lines (
                  edited, original, saved_view . headline_of_handle ))
  assign_placed_view_requests (edited, saved_view . headline_of_handle)
  reworded_pairs = [] if errors else reworded_headline_pairs (edited, original)
  if arguments . keep_ids:
    give_new_headlines_their_removed_handles (edited, reworded_pairs)
  questions = (
    [] if arguments . keep_ids or arguments . all_new
    else reworded_question_lines (
           edited, reworded_pairs, saved_view . headline_of_handle ))
  changes = [] if errors else change_report_lines (
    edited, original, saved_view . headline_of_handle )
  forks = PredictedForks () if errors else predicted_forks (
    edited, original, saved_view . headline_of_handle, ownership,
    arguments . fork_into, bool (changes) )
  errors = errors + forks . error_lines
  changes = [] if errors else changes + forks . lines + questions
  return SaveReport ( error_lines = errors, change_lines = changes,
                      question_lines = questions,
                      note_lines = word_check . note_lines, forks = forks )

def register_view_by_saving_it_unchanged (connection, saved_view, refusal):
  """Save SAVED_VIEW's full view back unchanged, as the view it is (a
  buffer of the diff view, one drawn from a skgrepo's perspective), which
  writes nothing and leaves CONNECTION's server holding the view: a
  request that works on a held view can then follow. Return the save's
  answer; raise REFUSAL, with the server's errors, when the server
  refuses it, as it does when a node the view shows changed since it
  was drawn."""
  sidecar = saved_view . sidecar
  terminal = connection . save_buffer (
    sidecar [ "view_id" ], saved_view . full_text, saved_view . full_text,
    sidecar [ "request" ] . get ("buffer"),
    perspective = perspective_of_sidecar (sidecar) ) [-1]
  if ( response_type (terminal) != "save-result"
       or not isinstance ( response_field (terminal, "content"), str )
       or isinstance ( response_field (terminal, "content"), Symbol )):
    raise SkgToolError (
      refusal + " The server said: " + " " . join (
        response_field (terminal, "errors") or [ render_sexp (terminal) ] ))
  return terminal

def compact_view_has_edits (saved_view):
  """Whether the compact view differs from its full view in anything a
  save would send: a bad handle, a change, a request word."""
  edited = edited_outline (saved_view)
  return bool (
    handle_error_lines (edited, saved_view . headline_of_handle)
    or change_report_lines (
         edited, outline_of_full_view (saved_view . parsed_view),
         saved_view . headline_of_handle ))

def current_buffer_of_edited_view (saved_view):
  """The buffer a save of SAVED_VIEW's edited compact view would send,
  as far as the nodes it names go: what a client's dirty buffer holds.
  A placed node keeps its typed title; the words of 'subtree' are not
  given below. A compact view whose handles are in error has no such
  buffer, and is refused, naming it."""
  edited = edited_outline (saved_view)
  if handle_error_lines (edited, saved_view . headline_of_handle):
    raise SkgToolError (
      f"The held view {saved_view . compact_path} has edits in error, so the "
      "save cannot tell the server what it would change; fix them (its "
      "'save' says how), or 'close' it." )
  for headline in edited:
    if headline . is_placed ():
      headline . placed_pid = headline . placed_node_id
      headline . placed_title = headline . title
  return rebuilt_full_buffer (
    edited, saved_view . headline_of_handle, saved_view . full_text )

def connection_for_saved_view (saved_view):
  """The connection on which to save or extend SAVED_VIEW: the
  daemon's, when one runs this command and talks to the sidecar's
  server, unless it holds the sidecar's view ID for other files (a
  later view under the same view ID), in which case a connection of
  the command's own, as without a daemon."""
  port = saved_view . sidecar [ "port" ]
  connection = connect_to_server (port)
  if connection . holds_view_for_other_files (
      saved_view . sidecar [ "view_id" ], saved_view . directory,
      saved_view . base_name ):
    return SkgConnection (port)
  return connection

def refuse_while_held_views_have_edits (connection, what):
  """Refuse WHAT while a view CONNECTION holds has unsaved edits, as
  the clients refuse a request whose rerender would replace an unsaved
  buffer. A command's own connection holds none."""
  for view_id, files_stem in connection . held_views ():
    try:
      saved_view = SavedView ( files_stem + ".view.json" )
    except ( OSError, ValueError ):
      continue
    if compact_view_has_edits (saved_view):
      raise SkgToolError (
        f"Nothing was sent: {saved_view . compact_path} has unsaved edits, "
        f"which {what} would rerender away; save them, or undo them, first." )

# ------------------------------------------------------------
# Rebuilding the full buffer
# ------------------------------------------------------------

def rebuilt_full_buffer (edited, headline_of_handle, full_text):
  """Each headline at its edited level and title, with the metadata of
  the full-view headline its handle names, plus the '(editRequest ...)'
  its handle's tokens ask for; a headline without a handle bare, which
  the save reads as a new node; bodies as edited."""
  lines = []
  for headline in edited:
    lines . append ( rebuilt_headline_line (headline, headline_of_handle) )
    lines += headline . body
  text = "\n" . join (lines)
  return text + "\n" if full_text . endswith ("\n") else text

def rebuilt_headline_line (headline, headline_of_handle):
  stars = "*" * headline . level
  if headline . is_placed ():
    return stars + " " + placed_node_metadata_text (headline) + " " + (
      headline . placed_title )
  metadata_text = (
    None if headline . handle is None
    else headline_of_handle [ headline . handle ] . metadata_text )
  if metadata_text is None and headline . repo_words ():
    # a new headline given its home skgrepo, as 'skg-set-repo' gives
    # a headline without metadata ('skg--populate-minimal-node-metadata')
    metadata_text = "(skg node (skgRepos (nodeRepo " + word_parameter (
      headline . repo_words () [0] ) + ")))"
    return stars + " " + metadata_text + " " + headline . title
  if metadata_text is None:
    return stars + " " + headline . title
  if headline . repo_words ():
    metadata_text = metadata_text_with_nodeRepo (
      metadata_text, word_parameter ( headline . repo_words () [0] ))
  if "detach" in headline . claim_words ():
    metadata_text = metadata_text_with_part (
      metadata_text, "noClaim", metadata_parts_following ("noClaim") )
  if "attach" in headline . claim_words ():
    metadata_text = metadata_text_without_atom (metadata_text, "noClaim")
  view_requests = view_requests_text (headline)
  if view_requests:
    metadata_text = metadata_text_with_part (
      metadata_text, view_requests, metadata_parts_following ("viewRequests") )
  request = edit_request_text (
    headline, headline_of_handle [ headline . handle ] )
  if request:
    metadata_text = metadata_text_with_part (
      metadata_text, request, metadata_parts_following ("editRequest") )
  return ( stars + " " + metadata_text +
           ( " " + headline . title if headline . title else "" ))

def metadata_parts_following (part_name):
  """The names of the parts of '(skg ...)' the server writes after
  PART_NAME ('parts' in 'shared/metadata-fields.json'), a part
  written there going before the first of them that is present."""
  return tuple ( METADATA_PARTS [ METADATA_PARTS . index (part_name) + 1 : ] )

def metadata_text_with_nodeRepo (metadata_text, skgrepo):
  """METADATA_TEXT with its home skgrepo '(nodeRepo ...)' SKGREPO."""
  if re . search ( r'\(nodeRepo [^()\s]+\)', metadata_text ):
    return re . sub ( r'\(nodeRepo [^()\s]+\)', "(nodeRepo " + skgrepo + ")",
                      metadata_text, count = 1 )
  return metadata_text_with_part (
    metadata_text, "(skgRepos (nodeRepo " + skgrepo + "))",
    metadata_parts_following ("skgRepos") )

def metadata_text_without_atom (metadata_text, atom):
  """METADATA_TEXT without the bare atom ATOM at its top level."""
  for element_start, element in top_level_elements_with_offsets (metadata_text):
    if isinstance (element, Symbol) and element == atom:
      return ( metadata_text [ : element_start ] +
               metadata_text [ element_start + len (atom) : ] . lstrip (" ") )
  return metadata_text

def placed_node_metadata_text (headline):
  """The metadata the clients' 'skg-paste-node' writes
  ('skg--insert-node-from-entry' in 'elisp/skg-linkstack.el'): the ID,
  'writeProtected', and an editable-view request unless the buffer
  already holds an editable occurrence of the node."""
  return ( "(skg node (gnid " + headline . placed_pid + ") writeProtected" +
           ( " (viewRequests editableView)"
             if headline . placed_requests_editable else "" ) + ")" )

def view_requests_text (headline):
  """The '(viewRequests ...)' HEADLINE's tokens ask for, or None."""
  requests = [ ( WRITE_PROTECT_SUBTREE_REQUEST
                 if word == "write-protect" and headline . asks_subtree ()
                 else request )
               for word, request in VIEW_REQUEST_TOKENS
               if word in headline . view_request_words () ]
  return "(viewRequests " + " " . join (requests) + ")" if requests else None

# ------------------------------------------------------------
# The report of changes
# ------------------------------------------------------------

def change_report_lines (edited, original, headline_of_handle):
  """One line per change, by kind: deletions, removals, moves,
  reorderings, new headlines, repeated handles, titles, then a count
  of changed bodies."""
  original_index_of_handle = { headline . handle : index
                               for index, headline in enumerate (original) }
  first_edited_index_of_handle = {}
  repeated_indices = []
  for index, headline in enumerate (edited):
    if headline . handle is None:
      continue
    if headline . handle in first_edited_index_of_handle:
      repeated_indices . append (index)
    else:
      first_edited_index_of_handle [ headline . handle ] = index
  def described (handle):
    return described_handle (handle, headline_of_handle)
  def edited_parent_key (index):
    """The parent's handle; ('new', INDEX) for a new parent; None at
    the top level."""
    parent = edited [index] . parent
    if parent is None:
      return None
    return ( edited [parent] . handle if edited [parent] . handle is not None
             else ( "new", parent ))
  def original_parent_key (index):
    parent = original [index] . parent
    return None if parent is None else original [parent] . handle
  def place (key):
    if key is None:
      return "the top level"
    if isinstance (key, tuple):
      return "under the new headline '" + edited [ key [1] ] . title + "'"
    return "under " + described (key)
  def edited_place (index):
    """Where the edited headline at INDEX stands, as the report says it."""
    key = edited_parent_key (index)
    return "at the top level" if key is None else place (key)
  def depth_below_parent (outline, index):
    parent = outline [index] . parent
    return outline [index] . level - (
      0 if parent is None else outline [parent] . level )
  removed_handles = { headline . handle for headline in original
                      if headline . handle not in first_edited_index_of_handle }
  lines = request_report_lines (edited, headline_of_handle, described)
  removal_lines = []
  for index, headline in enumerate (original):
    if ( headline . handle not in removed_handles
         or original_parent_key (index) in removed_handles ):
      continue
    removed_below = sum (
      1 for descendant in descendant_indices (original, index)
      if original [descendant] . handle in removed_handles )
    removal_lines . append (
      "Removed: " + described (headline . handle) + ", from " +
      place ( original_parent_key (index) ) +
      ( f" (and {removed_below} headline"
        f"{'' if removed_below == 1 else 's'} below it)"
        if removed_below else "" ) + "." )
  lines += removal_lines
  for handle, index in first_edited_index_of_handle . items ():
    original_index = original_index_of_handle [handle]
    old_key = original_parent_key (original_index)
    new_key = edited_parent_key (index)
    if new_key != old_key:
      lines . append ( "Moved: " + described (handle) + ", from " +
                       place (old_key) + " to " + place (new_key) + "." )
    elif ( depth_below_parent (edited, index)
           != depth_below_parent (original, original_index) ):
      lines . append (
        "Moved: " + described (handle) +
        f", from level {original [original_index] . level} to level "
        f"{edited [index] . level}, still " + place (new_key) + "." )
  for parent_key in [ None ] + [ headline . handle for headline in original ]:
    if parent_key is not None and parent_key not in first_edited_index_of_handle:
      continue
    old_order = [ original [index] . handle
                  for index in range ( len (original) )
                  if original_parent_key (index) == parent_key
                  and original [index] . handle in first_edited_index_of_handle
                  and edited_parent_key ( first_edited_index_of_handle [
                        original [index] . handle ] ) == parent_key ]
    new_order = [ edited [index] . handle
                  for index in first_edited_index_of_handle . values ()
                  if edited_parent_key (index) == parent_key
                  and original_parent_key ( original_index_of_handle [
                        edited [index] . handle ] ) == parent_key ]
    if old_order != new_order:
      lines . append (
        "Reordered: " + ( "the top-level headlines" if parent_key is None
                          else "the children of " + described (parent_key) )
        + "." )
  for index, headline in enumerate (edited):
    if headline . handle is not None or headline . is_placed ():
      continue
    body_lines = len (headline . body)
    lines . append (
      "New: " + ( "'" + headline . title + "'" if headline . title
                  else "an untitled headline" ) +
      ", " + edited_place (index) +
      ( f" (with a {body_lines}-line body)" if body_lines else "" ) + "." )
  for index, headline in enumerate (edited):
    if not headline . is_placed ():
      continue
    lines . append (
      "Placed: " + compact_handle_text (headline) + " '" +
      headline . report_title () + "', " +
      edited_place (index) +
      ( ", write-protected, since the view already holds an editable "
        "occurrence of it"
        if headline . placed_pid is not None
        and not headline . placed_requests_editable else "" ) + "." )
  for index in repeated_indices:
    lines . append (
      "Repeated: " + described ( edited [index] . handle ) + ", again " +
      edited_place (index) +
      ": a second occurrence of the same node." )
  changed_bodies = []
  for handle, index in first_edited_index_of_handle . items ():
    old = original [ original_index_of_handle [handle] ]
    if edited [index] . title != old . title:
      lines . append ( f"Title: ⟨{handle}⟩ '{old . title}' → "
                       f"'{edited [index] . title}'." )
    if edited [index] . body != old . body:
      changed_bodies . append (handle)
  if changed_bodies:
    lines . append (
      f"Bodies changed: {len (changed_bodies)} (" +
      ", " . join ( f"⟨{handle}⟩" for handle in changed_bodies ) + ")." )
  return lines

def request_report_lines (edited, headline_of_handle, described):
  """The requests the handles' words ask for, by kind, each naming its
  headline through DESCRIBED; then, when the 'unprocessed' requests unmark
  more than one leaf, the question a client asks before such a
  gesture."""
  handled_headlines = [ headline for headline in edited
                        if headline . handle is not None ]
  def parent_text (headline):
    parent = headline . parent
    if parent is None:
      return "the top level"
    if edited [parent] . handle is not None:
      return described ( edited [parent] . handle )
    if edited [parent] . is_placed ():
      return compact_handle_text ( edited [parent] )
    return "the new headline '" + edited [parent] . title + "'"
  def with_word (word):
    return [ headline for headline in handled_headlines
             if word in headline . words () ]
  def with_word_prefix (prefix):
    return [ ( headline, word ) for headline in handled_headlines
             for word in headline . words () if word . startswith (prefix) ]
  lines = []
  for word, label, suffix in [
      ( "delete", "Delete", ", from the graph" ),
      ( "restoreNode", "Restore node", ", as HEAD has it" ),
      ( "restoreRel", "Restore membership", ", as HEAD has it" ),
      ( "stripAttribution", "Strip attribution",
        ": it no longer records the note it was forked from" ) ]:
    lines += [ label + ": " + described (headline . handle) + suffix + "."
               for headline in with_word (word) ]
  for headline, word in with_word_prefix (MERGE_WORD_PREFIX):
    acquiree = word_parameter (word)
    lines . append (
      "Merge: " + described (headline . handle) + " acquires ⟨node:" + acquiree
      + "⟩" + ( " '" + headline . merge_title + "'"
                if headline . merge_title is not None else "" )
      + ", as '(merge " + acquiree + ")': a nodeMerge makes the two one node, "
      "this one, with the relationships of both." )
  for headline, word in with_word_prefix (RELREPO_WORD_PREFIX):
    lines . append (
      "relRepo: " + described (headline . handle) + ", as '(relRepo " +
      word_parameter (word) + ")': its relationship to " +
      parent_text (headline) + " is recorded in '" + word_parameter (word) + "'." )
  for word, effect in [ ( "noSearchMatching", "no longer matches searches" ),
                        ( "searchMatching", "matches searches again" ) ]:
    lines += [ "Search matching: " + described (headline . handle) + ", as '" +
               edit_request_part_of_word (word) + "': the node " + effect + "."
               for headline in with_word (word) ]
  for headline, word in with_word_prefix (REPO_WORD_PREFIX):
    lines . append (
      "Repo: " + described (headline . handle) + ", from '" +
      str ( headline_node_repo (
        headline_of_handle [ headline . handle ] . metadata )) +
      "' to '" + word_parameter (word) + "': the node's home skgrepo moves." )
  lines += [ "Editable: " + described (headline . handle) +
             ", as '(viewRequests editableView)': the save makes it this "
             "view's editable occurrence of its node."
             for headline in with_word ("editable") ]
  lines += [ "Write-protect: " + described (headline . handle) +
             ( ", as '(viewRequests writeProtectedSubtreeView)': after saving "
               "the edits, the save makes it and every occurrence of its content "
               "subtree write-protected."
               if headline . asks_subtree () else
               ", as '(viewRequests writeProtectedView)': after saving its "
               "edits, the save makes it write-protected." )
             for headline in with_word ("write-protect") ]
  lines += [ "Detach: " + described (headline . handle) + ", from " +
             parent_text (headline) + ", as 'noClaim': the save takes it out "
             "of its parent's list, and the view still shows it."
             for headline in with_word ("detach") ]
  lines += [ "Attach: " + described (headline . handle) + ", to " +
             parent_text (headline) + ": the save puts it in its parent's list."
             for headline in with_word ("attach") ]
  for word, label in [ ( "processed", "Processed" ), ( "unprocessed", "Unprocessed" ) ]:
    for headline in handled_headlines:
      if headline . processed_words () != [ word ]:
        continue
      text = processed_request_text (
        headline, headline_of_handle [ headline . handle ] )
      lines . append ( label + ": " + described (headline . handle) +
                       ( ", as '" + text + "'." if text else
                         ", naming nothing on the headline itself." ))
  unmarked = sum ( unmarked_leaf_count (
                     headline, headline_of_handle [ headline . handle ] )
                   for headline in handled_headlines )
  if unmarked > 1:
    lines . append (
      f"Question: this unmarks {unmarked} changes. " + WIDE_UNMARK_QUESTION +
      " Saving with '--yes' answers yes." )
  return lines

# ------------------------------------------------------------
# Placing an existing node: a headline ending in '⟨node:ID⟩'
# ------------------------------------------------------------

def resolve_placed_nodes (connection, edited):
  """Give each placed node of the edited outline the ID and the title
  the server has for it, as the link annotations ask: 'link statuses',
  then 'titles by ids'. The save needs the title: a write-protected
  occurrence whose title differs from the graph's refuses the save.
  Return an error line per placed node the graph lacks, the skgrepo
  restriction hides, or whose title the server does not give."""
  placed = [ headline for headline in edited if headline . is_placed () ]
  if not placed:
    return []
  statuses = fetch_link_statuses_of_ids (
    connection, list ( dict . fromkeys (
      headline . placed_node_id for headline in placed )) )
  errors = []
  for headline in placed:
    where = ( compact_handle_text (headline) + " on line " +
              f"{headline . compact_line_number} of the compact view" )
    entry = statuses . get ( headline . placed_node_id )
    if entry is None or entry [ "status" ] == "missing":
      errors . append ( f"Error: {where} names no node of the graph." )
    elif entry [ "status" ] != "resolved":
      errors . append (
        f"Error: {where} names a node the skgrepo restriction hides." )
    elif entry . get ("title") is None:
      errors . append (
        f"Error: {where} names a node whose title the server does not "
        "give yet (its search index may lag a save by a moment); try again." )
    else:
      headline . placed_pid = entry [ "pid" ]
      headline . placed_title = entry [ "title" ]
  return errors

def resolve_merge_acquirees (connection, edited):
  """Give each headline whose handle carries 'merge:ID' the title the
  server has for the acquiree ID, as 'resolve_placed_nodes' asks;
  return an error line per acquiree the graph lacks or the skgrepo
  restriction hides."""
  merging = [ headline for headline in edited if headline . merge_acquiree () ]
  if not merging:
    return []
  statuses = fetch_link_statuses_of_ids (
    connection, list ( dict . fromkeys (
      headline . merge_acquiree () for headline in merging )) )
  errors = []
  for headline in merging:
    where = ( f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
              "of the compact view" )
    entry = statuses . get ( headline . merge_acquiree () )
    if entry is None or entry [ "status" ] == "missing":
      errors . append ( f"Error: {where} carries 'merge:"
                        f"{headline . merge_acquiree ()}', which names no node "
                        "of the graph." )
    elif entry [ "status" ] != "resolved":
      errors . append ( f"Error: {where} carries 'merge:"
                        f"{headline . merge_acquiree ()}', which names a node "
                        "the skgrepo restriction hides." )
    else:
      headline . merge_title = entry . get ("title")
  return errors

def assign_placed_view_requests (edited, headline_of_handle):
  """Each resolved placed node asks for an editable view unless the
  rebuilt buffer already holds an editable occurrence of its node, or
  one that asks to be made editable: a buffer holds at most one, as
  'skg--buffer-has-editable-occurrence-p' in 'elisp/skg-linkstack.el'
  checks before a paste. Placed nodes are taken in order, so of two
  placements of one node the first asks."""
  for index, headline in enumerate (edited):
    if headline . is_placed () and headline . placed_pid is not None:
      headline . placed_requests_editable = not buffer_holds_editable_occurrence (
        edited, headline_of_handle, headline . placed_pid, index )

def buffer_holds_editable_occurrence (edited, headline_of_handle, pid,
                                      except_index):
  """Whether a headline of the edited outline but EXCEPT_INDEX is, or
  asks to become, an editable occurrence of the node PID."""
  for index, headline in enumerate (edited):
    if index == except_index:
      continue
    if headline . is_placed ():
      if headline . placed_pid == pid and headline . placed_requests_editable:
        return True
      continue
    view_line = ( headline_of_handle . get ( headline . handle )
                  if headline . handle is not None else None )
    if ( view_line is None or view_line . metadata is None
         or headline_kind ( view_line . metadata ) != "node"
         or headline_gnid ( view_line . metadata ) != pid ):
      continue
    if ( not metadata_has_atom ( view_line . metadata, "writeProtected" )
         or "editable" in headline . view_request_words () ):
      return True
  return False

# ------------------------------------------------------------
# Forking: an edit of a foreign node, or the word 'fork'
# ------------------------------------------------------------

class PredictedForks:
  """What the report says a save forks: 'error_lines' that stop it,
  'lines' a 'Fork:' line per fork, 'gnids' the ID of each node it
  forks, and 'clone_skgrepo', the owned skgrepo a clone goes to when
  the server leaves the choice to the client (None when no choice
  can be made: no skgrepo is owned, or several are and
  '--fork-into' names none)."""
  def __init__ (self):
    self . error_lines = []
    self . lines = []
    self . gnids = []
    self . clone_skgrepo = None

def predicted_forks (edited, original, headline_of_handle, ownership,
                     fork_into, other_changes):
  """The forks the edited compact view asks for, as the server detects
  them ('apply_foreign_policy' in 'server/from_text/validate.rs'): an
  editable occurrence of a foreign node whose title, body or content
  differs from the full view's, and a node whose handle carries
  'fork', as the clients' 'skg-fork-node' marks one. OWNERSHIP is
  {SKGREPO: OWNED}; FORK_INTO the '--fork-into' skgrepo or None;
  OTHER_CHANGES whether the report lists changes besides forks, which
  a 'fork' refuses, as the clients refuse to fork from a buffer with
  unsaved changes. An edit of a write-protected occurrence of a
  foreign node is an error: the server refuses a changed title,
  ignores a body, and claims nothing for a child there, so it forks
  nothing; making it editable first ('editable', then a save) is the
  gesture, as 'docs/forks.org' says. Only an occurrence in content
  position (a root, or a node's child) forks: a member of a folder
  does not, and a foreign member as such of a forks folder (a forkee
  or forker as such), whose content is shown in part, takes no edit
  of its title or body."""
  forks = PredictedForks ()
  original_index_of_handle = { headline . handle : index
                               for index, headline in enumerate (original) }
  seen_handles = set ()
  for index, headline in enumerate (edited):
    if headline . handle is None or headline . handle in seen_handles:
      continue
    seen_handles . add ( headline . handle )
    view_line = headline_of_handle . get ( headline . handle )
    if ( view_line is None or view_line . metadata is None
         or headline_kind ( view_line . metadata ) != "node" ):
      continue
    described = described_handle ( headline . handle, headline_of_handle )
    gnid = headline_gnid ( view_line . metadata )
    if "fork" in headline . view_request_words ():
      if other_changes:
        forks . error_lines . append (
          f"Error: {described} carries 'fork', but the compact view has "
          "other edits; save them first, as a client forks only from a "
          "saved buffer, so that the clone is the node as it is saved." )
      forks . lines . append (
        f"Fork: {described}, by request: a new clone of it forks it." )
      forks . gnids . append (gnid)
      continue
    skgrepo = headline_node_repo ( view_line . metadata )
    if ( ownership . get (skgrepo) is not False
         or not in_content_position (edited, index, headline_of_handle) ):
      continue
    write_protected = metadata_has_atom ( view_line . metadata, "writeProtected" )
    changed = changed_parts_of_node_occurrence (
      edited, index, original, original_index_of_handle [ headline . handle ],
      headline_of_handle, write_protected )
    if not changed:
      continue
    if write_protected:
      forks . error_lines . append (
        f"Error: {described}, of the foreign skgrepo '{skgrepo}', is "
        f"write-protected, and its {' and ' . join (changed)} changed. A "
        "write-protected occurrence saves nothing of its own, so this would "
        "fork nothing. To fork it, undo the edit, write 'editable' in its "
        "handle and save; then edit it and save again." )
      continue
    forks . lines . append (
      f"Fork: {described}, of the foreign skgrepo '{skgrepo}', whose "
      f"{' and ' . join (changed)} changed: the edit goes to a new clone of "
      "it, which forks it; the original stays as it is." )
    forks . gnids . append (gnid)
  if not forks . lines:
    return forks
  settle_clone_skgrepo (forks, ownership, fork_into)
  return forks

def settle_clone_skgrepo (forks, ownership, fork_into):
  """Set 'forks . clone_skgrepo', the owned skgrepo the clones of FORKS
  go to: FORK_INTO if it names an owned one, else the one skgrepo this
  server owns. Where neither settles it, add an error line; where one
  does, a line saying which, and how. OWNERSHIP and FORK_INTO are as
  'predicted_forks' takes them."""
  owned = [ name for name, is_owned in ownership . items () if is_owned ]
  if fork_into is not None:
    if fork_into in owned:
      forks . clone_skgrepo = fork_into
    else:
      forks . error_lines . append (
        f"Error: '--fork-into {fork_into}' names no owned skgrepo of this "
        "server; its owned ones are: " + ( ", " . join (owned) or "none" ) + "." )
  elif len (owned) == 1:
    forks . clone_skgrepo = owned [0]
  else:
    forks . error_lines . append (
      "Error: this save forks, and the clone needs an owned skgrepo: " +
      ( "this server owns none." if not owned else
        "this server owns several (" + ", " . join (owned) +
        "); name one with '--fork-into REPO'." ))
  if forks . clone_skgrepo is not None:
    forks . lines . append (
      f"Fork into: '{forks . clone_skgrepo}'" +
      ( ", as '--fork-into' says" if fork_into is not None
        else ", the one skgrepo this server owns" ) +
      ", unless the edit settles another. The server asks before forking; "
      "'--yes' approves." )

def in_content_position (edited, index, headline_of_handle):
  """Whether the headline at INDEX of the edited outline stands where
  content does: at the top level, or under a node occurrence (one
  with a handle naming a node, a new headline, or a placed node)."""
  parent = edited [index] . parent
  if parent is None or edited [parent] . handle is None:
    return True
  view_line = headline_of_handle . get ( edited [parent] . handle )
  return ( view_line is not None and view_line . metadata is not None
           and headline_kind ( view_line . metadata ) == "node" )

def changed_parts_of_node_occurrence (edited, edited_index, original,
                                      original_index, headline_of_handle,
                                      write_protected):
  """Which of 'title', 'body' and 'children' of a node occurrence the
  edit changed: its children as the save reads them, the node
  occurrences under it that claim membership, in order. Under a
  write-protected occurrence, where nothing claims, removing or
  reordering children changes nothing, so only an added one counts."""
  changed = []
  if edited [edited_index] . title != original [original_index] . title:
    changed . append ("title")
  if edited [edited_index] . body != original [original_index] . body:
    changed . append ("body")
  edited_children = claiming_children_keys ( edited, edited_index, headline_of_handle )
  original_children = claiming_children_keys ( original, original_index, headline_of_handle )
  if ( set (edited_children) - set (original_children) if write_protected
       else edited_children != original_children ):
    changed . append ("children")
  return changed

def claiming_children_keys (outline, index, headline_of_handle):
  """The children of OUTLINE's headline INDEX that are node
  occurrences claiming membership in it, in order: each by its handle,
  a placed node by ('node', ID), a new headline by ('new', INDEX). A
  child claims unless its metadata carries 'noClaim'; in the edited
  outline, 'detach' and 'attach' in its handle say otherwise."""
  keys = []
  for child_index, child in enumerate (outline):
    if child . parent != index:
      continue
    if child . is_placed ():
      keys . append (( "node", child . placed_node_id ))
      continue
    if child . handle is None:
      keys . append (( "new", child_index ))
      continue
    view_line = headline_of_handle . get ( child . handle )
    if ( view_line is not None and view_line . metadata is not None
         and headline_kind ( view_line . metadata ) == "node"
         and ( "attach" in child . claim_words () or
               ( "detach" not in child . claim_words ()
                 and not metadata_has_atom ( view_line . metadata, "noClaim" )))):
      keys . append ( child . handle )
  return keys
