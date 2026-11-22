"""'save': 'command_save' and what it does with an edited outline.
It rebuilds the full buffer, reports the changes it would make,
places existing nodes, predicts forks, stops on retyped headlines,
answers the server's questions and writes the view the server
rerenders. Also what other commands need of a save:
'register_view_by_saving_it_unchanged', 'compact_view_has_edits',
'refuse_while_held_views_have_edits'."""

import difflib
import os
import re

from sexp import (
  Symbol, quote_string, render_sexp, sexp_to_text,
  top_level_elements_with_offsets )
from server_connection import (
  SkgConnection, SkgToolError, connect_to_server,
  overPrivateText_answer, request_line, require_response_type,
  response_field, response_text_field, response_type )
from full_view import (
  METADATA_PARTS, headline_gnid, headline_kind, headline_node_repo,
  metadata_has_atom, parse_full_view )
from compact_view import (
  fetch_link_statuses_of_ids, fetch_repo_ownership,
  processed_summary_text )
from views import (
  RenderedView, SavedView, load_saved_view, metadata_text_with_part,
  perspective_fields_of_sidecar, perspective_of_sidecar, release_view,
  release_withheld_save_rerender, require_port_of_sidecar,
  write_view_files )
from edited_compact_view import (
  MERGE_WORD_PREFIX, NEW_NODE_HANDLE_WORD, OutlineWordCheck,
  RELREPO_WORD_PREFIX, REPO_WORD_PREFIX, VIEW_REQUEST_TOKENS,
  WRITE_PROTECT_SUBTREE_REQUEST, compact_handle_text,
  descendant_indices, described_handle, edit_request_part_of_word,
  edit_request_text, edited_outline, handle_bracket_title_error_lines,
  handle_error_lines, outline_of_full_view, outline_word_check,
  processed_request_text, unmarked_leaf_count, word_parameter )

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

def send_save_and_write_its_rerender (connection, saved_view, buffer_text,
                                      arguments, forks):
  """Send the save of BUFFER_TEXT, answering the server's questions as
  ARGUMENTS say and approving only the forks FORKS predicts
  ('answer_save_question'); release a withheld rerender when
  '--show-overPrivateText' allows; print the server's answer, and
  write its re-rendered view over SAVED_VIEW's three files. Return
  their paths."""
  answers = SaveAnswers ()
  for attempt in range (SAVE_ATTEMPTS_LIMIT):
    messages = connection . save_buffer (
      saved_view . sidecar [ "view_id" ], buffer_text,
      saved_view . full_text,
      saved_view . sidecar [ "request" ] . get ("buffer"), answers,
      perspective_of_sidecar (saved_view . sidecar) )
    if response_type ( messages [-1] ) not in SAVE_QUESTION_TYPES:
      break
    answer_save_question ( messages [-1], answers, arguments, forks )
  terminal = messages [-1]
  if response_type (terminal) == "overPrivateText-telescope-confirmation":
    overPrivateText_answer (
      terminal, arguments . show_overPrivateText, "the save succeeded, but ",
      "The save succeeded, but its re-rendered view is withheld; the files "
      "are out of date. View the node again with '--show-overPrivateText'.",
      approval = "the withheld rerender is asked for" )
    terminal = release_withheld_save_rerender (
      connection, terminal, saved_view . sidecar [ "view_id" ] )
  rerendered = rerendered_view_of_save ( terminal, saved_view . sidecar )
  connection . log_view_event (
    "saved", saved_view . directory, saved_view . base_name )
  written_paths = write_view_files (
    connection, rerendered, saved_view . directory, saved_view . base_name,
    saved_view . sidecar [ "summary" ] )
  release_view (connection, rerendered . view_id)
  return written_paths

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
  ('skg--insert-node-from-entry' in 'elisp/skg-id-search.el'): the ID,
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
  'skg--buffer-has-editable-occurrence-p' in 'elisp/skg-id-search.el'
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
  owned = [ name for name, is_owned in ownership . items () if is_owned ]
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
  return forks

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

# ------------------------------------------------------------
# Retyped headlines: a new headline whose title is, or resembles, a
# removed one's
# ------------------------------------------------------------
# Words too common to say what a title is about: articles,
# prepositions, conjunctions, pronouns and the commonest verbs. The
# fuzzy match ignores them.
TITLE_STOPWORDS = frozenset ( """
  a an the
  of in on at to for from by with about into onto over under up down
  out off as than via per after before between through during without
  within against among around behind below above across along toward
  towards upon
  and or but nor so if then
  i me my mine we us our ours you your yours he him his she her hers
  it its they them their theirs this that these those who whom whose
  which what
  is are was were be been being am has have had having do does did
  done will would can could shall should may might must
  """ . split () )
# The suffixes the crude stemmer strips, tried in this order; the
# first one a word ends in that leaves a stem this long is stripped.
STEMMED_SUFFIXES = [ "ing", "ed", "es", "ly", "s" ]
MINIMUM_STEM_LENGTH = 3
# A new headline and a removed one probably state the same thing
# reworded when either signal reaches its threshold; the ratio counts
# only when both titles have at least REWORDED_RATIO_MINIMUM_WORDS
# normalized words, since on shorter titles it fires on one shared
# word ("budget meeting" / "budget notes").
REWORDED_DICE_THRESHOLD = 0.6
REWORDED_RATIO_THRESHOLD = 0.75
REWORDED_RATIO_MINIMUM_WORDS = 3

def retyped_headline_error_lines (edited, original, headline_of_handle):
  """An error per new headline, not marked '⟨new⟩', whose title equals
  the title of a headline the edit removed: retyping a headline
  instead of keeping its handle would lose its node's ID."""
  removed = removed_titled_headlines (edited, original)
  errors = []
  for index in unmarked_new_titled_indices (edited):
    headline = edited [index]
    handles = [ handle for handle, title in removed
                if title == headline . title ]
    if not handles:
      continue
    restored = ( f"end it with ⟨{handles [0]}⟩" if len (handles) == 1
                 else "end it with one of those handles" )
    errors . append (
      f"Error: the new headline '{headline . title}' on line "
      f"{headline . compact_line_number} of the compact view has the title "
      "of the removed " + ", " . join (
        described_handle (handle, headline_of_handle) for handle in handles )
      + f". If it is that node, moved or reworded, {restored}; if it is "
      f"another node, end it with '⟨{NEW_NODE_HANDLE_WORD}⟩'." )
  return errors

class RewordedHeadlinePair:
  """A new headline, by its index in the edited outline, and the
  removed headline, by handle, whose title it probably rewords; and
  the other removed headlines its title resembles, best first."""
  def __init__ (self, new_index, removed_handle, other_removed_handles):
    self . new_index = new_index
    self . removed_handle = removed_handle
    self . other_removed_handles = other_removed_handles

def reworded_headline_pairs (edited, original):
  """Each new headline not marked '⟨new⟩' paired with the removed
  headline whose title it probably rewords, one to one, in the edited
  outline's order. Candidate pairs are taken best first, by the larger
  of their two signals, and a pair either of whose headlines is
  already paired is skipped."""
  removed = removed_titled_headlines (edited, original)
  candidates_of_new_index = {}
  for index in unmarked_new_titled_indices (edited):
    for removed_order, ( handle, title ) in enumerate (removed):
      if titles_probably_reworded ( edited [index] . title, title ):
        candidates_of_new_index . setdefault ( index, [] ) . append (
          ( - max ( title_similarity ( edited [index] . title, title )),
            removed_order, handle ))
  ranked = sorted (
    ( candidate + ( index, )
      for index, candidates in candidates_of_new_index . items ()
      for candidate in candidates ),
    key = lambda candidate : ( candidate [0], candidate [3], candidate [1] ))
  pairs = []
  paired_indices, paired_handles = set (), set ()
  for _, _, handle, index in ranked:
    if index in paired_indices or handle in paired_handles:
      continue
    paired_indices . add (index)
    paired_handles . add (handle)
    pairs . append ( RewordedHeadlinePair (
      index, handle,
      [ other for _, _, other in sorted ( candidates_of_new_index [index] )
        if other != handle ] ))
  return sorted ( pairs, key = lambda pair : pair . new_index )

def reworded_question_lines (edited, pairs, headline_of_handle):
  """A question per pair: is the new headline the removed one, reworded?"""
  lines = []
  for pair in pairs:
    headline = edited [ pair . new_index ]
    lines . append (
      "Question: removed " +
      described_handle ( pair . removed_handle, headline_of_handle ) +
      f" / new '{headline . title}' (line {headline . compact_line_number}): "
      "probably the same statement reworded; keep its ID?" +
      ( " Its title also resembles " + ", " . join (
          described_handle (handle, headline_of_handle)
          for handle in pair . other_removed_handles ) + "."
        if pair . other_removed_handles else "" ))
  return lines

def give_new_headlines_their_removed_handles (edited, pairs):
  """The answer '--keep-ids': each pair's new headline takes the removed
  headline's handle, and so its metadata and node, with its new title."""
  for pair in pairs:
    edited [ pair . new_index ] . handle = pair . removed_handle

def removed_titled_headlines (edited, original):
  """[(HANDLE, TITLE)] of each titled headline of the full view whose
  handle the edited compact view no longer has, in the full view's
  order."""
  kept_handles = { headline . handle for headline in edited
                   if headline . handle is not None }
  return [ ( headline . handle, headline . title ) for headline in original
           if headline . handle not in kept_handles and headline . title ]

def unmarked_new_titled_indices (edited):
  return [ index for index, headline in enumerate (edited)
           if headline . handle is None and not headline . marked_new
           and not headline . is_placed () and headline . title ]

def titles_probably_reworded (left_title, right_title):
  """Whether two different titles probably state the same thing
  reworded: their normalized words' Dice coefficient reaches its
  threshold, or difflib's ratio of their normalized texts reaches its
  own and each title has REWORDED_RATIO_MINIMUM_WORDS normalized
  words."""
  if left_title == right_title:
    return False
  dice, ratio = title_similarity (left_title, right_title)
  return ( dice >= REWORDED_DICE_THRESHOLD
           or ( ratio >= REWORDED_RATIO_THRESHOLD
                and min ( len ( normalized_title_words (left_title) ),
                          len ( normalized_title_words (right_title) ))
                    >= REWORDED_RATIO_MINIMUM_WORDS ))

def title_similarity (left_title, right_title):
  """(DICE, RATIO): the Dice coefficient of the two titles' sets of
  normalized words, 2 |A ∩ B| / (|A| + |B|), and
  'difflib.SequenceMatcher''s ratio of their normalized words joined
  by spaces. Both are 0 when either title has no word left."""
  left_words = normalized_title_words (left_title)
  right_words = normalized_title_words (right_title)
  if not left_words or not right_words:
    return ( 0.0, 0.0 )
  left_set, right_set = set (left_words), set (right_words)
  return ( 2 * len ( left_set & right_set ) / ( len (left_set) + len (right_set) ),
           difflib . SequenceMatcher (
             None, " " . join (left_words), " " . join (right_words)
           ) . ratio () )

def normalized_title_words (title):
  """TITLE's words as the fuzzy match compares them: lowercased,
  apostrophes dropped ("it's" is 'its'), every other run of
  punctuation a word break, stopwords dropped, each word crudely
  stemmed."""
  words = re . sub ( r'[\W_]+', " ",
                     re . sub ( "['’]", "", title . lower () )) . split ()
  return [ crude_stem (word) for word in words
           if word not in TITLE_STOPWORDS ]

def crude_stem (word):
  """WORD without the first of STEMMED_SUFFIXES it ends in that leaves
  a stem of MINIMUM_STEM_LENGTH letters, and then without a final 'e'
  (on a stem longer than that), so that 'save', 'saves', 'saved' and
  'saving' are all 'sav'. '-es' is stripped only after s, x, z, ch
  or sh ('boxes', not 'makes'), and '-s' never after another s
  ('class')."""
  for suffix in STEMMED_SUFFIXES:
    stem = word [ : - len (suffix) ]
    if ( not word . endswith (suffix)
         or len (stem) < MINIMUM_STEM_LENGTH
         or ( suffix == "es"
              and not stem . endswith (( "s", "x", "z", "ch", "sh" )) )
         or ( suffix == "s" and stem . endswith ("s") )):
      continue
    word = stem
    break
  return ( word [ : -1 ]
           if word . endswith ("e") and len (word) > MINIMUM_STEM_LENGTH
           else word )

# ------------------------------------------------------------
# The fork confirmation
# ------------------------------------------------------------

# What the server's fork confirmation shows as the skgrepo of a clone
# whose skgrepo the edit did not settle, which the client must replace
# ('FORK_SKGREPO_PLACEHOLDER' in 'server/from_text/fork.rs').
FORK_SKGREPO_PLACEHOLDER = "PICK-A-REPO"
# The comment the server writes above such a clone.
SUGGESTED_FORK_SKGREPO_PATTERN = re . compile (
  r'^# Suggested repo for the clone below: (.+)$' )

class ForkConfirmationClone:
  """A clone-to-be of the fork confirmation: its title, the skgrepo
  shown for it (perhaps the placeholder), the server's suggestion
  above it (or None), and 'originals', [(ID, SKGREPO, TITLE)], the
  nodes it forks."""
  def __init__ (self, title, skgrepo, suggested_skgrepo):
    self . title = title
    self . skgrepo = skgrepo
    self . suggested_skgrepo = suggested_skgrepo
    self . originals = []

def fork_confirmation_clones (content):
  """The clones-to-be of the fork confirmation's buffer CONTENT, read
  as the clients read it ('skg--fork-repos-from-confirmation-buffer'
  in 'elisp/skg-request-save/skg-save-confirmations.el'): each top-level headline with
  metadata is a clone-to-be, and each headline with an ID under one
  that names its skgrepo is an original it forks. The instructions
  headline has no metadata."""
  clones = []
  clone = None
  previous_text = ""
  for view_line in parse_full_view (content):
    if view_line . is_headline and len (view_line . stars) == 1:
      clone = None
      if view_line . metadata is not None:
        suggestion = SUGGESTED_FORK_SKGREPO_PATTERN . match (previous_text)
        clone = ForkConfirmationClone (
          view_line . title, headline_node_repo (view_line . metadata),
          suggestion . group (1) . strip () if suggestion else None )
        clones . append (clone)
    elif ( view_line . is_headline and len (view_line . stars) == 2
           and clone is not None and clone . skgrepo is not None
           and view_line . metadata is not None
           and headline_gnid (view_line . metadata) is not None ):
      clone . originals . append ((
        headline_gnid (view_line . metadata),
        headline_node_repo (view_line . metadata), view_line . title ))
    previous_text = view_line . text
  return clones

def approved_fork_skgrepos_of_confirmation (confirmation, forks):
  """Print the fork confirmation, as the clients' confirmation buffer
  shows it: each clone-to-be, its skgrepo, and the original it forks.
  Return [(ID, SKGREPO)], each original's ID with its clone's skgrepo,
  which approves it: a placeholder is replaced by FORKS'
  'clone_skgrepo', as a client's prompt replaces it. Raise, saving
  nothing, when the server forks a node the report did not list
  (FORKS' 'gnids'): '--yes' approved only what the report showed."""
  clones = fork_confirmation_clones (
    sexp_to_text ( response_field (confirmation, "content") or "" ))
  originals = [ original for clone in clones for original in clone . originals ]
  print ( f"Server: fork confirmation: the save forks {len (originals)} "
          f"node{'' if len (originals) == 1 else 's'}, and writes nothing "
          "until that is approved." )
  approved = []
  for clone in clones:
    placeholder = clone . skgrepo == FORK_SKGREPO_PLACEHOLDER
    skgrepo = forks . clone_skgrepo if placeholder else clone . skgrepo
    print ( f"Clone-to-be: '{clone . title}', in " +
            ( f"'{skgrepo}'" if skgrepo is not None else "no skgrepo yet" ) +
            ( "" if placeholder else ", as the edit settles it" ) +
            ( f" (the server suggests '{clone . suggested_skgrepo}')"
              if clone . suggested_skgrepo else "" ) + "." )
    for gnid, original_skgrepo, title in clone . originals:
      print ( f"  It forks '{title}' ({gnid}), of the skgrepo "
              f"'{original_skgrepo}'." )
      approved . append (( gnid, skgrepo ))
  unlisted = [ gnid for gnid, _, _ in originals if gnid not in forks . gnids ]
  if unlisted or not approved:
    raise SkgToolError (
      "Nothing was saved: the server asks to fork " +
      ( ", " . join (unlisted) if unlisted else "nothing it names" ) +
      ", which the report did not list as a fork; '--yes' approves only "
      "what the report showed. A client can review it." )
  if any ( skgrepo is None for _, skgrepo in approved ):
    raise SkgToolError (
      "Nothing was saved: a clone needs an owned skgrepo; name one with "
      "'--fork-into REPO'." )
  print ( "Approved: the save is sent again with the forks approved." )
  return approved

# ------------------------------------------------------------
# The save's other questions, and the answers its retries carry
# ------------------------------------------------------------

# The questions a save may ask in place of its result, each answered by
# a retry of the same save that carries the answer.
SAVE_QUESTION_TYPES = (
  "delete-from-confirmation", "strandeds-confirmation",
  "telescope-hoist-confirmation", "fork-confirmation" )
# How many times one save may be sent: once, and once per question
# (the Hoist question can come again when a telescope became dirty
# meanwhile).
SAVE_ATTEMPTS_LIMIT = 8

class SaveAnswers:
  """The answers a save carries on its retries, each retry carrying
  every earlier one, as the clients' do ('skg--save-request-sexp' in
  'elisp/skg-request-save.el'): 'fork_skgrepos', [(ID, SKGREPO)], the
  approved forks; 'hoist_pids', the telescopes approved for Hoist;
  'strandeds_reset', None until the strandeds are reviewed, then the
  [(RELATION, RECORDER, MEMBER)] to reset to default; 'delete_from',
  'visible' or 'all'."""
  def __init__ (self):
    self . fork_skgrepos = None
    self . hoist_pids = None
    self . strandeds_reset = None
    self . delete_from = None

  def request_fields_text (self):
    """The fields the save request carries for these answers, each
    after a space."""
    fields = []
    if self . fork_skgrepos is not None:
      fields += [ '(approved-forks . "true")',
                  "(fork-repos (" + " " . join (
                    "(" + quote_string (gnid) + " . " + quote_string (skgrepo) + ")"
                    for gnid, skgrepo in self . fork_skgrepos ) + "))" ]
    if self . hoist_pids:
      fields . append ( "(approved-hoist-pids " +
                        " " . join ( map ( quote_string, self . hoist_pids )) + ")" )
    if self . strandeds_reset is not None:
      fields . append ( '(strandeds-reviewed . "true")' )
      if self . strandeds_reset:
        fields . append ( "(reset-strandeds " + " " . join (
          "(" + " " . join ( map ( quote_string, triple )) + ")"
          for triple in self . strandeds_reset ) + ")" )
    if self . delete_from is not None:
      fields . append ( "(delete-from . " + quote_string (self . delete_from) + ")" )
    return "" . join ( " " + field for field in fields )

def answer_save_question (question, answers, arguments, forks):
  """Print the save's QUESTION, and record in ANSWERS the answer the
  command line gives it, so that the save is sent again with it; raise,
  sending nothing more, when it gives none. '--yes' alone takes the
  answer that writes nothing more: Abort, cancel, decline. FORKS are
  what the report predicted, which approves a fork confirmation."""
  kind = response_type (question)
  if kind == "fork-confirmation":
    answers . fork_skgrepos = approved_fork_skgrepos_of_confirmation (question, forks)
  elif kind == "delete-from-confirmation":
    answers . delete_from = delete_from_answer (question, arguments . delete_from)
  elif kind == "strandeds-confirmation":
    answers . strandeds_reset = strandeds_answer (question, arguments . strandeds)
  elif kind == "telescope-hoist-confirmation":
    answers . hoist_pids = hoist_answer (question, arguments . hoist)

def delete_from_answer (question, delete_from):
  """'visible' or 'all', as DELETE_FROM ('--delete-from') says, for a
  delete-from confirmation ('skg--delete-from-confirmation-handler'):
  records the skgrepo restriction keeps out name a node the save
  deletes."""
  print ( f"Server: delete-from confirmation: "
          f"{response_text_field (question, 'count')} record(s) the skgrepo "
          "restriction keeps out name a node this save deletes; nothing was "
          "written. " + response_text_field (question, "prompt") )
  if delete_from is None:
    raise SkgToolError (
      "Nothing was saved (cancelled): save again with '--delete-from "
      "visible' to strip the deleted IDs from the records the restriction "
      "shows only, or '--delete-from all' to strip them from every owned "
      "record." )
  print ( "Delete from: " + ( "the visible records only"
                              if delete_from == "visible"
                              else "every owned record" ) +
          "; the save is sent again." )
  return delete_from

def strandeds_answer (question, strandeds):
  """The relationships to reset to default, as STRANDEDS
  ('--strandeds') says, for a strandeds confirmation
  ('skg--strandeds-confirmation-handler'): 'reset' every listed one,
  'keep' none. Each is printed as the clients list it."""
  listed = [
    tuple ( response_text_field (entry, field)
            for field in ( "relation", "recorder", "member", "relRepo", "default" ))
    for entry in ( response_field (question, "strandeds") or [] ) ]
  print ( f"Server: strandeds confirmation: the save's home skgrepo moves "
          f"would leave {len (listed)} relationship"
          f"{'' if len (listed) == 1 else 's'} in their old skgrepos; nothing "
          "was written." )
  for relation, recorder, member, relRepo, default in listed:
    print ( f"  {recorder} {relation} {member}: stays at {relRepo}; its default "
            f"is now {default}." )
  if strandeds is None:
    raise SkgToolError (
      "Nothing was saved: save again with '--strandeds reset' to move every "
      "listed relationship to its new default, or '--strandeds keep' to "
      "leave them where they are." )
  print ( "Strandeds: " + ( "each is reset to its default"
                            if strandeds == "reset" else "each stays where it is" )
          + "; the save is sent again." )
  return ( [ ( relation, recorder, member )
             for relation, recorder, member, _, _ in listed ]
           if strandeds == "reset" else [] )

def hoist_answer (question, hoist):
  """The telescopes a Hoist confirmation lists, approved when HOIST
  ('--hoist'); else Abort, which writes nothing
  ('skg--telescope-hoist-confirmation-handler')."""
  telescopes = [ ( response_text_field (entry, "pid"),
                   response_text_field (entry, "home") )
                 for entry in ( response_field (question, "telescopes") or [] ) ]
  print ( "Server: Hoist confirmation: " + response_text_field (question, "prompt") )
  for pid, home in telescopes:
    print ( f"  {pid}, whose home is '{home}'." )
  if not hoist:
    raise SkgToolError (
      "Nothing was saved (Abort). Save again with '--hoist' to Hoist, or "
      "repair the '.skg' files by hand." )
  print ( "Hoist: approved; the save is sent again." )
  return [ pid for pid, _ in telescopes ]

# ------------------------------------------------------------
# The server's answer
# ------------------------------------------------------------

def server_supports (connection, feature):
  """Whether CONNECTION's server names FEATURE among those its answer
  to "verify connection" says it supports. An older server names none,
  and ignores a request field it does not know: a save sent with
  '(preview . "true")' would be saved for real."""
  response = connection . request ( request_line ( "verify connection" ))
  require_response_type (response, "verify-connection")
  return feature in ( response_field (response, "supports") or [] )

def print_save_preview (terminal):
  """Print what the server's answer to a preview of the save, TERMINAL,
  says the save would hide and stop hiding, one line each, as the
  server words them ('docs/api-and-formats.org', "Save buffer"); and,
  when the server would refuse the save, its errors."""
  if response_type (terminal) != "save-preview":
    raise SkgToolError (
      "The server answered the preview of the save with "
      f"'{response_type (terminal)}', not 'save-preview': "
      + render_sexp (terminal) )
  for line in response_field (terminal, "hides") or []:
    print (line)
  errors = response_field (terminal, "errors") or []
  if errors:
    print ( "The server would refuse this save:" )
    for error in errors:
      print ( "Server error: " + error )

def rerendered_view_of_save (terminal, sidecar):
  """Print the server's answer to the save. Return its re-rendered
  view, a 'RenderedView'; raise if there is none."""
  kind = response_type (terminal)
  if kind != "save-result":
    print ( f"Server: {kind}." )
    for field_name in ( "prompt", "to-minibuffer" ):
      text = response_field (terminal, field_name)
      if text:
        print ( sexp_to_text (text) )
    raise SkgToolError (
      "Nothing was saved: the server asks a question this tool does not "
      "answer. A client can answer it." )
  content = response_field (terminal, "content")
  errors = response_field (terminal, "errors") or []
  warnings = response_field (terminal, "warnings") or []
  saved = isinstance (content, str) and not isinstance (content, Symbol)
  print ( "Server: " + ( "saved" if saved else "nothing was saved" ) +
          f", {len (errors)} error{'' if len (errors) == 1 else 's'}, "
          f"{len (warnings)} warning{'' if len (warnings) == 1 else 's'}." )
  for error in errors:
    print ( "Server error: " + error )
  for warning in warnings:
    print ( "Server warning: " + warning )
  processed_summary = response_field (terminal, "processed-summary")
  if saved and processed_summary:
    print ( "Processed: " + processed_summary_text (processed_summary) )
  for line in response_field (terminal, "hides") or []:
    print (line)
  if not saved:
    raise SkgToolError (
      "The server refused the save; the files are as they were." )
  return RenderedView (
    view_id  = sidecar [ "view_id" ],
    command  = sidecar [ "command" ],
    request  = sidecar [ "request" ],
    content  = content,
    errors   = errors,
    warnings = warnings,
    processed_summary = processed_summary,
    ** perspective_fields_of_sidecar (sidecar) )
