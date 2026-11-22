"""The diff view's commands and the commit: 'diff', 'extend' and
'commit'."""

import os
import sys

import server_connection
from sexp import sexp_to_text
from server_connection import (
  SkgToolError, connect_to_server, response_field )
from compact_view import count_phrase, processed_summary_text
from views import (
  RenderedView, fetch_diff_view_buffer, load_saved_view, release_view,
  require_port_of_sidecar, write_view_files )
from save import (
  compact_view_has_edits, connection_for_saved_view,
  register_view_by_saving_it_unchanged )
from daemon_client import (
  ask_daemon_of_port_to_rerender_diffView_buffers )

# The buffers of the diff view, in the order the clients request them.
DIFF_VIEW_BUFFERS = [ "all changes", "merges", "skgrepo changes",
                      "order-only changes" ]

def command_extend (arguments):
  """Send "extend diff view" for a buffer of the diff view,
  as the clients' 'C-c r u' does, and write the buffer the server
  rerenders over the three files. A client sends it only for a buffer
  with no unsaved changes, so this refuses an edited compact view. The
  server works on the buffer it registered under the view ID, so the
  full view is first registered there: it is saved back unchanged,
  naming its buffer of the diff view, which writes nothing and leaves
  the server holding this buffer, its roots and what it makes
  editable; its processed summary is printed as the one before
  extending. The server refuses that save if a node the
  buffer shows changed since it was rendered; 'diff' then writes a
  fresh buffer, which makes every change editable."""
  saved_view = load_saved_view (arguments . view_files)
  sidecar = saved_view . sidecar
  if sidecar [ "command" ] != "diff":
    raise SkgToolError (
      "Nothing was extended: " + os . path . basename (saved_view . full_path) +
      " is not a buffer of the diff view." )
  require_port_of_sidecar (arguments . port, sidecar)
  if compact_view_has_edits (saved_view):
    raise SkgToolError (
      "Nothing was extended: the compact view has edits; save them "
      "first, or write the buffer again with 'diff'." )
  connection = connection_for_saved_view (saved_view)
  try:
    terminal = register_view_by_saving_it_unchanged (
      connection, saved_view,
      "Nothing was extended: the server refused this buffer as it "
      "stands; run 'diff' for a fresh one, which makes every change "
      "editable." )
    print ( "Before extending: " + processed_summary_text (
      response_field (terminal, "processed-summary") ))
    response, rerendered = connection . extend_diff_view (
      sidecar [ "view_id" ] )
    errors = response_field (response, "errors") or []
    for error in errors:
      print ( "Server error: " + error )
    if errors or rerendered is None:
      raise SkgToolError (
        "Nothing was extended; the files are as they were." )
    processed_summary = response_field (response, "processed-summary")
    print ( "Extended: " + processed_summary_text (processed_summary) )
    connection . log_view_event (
      "extended", saved_view . directory, saved_view . base_name )
    view = RenderedView (
      view_id  = sidecar [ "view_id" ],
      command  = sidecar [ "command" ],
      request  = sidecar [ "request" ],
      content  = sexp_to_text ( response_field (rerendered, "content") ),
      errors   = [],
      warnings = response_field (response, "warnings") or [],
      processed_summary = processed_summary )
    written_paths = write_view_files (
      connection, view, saved_view . directory, saved_view . base_name,
      sidecar [ "summary" ] )
    release_view (connection, view . view_id)
  finally:
    connection . close ()
  for path in written_paths:
    print (path)

def command_diff (arguments):
  """Request each buffer of the diff view in the clients' order, on
  one connection, and write each nonempty one as 'view' writes a view,
  named BASE_BUFFER ('diff_all-changes' and so on), its compact
  summary opening with the processed summary the server's answer
  carries. Each buffer is closed before the next is requested, unless
  a daemon holds it."""
  connection = connect_to_server (arguments . port)
  written_paths = []
  try:
    for buffer_name in DIFF_VIEW_BUFFERS:
      view = fetch_diff_view_buffer (connection, buffer_name)
      if view is None:
        continue
      written_paths += write_view_files (
        connection, view, arguments . out_dir,
        ( arguments . name or "diff" ) + "_" +
        buffer_name . replace (" ", "-"),
        not arguments . no_summary )
      release_view (connection, view . view_id)
  finally:
    connection . close ()
  if not written_paths:
    print ( "The diff view is empty: nothing changed since the last commits." )
  for path in written_paths:
    print (path)

def commit_commits_everything (arguments):
  """Whether 'commit' commits every changed node: with '--all', or
  with '--yes', which only a commit of everything can need, so that
  'commit "MSG" --yes' commits what it committed before the commit of
  the processed existed."""
  return arguments . all or arguments . yes

def command_commit (arguments):
  """Send "stage and commit" with the message. Without '--all' (or
  '--yes'), it commits every node whose changes are all processed,
  except one that needs a node whose changes are not, and asks nothing.
  With '--all' it commits every changed node; when the server asks for
  confirmation (some changed node is not processed), print why and ask
  for a typed "yes", which '--yes' gives; on yes, resend it confirmed.
  Print what went in, how many changed nodes are left, and why each
  processed node left waits. The server rerenders the committing
  connection's views afterward; a commit on a connection of its own
  (one that may ask for a typed "yes", which cannot cross the daemon's
  socket) then asks the daemon for its port, if one runs, to rerender
  its buffers of the diff view."""
  if not arguments . message . strip ():
    raise SkgToolError ( "A commit message is required; nothing was sent." )
  processed_only = not commit_commits_everything (arguments)
  connection = connect_to_server (arguments . port)
  try:
    response = connection . stage_and_commit (
      arguments . message, False, processed_only )
    if response_field (response, "needs-confirmation") is not None:
      print ( "Not every change is processed: " +
              processed_summary_text (
                response_field (response, "processed-summary") ))
      if not ( arguments . yes or typed_yes ( "Type yes to commit anyway: " )):
        print ( "Nothing was committed." )
        return
      response = connection . stage_and_commit (arguments . message, True)
  finally:
    connection . close ()
  commits = response_field (response, "commits") or []
  errors = response_field (response, "errors") or []
  print ( commit_result_text (response) )
  for error in errors:
    print ( "Server error: " + error )
  for warning in response_field (response, "warnings") or []:
    print ( "Server warning: " + warning )
  if commits and server_connection . RUNNING_DAEMON is None:
    ask_daemon_of_port_to_rerender_diffView_buffers (arguments . port)
  if errors:
    raise SkgToolError ( "The server reported errors; see above." )

def commit_result_text (response):
  """What a commit's RESPONSE says, as the clients say it
  ('skg--stage-and-commit-result-message' in
  'elisp/skg-request-stage-and-commit.el'): what went in, how many
  changed nodes are left, for each processed node that waits, what it
  waits for, and for each node left because it changed in a way no
  mark covers, how. The commit IDs are whole. Errors are printed
  apart."""
  commits = response_field (response, "commits") or []
  committed_nodes = response_field (response, "committed-nodes")
  left = response_field (response, "left-nodes") or 0
  waiting = response_field (response, "waiting") or []
  unmarkable = response_field (response, "unmarkable") or []
  errors = response_field (response, "errors") or []
  if commits:
    text = ( "Committed " +
             ( count_phrase (committed_nodes, "node") + ", "
               if isinstance (committed_nodes, int) else "" ) +
             "in " + ", " . join (
               sexp_to_text ( commit [0] ) + " (" + sexp_to_text ( commit [1] ) + ")"
               for commit in commits ) + "." )
  elif errors or waiting or unmarkable:
    text = "Nothing was committed."
  elif left == 0:
    text = "Nothing has changed, so nothing was committed."
  else:
    text = ( "Nothing is processed, so nothing was committed; "
             "'commit --all' commits everything." )
  if left > 0 and ( commits or waiting or unmarkable ):
    text += ( " " + count_phrase (left, "changed node") +
              ( " is left" if left == 1 else " are left" ) )
    if waiting:
      text += ( f"; {len (waiting)} of them " +
                ( "is processed but waits" if len (waiting) == 1
                  else "are processed but wait" ) + ": " +
                "; " . join ( waiting_clause (entry) for entry in waiting ))
    if unmarkable:
      text += ( f"; {len (unmarkable)} of them changed in " +
                ( "a way" if len (unmarkable) == 1 else "ways" ) +
                " no mark covers, which 'commit --all' or Git can commit: " +
                "; " . join ( unmarkable_clause (entry) for entry in unmarkable ))
    text += "."
  return text

def unmarkable_clause (entry):
  """ENTRY, '(PID TITLE (CHANGE ...))' from a commit's 'unmarkable':
  the node's title, and how it changed, in the server's words."""
  return ( f"'{sexp_to_text ( entry [1] )}' (" +
           ", " . join ( sexp_to_text (change) for change in entry [2] ) + ")" )

def waiting_clause (entry):
  """What ENTRY, '(PID TITLE REASON PID TITLE)' from a commit's
  'waiting', waits for, and why."""
  title, reason, other_title = ( sexp_to_text ( entry [1] ),
                                 sexp_to_text ( entry [2] ),
                                 sexp_to_text ( entry [4] ) )
  if reason == "names-new":
    return f"'{title}' names '{other_title}', which is new and does not go in"
  if reason == "still-named":
    return ( f"'{title}' is deleted, but '{other_title}', which does not go "
             "in, still names it" )
  if reason == "merge":
    return f"'{title}' is part of a merge with '{other_title}', which does not go in"
  return f"'{title}' waits for '{other_title}' ({reason})"

def typed_yes (prompt):
  """Whether the person at standard input types exactly 'yes'. With no
  input to read, the answer is no."""
  print (prompt, end = "", flush = True)
  try:
    answer = sys . stdin . readline ()
  except OSError:
    answer = ""
  if not answer . endswith ("\n") or not sys . stdin . isatty ():
    # The terminal echoed no answer to end the prompt's line.
    print ()
  return answer . strip () == "yes"
