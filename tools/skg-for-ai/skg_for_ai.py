#!/usr/bin/env python3
"""PURPOSE: a client of a running skg server for programs and people
without an editor. It shows the graph the way a person reads it in a
client, but as two plain files per view, and it speaks the protocol
directly for anything else.

For a requested view it writes, into a directory the caller names:
- NAME.full.org: the view exactly as the server rendered it.
- NAME.compact.org: the same lines, each headline's '(skg ...)'
  metadata replaced by a handle '⟨ID HERALDS⟩' at the end of the
  headline, ID being the headline's viewnode ID, as its metadata in
  the full view gives it, and HERALDS every herald a client shows
  for it, as text; and every '[[id:...]]' link followed by its
  target's title.
  A summary of the view's facts precedes the tree unless
  '--no-summary' is given.
- NAME.view.json: the server's view ID, the request, the generation
  the server rendered the view at, and the server's errors and
  warnings.

View commands: 'view ID', 'search TERMS...', 'forest KIND ID'
('contains ID' and 'links-here ID' name two kinds), 'diff'. Each
prints the paths it wrote. 'forest' draws the folder KIND under the
node, as the clients' 'C-c l' commands do, saving as they do. 'diff'
writes each nonempty buffer of the diff view, its handles carrying
the change facts.
'new TITLE REPO' creates a root node in an owned skgrepo, as a save of
a new empty view does, and writes its view as 'view' would.
'save SIDECAR' rebuilds the full buffer from an edited compact view,
prints the changes it would make, and with '--yes' saves it and
writes the server's re-rendered view over the three files. Words in a
handle ask for edit requests: 'delete', 'ready', 'unready', 'all',
'restoreNode', 'restoreRel'.
'integrate SIDECAR' makes every change a buffer of the diff view does
not make editable editable there, as the clients' 'C-c r u' does, and
writes the buffer again.
'commit MESSAGE' stages and git-commits every change in the owned
skgrepos, asking first when the server says not all is ready.
Other commands: 'close SIDECAR'; 'raw REQUEST', which sends one
request line and prints every response; 'import INPUT_DIR REPO',
which previews and applies a Markdown and Org import.
The port is SKG_PORT (default 1730), or '--port'.

Standard library only. The README beside this file explains the
formats; the protocol is in 'docs/api-and-formats.org'.
"""

import argparse
import json
import os
import re
import socket
import sys
import uuid

REPO_ROOT = os . path . dirname ( os . path . dirname (
  os . path . dirname ( os . path . abspath (__file__) )))
SHARED_FOLDER = os . path . join (REPO_ROOT, "shared")
HOST = "127.0.0.1"
DEFAULT_PORT = 1730
FIRST_MESSAGE_TIMEOUT_SECONDS = 120
IMPORT_TIMEOUT_SECONDS = 600
# How long 'raw' waits after a response for another before it stops.
RAW_QUIET_SECONDS = 3
LINK_PATTERN = re . compile (
  r'\[\[id:([^\]\[]+)\](?:\[[^\]]*\])?\]')
HEADLINE_PATTERN = re . compile (r'^(\*+) (.*)$')
# The buffers of the diff view, in the order the clients request them.
DIFF_VIEW_BUFFERS = [ "all changes", "merges", "skgrepo changes",
                      "order-only changes" ]
# What a client asks before a readiness gesture that unmarks more
# than one leaf ('skg--readiness-wide-unmark-question' in
# 'elisp/skg-readiness.el').
WIDE_UNMARK_QUESTION = (
  "Unmarking this node commit-ready will unmark every contributing "
  "decision. You might discard a lot of hard-won information. Continue?" )
# The folders a client can request from a node ('C-c l' in Emacs; the
# 'skg-show-folderOf-*' commands in 'elisp/skg-request-views.el'), each
# by its '(viewRequests ...)' request: 'forest KIND ID'. None for the
# containers folder, which a fresh content view already draws under
# its root.
FOREST_VIEW_REQUESTS = {
  "containers"    : None,
  "mentioners"    : "(folder mentioners)",
  "subscribesTo"  : "(folder subscribesTo)",
  "overrides"     : "(folder overrides)",
  "hidesFromSubs" : "(folder hidesFromSubs)",
  "aliases"       : "(folder aliases)",
  "flags"         : "flags" }
# Commands that name one forest kind.
FOREST_COMMAND_ALIASES = { "contains" : "containers",
                           "links-here" : "mentioners" }
SAVE_TERMINAL_RESPONSE_TYPES = {
  "save-result", "delete-from-confirmation", "strandeds-confirmation",
  "telescope-hoist-confirmation", "fork-confirmation",
  "overPrivateText-telescope-confirmation", "error" }

# ============================================================
# Commands
# ============================================================

def main (argv):
  arguments = parse_command_line (argv)
  if arguments . command == "close":
    command_close (arguments)
    return
  if arguments . command == "raw":
    command_raw (arguments)
    return
  if arguments . command == "import":
    command_import (arguments)
    return
  if arguments . command == "save":
    command_save (arguments)
    return
  if arguments . command == "new":
    command_new (arguments)
    return
  if arguments . command == "diff":
    command_diff (arguments)
    return
  if arguments . command == "commit":
    command_commit (arguments)
    return
  if arguments . command == "integrate":
    command_integrate (arguments)
    return
  connection = SkgConnection (arguments . port)
  try:
    if arguments . command == "view":
      view = fetch_single_root_view (connection, arguments . id)
    elif arguments . command in FOREST_COMMAND_ALIASES:
      view = fetch_forest_view (
        connection, FOREST_COMMAND_ALIASES [ arguments . command ],
        arguments . id )
    elif arguments . command == "forest":
      view = fetch_forest_view (connection, arguments . kind, arguments . id)
    elif arguments . command == "search":
      view = fetch_search_view (connection, " " . join (arguments . terms))
    require_view_has_headlines (view)
    view [ "command" ] = arguments . command
    base_name = arguments . name or default_base_name (arguments)
    written_paths = write_view_files (
      connection, view, arguments . out_dir, base_name,
      not arguments . no_summary )
    close_view_on_server (connection, view [ "view_id" ])
  finally:
    connection . close ()
  for path in written_paths:
    print (path)

def parse_command_line (argv):
  port_option = argparse . ArgumentParser ( add_help = False )
  port_option . add_argument (
    "--port", type = int,
    default = int ( os . environ . get ( "SKG_PORT", str (DEFAULT_PORT) )),
    help = "the server's port (default: SKG_PORT, else 1730)" )
  common_options = argparse . ArgumentParser (
    add_help = False, parents = [ port_option ] )
  common_options . add_argument (
    "--out-dir", default = ".",
    help = "the directory to write into (default: the current one)" )
  common_options . add_argument (
    "--name", default = None,
    help = "the files' base name (default: COMMAND_ARGUMENT)" )
  common_options . add_argument (
    "--no-summary", action = "store_true",
    help = "omit the summary block from the compact view" )
  parser = argparse . ArgumentParser (
    prog = "skg_for_ai.py",
    description = ( "A skg client: write a view as a full and a compact "
                    "Org file, or send requests directly." ))
  commands = parser . add_subparsers ( dest = "command", required = True )
  for command, help_text in [
      ( "view", "the recursive content view of a node" ),
      ( "contains", "'forest containers ID'" ),
      ( "links-here", "'forest mentioners ID'" ) ]:
    commands . add_parser (
      command, help = help_text, parents = [ common_options ]
    ) . add_argument ("id")
  forest_parser = commands . add_parser (
    "forest", help = ( "a node's view with the folder KIND under its "
                       "root, as the clients' 'C-c l' commands draw it" ),
    parents = [ common_options ] )
  forest_parser . add_argument ( "kind", choices = list (FOREST_VIEW_REQUESTS) )
  forest_parser . add_argument ("id")
  commands . add_parser (
    "search", help = "the search buffer for TERMS",
    parents = [ common_options ] ) . add_argument ( "terms", nargs = "+" )
  commands . add_parser (
    "diff", help = ( "the diff view's nonempty buffers: every change in "
                     "the owned skgrepos since their last git-commits" ),
    parents = [ common_options ] )
  integrate_parser = commands . add_parser (
    "integrate", help = ( "make the changes a buffer of the diff view does "
                          "not make editable editable there, and write it "
                          "again" ))
  integrate_parser . add_argument (
    "view_files",
    help = ( "the BASE.view.json of a buffer of the diff view, or a "
             "directory holding only one" ))
  integrate_parser . add_argument (
    "--port", type = int, default = None,
    help = "the server's port; it must be the one the sidecar records" )
  commit_parser = commands . add_parser (
    "commit", help = ( "stage and git-commit every change in the owned "
                       "skgrepos, with MESSAGE" ),
    parents = [ port_option ] )
  commit_parser . add_argument ( "message" )
  commit_parser . add_argument (
    "--yes", action = "store_true",
    help = ( "answer yes when the server asks to confirm, because not "
             "every change is ready or something is already staged" ))
  commands . add_parser (
    "close", help = "send 'close view' for a sidecar's view ID",
    parents = [ common_options ] ) . add_argument ( "sidecar" )
  raw_parser = commands . add_parser (
    "raw", help = "send one request line; print every response",
    parents = [ port_option ] )
  raw_parser . add_argument ( "request", help = "the request, one sexp" )
  raw_parser . add_argument (
    "--saved-buffer", metavar = "FILE", default = None,
    help = ( "send FILE's text as the length-prefixed body a "
             "'save buffer' request carries, and read up to the "
             "save's terminal response" ))
  new_parser = commands . add_parser (
    "new", help = ( "create a root node TITLE in the owned skgrepo REPO, "
                    "and write its view" ),
    parents = [ common_options ] )
  new_parser . add_argument ( "title" )
  new_parser . add_argument ( "repo", help = "an owned skgrepo's name" )
  save_parser = commands . add_parser (
    "save", help = ( "save an edited compact view: print the changes, "
                     "and with '--yes' send them" ))
  save_parser . add_argument (
    "view_files",
    help = ( "the BASE.view.json of the view, or a directory holding "
             "only one" ))
  save_parser . add_argument (
    "--port", type = int, default = None,
    help = "the server's port; it must be the one the sidecar records" )
  save_parser . add_argument (
    "--yes", action = "store_true",
    help = ( "save, unless the report finds an error; this also answers "
             "yes to the question asked before unmarking several changes" ))
  import_parser = commands . add_parser (
    "import", help = "preview and apply a Markdown and Org import",
    parents = [ port_option ] )
  import_parser . add_argument (
    "input_dir", help = "the directory to import, as the server sees it" )
  import_parser . add_argument ( "repo", help = "the owned destination skgrepo" )
  import_parser . add_argument (
    "--preview-only", action = "store_true",
    help = "cancel after the preview instead of applying it" )
  return parser . parse_args (argv)

def default_base_name (arguments):
  if arguments . command == "search":
    return base_name_for ( "search", " " . join (arguments . terms) )
  if arguments . command == "forest":
    return base_name_for ( "forest-" + arguments . kind, arguments . id )
  return base_name_for ( arguments . command, arguments . id )

def base_name_for (command, argument):
  return ( command + "_" +
           re . sub ( r'[^A-Za-z0-9_.-]+', "-", argument ) . strip ("-") )

def command_close (arguments):
  """Send 'close view' for the view ID a sidecar records, and mark the
  sidecar closed. Every other command already closes its own view
  before it disconnects, and the server forgets a connection's views
  when it ends, so this matters only for a view some longer-lived
  connection holds."""
  with open (arguments . sidecar, encoding = "utf-8") as sidecar_file:
    sidecar = json . load (sidecar_file)
  connection = SkgConnection (arguments . port)
  try:
    close_view_on_server (connection, sidecar [ "view_id" ])
  finally:
    connection . close ()
  sidecar [ "closed" ] = True
  write_text_file ( arguments . sidecar,
                    json . dumps (sidecar, indent = 2, ensure_ascii = False)
                    + "\n" )
  print (arguments . sidecar)

def command_raw (arguments):
  """Send ARGUMENTS.request, and with '--saved-buffer' its save body;
  print each response as the server sent it. Without a body, every
  response arriving until the server is quiet for RAW_QUIET_SECONDS."""
  connection = SkgConnection (arguments . port)
  try:
    if arguments . saved_buffer is None:
      messages = connection . raw_request (arguments . request)
    else:
      with open (arguments . saved_buffer, encoding = "utf-8") as buffer_file:
        messages = connection . raw_save (
          arguments . request, buffer_file . read () )
  finally:
    connection . close ()
  print_messages (messages)

def command_import (arguments):
  """Preview the import, then apply it, or with '--preview-only'
  cancel it, on one connection: the server binds the approval token
  to the connection that previewed. A request for a host root, sent
  when the input links to files by absolute path, is answered with
  the empty one, which leaves those links unresolved."""
  preview_fields = [ ( "action", "preview" ),
                     ( "input-directory", arguments . input_dir ),
                     ( "destination-repo", arguments . repo ) ]
  messages = []
  connection = SkgConnection (arguments . port)
  try:
    def send (fields):
      message = connection . request_text (
        request_line ( "import md and org", *fields ),
        IMPORT_TIMEOUT_SECONDS )
      messages . append (message)
      return parse_sexp (message)
    preview = send (preview_fields)
    if response_type (preview) == "import-md-and-org-host-mapping-needed":
      preview = send ( preview_fields + [ ( "host-root", "" ) ] )
    token = response_field (preview, "approval-token")
    if token is not None:
      send ( [ ( "action", "cancel" ) ] if arguments . preview_only
             else [ ( "action", "apply" ),
                    ( "approval-token", sexp_to_text (token) ) ] )
  finally:
    connection . close ()
  print_messages (messages)

def command_save (arguments):
  """Rebuild the full buffer from the edited compact view and print the
  changes it makes. With '--yes', and when the report finds no bad
  handle, open the view again on the save's connection, send the
  save, print the server's answer and write its re-rendered view over
  the three files. The save carries the generation the view was
  rendered at, so the server refuses it if a node it would write
  changed since. A refused save leaves the files as they were."""
  saved_view = load_saved_view (arguments . view_files)
  if ( arguments . port is not None
       and arguments . port != saved_view . sidecar [ "port" ] ):
    raise SkgToolError (
      f"The view came from the server on port {saved_view . sidecar [ 'port' ]}, "
      f"not {arguments . port}." )
  edited = parse_edited_compact_view (saved_view . compact_text)
  original = outline_of_full_view (saved_view . parsed_view)
  errors = handle_error_lines (edited, saved_view . headline_of_handle)
  changes = [] if errors else change_report_lines (
    edited, original, saved_view . headline_of_handle )
  connection = SkgConnection ( saved_view . sidecar [ "port" ] )
  try:
    print ( "Changes in " + os . path . basename (saved_view . compact_path) +
            ", against " + os . path . basename (saved_view . full_path) +
            " (view ID " + saved_view . sidecar [ "view_id" ] + "):" )
    for line in errors + (
        changes or ( [] if errors else [ "No changes." ] )):
      print (line)
    if errors:
      raise SkgToolError (
        "Nothing was saved: some handles are in error; see above." )
    if not changes:
      print ( "Nothing to save." )
      return
    if not arguments . yes:
      print ( "Not saved. Run again with '--yes' to save." )
      return
    buffer_text = rebuilt_full_buffer (
      edited, saved_view . headline_of_handle, saved_view . full_text )
    held_viewnode_ids = reopen_view_for_save (connection, saved_view . sidecar)
    renumbered = [ handle for handle in dict . fromkeys (
                     headline . handle for headline in edited
                     if headline . handle is not None )
                   if handle not in held_viewnode_ids ]
    if renumbered:
      print ( "Handles the save may change: " + ", " . join (
                described_handle ( handle, saved_view . headline_of_handle )
                for handle in renumbered ) +
              ". The view the server opened again does not hold these "
              "viewnode IDs, so it gives each headline a new one." )
    messages = connection . save_buffer (
      saved_view . sidecar [ "view_id" ], buffer_text,
      saved_view . sidecar [ "rendered_at" ] )
    rerendered = rerendered_view_of_save ( messages [-1], saved_view . sidecar )
    written_paths = write_view_files (
      connection, rerendered, saved_view . directory, saved_view . base_name,
      saved_view . sidecar [ "summary" ] )
    close_view_on_server (connection, rerendered [ "view_id" ])
  finally:
    connection . close ()
  for path in written_paths:
    print (path)

def reopen_view_for_save (connection, sidecar, outcome = "Nothing was saved"):
  """Open the sidecar's view again on CONNECTION, under its view ID,
  as the command that wrote it opened it, so that the server holds it
  as it holds a client's open buffer; return the viewnode IDs of the
  view it now holds. The server keeps a view per connection, and the
  tool's connection ends with each command. A save keeps every
  viewnode ID of the saved buffer that the server's view of it holds,
  and gives a fresh one to every other headline; and it re-renders as
  a buffer of the diff view, with the readiness summary, only a view
  the server holds as one. The save itself is read from the saved
  text, against the generation the sidecar records. OUTCOME opens the
  refusal when a buffer of the diff view is now empty."""
  request = sidecar [ "request" ]
  if "buffer" in request:
    view = fetch_diff_view_buffer (
      connection, request [ "buffer" ], sidecar [ "view_id" ] )
    if view is None:
      raise SkgToolError (
        f"{outcome}: the diff view's '{request [ 'buffer' ]}' buffer is "
        "empty now (its changes were committed or undone); run 'diff' again." )
  elif "terms" in request:
    view = fetch_search_view ( connection, request [ "terms" ] )
  elif "view_request" in request:
    view = fetch_view_with_root_folder_request (
      connection, request [ "id" ], request [ "view_request" ] )
  else:
    view = fetch_single_root_view ( connection, request [ "id" ] )
  if view [ "view_id" ] != sidecar [ "view_id" ]:
    raise SkgToolError (
      f"{outcome}: opening the view again gave the view ID "
      f"{view [ 'view_id' ]}, not the sidecar's {sidecar [ 'view_id' ]}." )
  return set ( headlines_by_viewnode_id (
    parse_full_view ( view [ "content" ] )) )

def command_integrate (arguments):
  """Send "integrate unshown changes" for a buffer of the diff view,
  as the clients' 'C-c r u' does, and write the buffer the server
  rerenders over the three files. A client sends it only for a buffer
  with no unsaved changes, so this refuses an edited compact view. The
  server works on the buffer it registered under the view ID, so the
  full view is first registered there: the buffer is opened again
  under that ID and the full view saved back unchanged, which writes
  nothing and leaves the server holding this buffer, its roots and
  what it makes editable; its readiness summary is printed as the one
  before integrating. The server refuses that save if a node the
  buffer shows changed since it was rendered; 'diff' then writes a
  fresh buffer, which makes every change editable."""
  saved_view = load_saved_view (arguments . view_files)
  sidecar = saved_view . sidecar
  if sidecar [ "command" ] != "diff":
    raise SkgToolError (
      "Nothing was integrated: " + os . path . basename (saved_view . full_path) +
      " is not a buffer of the diff view." )
  if ( arguments . port is not None
       and arguments . port != sidecar [ "port" ] ):
    raise SkgToolError (
      f"The view came from the server on port {sidecar [ 'port' ]}, "
      f"not {arguments . port}." )
  edited = parse_edited_compact_view (saved_view . compact_text)
  if ( handle_error_lines (edited, saved_view . headline_of_handle)
       or change_report_lines (
         edited, outline_of_full_view (saved_view . parsed_view),
         saved_view . headline_of_handle )):
    raise SkgToolError (
      "Nothing was integrated: the compact view has edits; save them "
      "first, or write the buffer again with 'diff'." )
  connection = SkgConnection ( sidecar [ "port" ] )
  try:
    reopen_view_for_save (connection, sidecar, "Nothing was integrated")
    terminal = connection . save_buffer (
      sidecar [ "view_id" ], saved_view . full_text,
      sidecar [ "rendered_at" ] ) [-1]
    if ( response_type (terminal) != "save-result"
         or not isinstance ( response_field (terminal, "content"), str )
         or isinstance ( response_field (terminal, "content"), Symbol )):
      raise SkgToolError (
        "Nothing was integrated: the server refused this buffer as it "
        "stands; run 'diff' for a fresh one, which makes every change "
        "editable. The server said: " + " " . join (
          response_field (terminal, "errors") or [ render_sexp (terminal) ] ))
    print ( "Before integrating: " + readiness_summary_text (
      response_field (terminal, "readiness-summary") ))
    response, rerendered = connection . integrate_unshown_changes (
      sidecar [ "view_id" ] )
    errors = response_field (response, "errors") or []
    for error in errors:
      print ( "Server error: " + error )
    if errors or rerendered is None:
      raise SkgToolError (
        "Nothing was integrated; the files are as they were." )
    readiness_summary = response_field (response, "readiness-summary")
    print ( "Integrated: " + readiness_summary_text (readiness_summary) )
    view = { "view_id"     : sidecar [ "view_id" ],
             "command"     : sidecar [ "command" ],
             "request"     : sidecar [ "request" ],
             "content"     : sexp_to_text ( response_field (rerendered, "content") ),
             "rendered_at" : rendered_at_of (rerendered),
             "errors"      : [],
             "warnings"    : response_field (response, "warnings") or [],
             "readiness_summary" : readiness_summary }
    written_paths = write_view_files (
      connection, view, saved_view . directory, saved_view . base_name,
      sidecar [ "summary" ] )
    close_view_on_server (connection, view [ "view_id" ])
  finally:
    connection . close ()
  for path in written_paths:
    print (path)

def command_new (arguments):
  """Create a root node titled ARGUMENTS.title whose home is the
  owned skgrepo ARGUMENTS.repo, by saving a buffer holding only its
  headline, as a client saves a new empty view ('C-c v e' in Emacs).
  Then write the new node's view as 'view' does, named by default
  'view_ID', so the edit-and-save loop continues from it."""
  title = arguments . title . strip ()
  if not title or "\n" in title:
    raise SkgToolError ( "A title is one nonempty line." )
  if not re . fullmatch ( r'[^\s()";]+', arguments . repo ):
    raise SkgToolError (
      f"'{arguments . repo}' is not a skgrepo name this command can "
      "write into metadata." )
  connection = SkgConnection (arguments . port)
  try:
    ownership = fetch_repo_ownership (connection)
    if ownership . get (arguments . repo) is not True:
      raise SkgToolError (
        f"'{arguments . repo}' is not an owned skgrepo of this server; "
        "its owned ones are: " +
        ", " . join ( name for name, owned in ownership . items ()
                      if owned ) + "." )
    creating_view_id = "new-root:" + str ( uuid . uuid4 () )
    messages = connection . save_buffer (
      creating_view_id,
      "* (skg node (skgRepos (nodeRepo " + arguments . repo + "))) " +
      title + "\n",
      # No view response drew this buffer, as none draws a client's
      # new empty view; like one, it is dated by the generation the
      # server answers when it is opened.
      fetch_runtime_generation (connection) )
    rerendered = rerendered_view_of_save (
      messages [-1], { "view_id" : creating_view_id, "command" : "new",
                       "request" : {} } )
    close_view_on_server (connection, creating_view_id)
    new_root = next ( ( line for line in
                        parse_full_view ( rerendered [ "content" ] )
                        if line . is_headline ), None )
    node_id = ( headline_gnid (new_root . metadata)
                if new_root is not None and new_root . metadata is not None
                else None )
    if node_id is None:
      raise SkgToolError (
        "The save succeeded, but its re-rendered view names no new node." )
    view = fetch_single_root_view (connection, node_id)
    require_view_has_headlines (view)
    view [ "command" ] = "view"
    written_paths = write_view_files (
      connection, view, arguments . out_dir,
      arguments . name or base_name_for ( "view", node_id ),
      not arguments . no_summary )
    close_view_on_server (connection, view [ "view_id" ])
  finally:
    connection . close ()
  print ( "New node: " + node_id )
  for path in written_paths:
    print (path)

def command_diff (arguments):
  """Request each buffer of the diff view in the clients' order, on
  one connection, and write each nonempty one as 'view' writes a view,
  named BASE_BUFFER ('diff_all-changes' and so on), its compact
  summary opening with the readiness summary the server's answer
  carries. Each buffer is closed before the next is requested."""
  connection = SkgConnection (arguments . port)
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
      close_view_on_server (connection, view [ "view_id" ])
  finally:
    connection . close ()
  if not written_paths:
    print ( "The diff view is empty: nothing changed since the last commits." )
  for path in written_paths:
    print (path)

def command_commit (arguments):
  """Send "stage and commit" with the message. When the server asks
  for confirmation (not every changed file is ready, or some owned
  gitrepo has entries already staged), print why and ask for a typed
  "yes", which '--yes' gives; on yes, resend it confirmed. Print what
  was committed."""
  if not arguments . message . strip ():
    raise SkgToolError ( "A commit message is required; nothing was sent." )
  connection = SkgConnection (arguments . port)
  try:
    response = connection . stage_and_commit (arguments . message, False)
    if response_field (response, "needs-confirmation") is not None:
      print ( "Not every change is ready to commit: " +
              readiness_summary_text (
                response_field (response, "readiness-summary") ))
      already_staged = response_field (response, "already-staged")
      for gitrepo_and_paths in already_staged or []:
        print ( "Already staged in " + sexp_to_text ( gitrepo_and_paths [0] )
                + ", so committed too: " +
                ", " . join ( sexp_to_text (path)
                              for path in gitrepo_and_paths [ 1 : ] ))
      if not ( arguments . yes or typed_yes ( "Type yes to commit anyway: " )):
        print ( "Nothing was committed." )
        return
      response = connection . stage_and_commit (arguments . message, True)
  finally:
    connection . close ()
  commits = response_field (response, "commits") or []
  errors = response_field (response, "errors") or []
  print ( "Committed in " + ", " . join (
            sexp_to_text ( commit [0] ) + " (" + sexp_to_text ( commit [1] ) + ")"
            for commit in commits ) + "."
          if commits else "Nothing was committed." )
  for error in errors:
    print ( "Server error: " + error )
  for warning in response_field (response, "warnings") or []:
    print ( "Server warning: " + warning )
  if errors:
    raise SkgToolError ( "The server reported errors; see above." )

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

def print_messages (messages):
  for index, message in enumerate (messages):
    print ( f"--- message {index} ({len (message)} chars) ---" )
    print (message)

# ============================================================
# Fetching views
# ============================================================

def require_view_has_headlines (view):
  """A search with no match answers with a message, and a view of an
  ID the graph lacks with an empty buffer: neither is a view."""
  content = view [ "content" ] or ""
  if not any ( HEADLINE_PATTERN . match (line)
               for line in content . split ("\n") ):
    raise SkgToolError (
      ( "The server returned no view: " + content . strip ()
        if content . strip ()
        else "The server returned an empty view; the graph may lack that ID." )
      + ( " Errors: " + json . dumps ( view [ "errors" ], ensure_ascii = False )
          if view [ "errors" ] else "" ))

def fetch_single_root_view (connection, node_id):
  view_id = "content-view:" + node_id
  response = connection . request (
    request_line ( "single root content view",
                   ( "id", node_id ), ( "view-id", view_id ) ))
  require_response_type (response, "content-view")
  return { "view_id"     : view_id,
           "request"     : { "id" : node_id },
           "content"     : response_field (response, "content"),
           "rendered_at" : rendered_at_of (response),
           "errors"      : response_field (response, "errors") or [],
           "warnings"    : response_field (response, "warnings") or [] }

def fetch_search_view (connection, terms):
  response = connection . request (
    request_line ( "text search", ( "terms", terms ) ))
  require_response_type (response, "search-results")
  return { "view_id"     : "search:" + terms,
           "request"     : { "terms" : terms },
           "content"     : response_field (response, "content") or "",
           "rendered_at" : rendered_at_of (response),
           "errors"      : [],
           "warnings"    : response_field (response, "warnings") or [] }

def fetch_diff_view_buffer (connection, buffer_name, view_id = None):
  """The diff view's buffer BUFFER_NAME, registered on CONNECTION under
  VIEW_ID (a fresh one when None), or None when the server answers
  that it would be empty. A refusal raises: it applies to every
  buffer."""
  view_id = view_id or "diff-view:" + str ( uuid . uuid4 () )
  response = connection . request (
    request_line ( "diff view", ( "buffer", buffer_name ),
                   ( "view-id", view_id ) ))
  require_response_type (response, "diff-view")
  errors = response_field (response, "errors") or []
  if response_field (response, "empty") is not None:
    return None
  content = response_field (response, "content")
  if errors or not content:
    raise SkgToolError (
      "The server refused the diff view: " + " " . join (errors) +
      " (The diff view needs a connection with no skgrepo restriction, "
      "and each owned skgrepo in a gitrepo with a HEAD commit.)" )
  return { "view_id"     : view_id,
           "command"     : "diff",
           "request"     : { "buffer" : buffer_name },
           "content"     : content,
           "rendered_at" : rendered_at_of (response),
           "errors"      : [],
           "warnings"    : response_field (response, "warnings") or [],
           "readiness_summary" : response_field (
             response, "readiness-summary" ) }

def fetch_forest_view (connection, kind, node_id):
  """The node's view with the folder KIND under its root, as the
  clients' 'C-c l' command for KIND draws it. A fresh content view
  already holds the root's containers folder, so for 'containers' it
  is that view, and no save is sent."""
  view_request = FOREST_VIEW_REQUESTS [kind]
  if view_request is None:
    return fetch_single_root_view (connection, node_id)
  return fetch_view_with_root_folder_request (connection, node_id, view_request)

def fetch_view_with_root_folder_request (connection, node_id, view_request):
  """Open the node's view, then save it unchanged but for a
  '(viewRequests VIEW_REQUEST)' on its root, as the clients' 'C-c l'
  commands do. The save's re-rendered buffer is the view. Nothing in
  the buffer differs from the graph, so the save writes no file."""
  fresh_view = fetch_single_root_view (connection, node_id)
  requesting_buffer = buffer_with_view_request_on_first_headline (
    fresh_view [ "content" ], view_request )
  messages = connection . save_buffer (
    fresh_view [ "view_id" ], requesting_buffer, fresh_view [ "rendered_at" ] )
  terminal = messages [-1]
  require_response_type (terminal, "save-result")
  content = response_field (terminal, "content")
  errors = response_field (terminal, "errors") or []
  if not content:
    raise SkgToolError ( "The save that requests the folder failed:\n" +
                         "\n" . join (errors) )
  return { "view_id"     : fresh_view [ "view_id" ],
           "request"     : { "id" : node_id, "view_request" : view_request },
           "content"     : content,
           "rendered_at" : rendered_at_of (terminal),
           "errors"      : errors,
           "warnings"    : response_field (terminal, "warnings") or [] }

def buffer_with_view_request_on_first_headline (buffer_text, view_request):
  """BUFFER_TEXT with '(viewRequests VIEW_REQUEST)' in the metadata of
  its first headline, where the server writes it: before '(view ...)'
  and '(diff ...)', or else last."""
  lines = buffer_text . split ("\n")
  index = next ( i for i, line in enumerate (lines)
                 if HEADLINE_PATTERN . match (line) )
  span = metadata_span_of_headline (lines [index])
  if span is None:
    raise SkgToolError ( "The view's first headline has no metadata." )
  start, end = span
  line = lines [index]
  lines [index] = (
    line [ : start ] +
    metadata_text_with_part (
      line [ start : end ], "(viewRequests " + view_request + ")",
      ( "view", "diff" )) +
    line [ end : ] )
  return "\n" . join (lines)

def metadata_text_with_part (metadata_text, part_text, following_part_heads):
  """METADATA_TEXT, a '(skg ...)', with PART_TEXT inserted where the
  server writes it: before the first part headed by one of
  FOLLOWING_PART_HEADS, or else last."""
  for element_start, element in top_level_elements_with_offsets (metadata_text):
    if ( isinstance (element, list) and element
         and element [0] in following_part_heads ):
      return ( metadata_text [ : element_start ] + part_text + " " +
               metadata_text [ element_start : ] )
  return metadata_text [ : -1 ] + " " + part_text + ")"

def close_view_on_server (connection, view_id):
  connection . request (
    request_line ( "close view", ( "view-id", view_id ) ))

# ============================================================
# Writing the files
# ============================================================

def write_view_files (connection, view, out_dir, base_name, show_summary):
  os . makedirs (out_dir, exist_ok = True)
  full_path    = os . path . join (out_dir, base_name + ".full.org")
  compact_path = os . path . join (out_dir, base_name + ".compact.org")
  sidecar_path = os . path . join (out_dir, base_name + ".view.json")
  full_text = view [ "content" ]
  parsed_view = parse_full_view (full_text)
  link_statuses = fetch_link_statuses (connection, parsed_view)
  repo_ownership = fetch_repo_ownership (connection)
  herald_tables = fetch_herald_tables (connection)
  compact_lines = []
  if show_summary:
    compact_lines += summary_lines (
      parsed_view, link_statuses, repo_ownership, view,
      os . path . basename (full_path) )
  compact_lines += compact_tree_lines (
    parsed_view, link_statuses, herald_tables )
  write_text_file (full_path, full_text)
  write_text_file (compact_path, "\n" . join (compact_lines) + "\n")
  write_text_file (
    sidecar_path,
    json . dumps (
      { "view_id"      : view [ "view_id" ],
        "command"      : view [ "command" ],
        "request"      : view [ "request" ],
        "port"         : connection . port,
        "full_view"    : os . path . basename (full_path),
        "compact_view" : os . path . basename (compact_path),
        "rendered_at"  : view [ "rendered_at" ],
        "errors"       : view [ "errors" ],
        "warnings"     : view [ "warnings" ],
        "summary"      : show_summary,
        "closed"       : True },
      indent = 2, ensure_ascii = False ) + "\n" )
  return [ full_path, compact_path, sidecar_path ]

def write_text_file (path, text):
  with open (path, "w", encoding = "utf-8") as output_file:
    output_file . write (text)

# ============================================================
# The full view, parsed
# ============================================================

class ViewLine:
  """One line of the full view. A headline has 'stars', 'title' (the
  text after its metadata), 'metadata' (the parsed '(skg ...)', or
  None) and 'metadata_text' (its text); a body line has only 'text'."""
  def __init__ (self, line_number, text):
    self . line_number = line_number
    self . text = text
    self . is_headline = False
    self . stars = None
    self . title = None
    self . metadata = None
    self . metadata_text = None

def parse_full_view (full_text):
  lines = full_text . split ("\n")
  if lines and lines [-1] == "":
    lines = lines [ : -1 ]
  parsed = []
  for index, text in enumerate (lines):
    view_line = ViewLine (index + 1, text)
    match = HEADLINE_PATTERN . match (text)
    if match:
      view_line . is_headline = True
      view_line . stars = match . group (1)
      span = metadata_span_of_headline (text)
      if span is None:
        view_line . title = match . group (2)
      else:
        view_line . metadata_text = text [ span [0] : span [1] ]
        view_line . metadata = parse_sexp (view_line . metadata_text)
        view_line . title = text [ span [1] : ] . lstrip (" ")
    parsed . append (view_line)
  return parsed

def metadata_span_of_headline (headline_text):
  """The [start, end) offsets of the '(skg ...)' right after the stars
  and a space, or None."""
  match = HEADLINE_PATTERN . match (headline_text)
  if not match:
    return None
  start = match . start (2)
  if headline_text [ start : start + 5 ] not in ( "(skg ", "(skg)" ):
    # As the clients read it: a title may start "(skgrepos ...".
    return None
  end = end_of_balanced_sexp (headline_text, start)
  return ( start, end ) if end is not None else None

# ============================================================
# Facts from one headline's metadata
# ============================================================

def metadata_kinds ():
  with open ( os . path . join (SHARED_FOLDER, "metadata-fields.json"),
              encoding = "utf-8" ) as table_file:
    return json . load (table_file) [ "kinds" ]

KIND_ATOMS = { atom for kind in metadata_kinds () for atom in kind [ "atoms" ] }
FOLDER_ATOMS = { atom for kind in metadata_kinds () if kind [ "kind" ] == "folder"
                 for atom in kind [ "atoms" ] }

def headline_kind (metadata):
  """The kind atom's text, with any parameter ('flag hadId'), or
  'node' when the metadata names none."""
  for element in metadata [ 1 : ]:
    if isinstance (element, Symbol) and element in KIND_ATOMS:
      return str (element)
    if ( isinstance (element, list) and element
         and isinstance (element [0], Symbol) and element [0] in KIND_ATOMS ):
      return " " . join ( sexp_to_text (part) for part in element )
  return "node"

def is_folder_kind (kind):
  return kind . endswith ("Folder")

def folder_atom (metadata):
  """The folder atom of a folder headline's metadata, or None."""
  return next ( ( str (element) for element in metadata [ 1 : ]
                  if isinstance (element, Symbol) and element in FOLDER_ATOMS ),
                None )

def metadata_has_atom (metadata, atom_name):
  return any ( isinstance (element, Symbol) and element == atom_name
               for element in metadata [ 1 : ] )

def metadata_form (sexp, head_name):
  """The first element of SEXP (after its head) that is a list headed
  by HEAD_NAME, or None."""
  if not isinstance (sexp, list):
    return None
  for element in sexp [ 1 : ]:
    if ( isinstance (element, list) and element
         and isinstance (element [0], Symbol) and element [0] == head_name ):
      return element
  return None

def headline_viewnode_id (metadata):
  """The viewnode ID of '(vnid X)', the first part of every headline's
  metadata the server renders, or None."""
  form = metadata_form (metadata, "vnid") if metadata is not None else None
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def headline_gnid (metadata):
  form = metadata_form (metadata, "gnid")
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def headline_node_repo (metadata):
  skgrepos = metadata_form (metadata, "skgRepos")
  form = metadata_form (skgrepos, "nodeRepo")
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def relation_side (metadata, relation_name, side_name):
  """( COUNT, ANCESTOR_GENERATIONS ) of a relation side in the
  headline's '(view (rels ...))', or None."""
  view = metadata_form (metadata, "view")
  rels = metadata_form (view, "rels")
  side = metadata_form ( metadata_form (rels, relation_name), side_name )
  if side is None:
    return None
  return ( first_number (side), ancestor_generations (side) )

# ============================================================
# Relationship heralds, as the clients render them, styles dropped.
# The logic mirrors 'render_rel_facts' in 'nvim/lua/skg/heralds.lua'
# and 'heralds--render-rel-facts' in 'elisp/heralds-minor-mode.el';
# 'docs/heralds.org', "Relationship heralds", states the rules.
# ============================================================

def relations_in_display_order ():
  with open ( os . path . join (SHARED_FOLDER, "relations.json"),
              encoding = "utf-8" ) as relations_file:
    relations = json . load (relations_file) [ "relations" ]
  return [ ( relation [ "name" ], relation [ "letter" ] )
           for relation in sorted (
               relations, key = lambda r : r [ "display_position" ] ) ]

RELATIONS_IN_DISPLAY_ORDER = relations_in_display_order ()
# Ordinary content: the parent contains it, and nothing else does. A
# handle leaves this token out; every other form stays.
BARE_PARENT_CONTAINS_TOKEN = "aC"
PROPERTY_COUNT_LETTERS = [ ( "aliases", "A" ), ( "extraIds", "I" ),
                           ( "flags", "F" ) ]

def relationship_heralds_text (metadata):
  """The headline's relationship and property-count tokens,
  space-separated, or '' when it has none."""
  view = metadata_form (metadata, "view")
  if view is None:
    return ""
  rels = metadata_form (view, "rels")
  props = metadata_form (view, "props")
  overrides_here = metadata_form (metadata, "overridesHere") is not None
  tokens = []
  for relation_name, letter in RELATIONS_IN_DISPLAY_ORDER:
    form = metadata_form (rels, relation_name)
    if form is not None or ( relation_name == "overrides" and overrides_here ):
      token = relation_token (relation_name, letter, form, overrides_here)
      if token:
        tokens . append (token)
  for property_name, letter in PROPERTY_COUNT_LETTERS:
    form = metadata_form (props, property_name)
    if form is not None:
      tokens . append ( letter + str ( first_number (form) ))
  return " " . join (tokens)

def relation_token (relation_name, letter, form, overrides_here):
  in_side = relation_side_text (relation_name, "in", form)
  out_side = relation_side_text (relation_name, "out", form)
  here = relation_name == "overrides" and overrides_here
  if not in_side and not out_side and not here:
    return ""
  return in_side + letter + ( "ĥ" if here else "" ) + out_side

def relation_side_text (relation_name, side_name, form):
  side = metadata_form (form, side_name)
  if side is None:
    return ""
  count = first_number (side)
  generations = ancestor_generations (side)
  subset_name = ( "unintegrated"
                  if ( relation_name, side_name ) == ( "contains", "out" )
                  else "substantive"
                  if ( relation_name, side_name ) == ( "linksTo", "in" )
                  else None )
  subset = metadata_form (side, subset_name) if subset_name else None
  if subset is not None:
    return fraction_text ( count, generations, first_number (subset),
                           ancestor_generations (subset) )
  return side_text (count, generations)

def side_text (count, generations):
  if count == 0 and not generations:
    return ""
  return part_text (count, generations)

def part_text (count, generations, member_generations = None):
  """COUNT, unless the ancestor flags account for every member, then
  the flags for GENERATIONS."""
  members = distinct_generations (
    generations if member_generations is None else member_generations )
  numeral = "" if count == len (members) else str (count)
  return numeral + "" . join (
    ancestor_flag (generation)
    for generation in distinct_generations (generations) )

def fraction_text (total, total_generations, numerator, numerator_generations):
  if numerator > total:
    raise SkgToolError (
      f"herald subset {numerator} exceeds total {total}" )
  if total == 0:
    return ""
  if numerator == 0:
    return side_text (total, total_generations)
  total_generations = distinct_generations (total_generations)
  numerator_generations = distinct_generations (numerator_generations)
  remaining_generations = [ generation for generation in total_generations
                            if generation not in numerator_generations ]
  text = part_text (numerator, numerator_generations) + "/"
  if numerator != total:
    text += part_text (total, remaining_generations, total_generations)
  return text

def ancestor_flag (generation):
  if 1 <= generation <= 26:
    return chr ( ord ("a") + generation - 1 )
  return "{" + str (generation) + "}"

def distinct_generations (generations):
  return sorted ( set (generations) )

def first_number (sexp):
  for element in sexp [ 1 : ]:
    if isinstance (element, int):
      return element
  return 0

def ancestor_generations (side):
  ancestors = metadata_form (side, "ancestors")
  if ancestors is None:
    return []
  return [ element for element in ancestors [ 1 : ]
           if isinstance (element, int) ]

# ============================================================
# Links
# ============================================================

def link_target_ids_in_order (parsed_view):
  ids = []
  for view_line in parsed_view:
    for match in LINK_PATTERN . finditer ( visible_text_of_line (view_line) ):
      if match . group (1) not in ids:
        ids . append ( match . group (1) )
  return ids

def visible_text_of_line (view_line):
  """What a reader sees of the line: a headline's title, a body
  line's text."""
  return view_line . title if view_line . is_headline else view_line . text

def fetch_link_statuses (connection, parsed_view):
  """{ID: {'status': 'resolved'|'restricted'|'missing', 'pid', 'label',
  'title'}}: the 'link statuses' request the clients' link annotations
  send, then 'titles by ids' for the resolved targets' titles."""
  ids = link_target_ids_in_order (parsed_view)
  if not ids:
    return {}
  response = connection . request (
    "((request . \"link statuses\") (request-id . \"skg-for-ai\") (ids " +
    " " . join ( quote_string (target) for target in ids ) + "))" )
  require_response_type (response, "link-statuses")
  statuses = {}
  for row in response_field (response, "results") or []:
    target = sexp_to_text ( row [0] )
    status = sexp_to_text ( row [1] )
    statuses [target] = { "status" : status }
    if status == "resolved":
      statuses [target] [ "pid" ] = sexp_to_text ( row [2] )
      statuses [target] [ "label" ] = sexp_to_text ( row [3] )
  titles = fetch_titles (
    connection, sorted ({ entry [ "pid" ] for entry in statuses . values ()
                          if entry [ "status" ] == "resolved" }) )
  for entry in statuses . values ():
    if entry [ "status" ] == "resolved":
      entry [ "title" ] = titles . get ( entry [ "pid" ] )
  return statuses

def fetch_titles (connection, ids):
  if not ids:
    return {}
  response = connection . request (
    "((request . \"titles by ids\") (ids " +
    " " . join ( quote_string (target) for target in ids ) + "))" )
  if response_type (response) != "titles-by-ids":
    return {}
  titles = {}
  for pair in response_field (response, "content") or []:
    # each pair is ("ID" . "TITLE")
    titles [ sexp_to_text ( pair [0] ) ] = sexp_to_text ( pair [-1] )
  return titles

def annotate_links (text, link_statuses):
  def annotated (match):
    return match . group (0) + link_annotation (
      link_statuses . get ( match . group (1) ))
  return LINK_PATTERN . sub (annotated, text)

def link_annotation (entry):
  if entry is None or entry [ "status" ] == "missing":
    return "⟦✗ missing⟧"
  if entry [ "status" ] != "resolved":
    return "⟦✗ " + entry [ "status" ] + "⟧"
  title = entry . get ( "title" ) or "(title not found)"
  return "⟦→ " + title + " ⌂:" + entry [ "label" ] + "⟧"

# ============================================================
# Repo ownership: whether each configured skgrepo is owned or foreign,
# as the server's 'repos' request reports it.
# ============================================================

def fetch_repo_ownership (connection):
  """{REPO: True (owned) | False (foreign)}, for every skgrepo of the
  config. A skgrepo it lacks is of unknown ownership."""
  response = connection . request ( request_line ("repos") )
  require_response_type (response, "repos")
  ownership = {}
  for entry in response_field (response, "repos") or []:
    entry = [ None ] + entry
    name = sexp_to_text ( metadata_form (entry, "name") [1] )
    owned = sexp_to_text ( metadata_form (entry, "owned") [1] )
    ownership [name] = owned == "true"
  return ownership

# ============================================================
# The runtime generation, which dates a buffer no view response drew
# ============================================================

def fetch_runtime_generation (connection):
  """The server's current runtime generation, as the text N: what a
  buffer no view response drew is rendered at."""
  response = connection . request ( request_line ("runtime generation") )
  require_response_type (response, "runtime-generation")
  return sexp_to_text ( response_field (response, "content") )

# ============================================================
# The compact view
# ============================================================

def compact_tree_lines (parsed_view, link_statuses, herald_tables):
  contexts = herald_contexts (parsed_view)
  return [ compact_line ( view_line, link_statuses, herald_tables,
                          contexts . get ( view_line . line_number ))
           for view_line in parsed_view ]

def compact_line (view_line, link_statuses, herald_tables, context):
  if not view_line . is_headline:
    return annotate_links (view_line . text, link_statuses)
  title = annotate_links (view_line . title, link_statuses)
  handle = handle_text (view_line, herald_tables, context)
  return ( view_line . stars + " " + title +
           ( " " if title else "" ) + handle )

def handle_text (view_line, herald_tables, context):
  """'⟨ID HERALDS folded⟩': ID the headline's viewnode ID, HERALDS
  every herald a client shows for it but a bare 'aC', and 'folded'
  when a client shows it collapsed."""
  viewnode_id = headline_viewnode_id (view_line . metadata)
  if viewnode_id is None:
    raise SkgToolError (
      f"Line {view_line . line_number} of the view is a headline without "
      "a viewnode ID, so it can have no handle: " + view_line . text )
  parts = [ viewnode_id ]
  heralds = herald_text (
    view_line . metadata, herald_tables, context,
    omitted_relationship_tokens = ( BARE_PARENT_CONTAINS_TOKEN, ))
  if heralds:
    parts . append (heralds)
  if metadata_has_atom (view_line . metadata, "folded"):
    parts . append ("folded")
  return "⟨" + " " . join (parts) + "⟩"

# ============================================================
# Every herald, as the clients render a headline's metadata, styles
# dropped. The server serves the rule table ('herald rules'), which
# the lens below applies to the metadata; it mirrors
# 'nvim/lua/skg/sexpr/lens.lua', the port of
# 'elisp/skg-sexpr/skg-lens.el', whose docstrings give the rule
# grammar. The rule table only positions the relationship heralds,
# with a sentinel that 'relationship_heralds_text' fills. Three
# heralds need what one headline's metadata cannot say, which a
# client supplies from the headlines around it and the label table
# ('docs/heralds.org', "What the server serves, and what the clients
# add"): a 'repoBoundary' gets its skgrepo's label, a pending
# detachment the atom 'pendingDetachment', and a changed folder its
# changed label in place of the lens's. 'chunks_from_metadata' in
# 'nvim/lua/skg/heralds.lua' and 'heralds-from-metadata' in
# 'elisp/heralds-minor-mode.el' do the same.
# ============================================================

def herald_styles ():
  with open ( os . path . join (SHARED_FOLDER, "herald-styles.json"),
              encoding = "utf-8" ) as styles_file:
    return json . load (styles_file)

HERALD_STYLES = herald_styles ()
# The style keywords a rule may open with: each style's name in
# capitals, as the served rule table spells it.
STYLE_KEYWORDS = { name . upper () for name in HERALD_STYLES [ "styles" ] }
FOLDER_HERALDS = HERALD_STYLES [ "folder_heralds" ]
# What the rule table emits where the relationship heralds go
# ('RELS_SPANS_SENTINEL' in 'server/heralds.rs').
RELATIONSHIP_HERALDS_SENTINEL = "__RELS_SPANS__"

class HeraldTables:
  """The two tables the server serves over 'herald rules': the rule
  table, parsed, and {REPO: LABEL}, each configured skgrepo's herald
  label."""
  def __init__ (self, rules, skgrepo_labels):
    self . rules = rules
    self . skgrepo_labels = skgrepo_labels

def fetch_herald_tables (connection):
  """The connection's HeraldTables, fetched on its first use, so once
  per command."""
  if connection . herald_tables is None:
    response = connection . request ( request_line ("herald rules") )
    require_response_type (response, "herald-rules")
    connection . herald_tables = herald_tables_of_response (response)
  return connection . herald_tables

def herald_tables_of_response (response):
  """The HeraldTables of a 'herald rules' response: its
  '(content "RULES")' and '(skgrepoLabels ("REPO" "LABEL")...)'."""
  labels = metadata_form ( [ None ] + response, "skgrepoLabels" ) or [ None ]
  return HeraldTables (
    parse_sexp ( response_field (response, "content") ),
    { sexp_to_text ( entry [0] ) : sexp_to_text ( entry [1] )
      for entry in labels [ 1 : ] } )

class HeraldContext:
  """What a headline's heralds need from the headlines around it.
  'folder_change' is None, or, for a folder a member of which carries
  a change of its relationship to the folder's viewparent, whether
  every such change is ready; 'pending_detachment' whether the
  headline is a pending detachment."""
  def __init__ (self, folder_change = None, pending_detachment = False):
    self . folder_change = folder_change
    self . pending_detachment = pending_detachment

def herald_text (metadata, herald_tables, context = HeraldContext (),
                 omitted_relationship_tokens = ()):
  """The heralds a client shows for METADATA, the parsed '(skg ...)',
  as text: tokens separated by a space, but those the rule table
  marks ABUT, which join the one before. OMITTED_RELATIONSHIP_TOKENS
  are relationship tokens left out."""
  folder = folder_atom (metadata)
  if ( context . folder_change is not None and folder is not None
       and folder in FOLDER_HERALDS [ "changed_labels" ] ):
    return changed_folder_herald_text (folder, context . folder_change)
  lensed = metadata_with_repoBoundary_label (
    metadata, herald_tables . skgrepo_labels )
  if context . pending_detachment:
    lensed = lensed + [ Symbol ("pendingDetachment") ]
  relationship_text = " " . join (
    token for token in relationship_heralds_text (metadata) . split ()
    if token not in omitted_relationship_tokens )
  return lens_tokens_as_text (
    lens_tokens (lensed, herald_tables . rules), relationship_text )

def changed_folder_herald_text (folder, ready):
  """A changed folder's herald: its ☮ when it is write-protected, its
  changed label, then ✓ when READY."""
  return ( ( "☮ " if folder in FOLDER_HERALDS [ "write_protected" ] else "" )
           + FOLDER_HERALDS [ "changed_labels" ] [folder]
           + ( "✓" if ready else "" ))

def metadata_with_repoBoundary_label (metadata, skgrepo_labels):
  """METADATA with the atom 'repoBoundary' in its '(view ...)'
  replaced by '(repoBoundary LABEL)', LABEL the label SKGREPO_LABELS
  gives its nodeRepo, or that nodeRepo when the table lacks it. A
  headline naming no nodeRepo keeps the bare atom, which renders "⌂"."""
  repo = headline_node_repo (metadata)
  if repo is None:
    return metadata
  label = skgrepo_labels . get (repo, repo)
  return [ [ [ Symbol ("repoBoundary"), label ]
             if is_symbol_named (fact, "repoBoundary") else fact
             for fact in part ]
           if is_list_headed_by (part, "view") else part
           for part in metadata ]

def lens_tokens_as_text (tokens, relationship_text):
  """TOKENS joined as a client shows them: separated by a space, but a
  token marked abut, which joins the one before; structural colons
  stripped. The first sentinel token becomes RELATIONSHIP_TEXT, and
  any later one is dropped (the rule table emits one for '(rels ...)'
  and one for '(props ...)')."""
  pieces = []
  relationship_text_placed = False
  for text, abut in tokens:
    if text == RELATIONSHIP_HERALDS_SENTINEL:
      if not relationship_text or relationship_text_placed:
        continue
      text = relationship_text
      relationship_text_placed = True
    else:
      text = without_structural_colons (text)
      if not text:
        continue
    if pieces and not abut:
      pieces . append (" ")
    pieces . append (text)
  return "" . join (pieces)

def without_structural_colons (text):
  """TEXT without the colons the lens joins atoms with where they
  mean nothing: a colon is structural when the character before or
  after it is not alphanumeric, '+', '-' or a space, except a colon
  after '⌂' ("⌂:LABEL") or before '~' ("request:~NAME").
  ('strip_structural_colons' in 'nvim/lua/skg/heralds.lua'.)"""
  def keeps_adjacent_colon (character):
    return character in "+ -" or character . isalnum ()
  kept = []
  index = 0
  while index < len (text):
    previous = text [ index - 1 ] if index > 0 else None
    this = text [index]
    following = text [ index + 1 ] if index + 1 < len (text) else None
    if ( following == ":" and this not in ":⌂"
         and not keeps_adjacent_colon (this) ):
      kept . append (this)
      index += 2
    elif ( this == ":" and following is not None and previous != "⌂"
           and following != "~" and not keeps_adjacent_colon (following) ):
      kept . append (following)
      index += 2
    else:
      kept . append (this)
      index += 1
  return "" . join (kept)

# ------------------------------------------------------------
# The lens: a rule table applied to a sexp. A token is ( TEXT, ABUT ).
# ------------------------------------------------------------

def lens_tokens (sexp, rules):
  """RULES applied to SEXP, when RULES' label is SEXP's head."""
  if not isinstance (sexp, list) or not isinstance (rules, list):
    return []
  header = lens_rule_header (rules)
  if not sexp or not sexp_values_equal ( sexp [0], header [ "label" ] ):
    return []
  return lens_transform_from (sexp, header)

def lens_rule_header (rule):
  """RULE parsed:
    RULE        ::= WHEN-BODY | [STYLE] (INTERC-BODY | SIMPLE-BODY)
    WHEN-BODY   ::= WHEN LABEL... CHILDREN...
    INTERC-BODY ::= INTERC SEP [LABEL] CHILDREN...
    SIMPLE-BODY ::= LABEL [ABUT] CHILDREN...
  A simple body is 'non_leaf' when a child is a list, else 'any_leaf'
  when its label is ANY, else 'atomic_leaf'."""
  index = 0
  if is_symbol_named ( rule [index], "WHEN" ):
    index += 1
    labels = []
    while index < len (rule) and isinstance ( rule [index], Symbol ):
      labels . append ( rule [index] )
      index += 1
    return { "kind" : "when", "labels" : labels, "label" : None,
             "abut" : False, "children" : rule [ index : ] }
  if isinstance ( rule [index], Symbol ) and rule [index] in STYLE_KEYWORDS:
    index += 1
  if is_symbol_named ( rule [index], "INTERC" ):
    separator = rule [ index + 1 ]
    index += 2
    label = None
    if ( index < len (rule) and isinstance ( rule [index], Symbol )
         and rule [index] not in STYLE_KEYWORDS ):
      label = rule [index]
      index += 1
    return { "kind" : "interc", "separator" : separator, "label" : label,
             "abut" : False, "children" : rule [ index : ] }
  label = rule [index]
  index += 1
  abut = index < len (rule) and is_symbol_named ( rule [index], "ABUT" )
  if abut:
    index += 1
  children = rule [ index : ]
  kind = ( "non_leaf" if any ( isinstance (child, list) for child in children )
           else "any_leaf" if is_symbol_named (label, "ANY")
           else "atomic_leaf" )
  return { "kind" : kind, "label" : label, "abut" : abut,
           "children" : children }

def lens_transform_from (sexp, header):
  """The tokens of HEADER's list children applied to SEXP, each after
  the concatenated string children; the string children alone when
  no list child gives a token."""
  prefix = lens_string_children (header [ "children" ])
  results = [ token for child in header [ "children" ]
              if isinstance (child, list)
              for token in lens_dispatch (sexp, child) ]
  if not prefix:
    return results
  if not results:
    return [ ( prefix, False ) ]
  return [ ( prefix + text, False ) for text, abut in results ]

def lens_dispatch (sexp, rule_child):
  header = lens_rule_header (rule_child)
  kind = header [ "kind" ]
  if kind == "when":
    # The children apply to SEXP itself when an element of SEXP is
    # one of the labels, or a list headed by one.
    return ( lens_transform_from (sexp, header)
             if any ( lens_atomic_matches (sexp, label)
                      or lens_children_with_label (sexp, label)
                      for label in header [ "labels" ] )
             else [] )
  if kind == "interc":
    return lens_interc_tokens (sexp, header)
  if kind == "any_leaf":
    return lens_any_leaf_tokens (sexp, header)
  if kind == "atomic_leaf":
    if not header [ "children" ]:
      return []
    return [ ( lens_joined_text ( header [ "children" ] ), header [ "abut" ] )
             for _ in lens_atomic_matches ( sexp, header [ "label" ] ) ]
  return [ token
           for match in lens_children_with_label ( sexp, header [ "label" ] )
           for token in lens_transform_from (match, header) ]

def lens_interc_tokens (sexp, header):
  """With a label, a token per child of SEXP headed by it; without, at
  most one, of SEXP itself. A token joins with the separator the
  nonempty outputs of the list sub-rules, after the string children;
  it is left out when every output is empty."""
  prefix = lens_string_children (header [ "children" ])
  separator = lens_display_text ( header [ "separator" ] )
  targets = ( lens_children_with_label ( sexp, header [ "label" ] )
              if header [ "label" ] is not None else [ sexp ] )
  results = []
  for target in targets:
    slots = [ "" . join ( text for text, abut in lens_dispatch (target, child) )
              for child in header [ "children" ] if isinstance (child, list) ]
    if any (slots):
      results . append (
        ( prefix + separator . join ( slot for slot in slots if slot ),
          False ) )
  return results

def lens_any_leaf_tokens (sexp, header):
  """'(ANY CHILD...)': one token of the children, or, when a child is
  IT, a token per element of SEXP after its head, that element in
  IT's place."""
  children = header [ "children" ]
  if not children:
    return []
  if not any ( is_symbol_named (child, "IT") for child in children ):
    return [ ( lens_joined_text (children), header [ "abut" ] ) ]
  return [ ( lens_joined_text (
               [ element if is_symbol_named (child, "IT") else child
                 for child in children ] ),
             header [ "abut" ] )
           for element in sexp [ 1 : ] ]

def lens_joined_text (values):
  return ":" . join ( lens_display_text (value) for value in values )

def lens_display_text (value):
  return render_sexp (value) if isinstance (value, list) else str (value)

def lens_string_children (children):
  return "" . join ( child for child in children
                     if isinstance (child, str)
                     and not isinstance (child, Symbol) )

def lens_children_with_label (sexp, label):
  return [ element for element in sexp [ 1 : ]
           if isinstance (element, list) and element
           and sexp_values_equal ( element [0], label ) ]

def lens_atomic_matches (sexp, label):
  return [ element for element in sexp [ 1 : ]
           if not isinstance (element, list)
           and sexp_values_equal (element, label) ]

def sexp_values_equal (left, right):
  """Equality of parsed sexps, a symbol never equal to a string."""
  if isinstance (left, list) or isinstance (right, list):
    return ( isinstance (left, list) and isinstance (right, list)
             and len (left) == len (right)
             and all ( sexp_values_equal (l, r)
                       for l, r in zip (left, right) ))
  return type (left) is type (right) and left == right

def is_symbol_named (value, name):
  return isinstance (value, Symbol) and value == name

def is_list_headed_by (value, name):
  return isinstance (value, list) and bool (value) and is_symbol_named (
    value [0], name )

# ------------------------------------------------------------
# What a headline's heralds need from the headlines around it, read
# from the full view as a client reads it from its buffer
# ('folder_change_readiness_in_lines' and
# 'pending_detachment_in_lines' in 'nvim/lua/skg/metadata.lua').
# ------------------------------------------------------------

def herald_contexts (parsed_view):
  """{LINE_NUMBER: HeraldContext} for each headline of the full view
  that has metadata."""
  headlines = [ line for line in parsed_view if line . is_headline ]
  parents = parent_indices_by_level (
    [ len ( line . stars ) for line in headlines ] )
  def metadata_at (index):
    return None if index is None else headlines [index] . metadata
  contexts = {}
  for index, line in enumerate (headlines):
    if line . metadata is None:
      continue
    parent = parents [index]
    contexts [ line . line_number ] = HeraldContext (
      folder_change_readiness (headlines, index),
      is_pending_detachment (
        line . metadata, metadata_at (parent),
        metadata_at ( None if parent is None else parents [parent] )))
  return contexts

def parent_indices_by_level (levels):
  """The index of each headline's parent, by the outline LEVELS, or
  None for a top-level one."""
  parents = []
  open_ancestors = [] # indices, outermost first
  for index, level in enumerate (levels):
    while open_ancestors and levels [ open_ancestors [-1] ] >= level:
      open_ancestors . pop ()
    parents . append ( open_ancestors [-1] if open_ancestors else None )
    open_ancestors . append (index)
  return parents

def folder_change_readiness (headlines, index):
  """For a folder headline, None when none of its members (its child
  headlines) carries a change of its relationship to the folder's
  viewparent, else whether every such change is ready; None for any
  other headline."""
  if ( headlines [index] . metadata is None
       or folder_atom ( headlines [index] . metadata ) is None ):
    return None
  level = len ( headlines [index] . stars )
  members = [] # each ( METADATA, [CHILD METADATA] )
  for later in headlines [ index + 1 : ]:
    later_level = len ( later . stars )
    if later_level <= level:
      break
    if later_level == level + 1:
      members . append ( ( later . metadata, [] ) )
    elif ( later_level == level + 2 and members
           and later . metadata is not None ):
      members [-1] [1] . append ( later . metadata )
  readinesses = [ membership_change_readiness (member, children)
                  for member, children in members if member is not None ]
  readinesses = [ ready for ready in readinesses if ready is not None ]
  return all (readinesses) if readinesses else None

def membership_change_readiness (member, children):
  """None when the folder member MEMBER carries no change of its
  relationship to the folder's viewparent (a '(rel ...)' in its
  '(diff ...)', or a 'relRepoMove' child), else whether it is ready."""
  changed, ready = False, True
  diff = metadata_form (member, "diff")
  if metadata_form (diff, "rel") is not None:
    changed = True
    leaves = leaves_fact (member)
    ready = objects_ready (
      member, [ "rel" ] if leaves and "rel" in leaves [1] else [] )
  for child in children:
    if metadata_form (child, "relRepoMove") is not None:
      changed = True
      ready = ready and metadata_form (
        metadata_form (child, "diff"), "ready" ) is not None
  return ready if changed else None

def objects_ready (metadata, objects):
  """Whether the headline's '(ready ...)' names every one of OBJECTS;
  with none listed, whether it has a '(ready ...)'."""
  ready = metadata_form ( metadata_form (metadata, "diff"), "ready" )
  if ready is None:
    return False
  return set (objects) <= { str (element) for element in ready [ 1 : ] }

def is_pending_detachment (metadata, parent, grandparent):
  """Whether a headline with METADATA, under a parent with PARENT
  whose own parent has GRANDPARENT, carries 'noClaim', is warranted by
  the graph (no 'noWarrant' or 'ghostWarrant', no '(rel removed)'),
  and stands under a parent that reads its children's claims: a node
  that is not write-protected, or a folder that is not write-protected
  whose viewparent is such a node."""
  view = metadata_form (metadata, "view") or []
  rel = metadata_form ( metadata_form (metadata, "diff"), "rel" )
  if ( not metadata_has_atom (metadata, "noClaim")
       or metadata_has_atom (view, "noWarrant")
       or metadata_has_atom (view, "ghostWarrant")
       or ( rel is not None and len (rel) == 2
            and is_symbol_named ( rel [1], "removed" ))):
    return False
  def is_editable_node (sexp):
    return ( sexp is not None and metadata_has_atom (sexp, "node")
             and not metadata_has_atom (sexp, "writeProtected") )
  if parent is not None and folder_atom (parent) is not None:
    return ( folder_atom (parent) not in FOLDER_HERALDS [ "write_protected" ]
             and is_editable_node (grandparent) )
  return is_editable_node (parent)

# ============================================================
# Change facts: a headline's '(diff (leaves ...) (ready ...))', which
# the summary of a buffer of the diff view counts and a readiness
# request names ('docs/api-and-formats.org', "Metadata of the diff
# view").
# ============================================================

def leaves_fact (metadata):
  """( N, OBJECTS ) of the headline's '(diff (leaves N OBJECT...))',
  or None when it names no leaf."""
  leaves = metadata_form ( metadata_form (metadata, "diff"), "leaves" )
  if leaves is None:
    return None
  return ( first_number (leaves),
           [ str (element) for element in leaves [ 1 : ]
             if not isinstance (element, int) ] )

def headline_is_ready (metadata):
  """Whether a headline that names leaves is ready: its '(ready ...)'
  names every object its '(leaves ...)' lists, or, listing none, is
  there."""
  ready = metadata_form ( metadata_form (metadata, "diff"), "ready" )
  if ready is None:
    return False
  ready_objects = { str (element) for element in ready [ 1 : ] }
  return set ( leaves_fact (metadata) [1] ) <= ready_objects

# ============================================================
# The summary
# ============================================================

def summary_lines (parsed_view, link_statuses, repo_ownership, view,
                   full_file_name):
  """Org comment lines, each a few 'NAME COUNT' facts of the view."""
  headlines = [ line . metadata for line in parsed_view
                if line . is_headline and line . metadata is not None ]
  occurrences = [ metadata for metadata in headlines
                  if headline_kind (metadata) == "node" ]
  other_kind_counts = {}
  for metadata in headlines:
    kind = headline_kind (metadata)
    if kind != "node":
      kind = "folders" if is_folder_kind (kind) else kind
      other_kind_counts [kind] = other_kind_counts . get (kind, 0) + 1
  repo_counts = {}
  for metadata in occurrences:
    repo = headline_node_repo (metadata) or "(none)"
    repo_counts [repo] = repo_counts . get (repo, 0) + 1
  def count_where (predicate, among = headlines):
    return sum ( 1 for metadata in among if predicate (metadata) )
  link_count = sum (
    len ( LINK_PATTERN . findall ( visible_text_of_line (line) ))
    for line in parsed_view )
  statuses = [ entry [ "status" ] for entry in link_statuses . values () ]
  resolved_count = statuses . count ("resolved")
  unresolved_detail = ", " . join (
    status + " " + str ( statuses . count (status) )
    for status in sorted ( set (statuses) - { "resolved" } ))
  def facts (*pairs):
    return "# " + ", " . join (
      name + " " + str (count) for name, count in pairs ) + "."
  return ( diff_summary_lines (parsed_view, view)
           if view [ "command" ] == "diff" else [] ) + [
    "# skg-for-ai compact view of " + full_file_name +
      " (view ID " + view [ "view_id" ] + ")",
    "# Each headline ends in its handle ⟨ID HERALDS⟩: ID its viewnode ID,",
    "# the '(vnid ID)' of its metadata in the full view, and HERALDS what a",
    "# client shows for it, but a bare 'aC' (only the parent contains it);",
    "# 'folded' marks a headline a client shows collapsed.",
    facts ( ( "Headlines", len (headlines) ),
            ( "node occurrences", len (occurrences) ),
            ( "distinct nodes",
              len ({ headline_gnid (metadata) for metadata in occurrences }) ),
            * sorted ( other_kind_counts . items () )),
    "# Skgrepos: " + ( ", " . join (
      repo + " (" + ownership_word ( repo_ownership . get (repo) ) + ") " +
      str (count)
      for repo, count in sorted ( repo_counts . items () ))
      or "none" ) + ".",
    facts ( ( "Foreign",
              count_where ( lambda metadata : repo_ownership . get (
                headline_node_repo (metadata) ) is False, occurrences )),
            ( "write-protected",
              count_where ( lambda metadata : metadata_has_atom (
                metadata, "writeProtected" ))),
            ( "folded",
              count_where ( lambda metadata : metadata_has_atom (
                metadata, "folded" )))),
    facts ( ( "Contained elsewhere too",
              count_where (contained_by_a_node_outside_its_ancestry) ),
            ( "subscribed to",
              count_where ( lambda metadata :
                side_count (metadata, "subscribesTo", "in") > 0 )),
            ( "subscribing",
              count_where ( lambda metadata :
                side_count (metadata, "subscribesTo", "out") > 0 ))),
    facts ( ( "Links", link_count ),
            ( "distinct targets", len (statuses) ),
            ( "resolved", resolved_count ),
            ( "unresolved", len (statuses) - resolved_count )) [ : -1 ] +
      ( " (" + unresolved_detail + ")." if unresolved_detail else "." ),
    facts ( ( "Server errors", len ( view [ "errors" ] )),
            ( "warnings", len ( view [ "warnings" ] ))) [ : -1 ] +
      ( " (listed in the .view.json file)."
        if view [ "errors" ] or view [ "warnings" ] else "." ) ]

def diff_summary_lines (parsed_view, view):
  """The readiness summary the server sent with a buffer of the diff
  view, and the count of its headlines with change facts, ready and
  not."""
  changed_headlines = [ line . metadata for line in parsed_view
                        if line . is_headline and line . metadata is not None
                        and leaves_fact (line . metadata) is not None ]
  ready_count = sum ( 1 for metadata in changed_headlines
                      if headline_is_ready (metadata) )
  summary = view . get ( "readiness_summary" )
  return [
    "# Readiness: " + ( readiness_summary_text (summary) if summary
                        else "the server sent no readiness summary." ),
    f"# Headlines with change facts {len (changed_headlines)}, ready "
    f"{ready_count}, not ready {len (changed_headlines) - ready_count}." ]

def readiness_summary_text (summary):
  """The sentence a client shows for the readiness summary SUMMARY,
  '((ready N) (changed M) (foreign F) (order-only O) (notEditable K))'
  ('skg--readiness-summary-message' in 'elisp/skg-readiness.el')."""
  counts = { str ( entry [0] ) : first_number ( [ None ] + entry [ 1 : ] )
             for entry in summary or [] }
  ready = counts . get ("ready", 0)
  changed = counts . get ("changed", 0)
  not_editable = counts . get ("notEditable", 0)
  foreign = counts . get ("foreign", 0)
  complete = ready == changed
  return (
    ( f"Commit ready. All {ready} of {changed} changed files are ready"
      if complete else f"{ready} of {changed} changed files are ready" ) +
    ( f", {not_editable} changes are not editable in this buffer"
      if not_editable else "" ) +
    ( f", {foreign} files changed in foreign skgrepos" if foreign else "" ) +
    "." )

def ownership_word (owned):
  return { True : "owned", False : "foreign", None : "ownership unknown" } [owned]

def contained_by_a_node_outside_its_ancestry (metadata):
  """Whether the 'contains' in-side counts a container its ancestor
  flags do not name: one not above this occurrence in the view."""
  side = relation_side (metadata, "contains", "in")
  if side is None:
    return False
  count, generations = side
  return count > len ( distinct_generations (generations) )

def side_count (metadata, relation_name, side_name):
  side = relation_side (metadata, relation_name, side_name)
  return side [0] if side else 0

# ============================================================
# Saving an edited compact view
# ============================================================

# A compact headline's handle, at the end of the line: its viewnode
# ID, then any display tokens.
COMPACT_HANDLE_PATTERN = re . compile (
  r'^(.*?) ?⟨([0-9a-z]+)((?: [^⟨⟩]*)?)⟩\s*$' )
# The handle tokens that ask for edit requests. The rebuild writes
# them into the headline's '(editRequest ...)': one of the first three,
# the request of that name, and after it one readiness request,
# '(ready OBJECT...)' or '(unready OBJECT...)'.
EDIT_REQUEST_TOKENS = [ "delete", "restoreNode", "restoreRel" ]
READINESS_TOKENS = [ "ready", "unready" ]
# With 'ready' or 'unready': name only the object 'all'.
ALL_TOKEN = "all"
# A link followed by the annotation 'annotate_links' appended to it.
ANNOTATED_LINK_PATTERN = re . compile (
  r'(\[\[id:[^\]\[]+\](?:\[[^\]]*\])?\])⟦[→✗][^⟧]*⟧' )

class SavedView:
  """The three files a view command wrote, read back."""
  def __init__ (self, sidecar_path):
    with open (sidecar_path, encoding = "utf-8") as sidecar_file:
      self . sidecar = json . load (sidecar_file)
    self . directory = os . path . dirname ( os . path . abspath (sidecar_path) )
    self . base_name = os . path . basename (sidecar_path) [ : - len (".view.json") ]
    self . full_path = os . path . join (
      self . directory, self . sidecar [ "full_view" ] )
    self . compact_path = os . path . join (
      self . directory, self . sidecar [ "compact_view" ] )
    with open (self . full_path, encoding = "utf-8") as full_file:
      self . full_text = full_file . read ()
    with open (self . compact_path, encoding = "utf-8") as compact_file:
      self . compact_text = compact_file . read ()
    self . parsed_view = parse_full_view (self . full_text)
    self . headline_of_handle = headlines_by_viewnode_id (self . parsed_view)

def headlines_by_viewnode_id (parsed_view):
  """{VIEWNODE ID: VIEWLINE} for each headline of the full view."""
  return { headline_viewnode_id ( line . metadata ) : line
           for line in parsed_view
           if line . is_headline
           and headline_viewnode_id ( line . metadata ) is not None }

def load_saved_view (path):
  """PATH is a sidecar, or a directory holding exactly one."""
  if not os . path . isdir (path):
    return SavedView (path)
  sidecars = sorted ( name for name in os . listdir (path)
                      if name . endswith (".view.json") )
  if len (sidecars) != 1:
    raise SkgToolError (
      f"{path} holds {len (sidecars)} '.view.json' files"
      + ( ": " + ", " . join (sidecars) if sidecars else "" )
      + "; name the one to save." )
  return SavedView ( os . path . join ( path, sidecars [0] ))

class OutlineHeadline:
  """A headline of the full view or of the edited compact view.
  'handle' is its viewnode ID (None for a new headline),
  'parent' the index of its parent in the same outline (None at the
  top level), 'tokens' the display tokens of its handle, 'body' its
  body lines, link annotations stripped."""
  def __init__ (self, handle, level, title, tokens, compact_line_number):
    self . handle = handle
    self . level = level
    self . title = title
    self . tokens = tokens
    self . compact_line_number = compact_line_number
    self . body = []
    self . parent = None

  def edit_request_words (self):
    return [ word for word in EDIT_REQUEST_TOKENS if word in self . tokens ]

  def readiness_words (self):
    return [ word for word in READINESS_TOKENS if word in self . tokens ]

  def names_only_all (self):
    return ALL_TOKEN in self . tokens

  def has_requests (self):
    return bool ( self . edit_request_words () or self . readiness_words ()
                  or self . names_only_all () )

def parse_edited_compact_view (compact_text):
  """The edited compact view's headlines, in order. The summary's
  comment lines before the first headline are dropped."""
  lines = compact_text . split ("\n")
  if lines and lines [-1] == "":
    lines = lines [ : -1 ]
  headlines = []
  for index, text in enumerate (lines):
    match = HEADLINE_PATTERN . match (text)
    if match:
      headlines . append ( edited_headline (
        index + 1, len ( match . group (1) ), match . group (2) ))
    elif headlines:
      headlines [-1] . body . append ( strip_link_annotations (text) )
    elif text . strip () and not text . startswith ("#"):
      raise SkgToolError (
        f"Line {index + 1} of the compact view is text before its first "
        f"headline: {text!r}" )
  assign_parents (headlines)
  return headlines

def edited_headline (compact_line_number, level, text_after_stars):
  match = COMPACT_HANDLE_PATTERN . match (text_after_stars)
  if not match:
    return OutlineHeadline (
      None, level, strip_link_annotations (text_after_stars) . rstrip (),
      [], compact_line_number )
  return OutlineHeadline (
    match . group (2), level,
    strip_link_annotations ( match . group (1) ),
    match . group (3) . split (), compact_line_number )

def strip_link_annotations (text):
  return ANNOTATED_LINK_PATTERN . sub ( r'\1', text )

def outline_of_full_view (parsed_view):
  headlines = []
  for view_line in parsed_view:
    if view_line . is_headline:
      headlines . append ( OutlineHeadline (
        headline_viewnode_id ( view_line . metadata ),
        len (view_line . stars), view_line . title, [], None ))
    elif headlines:
      headlines [-1] . body . append (view_line . text)
  assign_parents (headlines)
  return headlines

def assign_parents (headlines):
  for headline, parent in zip (
      headlines, parent_indices_by_level (
        [ headline . level for headline in headlines ] )):
    headline . parent = parent

def handle_error_lines (edited, headline_of_handle):
  """A line per handle that names no headline of the full view, and
  per handle whose request tokens make no one '(editRequest ...)'.
  HEADLINE_OF_HANDLE maps each viewnode ID of the full view to its
  headline. Whether the server accepts a request is the server's to
  say."""
  errors = []
  for headline in edited:
    if headline . handle is None:
      continue
    where = ( f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
              "of the compact view" )
    view_line = headline_of_handle . get ( headline . handle )
    if view_line is None:
      errors . append (
        f"Error: {where} names no headline of the full view." )
    elif headline . has_requests ():
      errors += [ f"Error: {where} {problem}"
                  for problem in request_token_problems (headline, view_line) ]
  return errors

def request_token_problems (headline, view_line):
  """Why HEADLINE's request tokens make no one '(editRequest ...)'
  for VIEW_LINE, the headline its handle names."""
  def quoted (words):
    return " and " . join ( "'" + word + "'" for word in words )
  problems = []
  if view_line . metadata is None:
    problems . append ( "carries a request, but the headline has no metadata." )
  if len ( headline . edit_request_words () ) > 1:
    problems . append (
      f"carries {quoted ( headline . edit_request_words () )}; a headline "
      "takes one edit request." )
  if len ( headline . readiness_words () ) > 1:
    problems . append ( "carries both 'ready' and 'unready'." )
  if headline . names_only_all () and not headline . readiness_words ():
    problems . append ( "carries 'all' without 'ready' or 'unready'." )
  return problems

def readiness_request_text (headline, view_line):
  """'(ready OBJECT...)' or '(unready OBJECT...)' for HEADLINE's
  readiness token, or None. Its objects are 'all' when the handle
  says 'all', and otherwise every object the headline's '(leaves ...)'
  lists, as a client's one-level gesture names them; a headline that
  lists none gets the bare request."""
  if not headline . readiness_words ():
    return None
  leaves = leaves_fact (view_line . metadata)
  objects = ( [ ALL_TOKEN ] if headline . names_only_all ()
              else leaves [1] if leaves else [] )
  return "(" + " " . join ( headline . readiness_words () + objects ) + ")"

def edit_request_text (headline, view_line):
  """The '(editRequest ...)' HEADLINE's tokens ask for, or None."""
  parts = headline . edit_request_words ()
  readiness = readiness_request_text (headline, view_line)
  if readiness:
    parts = parts + [ readiness ]
  return "(editRequest " + " " . join (parts) + ")" if parts else None

def unmarked_leaf_count (headline, view_line):
  """How many leaves an 'unready' on HEADLINE unmarks, counted as a
  client counts them: N of '(leaves N ...)' when the request names
  every object listed, else one per object (one for a bare request)."""
  if headline . readiness_words () != [ "unready" ]:
    return 0
  leaves = leaves_fact (view_line . metadata)
  if not headline . names_only_all ():
    return leaves [0] if leaves else 1
  return ( leaves [0] if leaves and leaves [1] == [ ALL_TOKEN ] else 1 )

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
  metadata_text = (
    None if headline . handle is None
    else headline_of_handle [ headline . handle ] . metadata_text )
  if metadata_text is None:
    return stars + " " + headline . title
  request = edit_request_text (
    headline, headline_of_handle [ headline . handle ] )
  if request:
    metadata_text = metadata_text_with_part (
      metadata_text, request, ( "viewRequests", "view", "diff" ))
  return ( stars + " " + metadata_text +
           ( " " + headline . title if headline . title else "" ))

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
  def depth_below_parent (outline, index):
    parent = outline [index] . parent
    return outline [index] . level - (
      0 if parent is None else outline [parent] . level )
  removed_handles = { headline . handle for headline in original
                      if headline . handle not in first_edited_index_of_handle }
  lines = request_report_lines (
    [ headline for headline in edited if headline . handle is not None ],
    headline_of_handle, described )
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
    if headline . handle is not None:
      continue
    body_lines = len (headline . body)
    lines . append (
      "New: " + ( "'" + headline . title + "'" if headline . title
                  else "an untitled headline" ) +
      ", " + ( "at the top level" if edited_parent_key (index) is None
               else place ( edited_parent_key (index) )) +
      ( f" (with a {body_lines}-line body)" if body_lines else "" ) + "." )
  for index in repeated_indices:
    lines . append (
      "Repeated: " + described ( edited [index] . handle ) + ", again " +
      ( "at the top level" if edited_parent_key (index) is None
        else place ( edited_parent_key (index) )) +
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

def described_handle (handle, headline_of_handle):
  """'⟨ID⟩ 'TITLE'', or for a headline without a title '⟨ID⟩ (KIND)',
  from the full view."""
  view_line = headline_of_handle [handle]
  if view_line . title:
    return f"⟨{handle}⟩ '{view_line . title}'"
  return f"⟨{handle}⟩ ({headline_kind (view_line . metadata)})"

def request_report_lines (handled_headlines, headline_of_handle, described):
  """The edit requests the handles' tokens ask for, by kind, each
  naming its headline through DESCRIBED; then, when the 'unready'
  requests unmark more than one leaf, the question a client asks
  before such a gesture."""
  lines = []
  for word, label, suffix in [
      ( "delete", "Delete", ", from the graph" ),
      ( "restoreNode", "Restore node", ", as HEAD has it" ),
      ( "restoreRel", "Restore membership", ", as HEAD has it" ) ]:
    lines += [ label + ": " + described (headline . handle) + suffix + "."
               for headline in handled_headlines
               if word in headline . edit_request_words () ]
  for word, label in [ ( "ready", "Ready" ), ( "unready", "Unready" ) ]:
    lines += [ label + ": " + described (headline . handle) + ", as '" +
               readiness_request_text (
                 headline, headline_of_handle [ headline . handle ] ) + "'."
               for headline in handled_headlines
               if headline . readiness_words () == [ word ] ]
  unmarked = sum ( unmarked_leaf_count (
                     headline, headline_of_handle [ headline . handle ] )
                   for headline in handled_headlines )
  if unmarked > 1:
    lines . append (
      f"Question: this unmarks {unmarked} changes. " + WIDE_UNMARK_QUESTION +
      " Saving with '--yes' answers yes." )
  return lines

def descendant_indices (outline, index):
  level = outline [index] . level
  descendants = []
  for later in range ( index + 1, len (outline) ):
    if outline [later] . level <= level:
      break
    descendants . append (later)
  return descendants

# ------------------------------------------------------------
# The server's answer
# ------------------------------------------------------------

def rerendered_view_of_save (terminal, sidecar):
  """Print the server's answer to the save. Return its re-rendered
  view, as the view commands describe one; raise if there is none."""
  kind = response_type (terminal)
  if kind != "save-result":
    print ( f"Server: {kind}." )
    for field_name in ( "prompt", "to-minibuffer" ):
      text = response_field (terminal, field_name)
      if text:
        print ( sexp_to_text (text) )
    if kind == "overPrivateText-telescope-confirmation":
      raise SkgToolError (
        "The save succeeded, but the server withholds its re-rendered "
        "view until the release is approved, which this tool does not "
        "do. The files are out of date; view the node again." )
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
  readiness_summary = response_field (terminal, "readiness-summary")
  if saved and readiness_summary:
    print ( "Readiness: " + readiness_summary_text (readiness_summary) )
  if not saved:
    raise SkgToolError (
      "The server refused the save; the files are as they were." )
  return { "view_id"     : sidecar [ "view_id" ],
           "command"     : sidecar [ "command" ],
           "request"     : sidecar [ "request" ],
           "content"     : content,
           "rendered_at" : rendered_at_of (terminal),
           "errors"      : errors,
           "warnings"    : warnings,
           "readiness_summary" : readiness_summary }

# ============================================================
# The connection
# ============================================================

class SkgToolError (Exception):
  pass

class SkgServerQuietError (SkgToolError):
  pass

class SkgConnectionClosedError (SkgToolError):
  pass

class SkgConnection:
  """One TCP connection to the server. The server keeps a
  connection's views until it ends."""

  def __init__ (self, port):
    self . port = port
    try:
      self . socket = socket . create_connection (( HOST, port ))
    except OSError as error:
      raise SkgToolError (
        f"No skg server answers on port {port}: {error}" )
    self . buffered = b""
    self . herald_tables = None # 'fetch_herald_tables' fills it

  def close (self):
    self . socket . close ()

  def request (self, line):
    """Send one request line; return its one response, parsed."""
    return parse_sexp (
      self . request_text (line, FIRST_MESSAGE_TIMEOUT_SECONDS) )

  def request_text (self, line, timeout_seconds):
    """Send one request line; return its one response's text."""
    self . send_line (line)
    return self . read_message (timeout_seconds)

  def raw_request (self, line):
    """Send one request line; return the text of every response that
    arrives before the server is quiet for RAW_QUIET_SECONDS."""
    self . send_line (line)
    messages = [ self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) ]
    while True:
      try:
        messages . append ( self . read_message (RAW_QUIET_SECONDS) )
      except SkgServerQuietError:
        if self . buffered:
          raise SkgToolError (
            "The server stopped partway through a response." )
        return messages
      except SkgConnectionClosedError:
        return messages

  def save_buffer (self, view_id, buffer_text, rendered_at):
    """Send a save of BUFFER_TEXT, rendered at the generation
    RENDERED_AT; return every response up to and including the
    terminal one, parsed."""
    return [ parse_sexp (message) for message in self . raw_save (
      request_line ( "save buffer", ( "view-id", view_id ),
                     ( "rendered-at", rendered_at ),
                     ( "point-lines-below-focused-headline", "0" ),
                     ( "point-column", "0" ),
                     ( "point-screen-lines-below-window-start", "0" )),
      buffer_text ) ]

  def stage_and_commit (self, message, confirmed):
    """Send "stage and commit" with MESSAGE, confirmed when CONFIRMED;
    return its response, parsed, after reading the rerender stream
    that follows it up to its 'rerender-done'."""
    self . send_line (
      request_line ( "stage and commit", ( "message", message )) [ : -1 ] +
      ( " (confirmed . t))" if confirmed else ")" ))
    response = parse_sexp ( self . read_message (IMPORT_TIMEOUT_SECONDS) )
    require_response_type (response, "stage-and-commit")
    while response_type ( parse_sexp (
        self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) )) != "rerender-done":
      pass
    return response

  def integrate_unshown_changes (self, view_id):
    """Send "integrate unshown changes" for VIEW_ID; return its
    response, parsed, and the 'rerender-view' message for VIEW_ID in
    the rerender stream that follows it (None when there is none),
    after reading that stream up to its 'rerender-done'."""
    self . send_line (
      request_line ( "integrate unshown changes", ( "view-id", view_id )))
    response = parse_sexp ( self . read_message (IMPORT_TIMEOUT_SECONDS) )
    require_response_type (response, "integrate-unshown-changes")
    rerendered = None
    while True:
      message = parse_sexp ( self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) )
      kind = response_type (message)
      if kind == "rerender-done":
        return response, rerendered
      if ( kind == "rerender-view"
           and sexp_to_text ( response_field (message, "view-id") ) == view_id ):
        rerendered = message

  def raw_save (self, line, buffer_text):
    """Send the save request LINE with BUFFER_TEXT as its saved buffer;
    return the text of every response up to and including the terminal
    one."""
    self . send_line (line)
    envelope = ( "((saved-buffer " + quote_string (buffer_text) +
                 ") (other-views ()))" ) . encode ("utf-8")
    self . socket . sendall (
      f"Content-Length: {len (envelope)}\r\n\r\n" . encode () + envelope )
    messages = []
    while True:
      message = self . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS)
      messages . append (message)
      if ( response_type ( parse_sexp (message) )
           in SAVE_TERMINAL_RESPONSE_TYPES ):
        return messages

  def send_line (self, line):
    self . socket . sendall ( line . encode ("utf-8") + b"\n" )

  def read_message (self, timeout_seconds):
    """One 'Content-Length: N\\r\\n\\r\\nPAYLOAD' message's payload.
    A server still initializing answers with a bare
    '((busy-initializing . ...))' line instead."""
    self . socket . settimeout (timeout_seconds)
    while b"\r\n\r\n" not in self . buffered:
      if self . buffered . startswith (b"((busy-initializing") and b"\n" in self . buffered:
        raise SkgToolError ( "The server is still initializing: " +
                             self . buffered . decode ("utf-8", "replace") . strip () )
      self . receive_more ()
    header, self . buffered = self . buffered . split (b"\r\n\r\n", 1)
    length = int ( header . decode () . split (":") [1] . strip () )
    while len (self . buffered) < length:
      self . receive_more ()
    payload = self . buffered [ : length ]
    self . buffered = self . buffered [ length : ]
    return payload . decode ("utf-8")

  def receive_more (self):
    try:
      chunk = self . socket . recv (65536)
    except socket . timeout:
      raise SkgServerQuietError ( "The server did not answer in time." )
    if not chunk:
      raise SkgConnectionClosedError ( "The server closed the connection." )
    self . buffered += chunk

def request_line (request_name, *fields):
  return ( "((request . " + quote_string (request_name) + ")" +
           "" . join ( " (" + key + " . " + quote_string (value) + ")"
                       for key, value in fields ) + ")" )

def response_type (response):
  form = metadata_form ( [ None ] + response, "response-type" )
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def response_field (response, field_name):
  """The value of a '(FIELD VALUE)' entry in a response alist: VALUE
  itself, or for '(FIELD V1 V2 ...)' the list of values. A field
  whose value is '()' gives []."""
  form = metadata_form ( [ None ] + response, field_name )
  if form is None:
    return None
  values = form [ 1 : ]
  if len (values) == 1:
    return values [0]
  return values

def rendered_at_of (response):
  """The generation a response's view was rendered at, '(rendered-at
  "N")', as the text N. Every response that carries a view has one."""
  value = response_field (response, "rendered-at")
  if value is None:
    raise SkgToolError ( "The server's view carries no 'rendered-at'." )
  return sexp_to_text (value)

def require_response_type (response, expected):
  actual = response_type (response)
  if actual == expected:
    return
  if actual == "overPrivateText-telescope-confirmation":
    raise SkgToolError (
      "The server asks to approve releasing over-private text, which "
      "this tool does not do: " + sexp_to_text (
        response_field (response, "prompt") or "" ))
  raise SkgToolError (
    f"Expected a {expected} response, got: " + render_sexp (response) [ : 2000 ] )

# ============================================================
# S-expressions as the server writes them: lists, double-quoted
# strings (only backslash and double quote escaped), bare symbols
# and integers.
# ============================================================

class Symbol (str):
  """A bare atom, as distinct from a double-quoted string."""
  pass

def parse_sexp (text):
  value, end = read_sexp_value (text, skip_whitespace (text, 0))
  return value

def read_sexp_value (text, position):
  character = text [position]
  if character == "(":
    elements = []
    position = skip_whitespace (text, position + 1)
    while text [position] != ")":
      element, position = read_sexp_value (text, position)
      elements . append (element)
      position = skip_whitespace (text, position)
    return elements, position + 1
  if character == '"':
    return read_sexp_string (text, position)
  end = position
  while end < len (text) and text [end] not in ' \t\n\r()"':
    end += 1
  atom = text [ position : end ]
  if re . fullmatch ( r'-?[0-9]+', atom ):
    return int (atom), end
  return Symbol (atom), end

def read_sexp_string (text, position):
  pieces = []
  position += 1
  while text [position] != '"':
    if text [position] == "\\":
      position += 1
    pieces . append ( text [position] )
    position += 1
  return "" . join (pieces), position + 1

def skip_whitespace (text, position):
  while position < len (text) and text [position] in " \t\n\r":
    position += 1
  return position

def end_of_balanced_sexp (text, start):
  """The offset just past the sexp starting at START, or None if the
  text ends first."""
  try:
    value, end = read_sexp_value (text, start)
  except IndexError:
    return None
  return end

def top_level_elements_with_offsets (list_text):
  """[(OFFSET, ELEMENT)] for each element of the list LIST_TEXT."""
  results = []
  position = skip_whitespace (list_text, 1)
  while list_text [position] != ")":
    element, end = read_sexp_value (list_text, position)
    results . append (( position, element ))
    position = skip_whitespace (list_text, end)
  return results

def sexp_to_text (value):
  """An atom's text; a list rendered as a sexp."""
  if isinstance (value, list):
    return render_sexp (value)
  return str (value)

def render_sexp (value):
  if isinstance (value, list):
    return "(" + " " . join ( render_sexp (element) for element in value ) + ")"
  if isinstance (value, Symbol) or isinstance (value, int):
    return str (value)
  return quote_string (value)

def quote_string (text):
  return '"' + text . replace ("\\", "\\\\") . replace ('"', '\\"') + '"'

if __name__ == "__main__":
  try:
    main ( sys . argv [ 1 : ] )
  except SkgToolError as error:
    print ( "skg_for_ai: " + str (error), file = sys . stderr )
    sys . exit (1)
