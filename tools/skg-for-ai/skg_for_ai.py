#!/usr/bin/env python3
"""PURPOSE: a client of a running skg server for programs and people
without an editor. It shows the graph the way a person reads it in a
client, but as two plain files per view, and it speaks the protocol
directly for anything else.

For a requested view it writes, into a directory the caller names:
- NAME.full.org: the view exactly as the server rendered it.
- NAME.compact.org: the same lines, each headline's '(skg ...)'
  metadata replaced by a handle '⟨N HERALDS⟩' at the end of the
  headline, N being the headline's 1-based line number in the full
  view, and every '[[id:...]]' link followed by its target's title.
  A summary of the view's facts precedes the tree unless
  '--no-summary' is given.
- NAME.view.json: the server's view ID, the request, the generation
  the server rendered the view at, and the server's errors and
  warnings.

View commands: 'view ID', 'search TERMS...', 'links-here ID',
'contains ID', 'diff'. Each prints the paths it wrote. 'links-here'
saves, as the clients' 'C-c l m' does. 'diff' writes each nonempty
buffer of the diff view, its handles carrying the change facts.
'new TITLE REPO' creates a root node in an owned skgrepo, as a save of
a new empty view does, and writes its view as 'view' would.
'save SIDECAR' rebuilds the full buffer from an edited compact view,
prints the changes it would make, and with '--yes' saves it and
writes the server's re-rendered view over the three files. Words in a
handle ask for edit requests: 'delete', 'ready', 'unready', 'all',
'restoreNode', 'restoreRel'.
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
  connection = SkgConnection (arguments . port)
  try:
    if arguments . command == "view":
      view = fetch_single_root_view (connection, arguments . id)
    elif arguments . command == "contains":
      # A fresh content view already carries the root's containers
      # folder, with each container's containerward chain.
      view = fetch_single_root_view (connection, arguments . id)
    elif arguments . command == "links-here":
      view = fetch_view_with_root_folder_request (
        connection, arguments . id, "mentioners")
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
      ( "contains", "a node's view, which shows what contains it" ),
      ( "links-here", "a node's view with its mentioners folder" ) ]:
    commands . add_parser (
      command, help = help_text, parents = [ common_options ]
    ) . add_argument ("id")
  commands . add_parser (
    "search", help = "the search buffer for TERMS",
    parents = [ common_options ] ) . add_argument ( "terms", nargs = "+" )
  commands . add_parser (
    "diff", help = ( "the diff view's nonempty buffers: every change in "
                     "the owned skgrepos since their last git-commits" ),
    parents = [ common_options ] )
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
  argument = ( " " . join (arguments . terms)
               if arguments . command == "search"
               else arguments . id )
  return base_name_for ( arguments . command, argument )

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
  handle, send the save, print the server's answer and write its
  re-rendered view over the three files. The save carries the
  generation the view was rendered at, so the server refuses it if a
  node it would write changed since. A refused save leaves the files
  as they were."""
  saved_view = load_saved_view (arguments . view_files)
  if ( arguments . port is not None
       and arguments . port != saved_view . sidecar [ "port" ] ):
    raise SkgToolError (
      f"The view came from the server on port {saved_view . sidecar [ 'port' ]}, "
      f"not {arguments . port}." )
  edited = parse_edited_compact_view (saved_view . compact_text)
  original = outline_of_full_view (saved_view . parsed_view)
  errors = handle_error_lines (edited, saved_view . parsed_view)
  changes = [] if errors else change_report_lines (
    edited, original, saved_view . parsed_view )
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
      edited, saved_view . parsed_view, saved_view . full_text )
    if saved_view . sidecar [ "command" ] == "diff":
      register_diff_view_buffer_for_save (connection, saved_view . sidecar)
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

def register_diff_view_buffer_for_save (connection, sidecar):
  """Open the sidecar's buffer of the diff view on CONNECTION, under
  its view ID, as a client's buffer stays open on its connection. The
  server keeps a view's buffer kind per connection, and a save
  re-renders as a buffer of the diff view, with the readiness summary,
  only a view registered as one. The save itself is read from the
  saved text, against the generation the sidecar records."""
  buffer_name = sidecar [ "request" ] [ "buffer" ]
  if fetch_diff_view_buffer (
      connection, buffer_name, sidecar [ "view_id" ] ) is None:
    raise SkgToolError (
      f"Nothing was saved: the diff view's '{buffer_name}' buffer is empty "
      "now (its changes were committed or undone); run 'diff' again." )

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
  named BASE_BUFFER ('diff_all-changes' and so on). Each buffer is
  then saved unchanged, which writes nothing, because the save's answer
  carries the readiness summary that opens the compact view's summary;
  the save's re-rendered buffer is the one written. Each buffer is
  closed before the next is requested."""
  connection = SkgConnection (arguments . port)
  written_paths = []
  try:
    for buffer_name in DIFF_VIEW_BUFFERS:
      view = fetch_diff_view_buffer (connection, buffer_name)
      if view is None:
        continue
      view = diff_view_with_readiness_summary (connection, view)
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
           "warnings"    : response_field (response, "warnings") or [] }

def diff_view_with_readiness_summary (connection, view):
  """VIEW, a buffer of the diff view open on CONNECTION, saved
  unchanged: a buffer of the diff view saves back to itself, and the
  save's answer carries the readiness summary, which the diff view's
  own answer lacks. Returns the save's re-rendered buffer with its
  summary, or VIEW as it was when the save did not succeed."""
  terminal = connection . save_buffer (
    view [ "view_id" ], view [ "content" ], view [ "rendered_at" ] ) [-1]
  content = response_field (terminal, "content")
  if ( response_type (terminal) != "save-result"
       or not isinstance (content, str) or isinstance (content, Symbol)
       or not content ):
    return dict ( view, readiness_summary = None )
  return dict ( view,
                content = content,
                rendered_at = rendered_at_of (terminal),
                errors = response_field (terminal, "errors") or [],
                warnings = response_field (terminal, "warnings") or [],
                readiness_summary = response_field (
                  terminal, "readiness-summary" ))

def fetch_view_with_root_folder_request (connection, node_id, relation_name):
  """Open the node's view, then save it unchanged but for a
  '(viewRequests (folder RELATION))' on its root, as the clients'
  'C-c l' commands do. The save's re-rendered buffer is the view.
  Nothing in the buffer differs from the graph, so the save writes no
  file."""
  fresh_view = fetch_single_root_view (connection, node_id)
  requesting_buffer = buffer_with_view_request_on_first_headline (
    fresh_view [ "content" ], "(folder " + relation_name + ")" )
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
           "request"     : { "id" : node_id,
                             "view_request" : "(folder " + relation_name + ")" },
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
  skgrepo_labels = fetch_skgrepo_labels (connection)
  compact_lines = []
  if show_summary:
    compact_lines += summary_lines (
      parsed_view, link_statuses, repo_ownership, view,
      os . path . basename (full_path) )
  compact_lines += compact_tree_lines (
    parsed_view, link_statuses, skgrepo_labels )
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

def kind_atoms ():
  with open ( os . path . join (SHARED_FOLDER, "metadata-fields.json"),
              encoding = "utf-8" ) as table_file:
    table = json . load (table_file)
  return { atom for kind in table [ "kinds" ] for atom in kind [ "atoms" ] }

KIND_ATOMS = kind_atoms ()

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

def fetch_skgrepo_labels (connection):
  """{REPO: LABEL}, each configured skgrepo's herald label, from the
  table the server sends beside its herald rules."""
  response = connection . request ( request_line ("herald rules") )
  require_response_type (response, "herald-rules")
  labels = metadata_form ( [ None ] + response, "skgrepoLabels" ) or [ None ]
  return { sexp_to_text ( entry [0] ) : sexp_to_text ( entry [1] )
           for entry in labels [ 1 : ] }

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

def compact_tree_lines (parsed_view, link_statuses, skgrepo_labels):
  return [ compact_line (view_line, link_statuses, skgrepo_labels)
           for view_line in parsed_view ]

def compact_line (view_line, link_statuses, skgrepo_labels):
  if not view_line . is_headline:
    return annotate_links (view_line . text, link_statuses)
  title = annotate_links (view_line . title, link_statuses)
  handle = handle_text (view_line, skgrepo_labels)
  return ( view_line . stars + " " + title +
           ( " " if title else "" ) + handle )

def handle_text (view_line, skgrepo_labels):
  """'⟨N KIND HERALDS ⌂:LABEL CHANGES folded⟩': N the line number in
  the full view, KIND only for a headline that is not a node
  occurrence (a folder, an alias, an unknown viewnode ...), HERALDS
  its relationship heralds but for a bare 'aC', '⌂:LABEL' its home
  skgrepo's label where that differs from its nearest occurrence
  ancestor's (or it is a root), CHANGES its diff heralds (in the diff
  view), and 'folded' when a client shows it collapsed."""
  parts = [ str (view_line . line_number) ]
  if view_line . metadata is not None:
    metadata = view_line . metadata
    kind = headline_kind (metadata)
    if kind != "node":
      parts . append (kind)
    parts += [ token for token in
               relationship_heralds_text (metadata) . split ()
               if token != BARE_PARENT_CONTAINS_TOKEN ]
    if metadata_has_atom ( metadata_form (metadata, "view") or [],
                           "repoBoundary" ):
      parts . append ( skgrepo_boundary_token (metadata, skgrepo_labels) )
    parts += diff_heralds_tokens (metadata)
    if metadata_has_atom (metadata, "folded"):
      parts . append ("folded")
  return "⟨" + " " . join (parts) + "⟩"

def skgrepo_boundary_token (metadata, skgrepo_labels):
  """'⌂:LABEL', LABEL the herald label of the headline's home
  skgrepo, as the clients render 'repoBoundary'; a bare '⌂' when the
  headline names no home skgrepo."""
  repo = headline_node_repo (metadata)
  if repo is None:
    return "⌂"
  return "⌂:" + skgrepo_labels . get (repo, repo)

# ============================================================
# Diff heralds, as the clients render a headline's '(diff ...)' change
# facts ('docs/heralds.org', "Diff heralds"; the rules are
# 'change_fact_rules_for_two_objects' and
# 'change_fact_rules_for_one_object' in 'server/heralds.rs'), styles
# dropped. A node or a ghost has two diff objects, its node (∃+ new,
# ∃- deleted, M modified) and its relationship to its viewparent (~+,
# ~-); every other headline that names leaves has one. A ✓ follows
# each object whose changes are all marked ready.
# ============================================================

TWO_OBJECT_KINDS = { "node", "ghost" }
RELATIONSHIP_ONLY_KINDS = { "unknown", "restricted" }

def diff_heralds_tokens (metadata):
  diff = metadata_form (metadata, "diff")
  if diff is None:
    return []
  ready = metadata_form (diff, "ready")
  ready_objects = ( None if ready is None
                    else { str (element) for element in ready [ 1 : ] } )
  def existence_sign (object_name):
    form = metadata_form (diff, object_name)
    if form is None or len (form) < 2:
      return None
    return { "added" : "+", "removed" : "-" } . get ( str ( form [1] ))
  kind = headline_kind (metadata)
  tokens = []
  if kind in TWO_OBJECT_KINDS:
    node_sign = existence_sign ("node")
    node_token = ( "∃" + node_sign if node_sign
                   else "M" if metadata_form (diff, "properties") is not None
                   else None )
    if node_token:
      tokens . append (
        node_token + ( "✓" if ready_objects
                       and ready_objects & { "node", "all" } else "" ))
    rel_sign = existence_sign ("rel")
    if rel_sign:
      tokens . append (
        "~" + rel_sign + ( "✓" if ready_objects
                           and "rel" in ready_objects else "" ))
  else:
    sign = existence_sign ("node") or existence_sign ("rel")
    readiness_mark = "✓" if ready_objects is not None else ""
    if sign:
      tokens . append (
        ( "~" if kind in RELATIONSHIP_ONLY_KINDS else "" ) + sign +
        readiness_mark )
    elif readiness_mark:
      tokens . append (readiness_mark)
  home_move = metadata_form (diff, "homeSkgrepoMove")
  if home_move is not None:
    tokens += [ "-" + sexp_to_text ( metadata_form (home_move, "old") [1] ),
                "+" + sexp_to_text ( metadata_form (home_move, "new") [1] ) ]
  moved_like = metadata_form (diff, "movedLikeAncestor")
  if moved_like is not None:
    tokens . append ( "Δ⌂=" + sexp_to_text ( moved_like [1] ))
  return tokens

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
    "# Each headline ends in ⟨N ...⟩, N its line number in the full view;",
    "# a bare 'aC' (only the parent contains it) is left out, and 'folded'",
    "# marks a headline a client shows collapsed.",
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

# A compact headline's handle, at the end of the line: its number,
# then any display tokens.
COMPACT_HANDLE_PATTERN = re . compile (
  r'^(.*?) ?⟨([0-9]+)((?: [^⟨⟩]*)?)⟩\s*$' )
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
  'handle' is its full-view line number (None for a new headline),
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
    int ( match . group (2) ), level,
    strip_link_annotations ( match . group (1) ),
    match . group (3) . split (), compact_line_number )

def strip_link_annotations (text):
  return ANNOTATED_LINK_PATTERN . sub ( r'\1', text )

def outline_of_full_view (parsed_view):
  headlines = []
  for view_line in parsed_view:
    if view_line . is_headline:
      headlines . append ( OutlineHeadline (
        view_line . line_number, len (view_line . stars), view_line . title,
        [], None ))
    elif headlines:
      headlines [-1] . body . append (view_line . text)
  assign_parents (headlines)
  return headlines

def assign_parents (headlines):
  open_ancestors = [] # indices, outermost first
  for index, headline in enumerate (headlines):
    while ( open_ancestors and
            headlines [ open_ancestors [-1] ] . level >= headline . level ):
      open_ancestors . pop ()
    headline . parent = open_ancestors [-1] if open_ancestors else None
    open_ancestors . append (index)

def handle_error_lines (edited, parsed_view):
  """A line per handle that names no headline of the full view, and
  per handle whose request tokens make no one '(editRequest ...)'.
  Whether the server accepts a request is the server's to say."""
  errors = []
  for headline in edited:
    if headline . handle is None:
      continue
    where = ( f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
              "of the compact view" )
    if not 1 <= headline . handle <= len (parsed_view):
      errors . append (
        f"Error: {where} names no line; the full view has "
        f"{len (parsed_view)} lines." )
      continue
    view_line = parsed_view [ headline . handle - 1 ]
    if not view_line . is_headline:
      errors . append (
        f"Error: {where} names line {headline . handle} of the full view, "
        "a body line, not a headline." )
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

def rebuilt_full_buffer (edited, parsed_view, full_text):
  """Each headline at its edited level and title, with the metadata of
  the full-view line its handle names, plus the '(editRequest ...)'
  its handle's tokens ask for; a headline without a handle bare, which
  the save reads as a new node; bodies as edited."""
  lines = []
  for headline in edited:
    lines . append ( rebuilt_headline_line (headline, parsed_view) )
    lines += headline . body
  text = "\n" . join (lines)
  return text + "\n" if full_text . endswith ("\n") else text

def rebuilt_headline_line (headline, parsed_view):
  stars = "*" * headline . level
  metadata_text = (
    None if headline . handle is None
    else parsed_view [ headline . handle - 1 ] . metadata_text )
  if metadata_text is None:
    return stars + " " + headline . title
  request = edit_request_text (
    headline, parsed_view [ headline . handle - 1 ] )
  if request:
    metadata_text = metadata_text_with_part (
      metadata_text, request, ( "viewRequests", "view", "diff" ))
  return ( stars + " " + metadata_text +
           ( " " + headline . title if headline . title else "" ))

# ------------------------------------------------------------
# The report of changes
# ------------------------------------------------------------

def change_report_lines (edited, original, parsed_view):
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
    """'⟨N⟩ TITLE', from the full view."""
    view_line = parsed_view [ handle - 1 ]
    if view_line . title:
      return f"⟨{handle}⟩ '{view_line . title}'"
    kind = ( headline_kind (view_line . metadata)
             if view_line . metadata is not None else "headline" )
    return f"⟨{handle}⟩ ({kind})"
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
    parsed_view, described )
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

def request_report_lines (handled_headlines, parsed_view, described):
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
                 headline, parsed_view [ headline . handle - 1 ] ) + "'."
               for headline in handled_headlines
               if headline . readiness_words () == [ word ] ]
  unmarked = sum ( unmarked_leaf_count (
                     headline, parsed_view [ headline . handle - 1 ] )
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
