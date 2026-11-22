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
- NAME.view.json: the server's view ID, the request, the view's
  perspective, and the server's errors and warnings.

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
handle ask for edit requests: 'delete', 'processed' or 'unprocessed'
with 'node', 'rel' or 'both' (and 'outward' for the clients' C-u C-u),
'restoreNode', 'restoreRel', 'stripAttribution', 'merge:ID',
'relRepo:NAME', 'noSearchMatching', 'searchMatching'; for view requests: 'editable',
'fork', 'write-protect'; 'detach' and 'attach' write or remove
'noClaim'; 'repo:NAME' moves the node; 'subtree' asks for the
clients' prefix-argument forms. A headline ending in '⟨node:ID⟩'
places that existing node there, as the clients' paste does. A save
that forks a node (an edited foreign node, or 'fork') is approved by
'--yes', the clone going to '--fork-into REPO' or the one owned
skgrepo. The server's other questions are answered by '--hoist',
'--delete-from', '--strandeds' and '--show-overPrivateText'; '--yes'
alone writes nothing more. A new headline whose title equals a
removed one's stops the save until its handle is restored or it is
marked '⟨new⟩'; one whose title resembles a removed one's is a
question, answered by '--keep-ids' or '--all-new' or on the headline.
'extend SIDECAR' extends a buffer of the diff view to every change it
does not make editable, as the clients' 'C-c r u' does, and writes the
buffer again.
'perspective SIDECAR [PERSPECTIVE]' switches a view to the
next perspective, as the clients' 'C-c v o'
does, or to the one named, and writes the view again; the sidecar
records it, which every later save names.
'commit MESSAGE' stages and git-commits every change in the owned
skgrepos, asking first when the server says not all is processed.
Other commands: 'close SIDECAR'; 'raw REQUEST', which sends one
request line and prints every response; 'import INPUT_DIR REPO',
which previews and applies a Markdown and Org import;
'delete-references ID', which removes the owned structured references
to an absent node; 'receipt ITEM TEXT', which writes a read receipt
under your fork of a mailed item and hides the item from your fork of
its mailbox, printing first what it would do, and with '--yes' doing it; 'restrict [NAME]', which prints or (through a
daemon) sets the skgrepo restriction; 'linkstack ACTION', the clients'
linkstack kept in a file.
The port is '--port', else SKG_PORT; there is no default. A command
that needs a server and finds no port there, in its sidecar, or in a
daemon of its folder is refused before it connects to anything.

'daemon --out-dir DIR' keeps one connection open and holds there
every view its commands write: when the server rerenders one (after
a save or a git-commit on that connection, or when files changed on
disk), it rewrites the view's three files, and it appends each event
to 'DIR/skg-for-ai.log'. While it runs, every command for its port
(but 'raw', 'import', and 'commit --all' without '--yes') is sent to it over
the Unix socket 'skg-for-ai-PORT.sock' in XDG_RUNTIME_DIR (else /tmp),
whatever folder it writes into, and runs there; so is every command
whose '--out-dir' (or whose sidecar's folder) is DIR and that names no
port, over 'DIR/skg-for-ai.sock'. Without a daemon, each command opens
its own connection. One daemon runs per port. 'daemon --stop --out-dir
DIR' stops it.

Standard library only. The README beside this file explains the
formats; the protocol is in 'docs/api-and-formats.org'.
"""

import argparse
import collections
import contextlib
import difflib
import io
import json
import os
import re
import select
import signal
import socket
import stat
import sys
import time
import traceback
import uuid

import server_connection
from sexp import (
  Symbol, metadata_form, parse_sexp, quote_string, render_sexp,
  sexp_to_text, top_level_elements_with_offsets )
from server_connection import (
  FIRST_MESSAGE_TIMEOUT_SECONDS, IMPORT_TIMEOUT_SECONDS,
  NO_PORT_MESSAGE, SkgConnection, SkgConnectionClosedError,
  SkgServerQuietError, SkgToolError, SkgViewAlreadyOpen,
  UNPROMPTED_REPO_CHANGE_MESSAGE_START, approved_overPrivateText_field,
  connect_to_server, overPrivateText_answer,
  repo_files_reloaded_count_clauses, request_line,
  require_response_type, response_field, response_text_field,
  response_type )
from full_view import (
  HEADLINE_PATTERN, METADATA_PARTS, folder_atom, headline_gnid,
  headline_kind, headline_node_repo, headline_viewnode_id,
  metadata_has_atom, parse_full_view, relation_side )
from compact_view import (
  NOT_HELD_ANNOTATION, RESTATED_WORDS, count_phrase,
  fetch_link_statuses_of_ids, fetch_repo_ownership, herald_contexts,
  is_editable_node, is_symbol_named, leaves_fact,
  membership_change_processed, parent_indices_by_level,
  processed_summary_text, reads_claims )
from views import (
  FOREST_VIEW_REQUESTS, RenderedView, SEARCH_OPTIONS, SavedView,
  close_view_on_server, fetch_diff_view_buffer, fetch_forest_view,
  fetch_search_view, fetch_single_root_view, load_saved_view,
  mark_sidecar_closed, metadata_text_with_part,
  perspective_fields_of_sidecar, perspective_of_sidecar, read_text_file,
  release_view, release_withheld_save_rerender, require_port_of_sidecar,
  require_view_has_headlines, sidecar_path_of, write_text_file,
  write_view_files )

# The sections of this file, in order, each under a banner of '='
# (subsections '-') whose first line is the title given here; grep
# for '^# TITLE' to reach one:
#   Commands: 'main', the command line, a function per command.
#   The chain of a headline; What a headline's heralds mean: 'chain'
#     and 'describe'.
#   Fetching views; Writing the files ('RenderedView').
#   The full view, parsed; Facts from one headline's metadata;
#   Relationship heralds; Links; Repo ownership; The compact view;
#   Every herald (- The lens; - What a headline's heralds need from the
#   headlines around it); Change facts; The summary.
#   Saving an edited compact view (- Words read against the outline;
#   - 'subtree'; - Rebuilding the full buffer; - The report of changes;
#   - Placing an existing node; - Forking; - Retyped headlines; - The
#   fork confirmation; - The save's other questions; - The server's
#   answer).
#   The daemon; The connection ('SkgConnection', 'DaemonConnection',
#   the errors, the response helpers); S-expressions.

# The buffers of the diff view, in the order the clients request them.
DIFF_VIEW_BUFFERS = [ "all changes", "merges", "skgrepo changes",
                      "order-only changes" ]
# What a client asks before a processed gesture that unmarks more
# than one leaf ('skg--processed-wide-unmark-question' in
# 'elisp/skg-processed.el').
WIDE_UNMARK_QUESTION = (
  "Unmarking this node processed will unmark every contributing "
  "decision. You might discard a lot of hard-won information. Continue?" )
# Commands that name one forest kind.
FOREST_COMMAND_ALIASES = { "contains" : "containers",
                           "links-here" : "mentioners" }
# The daemon's files, in the folder whose views it holds.
DAEMON_SOCKET_NAME = "skg-for-ai.sock"
DAEMON_LOG_NAME = "skg-for-ai.log"
# How often the daemon's idle wait looks up from its sockets, to
# notice a stop signal.
DAEMON_IDLE_POLL_SECONDS = 0.5
# How long the daemon waits for a command's request to arrive whole.
DAEMON_CLIENT_TIMEOUT_SECONDS = 10
# How many changed nodes a log line names before it counts the rest.
LOGGED_CHANGED_NODES_LIMIT = 6

# ============================================================
# Commands
# ============================================================

def main (argv):
  """Run the command ARGV names; return its exit code. A command whose
  folder has a daemon listening is sent to it and runs there."""
  arguments = parse_command_line (argv)
  if arguments . command == "daemon":
    command_daemon (arguments)
    return 0
  daemon_exit_code = exit_code_of_command_sent_to_daemon (arguments, argv)
  if daemon_exit_code is not None:
    return daemon_exit_code
  if arguments . port is None and command_needs_named_port (arguments):
    raise SkgToolError (NO_PORT_MESSAGE)
  run_command (arguments)
  return 0

def command_needs_named_port (arguments):
  """Whether the command talks to a server whose port only the
  command line or SKG_PORT can give: not 'save', 'extend', 'close' or
  'perspective', which take it from their sidecar; not
  'linkstack' but 'push', which keeps a file; not 'delete-references'
  without '--yes', which sends nothing."""
  if arguments . command in SIDECAR_COMMANDS:
    return False
  if arguments . command == "linkstack":
    return arguments . action == "push"
  if arguments . command in ( "chain", "describe" ):
    return False
  if arguments . command == "delete-references":
    return arguments . yes
  return True

def run_command (arguments):
  """Run the command ARGUMENTS names, in this process: a view command
  by 'run_view_command', every other by its own function. ('daemon'
  never comes here: 'main' runs it, and a daemon refuses it.)"""
  command_function = {
    "close"             : command_close,
    "raw"               : command_raw,
    "import"            : command_import,
    "save"              : command_save,
    "new"               : command_new,
    "diff"              : command_diff,
    "commit"            : command_commit,
    "extend"            : command_extend,
    "perspective"       : command_perspective,
    "delete-references" : command_delete_references,
    "receipt"           : command_receipt,
    "restrict"          : command_restrict,
    "linkstack"         : command_linkstack,
    "chain"             : command_chain,
    "describe"          : command_describe,
  } . get (arguments . command, run_view_command)
  command_function (arguments)

def run_view_command (arguments):
  """Run 'view', 'search', 'forest' or a forest alias ('contains',
  'links-here'): fetch the view, write its three files and print their
  paths."""
  connection = connect_to_server (arguments . port)
  try:
    show = arguments . show_overPrivateText
    if arguments . command == "view":
      try:
        view = fetch_single_root_view (connection, arguments . id, show)
      except SkgViewAlreadyOpen as already_open:
        # As a client lands in the open view's buffer, print the files
        # that show it, when the connection holds them.
        files_stem = connection . files_stem_of_view (already_open . view_id)
        if files_stem is None:
          raise
        print ( f"{already_open . node_id} is already a root of the view "
                f"{already_open . view_id}, whose files these are. If that "
                f"view's perspective put it there, switch it back with "
                f"'perspective SIDECAR root homes' to view "
                f"{already_open . node_id} by itself.", file = sys . stderr )
        for suffix in ( ".full.org", ".compact.org", ".view.json" ):
          print (files_stem + suffix)
        return
    elif arguments . command in FOREST_COMMAND_ALIASES:
      view = fetch_forest_view (
        connection, FOREST_COMMAND_ALIASES [ arguments . command ],
        arguments . id, show )
    elif arguments . command == "forest":
      view = fetch_forest_view (connection, arguments . kind, arguments . id, show)
    elif arguments . command == "search":
      view = fetch_search_view (
        connection, " " . join (arguments . terms),
        [ option for option in SEARCH_OPTIONS if getattr (arguments, option) ],
        show )
    require_view_has_headlines (view)
    view . command = arguments . command
    base_name = arguments . name or default_base_name (arguments)
    written_paths = write_view_files (
      connection, view, arguments . out_dir, base_name,
      not arguments . no_summary )
    release_view (connection, view . view_id)
  finally:
    connection . close ()
  for path in written_paths:
    print (path)

def parse_command_line (argv):
  port_option = argparse . ArgumentParser ( add_help = False )
  port_option . add_argument (
    "--port", type = int,
    default = ( int ( os . environ [ "SKG_PORT" ] )
                if os . environ . get ("SKG_PORT") else None ),
    help = ( "the server's port (default: SKG_PORT; there is no other, "
             "and a command that needs a server refuses without one)" ))
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
  common_options . add_argument (
    "--show-overPrivateText", action = "store_true",
    help = ( "under a skgrepo restriction, approve releasing text from "
             "overPrivateText telescopes when the server asks (a search "
             "then includes them; without it, it excludes them)" ))
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
  search_parser = commands . add_parser (
    "search", help = "the search buffer for TERMS",
    parents = [ common_options ] )
  search_parser . add_argument ( "terms", nargs = "+" )
  search_parser . add_argument (
    "--regex", action = "store_true",
    help = "read each term as a regular expression ('C-c f i' option r)" )
  search_parser . add_argument (
    "--body", action = "store_true",
    help = "search the bodies too, not only titles and aliases (option b)" )
  search_parser . add_argument (
    "--operators", action = "store_true",
    help = ( "keep Tantivy's phrase and operator syntax: AND, OR, quotes, "
             "+word, -word, grouping (option t)" ))
  commands . add_parser (
    "diff", help = ( "the diff view's nonempty buffers: every change in "
                     "the owned skgrepos since their last git-commits" ),
    parents = [ common_options ] )
  extend_parser = commands . add_parser (
    "extend", help = ( "extend a buffer of the diff view to the changes it "
                       "does not make editable, and write it again" ))
  extend_parser . add_argument (
    "view_files",
    help = ( "the BASE.view.json of a buffer of the diff view, or a "
             "directory holding only one" ))
  extend_parser . add_argument (
    "--port", type = int, default = None,
    help = "the server's port; it must be the one the sidecar records" )
  perspective_parser = commands . add_parser (
    "perspective",
    help = ( "show a view from the next perspective, or from the one "
             "named, and write the view again" ))
  perspective_parser . add_argument (
    "view_files",
    help = "the BASE.view.json of the view, or a directory holding only one" )
  perspective_parser . add_argument (
    "perspective", nargs = "*",
    help = ( "'next' (the default: the next in the view's cycle, as the "
             "clients' 'C-c v o' does), 'root homes' (each tree's root's "
             "home skgrepo, as a fresh view shows), or 'skgrepo NAME' "
             "(that skgrepo's, owned or foreign, in every tree); the "
             "words may be given apart or quoted together" ))
  perspective_parser . add_argument (
    "--port", type = int, default = None,
    help = "the server's port; it must be the one the sidecar records" )
  perspective_parser . add_argument (
    "--show-overPrivateText", action = "store_true",
    help = ( "under a skgrepo restriction, approve releasing text from "
             "overPrivateText telescopes the switched view would show" ))
  commit_parser = commands . add_parser (
    "commit", help = ( "git-commit, with MESSAGE, every node whose changes "
                       "are all processed, except one that needs a node "
                       "whose changes are not; with '--all', every changed "
                       "node of the owned skgrepos" ),
    parents = [ port_option ] )
  commit_parser . add_argument ( "message" )
  commit_parser . add_argument (
    "--all", action = "store_true",
    help = ( "commit every changed node, processed or not; the server "
             "asks first when some is not processed" ))
  commit_parser . add_argument (
    "--yes", action = "store_true",
    help = ( "commit every changed node, as '--all' does, answering yes "
             "when the server asks because some is not processed" ))
  close_parser = commands . add_parser (
    "close", help = "send 'close view' for a sidecar's view ID" )
  close_parser . add_argument ( "sidecar" )
  close_parser . add_argument (
    "--port", type = int, default = None,
    help = "the server's port; it must be the one the sidecar records" )
  raw_parser = commands . add_parser (
    "raw", help = "send one request line; print every response",
    parents = [ port_option ] )
  raw_parser . add_argument ( "request", help = "the request, one sexp" )
  raw_parser . add_argument (
    "--saved-buffer", metavar = "FILE", default = None,
    help = ( "send FILE's text as the length-prefixed body a "
             "'save buffer' request carries, and read up to the "
             "save's terminal response" ))
  raw_parser . add_argument (
    "--baseline", metavar = "FILE", default = None,
    help = ( "with '--saved-buffer', send FILE's text as the saved "
             "buffer's baseline, the text the server last drew for it; "
             "by default the empty baseline" ))
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
    help = ( "the BASE.view.json of the view (or its BASE.compact.org "
             "or BASE.full.org), or a directory holding only one" ))
  save_parser . add_argument (
    "--port", type = int, default = None,
    help = "the server's port; it must be the one the sidecar records" )
  save_parser . add_argument (
    "--yes", action = "store_true",
    help = ( "save, unless the report finds an error or asks whether a new "
             "headline rewords a removed one; this also answers yes to the "
             "question asked before unmarking several changes, and approves "
             "the forks the report lists" ))
  save_parser . add_argument (
    "--fork-into", metavar = "REPO", default = None,
    help = ( "the owned skgrepo of a fork's clone (default: the server's "
             "one owned skgrepo, when it owns exactly one)" ))
  save_parser . add_argument (
    "--hoist", action = "store_true",
    help = ( "answer the Hoist question Hoist: move the title and body a "
             "telescope selects below its home up to its home (without it, "
             "'--yes' answers Abort)" ))
  save_parser . add_argument (
    "--delete-from", choices = [ "visible", "all" ], default = None,
    help = ( "when records the skgrepo restriction keeps out name a node the "
             "save deletes, strip the ID from the visible records only, or "
             "from every owned record (without it, '--yes' cancels)" ))
  save_parser . add_argument (
    "--strandeds", choices = [ "reset", "keep" ], default = None,
    help = ( "when a move to another skgrepo strands relationships in their "
             "old skgrepos, reset each to its new default, or keep each where "
             "it is (without it, '--yes' saves nothing)" ))
  save_parser . add_argument (
    "--show-overPrivateText", action = "store_true",
    help = ( "under a skgrepo restriction, approve releasing the re-rendered "
             "view's text from overPrivateText telescopes (without it, the "
             "save happens and the files go stale)" ))
  reworded_answers = save_parser . add_mutually_exclusive_group ()
  reworded_answers . add_argument (
    "--keep-ids", action = "store_true",
    help = ( "answer each 'probably the same statement reworded' question "
             "yes: the new headline takes the removed one's handle, and so "
             "its node, with the new title" ))
  reworded_answers . add_argument (
    "--all-new", action = "store_true",
    help = ( "answer each 'probably the same statement reworded' question "
             "no: every new headline is a new node" ))
  import_parser = commands . add_parser (
    "import", help = "preview and apply a Markdown and Org import",
    parents = [ port_option ] )
  import_parser . add_argument (
    "input_dir", help = "the directory to import, as the server sees it" )
  import_parser . add_argument ( "repo", help = "the owned destination skgrepo" )
  import_parser . add_argument (
    "--preview-only", action = "store_true",
    help = "cancel after the preview instead of applying it" )
  import_parser . add_argument (
    "--host-root", metavar = "PATH", default = "",
    help = ( "the absolute host path of INPUT_DIR, for links to files by "
             "absolute path (default: none, which leaves them unresolved)" ))
  delete_references_parser = commands . add_parser (
    "delete-references",
    help = ( "remove every owned structured reference (contains, "
             "forks, hides) to the absent node ID" ),
    parents = [ port_option ] )
  delete_references_parser . add_argument ( "id" )
  delete_references_parser . add_argument (
    "--out-dir", default = None,
    help = "the folder whose daemon to run through, if one runs there" )
  delete_references_parser . add_argument (
    "--yes", action = "store_true", help = "send the request" )
  delete_references_parser . add_argument (
    "--leave-links", action = "store_true",
    help = ( "approve the cleanup when titles or bodies link to the ID: "
             "those links stay, dangling" ))
  receipt_parser = commands . add_parser (
    "receipt",
    help = ( "write a read receipt, a note TEXT, as the first child of your "
             "fork of ITEM (forking it if you hold none), and hide ITEM from "
             "your fork of the mailbox it is in; without '--yes', print what "
             "it would do" ),
    parents = [ port_option ] )
  receipt_parser . add_argument ( "item", help = "the ID of the mailed item" )
  receipt_parser . add_argument (
    "text", help = ( "the receipt's body, one line: how you processed the "
                     "item; it may hold links '[[id:ID][LABEL]]'" ))
  receipt_parser . add_argument (
    "--title", default = "Read.", help = "the receipt's title (default: 'Read.')" )
  receipt_parser . add_argument (
    "--mailbox", metavar = "ID", default = None,
    help = ( "the mailbox to hide ITEM from, or your fork of it, when ITEM is "
             "in several you read (default: the one it is in)" ))
  receipt_parser . add_argument (
    "--fork-into", metavar = "REPO", default = None,
    help = ( "the owned skgrepo of a new fork of ITEM (default: that of your "
             "fork of the mailbox, when ITEM is homed where the mailbox is)" ))
  receipt_parser . add_argument (
    "--no-hide", action = "store_true", help = "write the receipt, hiding nothing" )
  receipt_parser . add_argument (
    "--out-dir", default = None,
    help = "the folder whose daemon to run through, if one runs there" )
  receipt_parser . add_argument (
    "--yes", action = "store_true", help = "write the receipt and the hide" )
  restrict_parser = commands . add_parser (
    "restrict",
    help = ( "show the daemon's skgrepo restriction and the rungs, or set "
             "it: NAME, the most private owned skgrepo to show, and with "
             "'--foreign' whether foreign skgrepos are shown" ),
    parents = [ port_option ] )
  restrict_parser . add_argument ( "name", nargs = "?", default = None )
  restrict_parser . add_argument (
    "--foreign", choices = [ "shown", "hidden" ], default = None,
    help = "show or hide the foreign skgrepos" )
  restrict_parser . add_argument (
    "--out-dir", default = None,
    help = "the folder whose daemon to run through, if one runs there" )
  restrict_parser . add_argument (
    "--show-overPrivateText", action = "store_true",
    help = ( "approve releasing text from overPrivateText telescopes the "
             "views would show under the new restriction" ))
  chain_parser = commands . add_parser (
    "chain",
    help = ( "print the chain of the headline HANDLE in VIEW: links to the "
             "notes from the view's top down to it, joined by the graph's "
             "edges ('/' contains, '~' forks, '->' links to, 'hides'), as "
             "the clients' 'C-c U' pushes and 'C-c o l' pastes it; it needs "
             "no server" ),
    parents = [ port_option ] )
  chain_parser . add_argument (
    "view_files",
    help = ( "the view's BASE.view.json, BASE.full.org or BASE.compact.org, "
             "or a directory holding only one view" ))
  chain_parser . add_argument (
    "handle", help = "the headline's handle: its viewnode ID, as '0d'" )
  describe_parser = commands . add_parser (
    "describe",
    help = ( "print what the headline HANDLE in VIEW is, when it is a "
             "folder, and what each of its heralds means, one sentence a "
             "line, most informative first, as the clients' 'C-c v ?' "
             "shows them; it asks the server the view's sidecar names" ),
    parents = [ port_option ] )
  describe_parser . add_argument (
    "view_files",
    help = ( "the view's BASE.view.json, BASE.full.org or BASE.compact.org, "
             "or a directory holding only one view" ))
  describe_parser . add_argument (
    "handle", help = "the headline's handle: its viewnode ID, as '0d'" )
  linkstack_parser = commands . add_parser (
    "linkstack",
    help = ( "the linkstack, kept in OUT_DIR: 'push ID-OR-LINK', 'paste "
             "id|link|node', 'pop id|link|node', 'show'" ),
    parents = [ port_option ] )
  linkstack_parser . add_argument (
    "action", choices = [ "show", "push", "paste", "pop" ] )
  linkstack_parser . add_argument (
    "argument", nargs = "?", default = None,
    help = ( "for 'push', an ID or a link '[[id:ID][LABEL]]'; for 'paste' "
             "and 'pop', what to print: 'id', 'link' or 'node'" ))
  linkstack_parser . add_argument (
    "--out-dir", default = ".",
    help = "the folder of the linkstack's file (default: the current one)" )
  daemon_parser = commands . add_parser (
    "daemon", help = ( "keep one connection open, holding every view "
                       "its commands write, and rewrite a view's files "
                       "when the server rerenders it; commands for its "
                       "port, and those with its '--out-dir' naming no "
                       "port, run through it" ),
    parents = [ port_option ] )
  daemon_parser . add_argument (
    "--out-dir", default = ".",
    help = ( "the folder of its log and its socket (default: the current "
             "one)" ))
  daemon_parser . add_argument (
    "--stop", action = "store_true",
    help = "stop the daemon running in '--out-dir'" )
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
  connection holds: a daemon's (see "The daemon" in the README), which
  then no longer rewrites the view's files. A view ID the daemon holds
  for other files stays open. It talks to the sidecar's server."""
  with open (arguments . sidecar, encoding = "utf-8") as sidecar_file:
    sidecar = json . load (sidecar_file)
  require_port_of_sidecar (arguments . port, sidecar)
  directory = os . path . dirname ( os . path . abspath (arguments . sidecar) )
  base_name = os . path . basename (arguments . sidecar) [ : - len (".view.json") ]
  connection = connect_to_server ( sidecar [ "port" ] )
  try:
    if not connection . holds_view_for_other_files (
        sidecar [ "view_id" ], directory, base_name ):
      close_view_on_server (connection, sidecar [ "view_id" ])
      connection . forget_view ( sidecar [ "view_id" ] )
  finally:
    connection . close ()
  mark_sidecar_closed (arguments . sidecar)
  print (arguments . sidecar)

def command_raw (arguments):
  """Send ARGUMENTS.request, and with '--saved-buffer' its save body,
  whose baseline is the text of '--baseline' (by default empty);
  print each response as the server sent it. Without a body, every
  response arriving until the server is quiet for RAW_QUIET_SECONDS."""
  connection = SkgConnection (arguments . port)
  try:
    if arguments . saved_buffer is None:
      messages = connection . raw_request (arguments . request)
    else:
      with open (arguments . saved_buffer, encoding = "utf-8") as buffer_file:
        buffer_text = buffer_file . read ()
      baseline = ""
      if arguments . baseline is not None:
        with open (arguments . baseline, encoding = "utf-8") as baseline_file:
          baseline = baseline_file . read ()
      messages = connection . raw_save (
        arguments . request, buffer_text, baseline )
  finally:
    connection . close ()
  print_messages (messages)

def command_import (arguments):
  """Preview the import, then apply it, or with '--preview-only'
  cancel it, on one connection: the server binds the approval token
  to the connection that previewed. A request for a host root, sent
  when the input links to files by absolute path, is answered with
  '--host-root', the input directory's absolute path on the host; by
  default with the empty one, which leaves those links unresolved."""
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
      preview = send ( preview_fields + [ ( "host-root", arguments . host_root ) ] )
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

def command_perspective (arguments):
  """Switch a view's perspective: to the
  next in the view's cycle, as the clients' 'skg-cycle-perspective'
  ('C-c v o' in Emacs) does, which the server computes afresh; or to
  the one named ('perspective_of_argument'). The server switches the
  view it holds and rerenders it; the rerendered view is written over
  the three files, its sidecar records the perspective, which every
  later save of the view names, as the server needs, and the
  perspective is printed by the server's name for it. A client refuses the switch for
  a buffer with unsaved edits, which the rerender would replace, and
  so this refuses an edited compact view. A daemon holding the view
  sends the request on its connection; otherwise the full view is
  first saved back unchanged, naming the choice it was drawn with,
  which writes nothing and leaves the server holding it, as 'extend'
  does. When the switched view would show text from overPrivateText
  telescopes under a skgrepo restriction, the server asks first;
  '--show-overPrivateText' approves."""
  saved_view = load_saved_view (arguments . view_files)
  sidecar = saved_view . sidecar
  view_id = sidecar [ "view_id" ]
  require_port_of_sidecar (arguments . port, sidecar)
  if compact_view_has_edits (saved_view):
    raise SkgToolError (
      "Nothing was switched: the compact view has edits, which the "
      "switch's rerender would replace; save them first, or undo them." )
  requested = perspective_of_argument (arguments . perspective)
  connection = connection_for_saved_view (saved_view)
  registered = False
  try:
    if not connection . holds_view (view_id):
      register_view_by_saving_it_unchanged (
        connection, saved_view,
        "Nothing was switched: the server refused to take this view back "
        "as it stands; view the node again." )
      registered = True
    line = request_line ( "set perspective", ( "view-id", view_id ),
                          ( "perspective", requested ))
    response, rerendered, done = connection . request_with_rerender_of_view (
      line, view_id )
    if response_type (response) == "overPrivateText-telescope-confirmation":
      pids = overPrivateText_answer (
        response, arguments . show_overPrivateText, "",
        "Nothing was switched: run again with '--show-overPrivateText' to "
        "release that text." )
      response, rerendered, done = connection . request_with_rerender_of_view (
        line [ : -1 ] + approved_overPrivateText_field (pids) + ")", view_id )
    require_response_type (response, "perspective")
    refusal = response_field (response, "content")
    if refusal is not None:
      raise SkgToolError ( "The server refused: " + sexp_to_text (refusal) )
    perspective = response_text_field (response, "perspective")
    name = response_text_field (response, "perspective-name")
    if rerendered is None:
      raise SkgToolError (
        "The server switched the view but sent no rerender of it; view the "
        "node again. " + " " . join (
          sexp_to_text (error) for error in response_field (done, "errors") or [] ))
    connection . log_view_event (
      "perspective of", saved_view . directory, saved_view . base_name,
      ": " + name )
    written_paths = write_view_files (
      connection,
      RenderedView (
        view_id  = view_id,
        command  = sidecar [ "command" ],
        request  = sidecar [ "request" ],
        content  = sexp_to_text ( response_field (rerendered, "content") ),
        errors   = [ sexp_to_text (error) for error
                     in response_field (done, "errors") or [] ],
        warnings = [ sexp_to_text (warning) for warning
                     in response_field (done, "warnings") or [] ],
        perspective      = perspective,
        perspective_name = name ),
      saved_view . directory, saved_view . base_name, sidecar [ "summary" ] )
    release_view (connection, view_id)
  except SkgToolError:
    # The server holds a view registered for the request; a daemon's
    # connection would keep it, unwritten, until it stops.
    if registered:
      with contextlib . suppress (SkgToolError, OSError):
        release_view (connection, view_id)
    raise
  finally:
    connection . close ()
  print ( "Showing " + name + "." )
  for path in written_paths:
    print (path)

def perspective_of_argument (words):
  """The server's name ('docs/api-and-formats.org', "Set
  perspective") for the perspective the command line's WORDS name:
  'next' when there are none, else the words joined by spaces ('next',
  'root homes' or 'skgrepo NAME'), which the server checks."""
  return " " . join (words) if words else "next"

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
  connection = connect_to_server (arguments . port)
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
      "" ) # the baseline of a client's new empty view, the text it was created with
    rerendered = rerendered_view_of_save (
      messages [-1], { "view_id" : creating_view_id, "command" : "new",
                       "request" : {} } )
    close_view_on_server (connection, creating_view_id)
    connection . log_event ( "saved new root " + title )
    new_root = next ( ( line for line in
                        parse_full_view ( rerendered . content )
                        if line . is_headline ), None )
    node_id = ( headline_gnid (new_root . metadata)
                if new_root is not None and new_root . metadata is not None
                else None )
    if node_id is None:
      raise SkgToolError (
        "The save succeeded, but its re-rendered view names no new node." )
    view = fetch_single_root_view (connection, node_id)
    require_view_has_headlines (view)
    view . command = "view"
    written_paths = write_view_files (
      connection, view, arguments . out_dir,
      arguments . name or base_name_for ( "view", node_id ),
      not arguments . no_summary )
    release_view (connection, view . view_id)
  finally:
    connection . close ()
  print ( "New node: " + node_id )
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

def command_delete_references (arguments):
  """Send "delete references to absent node" for an ID the graph lacks,
  as the clients' 'skg-delete-references-to-absent-node' ('C-c d a')
  does: the server removes it from every owned 'contains', 'forks'
  and 'hides' list, and rerenders
  the views that showed one. Without '--yes' nothing is sent, since the
  server asks first only when titles or bodies link to the ID; when it
  asks, its warning is printed, and '--leave-links' approves, sending
  the request again with the preview it names. Like the clients, it
  refuses while a view the daemon holds has unsaved edits."""
  if not arguments . yes:
    print ( f"Not sent. This removes every owned structured reference to the "
            f"absent node {arguments . id} (in 'contains', 'forks' and "
            "'hides' lists); run again with '--yes'." )
    return
  connection = connect_to_server (arguments . port)
  try:
    refuse_while_held_views_have_edits (
      connection, "removing the references to an absent node" )
    line = request_line ( "delete references to absent node",
                          ( "id", arguments . id ))
    response = connection . request_then_rerender_stream (line)
    if response_type (response) == "delete-references-confirmation":
      print ( "Server: delete-references confirmation: titles or bodies link "
              f"to {arguments . id}; the cleanup leaves those links, dangling. "
              + response_text_field (response, "prompt") )
      print ( response_text_field (response, "content") . rstrip ("\n") )
      if not arguments . leave_links:
        raise SkgToolError (
          "Nothing was removed: run again with '--yes --leave-links' to "
          "remove the structured references and leave the links." )
      print ( "Approved: the request is sent again." )
      response = connection . request_then_rerender_stream (
        line [ : -1 ] + " (approved-preview . " + quote_string (
          response_text_field (response, "approved-preview") ) + "))" )
    if response_type (response) != "delete-references-result":
      raise SkgToolError (
        "The server refused: " + response_text_field (response, "content") )
    connection . log_event ( "removed the references to " + arguments . id )
  finally:
    connection . close ()
  print ( "Server: removed the references to " + arguments . id + ": " +
          response_text_field (response, "changed-memberships") +
          " membership(s) in " + response_text_field (response, "changed-nodes") +
          " node(s)." )
  print ( response_text_field (response, "content") . rstrip ("\n") )

def command_receipt (arguments):
  """Send "receipt" for ITEM, as the Emacs command 'skg-receipt' does:
  the server writes the note TEXT (titled '--title') as the first
  child of your fork of ITEM, forking it first if you hold none, and
  hides ITEM from your fork of the mailbox it is in, all in one write,
  then rerenders the views that show them. Without '--yes' it asks the
  server what it would do, and writes nothing. A refusal names what
  to choose: '--mailbox' or '--fork-into'. Like the clients, it
  refuses while a view the daemon holds has unsaved edits."""
  if "\n" in arguments . text or "\n" in arguments . title:
    raise SkgToolError ( "Nothing was sent: a receipt's text and title are "
                         "one line each." )
  fields = [ ( "item", arguments . item ), ( "text", arguments . text ),
             ( "title", arguments . title ) ]
  if arguments . mailbox is not None:
    fields . append (( "mailbox", arguments . mailbox ))
  if arguments . fork_into is not None:
    fields . append (( "fork-into", arguments . fork_into ))
  if arguments . no_hide:
    fields . append (( "hide", "false" ))
  if not arguments . yes:
    fields . append (( "preview", "true" ))
  connection = connect_to_server (arguments . port)
  try:
    if arguments . yes:
      refuse_while_held_views_have_edits ( connection, "the receipt" )
    response = connection . request_then_rerender_stream (
      request_line ( "receipt", *fields ))
    if response_type (response) != "receipt":
      raise SkgToolError (
        "The server refused: " + response_text_field (response, "content") )
    refusal = response_field (response, "refusal")
    if refusal is not None:
      raise SkgToolError ( "Nothing was written: " + sexp_to_text (refusal) +
                           receipt_choices_text (response) )
    if arguments . yes:
      connection . log_event ( "wrote a receipt for " + arguments . item )
  finally:
    connection . close ()
  if arguments . yes:
    print ( "Server: wrote the receipt " +
            response_text_field (response, "receipt-id") + " under " +
            response_text_field (response, "holder-id") + "." )
  else:
    print ( "Not written. The receipt would do this; run again with '--yes':" )
  for line in response_field (response, "lines") or []:
    print ( sexp_to_text (line) )

def receipt_choices_text (response):
  """The choices a receipt's refusal offers, as a sentence, or ""."""
  skgrepos = response_field (response, "choose-skgrepo")
  if skgrepos:
    return ( " Run again with '--fork-into' one of: " +
             ", " . join ( sexp_to_text (s) for s in skgrepos ) + "." )
  mailboxes = response_field (response, "choose-mailbox")
  if mailboxes:
    titles = response_field (response, "choose-mailbox-titles") or []
    return ( " Run again with '--mailbox' one of: " + ", " . join (
      f"{sexp_to_text (pid)} ('{sexp_to_text (title)}')"
      for pid, title in zip (mailboxes, titles) ) + "." )
  return ""

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

def command_restrict (arguments):
  """With neither NAME nor '--foreign', print the skgrepo restriction
  and its rungs, as 'skg-list-repo-sets' does. Otherwise set it, as
  the clients' 'skg-restrict-repo-set' ('C-c v l') does: NAME is the
  most private owned skgrepo to show, '--foreign' whether the foreign
  skgrepos are shown. The restriction belongs to the connection, so
  setting it needs a daemon, whose connection keeps it for every
  command run through it; the server rerenders the views it holds,
  which it rewrites. Refused while a held view has unsaved edits. When
  the new restriction would release text from overPrivateText
  telescopes, the server asks first; '--show-overPrivateText'
  approves."""
  connection = connect_to_server (arguments . port)
  try:
    if arguments . name is None and arguments . foreign is None:
      response = connection . request ( request_line ("list repo sets") )
      require_response_type (response, "repo-sets")
      rungs = [ sexp_to_text (rung) for rung in response_field (response, "sets") or [] ]
      print ( skgrepo_restriction_text (response, rungs) + "; available: " +
              ", " . join ( skgrepo_rung_label (rung, rungs) for rung in rungs ) + "." )
      return
    if not connection . holds_views_it_writes ():
      raise SkgToolError (
        "The skgrepo restriction belongs to a connection, and this command's "
        "own ends with it; start a daemon ('daemon --out-dir DIR &') and run "
        "'restrict' through it, so that the commands run through it see the "
        "restriction." )
    refuse_while_held_views_have_edits (
      connection, "switching the skgrepo restriction" )
    line = request_line (
      "set skgrepo restriction",
      * ( [ ( "name", arguments . name ) ] if arguments . name is not None else [] ),
      * ( [ ( "foreign-repos", arguments . foreign ) ]
          if arguments . foreign is not None else [] ))
    response = connection . request_then_rerender_stream (line)
    if response_type (response) == "overPrivateText-telescope-confirmation":
      pids = overPrivateText_answer (
        response, arguments . show_overPrivateText, "",
        "The skgrepo restriction is as it was: run again with "
        "'--show-overPrivateText' to release that text." )
      response = connection . request_then_rerender_stream (
        line [ : -1 ] + approved_overPrivateText_field (pids) + ")" )
    require_response_type (response, "skgrepo-restriction")
    refusal = response_field (response, "content")
    if refusal is not None:
      raise SkgToolError ( "The server refused: " + sexp_to_text (refusal) )
    rungs = [ sexp_to_text (rung) for rung in response_field (
      connection . request ( request_line ("list repo sets") ), "sets" ) or [] ]
    described = skgrepo_restriction_text (response, rungs)
    connection . log_event (described)
  finally:
    connection . close ()
  print (described)

def skgrepo_rung_label (rung, rungs):
  """How the clients show a rung ('skg--skgrepo-restriction-label'):
  the most private, which with foreign skgrepos shown is no
  restriction, says so."""
  return rung + ( " (no restriction)" if rungs and rung == rungs [-1] else "" )

def skgrepo_restriction_text (response, rungs):
  """What the clients say of a skgrepo restriction RESPONSE
  ('skg--skgrepo-restriction-message')."""
  rung = response_text_field (response, "restriction")
  if response_text_field (response, "foreign-repos") == "hidden":
    return f"Skgrepo restriction: {rung}; foreign repos hidden"
  return "Skgrepo restriction: " + skgrepo_rung_label (rung, rungs)

# The linkstack's file, in the folder '--out-dir' names: the stack as
# the clients' 'skg-view-linkstack' ('C-c v s') shows it, a headline
# per entry, its label as the title and its ID as the body, the top
# first. It may be edited by hand, as that buffer may.
LINKSTACK_FILE_NAME = "skg-for-ai.linkstack.org"
LINK_WITH_LABEL_PATTERN = re . compile ( r'\[\[id:([^\]\[]+)\](?:\[([^\]]*)\])?\]' )

def command_linkstack (arguments):
  """The clients' linkstack, kept in a file: 'push ID' ('C-c u') pushes
  the node ID with its title from the server, 'push [[id:ID][LABEL]]'
  the link's ID and label; 'show' ('C-c v s') prints the stack; 'paste
  id|link|node' ('C-c o i', 'o l', 'o n') prints, from the top entry,
  its ID, a link '[[id:ID][LABEL]]', or a headline's text 'LABEL
  ⟨node:ID⟩', which placed in a compact view puts that node there;
  'pop id|link|node' ('C-c O i', 'O l', 'O n') prints the same and
  removes the entry."""
  path = os . path . join (arguments . out_dir, LINKSTACK_FILE_NAME)
  stack = read_linkstack (path)
  if arguments . action == "show":
    if arguments . argument is not None:
      raise SkgToolError ( "'linkstack show' takes no argument." )
    print ( linkstack_text (stack) . rstrip ("\n") if stack
            else "The linkstack is empty." )
    return
  if arguments . action == "push":
    if arguments . argument is None:
      raise SkgToolError ( "'linkstack push' takes an ID or a link." )
    entry = linkstack_entry_to_push (arguments . argument, arguments . port)
    write_linkstack ( path, [ entry ] + stack )
    print ( f"Pushed: '{entry [1]}' ({entry [0]})." )
    return
  if arguments . argument not in ( "id", "link", "node" ):
    raise SkgToolError (
      f"'linkstack {arguments . action}' takes 'id', 'link' or 'node'." )
  if not stack:
    raise SkgToolError ( "The linkstack is empty." )
  gnid, label = stack [0]
  print ( { "id"   : gnid,
            "link" : f"[[id:{gnid}][{label}]]",
            "node" : f"{label} ⟨{PLACED_NODE_HANDLE_WORD}:{gnid}⟩" }
          [ arguments . argument ] )
  if arguments . action == "pop":
    write_linkstack ( path, stack [ 1 : ] )

# ============================================================
# The chain of a headline: its view-ancestry as links joined by the
# graph's edges, as the clients' 'skg-push-ancestry' ('C-c U') pushes
# it and 'skg-paste-link' pastes it ('elisp/skg-push-ancestry.el').
# ============================================================

# The relations an ancestry edge can be, in the order the edge prefers
# them when no birth fact chooses one; and what each reads as in a
# chain, between a note (first) and the one below it, by side: 'in'
# where the note above relates to the one below, 'out' the reverse.
ANCESTRY_RELATIONS = ( "contains", "forks", "linksTo", "hides" )
ANCESTRY_EDGE_TEXTS = {
  "contains" : { "in" : "/",     "out" : "\\" },
  "forks"    : { "in" : "~",     "out" : "~" },
  "linksTo"  : { "in" : "->",    "out" : "<-" },
  "hides"    : { "in" : "hides", "out" : "is hidden by" } }

def command_chain (arguments):
  """Print the chain of the headline whose handle is HANDLE in VIEW:
  the notes from the view's top-level one down to it, folder
  headlines skipped, each a link '[[id:ID][TITLE]]', joined by the
  edge the metadata records between them ('ancestry_edge'). Refuses,
  naming the two notes, where no edge joins a note to the one above."""
  parsed_view = load_saved_view (arguments . view_files) . parsed_view
  headlines = [ line for line in parsed_view if line . is_headline ]
  index = next ( ( i for i, line in enumerate (headlines)
                   if headline_viewnode_id (line . metadata) == arguments . handle ),
                 None )
  if index is None:
    raise SkgToolError ( f"No headline of the view has the handle {arguments . handle}." )
  if headline_gnid ( headlines [index] . metadata ) is None:
    raise SkgToolError ( f"The headline {arguments . handle} is no note: "
                         "a folder, say, which no chain can end in." )
  print ( chain_text ( headlines, index ))

def chain_text (headlines, index):
  """The chain of the headline HEADLINES [INDEX], root first."""
  below = headlines [index]
  pieces = [ chain_link (below) ]
  while True:
    found = nearest_note_above (headlines, headlines . index (below))
    if found is None:
      return " " . join ( reversed (pieces) )
    above, generation = found
    edge = ancestry_edge ( below . metadata, generation )
    if edge is None:
      raise SkgToolError (
        f"No edge of the graph joins '{below . title}' to '{above . title}', "
        "the note above it in this view, so no chain runs through them." )
    relation, sides = edge
    pieces += [ ANCESTRY_EDGE_TEXTS [relation] [ sides [0] ], chain_link (above) ]
    below = above

def chain_link (headline):
  return f"[[id:{headline_gnid (headline . metadata)}][{headline . title}]]"

def nearest_note_above (headlines, index):
  """( HEADLINE, GENERATION ) of the nearest ancestor of HEADLINES
  [INDEX] that names a node, GENERATION levels up, counting the
  headlines between (folders) as ancestor generations do; None at the
  top."""
  level = len ( headlines [index] . stars . strip () )
  generation = 0
  for candidate in reversed ( headlines [ : index ] ):
    candidate_level = len ( candidate . stars . strip () )
    if candidate_level < level:
      generation += level - candidate_level
      level = candidate_level
      if headline_gnid (candidate . metadata) is not None:
        return ( candidate, generation )
  return None

def ancestry_edge (metadata, generation):
  """( RELATION, SIDES ) joining the headline of METADATA to its
  ancestor GENERATION levels up, as 'skg--ancestry-edge' reads it: a
  birth fact for GENERATION chooses the relation, else the first of
  ANCESTRY_RELATIONS whose '(rels ...)' sides name GENERATION. None
  if no relation does."""
  view = metadata_form (metadata, "view")
  born = next ( ( fact for fact in ( metadata_form (view, "birth") or [] ) [ 1 : ]
                  if isinstance (fact, list) and len (fact) > 2
                  and fact [2] == generation ),
                None )
  relation = str ( born [0] ) if born else next (
    ( name for name in ANCESTRY_RELATIONS
      if ancestry_sides (metadata, name, generation) ), None )
  if relation is None:
    return None
  sides = ancestry_sides (metadata, relation, generation) or [ str ( born [1] ) ]
  if relation != "forks":
    sides = [ str ( born [1] ) ] if born else sides [ : 1 ]
  return ( relation, sides )

def ancestry_sides (metadata, relation, generation):
  return [ side for side in ( "in", "out" )
           if generation in ( relation_side (metadata, relation, side)
                              or ( 0, [] ) ) [1] ]

# ============================================================
# What a headline's heralds mean ('describe VIEW HANDLE'): the
# sentences the server writes ('server/herald_descriptions.rs') for
# the clients' 'skg-describe-at-point' ('C-c v ?').
# ============================================================

def command_describe (arguments):
  """Print the sentences saying what the headline whose handle is
  HANDLE in VIEW is, when it is a folder, and what each of its heralds
  means, one a line, most informative first, from the server the
  view's sidecar names."""
  saved_view = load_saved_view (arguments . view_files)
  require_port_of_sidecar (arguments . port, saved_view . sidecar)
  connection = SkgConnection ( saved_view . sidecar [ "port" ] )
  try:
    response = connection . request (
      describe_request_line ( saved_view . parsed_view, arguments . handle ))
  finally:
    connection . close ()
  require_response_type (response, "describe-heralds")
  error = response_field (response, "error")
  if error is not None:
    raise SkgToolError ( "The server could not describe it: " + sexp_to_text (error) )
  lines = response_field (response, "lines") or []
  for line in ( lines if isinstance (lines, list) else [ lines ] ):
    print ( sexp_to_text (line) )

def describe_request_line (parsed_view, handle):
  """The "describe heralds" request for the headline of PARSED_VIEW
  whose handle is HANDLE: its metadata as the full view holds it, with
  'pendingDetachment' when it is a pending detachment, and, for a
  folder of the diff view whose members' relationships changed,
  whether every such change is processed ('herald_contexts')."""
  headline = next ( ( line for line in parsed_view
                      if line . is_headline and line . metadata is not None
                      and headline_viewnode_id (line . metadata) == handle ),
                    None )
  if headline is None:
    raise SkgToolError ( f"No headline of the view has the handle {handle}." )
  context = herald_contexts (parsed_view) [ headline . line_number ]
  metadata_text = headline . metadata_text
  if context . pending_detachment:
    metadata_text = metadata_text [ : -1 ] + " pendingDetachment)"
  fields = [ ( "metadata", metadata_text ) ]
  if context . folder_change is not None:
    fields . append ( ( "folder-change",
                        "processed" if context . folder_change else "unprocessed" ))
  return request_line ("describe heralds", *fields)

def linkstack_entry_to_push (argument, port):
  """(ID, LABEL) for ARGUMENT, a link (its label, else the node's title)
  or an ID (the node's title), the title from the server on PORT, which
  must know the node."""
  link = LINK_WITH_LABEL_PATTERN . fullmatch ( argument . strip () )
  gnid = link . group (1) if link else argument . strip ()
  if link and link . group (2):
    return ( gnid, link . group (2) )
  if not WORD_PARAMETER_PATTERN . fullmatch (gnid):
    raise SkgToolError ( f"'{argument}' is neither an ID nor a link '[[id:ID][LABEL]]'." )
  connection = SkgConnection (port)
  try:
    entry = fetch_link_statuses_of_ids (connection, [ gnid ]) . get (gnid)
  finally:
    connection . close ()
  if entry is None or entry [ "status" ] != "resolved" or entry . get ("title") is None:
    raise SkgToolError (
      f"The server gives no title for {gnid}: " +
      ( "it names no node of the graph." if entry is None
        or entry [ "status" ] == "missing"
        else "the skgrepo restriction hides it." if entry [ "status" ] != "resolved"
        else "its search index may lag a save by a moment; try again." ))
  return ( entry [ "pid" ], entry [ "title" ] )

def read_linkstack (path):
  """[(ID, LABEL)] of the linkstack file at PATH, the top first; [] when
  there is none."""
  try:
    lines = read_text_file (path) . split ("\n")
  except FileNotFoundError:
    return []
  stack = []
  index = 0
  while index < len (lines):
    line = lines [index]
    if not line . strip ():
      index += 1
      continue
    if ( not line . startswith ("* ") or index + 1 >= len (lines)
         or not lines [ index + 1 ] . strip ()
         or lines [ index + 1 ] . startswith ("*") ):
      raise SkgToolError (
        f"Line {index + 1} of {path} is not an entry: a headline '* LABEL' "
        "with the ID on the next line." )
    stack . append (( lines [ index + 1 ] . strip (), line [ 2 : ] . strip () ))
    index += 2
  return stack

def linkstack_text (stack):
  return "" . join ( f"* {label}\n{gnid}\n" for gnid, label in stack )

def write_linkstack (path, stack):
  os . makedirs ( os . path . dirname ( os . path . abspath (path) ), exist_ok = True )
  write_text_file ( path, linkstack_text (stack) )

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
# Saving an edited compact view
# ============================================================

# A compact headline's handle, at the end of the line: its viewnode
# ID, then any display tokens.
COMPACT_HANDLE_PATTERN = re . compile (
  r'^(.*?) ?⟨([0-9a-z]+)((?: [^⟨⟩]*)?)⟩\s*$' )
# The handle tokens that ask for edit requests. The rebuild writes
# them into the headline's '(editRequest ...)': one of these
# ('stripAttribution' the clients' 'skg-strip-attribution', which
# removes a fork's attribution),
# the request of that name, and after it one processed request,
# '(processed OBJECT...)' or '(unprocessed OBJECT...)'.
EDIT_REQUEST_TOKENS = [ "delete", "restoreNode", "restoreRel", "stripAttribution" ]
PROCESSED_TOKENS = [ "processed", "unprocessed" ]
# With 'processed' or 'unprocessed', one of these says what it marks,
# as the clients' 'C-c m n', 'C-c m r' and 'C-c m b' do: each word with
# the objects it names.
PROCESSED_LETTER_TOKENS = { "node" : [ "node" ], "rel" : [ "rel" ],
                            "both" : [ "node", "rel" ] }
# With 'processed' or 'unprocessed': walk the subtree as 'subtree' does,
# but at a new node name the outward walk, as the clients' C-u C-u does.
OUTWARD_TOKEN = "outward"
# The walk each word asks the server for at a new node in content
# position.
WALK_OBJECT_OF_TOKEN = { "subtree" : "newContent", "outward" : "newOutward" }
# The handle words that ask for the edit requests with a parameter,
# each a prefix and its parameter: 'merge:ID', the clients'
# 'skg-set-merge-request' ('C-c s m'), whose node acquires ID by a
# nodeMerge; 'relRepo:NAME', their 'skg-set-relRepo' ('C-c s r'), the
# skgrepo NAME recording the headline's relationship to its parent.
MERGE_WORD_PREFIX = "merge:"
RELREPO_WORD_PREFIX = "relRepo:"
# The handle words that ask for the flag request, the clients'
# 'skg-set-flag-search-matching' ('C-c s x'), each with the flag's
# desired value.
SEARCH_MATCHING_TOKENS = { "noSearchMatching" : "true",
                           "searchMatching"   : "false" }
# The handle tokens that ask for view requests, each with the request
# the rebuild writes into the headline's '(viewRequests ...)':
# 'editable' the clients' 'skg-set-editable' ('C-c s e'), 'fork' their
# 'skg-fork-node' ('C-c m f'), 'write-protect' their
# 'skg-request-write-protected' ('C-c s w'; with 'subtree', 'C-c s W'
# and its request 'writeProtectedSubtreeView').
VIEW_REQUEST_TOKENS = [ ( "editable", "editableView" ), ( "fork", "fork" ),
                        ( "write-protect", "writeProtectedView" ) ]
WRITE_PROTECT_SUBTREE_REQUEST = "writeProtectedSubtreeView"
# The handle words that change whether a headline claims membership of
# its parent's list: 'detach' writes 'noClaim', as the clients'
# 'skg-detach' ('C-c s t') does, 'attach' removes it, as their
# 'skg-attach' ('C-c s a') does.
CLAIM_TOKENS = [ "detach", "attach" ]
# 'repo:NAME' moves the node to the owned skgrepo NAME, as the clients'
# 'skg-set-repo' ('C-c s s') does.
REPO_WORD_PREFIX = "repo:"
# 'subtree' asks for the subtree form of the other words of its handle,
# as a prefix argument (C-u) does in the clients: 'delete', 'processed',
# 'unprocessed', 'write-protect', 'repo:NAME', 'relRepo:NAME',
# 'noSearchMatching' and 'searchMatching'.
SUBTREE_TOKEN = "subtree"
# What a word may name: a skgrepo or an ID, written into metadata bare.
WORD_PARAMETER_PATTERN = re . compile ( r'[^\s()";⟨⟩]+' )
# A headline ending in '⟨node:ID⟩' places the existing node ID there,
# as the clients' 'skg-paste-node' ('C-c o n') does.
PLACED_NODE_HANDLE_WORD = "node"
PLACED_NODE_HANDLE_PATTERN = re . compile (
  r'^(.*?) ?⟨' + PLACED_NODE_HANDLE_WORD + r':([^\s⟨⟩]+)((?: [^⟨⟩]*)?)⟩\s*$' )
# A link followed by the annotation 'annotate_links' appended to it:
# '⟦→ ...⟧', '⟦✗ ...⟧' (which also matches the '⟦✗ missing⟧' views
# written before 'NOT_HELD_ANNOTATION' still hold), or that one.
ANNOTATED_LINK_PATTERN = re . compile (
  r'(\[\[id:[^\]\[]+\](?:\[[^\]]*\])?\])(?:⟦[→✗][^⟧]*⟧|' +
  re . escape (NOT_HELD_ANNOTATION) + r')' )

class OutlineHeadline:
  """A headline of the full view or of the edited compact view.
  'handle' is its viewnode ID (None for a new headline),
  'marked_new' whether a new headline says it is new on purpose, with
  the handle '⟨new⟩',
  'parent' the index of its parent in the same outline (None at the
  top level), 'tokens' the display tokens of its handle, 'body' its
  body lines, link annotations stripped. A headline ending in
  '⟨node:ID⟩' has no handle and 'placed_node_id' ID: it places that
  existing node there. 'resolve_placed_nodes' then sets 'placed_pid'
  and 'placed_title', what the server calls the node, and
  'assign_placed_view_requests' 'placed_requests_editable', whether
  its metadata asks for an editable view."""
  def __init__ (self, handle, level, title, tokens, compact_line_number,
                marked_new = False, placed_node_id = None):
    self . handle = handle
    self . marked_new = marked_new
    self . placed_node_id = placed_node_id
    self . placed_pid = None
    self . placed_title = None
    self . placed_requests_editable = False
    self . level = level
    self . title = title
    self . tokens = tokens
    self . compact_line_number = compact_line_number
    self . body = []
    self . parent = None
    # The words a 'subtree' above gives it, each with the handle of
    # the headline carrying that 'subtree' ('expand_subtree_words').
    self . subtree_words = []
    self . subtree_origin = {}
    # The objects its processed request names, as the processed word
    # that reaches it decides ('resolve_processed_words'); None until
    # then.
    self . processed_objects = None
    # The words of its own handle a 'subtree' on it does not apply to
    # it: its relationship is not of the kind the gesture sets, say.
    self . dropped_words = set ()
    self . merge_title = None # the acquiree's, once resolved

  def words (self):
    """The words of its handle, but those a 'subtree' on it drops, and
    those a 'subtree' gives it."""
    return [ word for word in dict . fromkeys ( self . tokens + self . subtree_words )
             if word not in self . dropped_words ]

  def edit_request_words (self):
    return [ word for word in self . words () if is_edit_request_word (word) ]

  def processed_words (self):
    return [ word for word in PROCESSED_TOKENS if word in self . words () ]

  def processed_letter_words (self):
    return [ word for word in self . tokens if word in PROCESSED_LETTER_TOKENS ]

  def asks_outward (self):
    return OUTWARD_TOKEN in self . tokens

  def view_request_words (self):
    return [ word for word, _ in VIEW_REQUEST_TOKENS if word in self . words () ]

  def claim_words (self):
    return [ word for word in CLAIM_TOKENS if word in self . words () ]

  def repo_words (self):
    return [ word for word in self . words ()
             if word . startswith (REPO_WORD_PREFIX) ]

  def asks_subtree (self):
    return SUBTREE_TOKEN in self . tokens

  def merge_acquiree (self):
    """The ID its 'merge:ID' names, or None."""
    return next ( ( word [ len (MERGE_WORD_PREFIX) : ]
                    for word in self . edit_request_words ()
                    if word . startswith (MERGE_WORD_PREFIX) ), None )

  def has_requests (self):
    return bool ( self . edit_request_words () or self . processed_words ()
                  or self . processed_letter_words () or self . asks_outward ()
                  or self . view_request_words ()
                  or self . claim_words () or self . repo_words ()
                  or self . asks_subtree () )

  def is_placed (self):
    return self . placed_node_id is not None

  def report_title (self):
    """The title the report gives the headline: for a placed node, the
    one the server has, once resolved."""
    return self . placed_title if self . placed_title is not None else self . title

def edited_outline (saved_view):
  """The edited compact view of SAVED_VIEW, parsed, each body the
  compact view left out because it restates its note's ('restatements')
  read back from the full view: a headline whose handle still carries
  'body=b' and that has no body there gets the body the full view gave
  it. Text written under it is its body instead; to empty the body,
  take the word away."""
  edited = parse_edited_compact_view (saved_view . compact_text)
  original_body = { headline . handle : headline . body
                    for headline in outline_of_full_view (saved_view . parsed_view) }
  for headline in edited:
    if ( RESTATED_WORDS [ "body" ] in headline . tokens
         and headline . handle in original_body
         and not any ( line . strip () for line in headline . body )):
      headline . body = list ( original_body [ headline . handle ] )
  return edited

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

# The heralds whose words include a handle word, which a handle's words
# leave out: the labels of the hider and hidden folders hold 'forks',
# a word for the fork operation, which no folder takes.
HERALD_PHRASES_HOLDING_HANDLE_WORDS = (
  "It hides these from its forks.",
  "These hide it from their forks.",
  "hides these, but no fork partner shown contains them." )

def handle_words (display_text):
  """The words of a handle's display text: its tokens, the heralds of
  'HERALD_PHRASES_HOLDING_HANDLE_WORDS' left out."""
  for phrase in HERALD_PHRASES_HOLDING_HANDLE_WORDS:
    display_text = display_text . replace (phrase, " ")
  return display_text . split ()

def edited_headline (compact_line_number, level, text_after_stars):
  placed = PLACED_NODE_HANDLE_PATTERN . match (text_after_stars)
  if placed:
    return OutlineHeadline (
      None, level, strip_link_annotations ( placed . group (1) ),
      handle_words ( placed . group (3) ), compact_line_number,
      placed_node_id = placed . group (2) )
  match = COMPACT_HANDLE_PATTERN . match (text_after_stars)
  if not match:
    return OutlineHeadline (
      None, level, strip_link_annotations (text_after_stars) . rstrip (),
      [], compact_line_number )
  marked_new = match . group (2) == NEW_NODE_HANDLE_WORD
  return OutlineHeadline (
    None if marked_new else match . group (2), level,
    strip_link_annotations ( match . group (1) ),
    handle_words ( match . group (3) ), compact_line_number, marked_new )

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
  per handle whose request tokens make no one '(editRequest ...)',
  and per '⟨new⟩' or '⟨node:ID⟩' carrying another word, and per
  placed node with a body or children. HEADLINE_OF_HANDLE maps each
  viewnode ID of the full view to its headline. Whether the server
  accepts a request is the server's to say."""
  errors = []
  for index, headline in enumerate (edited):
    # '⟨new⟩' takes one 'repo:NAME', its home skgrepo; '⟨node:ID⟩' no word.
    other_words = (
      headline . tokens [ 1 : ] if ( headline . marked_new
                                    and headline . repo_words () == headline . tokens [ : 1 ] )
      else headline . tokens )
    if ( headline . marked_new or headline . is_placed () ) and other_words:
      errors . append (
        "Error: " + compact_handle_text (headline) + " on line "
        f"{headline . compact_line_number} of the compact view carries "
        + ", " . join ( "'" + token + "'" for token in other_words )
        + "; it takes no other word" +
        ( " but one 'repo:NAME'" if headline . marked_new else "" ) + "." )
    elif headline . marked_new and headline . repo_words () and not (
        WORD_PARAMETER_PATTERN . fullmatch (
          word_parameter ( headline . repo_words () [0] ))):
      errors . append (
        "Error: " + compact_handle_text (headline) + " on line "
        f"{headline . compact_line_number} of the compact view carries "
        f"'{headline . repo_words () [0]}', which names nothing." )
    if headline . is_placed ():
      errors += placed_node_shape_error_lines (edited, index)
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

# A handle's brackets. A title may not gain them: the compact view
# would read what follows as a handle (or a handle as part of the
# title), so the save would not be what was typed.
HANDLE_BRACKETS = ( "⟨", "⟩" )

def handle_bracket_title_error_lines (edited, headline_of_handle):
  """An error per new headline whose title holds a handle's bracket,
  and per headline whose title is edited to hold one. A title the full
  view already has is left as it is; a placed node takes its title
  from the server. (A title's '⟦' and '⟧' are free: only the link
  annotations write them, quoting a title's as look-alikes.)"""
  errors = []
  for headline in edited:
    if headline . is_placed ():
      continue
    brackets = [ bracket for bracket in HANDLE_BRACKETS
                 if bracket in headline . title ]
    if not brackets:
      continue
    view_line = ( None if headline . handle is None
                  else headline_of_handle . get ( headline . handle ) )
    if view_line is not None and view_line . title == headline . title:
      continue
    where = (
      f"the new headline on line {headline . compact_line_number} "
      "of the compact view has the title" if view_line is None
      else f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
           "of the compact view has its title edited to" )
    errors . append (
      f"Error: {where} '{headline . title}': " + quoted_words (brackets)
      + ( " are the brackets" if len (brackets) > 1 else " is a bracket" )
      + " of a handle, and the compact view can misread a title holding "
      "one. Write the title without '⟨' and '⟩' "
      "('〈' and '〉', or '<' and '>', look alike)." )
  return errors

def compact_handle_text (headline):
  """The handle a headline without a viewnode ID was written with:
  '⟨new⟩' or '⟨node:ID⟩'."""
  if headline . is_placed ():
    return "⟨" + PLACED_NODE_HANDLE_WORD + ":" + headline . placed_node_id + "⟩"
  return "⟨" + NEW_NODE_HANDLE_WORD + "⟩"

def placed_node_shape_error_lines (edited, index):
  """Errors for the placed node at INDEX of the edited outline that has
  body lines or children. The headline is saved write-protected, as the
  clients' paste writes it, and a write-protected occurrence saves
  nothing of its own: its body would not be written, and no child of
  it claims membership, so a new child would be an orphan graphnode
  and a moved one would leave its parent for nowhere. Both wait for
  the save that places it, after which it is editable."""
  headline = edited [index]
  where = ( compact_handle_text (headline) + " on line " +
            f"{headline . compact_line_number} of the compact view" )
  errors = []
  if any ( line . strip () for line in headline . body ):
    errors . append (
      f"Error: {where} has a body; a placed node is saved write-protected, "
      "which saves no body. Save the placement first, then edit its body." )
  if any ( other . parent == index for other in edited ):
    errors . append (
      f"Error: {where} has children; a placed node is saved write-protected, "
      "under which nothing claims membership. Save the placement first, "
      "then add them." )
  return errors

def is_edit_request_word (word):
  """Whether WORD asks for an edit request: 'delete', 'restoreNode',
  'restoreRel', 'stripAttribution', 'merge:ID', 'relRepo:NAME', 'noSearchMatching',
  'searchMatching'."""
  return ( word in EDIT_REQUEST_TOKENS or word in SEARCH_MATCHING_TOKENS
           or word . startswith (MERGE_WORD_PREFIX)
           or word . startswith (RELREPO_WORD_PREFIX) )

def edit_request_part_of_word (word):
  """The part of '(editRequest ...)' an edit-request WORD asks for."""
  if word in SEARCH_MATCHING_TOKENS:
    return "(flag noSearchMatching " + SEARCH_MATCHING_TOKENS [word] + ")"
  if word . startswith (MERGE_WORD_PREFIX):
    return "(merge " + word [ len (MERGE_WORD_PREFIX) : ] + ")"
  if word . startswith (RELREPO_WORD_PREFIX):
    return "(relRepo " + word [ len (RELREPO_WORD_PREFIX) : ] + ")"
  return word

def word_parameter (word):
  """The parameter of a word 'PREFIX:PARAMETER' ('repo:', 'relRepo:',
  'merge:')."""
  return word . split (":", 1) [1]

def quoted_words (words):
  return " and " . join ( "'" + word + "'" for word in words )

# The kinds that may carry each kind of word: what the clients'
# commands accept ('shared/metadata-fields.json' says which kinds take
# 'noClaim').
RELREPO_KINDS = ( "node", "unknown", "restricted", "alias" )
CLAIM_KINDS = ( "node", "ghost", "unknown", "restricted", "alias", "id" )

def request_token_problems (headline, view_line):
  """Why HEADLINE's request tokens make no one '(editRequest ...)'
  for VIEW_LINE, the headline its handle names, or ask for what its
  kind does not take."""
  problems = []
  if view_line . metadata is None:
    problems . append ( "carries a request, but the headline has no metadata." )
    return problems
  kind = headline_kind (view_line . metadata)
  if len ( headline . edit_request_words () ) > 1:
    problems . append (
      f"carries {quoted_words ( headline . edit_request_words () )}"
      + subtree_origin_text (headline, headline . edit_request_words ())
      + "; a headline takes one edit request." )
  if len ( headline . processed_words () ) > 1:
    problems . append ( "carries both 'processed' and 'unprocessed'"
                        + subtree_origin_text (headline, PROCESSED_TOKENS) + "." )
  own_processed_words = [ word for word in headline . tokens if word in PROCESSED_TOKENS ]
  marking_words = headline . processed_letter_words () + (
    [ OUTWARD_TOKEN ] if headline . asks_outward () else [] )
  if own_processed_words and len ( headline . processed_letter_words () ) != 1:
    problems . append (
      f"carries '{own_processed_words [0]}', which takes one of 'node', 'rel' "
      "and 'both', saying what it marks"
      + ( f", but carries {quoted_words ( headline . processed_letter_words () )}."
          if headline . processed_letter_words () else "." ))
  if marking_words and not own_processed_words:
    problems . append (
      f"carries {quoted_words (marking_words)} without 'processed' or 'unprocessed'." )
  only_on_a_node = (
    headline . view_request_words () + headline . repo_words () +
    [ word for word in headline . edit_request_words ()
      if word . startswith (MERGE_WORD_PREFIX) or word in SEARCH_MATCHING_TOKENS
      or word == "stripAttribution" ] )
  if only_on_a_node and kind != "node":
    problems . append (
      f"carries {quoted_words (only_on_a_node)}, but is not a node "
      "occurrence; only a node takes it." )
  relRepo_words = [ word for word in headline . edit_request_words ()
                    if word . startswith (RELREPO_WORD_PREFIX) ]
  if relRepo_words and kind not in RELREPO_KINDS and not (
      # 'relRepo:NAME subtree' on a writable folder sets its members'
      headline . asks_subtree () and folder_atom (view_line . metadata)
      in WRITABLE_FOLDER_RELATIONSHIP_KINDS ):
    problems . append (
      f"carries {quoted_words (relRepo_words)}, but is no node, alias, "
      "unknown viewnode or restricted viewnode; only those stand for a "
      "relationship whose skgrepo can be set." )
  if headline . claim_words () and kind not in CLAIM_KINDS:
    problems . append (
      f"carries {quoted_words ( headline . claim_words () )}, but a headline of "
      f"this kind ({kind}) claims no membership." )
  if len ( headline . claim_words () ) > 1:
    problems . append ( "carries both 'detach' and 'attach'." )
  if len ( headline . repo_words () ) > 1:
    problems . append ( f"carries {quoted_words ( headline . repo_words () )}; "
                        "a node has one home skgrepo." )
  if ( "write-protect" in headline . view_request_words ()
       and "editable" in headline . view_request_words () ):
    problems . append ( "carries both 'write-protect' and 'editable'." )
  for word in ( headline . edit_request_words () + headline . repo_words () ):
    if ( ":" in word and
         not WORD_PARAMETER_PATTERN . fullmatch ( word_parameter (word) )):
      problems . append ( f"carries '{word}', which names nothing." )
  if headline . asks_subtree () and not words_with_a_subtree_form (headline):
    problems . append (
      "carries 'subtree' with no word it applies to; 'subtree' applies to "
      "'delete', 'processed', 'unprocessed', 'write-protect', 'repo:NAME', "
      "'relRepo:NAME', 'noSearchMatching' and 'searchMatching'." )
  return problems

def subtree_origin_text (headline, words):
  """' (WORD from 'subtree' on ⟨ID⟩, ...)' for those of WORDS a
  'subtree' above gave HEADLINE; empty when none."""
  origins = [ f"'{word}' from 'subtree' on ⟨{headline . subtree_origin [word]}⟩"
              for word in words if word in headline . subtree_origin
              and word not in headline . tokens ]
  return " (" + ", " . join (origins) + ")" if origins else ""

def words_with_a_subtree_form (headline):
  """The words of HEADLINE's own handle that 'subtree' applies to."""
  return [ word for word in headline . tokens
           if word in ( "delete", "processed", "unprocessed", "write-protect" )
           or word in SEARCH_MATCHING_TOKENS
           or word . startswith (REPO_WORD_PREFIX)
           or word . startswith (RELREPO_WORD_PREFIX) ]

# ------------------------------------------------------------
# Words read against the outline: what a 'subtree' gives the
# headlines below it, and what 'detach', 'attach', 'repo:NAME',
# 'relRepo:NAME' and the flag words need of where they stand
# ------------------------------------------------------------

# The writable partner folders, each with the kind of relationship
# 'relRepo:NAME subtree' sets from it ('skg--writable-folder-relations'
# in 'elisp/skg-metadata.el'). The folder's viewparent records the
# relationship to each member that claims; a member of the forks folder
# claiming nothing either forks the viewparent, recording that in its
# own file, or is a mirror of it only through others.
WRITABLE_FOLDER_RELATIONSHIP_KINDS = { "forksFolder" : "forkee" }
# The partner folders whose members' relationships belong to the other
# end, so that no relRepo can be set from there
# ('skg--relRepo-unsupported-folder-atoms').
RELREPO_UNSUPPORTED_FOLDERS = (
  "hiderFolder", "hiddenFolder",
  "containerFolder", "mentionerFolder", "acquiredByFolder",
  "acquiredFolder", "illegalEditFolder", "integratedElsewhereFolder",
  "hiddenInForkPartnerFolder", "hiddenByForkerFolder",
  "hiddenOutsideOfForkPartnersFolder" )

class OutlineWordCheck:
  """What reading the handles' words against the outline found:
  'error_lines', which stop the save, and 'note_lines', what a
  'subtree' leaves out and why, which the report prints with the
  changes."""
  def __init__ (self):
    self . error_lines = []
    self . note_lines = []

def outline_word_check (edited, headline_of_handle, ownership):
  """Give each headline the words a 'subtree' above it gives it
  ('expand_subtree_words'); then check what the words ask of where
  they stand: a headline given words must still make one edit
  request; 'detach' and 'attach' need a parent that reads claims;
  'repo:NAME' an owned skgrepo and an editable node; 'relRepo:NAME' an
  owned skgrepo and a relationship the save writes; a flag word an
  editable node of an owned skgrepo. OWNERSHIP is {SKGREPO: OWNED}."""
  check = OutlineWordCheck ()
  expand_subtree_words (edited, headline_of_handle, ownership, check)
  for index, headline in enumerate (edited):
    if headline . marked_new and headline . repo_words ():
      skgrepo = word_parameter ( headline . repo_words () [0] )
      if ownership . get (skgrepo) is not True:
        check . error_lines . append (
          f"Error: ⟨new⟩ on line {headline . compact_line_number} of the "
          f"compact view carries '{headline . repo_words () [0]}', but "
          f"'{skgrepo}' is no owned skgrepo of this server; its owned ones "
          f"are: {owned_skgrepos_text (ownership)}." )
    if headline . handle is None or headline . handle not in headline_of_handle:
      continue
    view_line = headline_of_handle [ headline . handle ]
    if view_line . metadata is None:
      continue
    where = ( f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
              "of the compact view" )
    problems = (
      ( request_token_problems (headline, view_line)
        if headline . subtree_words else [] ) +
      claim_problems (edited, index, headline_of_handle) +
      repo_problems (headline, view_line, ownership) +
      relRepo_problems (edited, index, headline_of_handle, ownership) +
      search_matching_problems (headline, view_line, ownership) )
    check . error_lines += [ f"Error: {where} {problem}" for problem in problems ]
  return check

def outline_metadata (edited, index, headline_of_handle):
  """The metadata the headline at INDEX of the edited outline is read
  with by the save, as far as its kind and editability go: its full
  view headline's; a placed node's, a write-protected occurrence; a
  new headline's, an editable node. None for no headline, or a handle
  the full view lacks."""
  if index is None:
    return None
  headline = edited [index]
  if headline . is_placed ():
    return [ Symbol ("skg"), Symbol ("node"), Symbol ("writeProtected") ]
  if headline . handle is None:
    return [ Symbol ("skg"), Symbol ("node") ]
  view_line = headline_of_handle . get ( headline . handle )
  return None if view_line is None else view_line . metadata

def handled_metadata (edited, index, headline_of_handle):
  """The full view's metadata of the headline at INDEX, which a word can
  be written into; None for a new or placed headline."""
  if edited [index] . handle is None:
    return None
  return outline_metadata (edited, index, headline_of_handle)

def parent_reads_claims (edited, index, headline_of_handle):
  parent = edited [index] . parent
  return reads_claims (
    outline_metadata (edited, parent, headline_of_handle),
    None if parent is None else outline_metadata (
      edited, edited [parent] . parent, headline_of_handle ))

def claim_problems (edited, index, headline_of_handle):
  """Why 'detach' or 'attach' cannot stand on the headline at INDEX:
  its parent reads no claims, or it already claims (or does not) what
  the word asks."""
  headline = edited [index]
  if not headline . claim_words ():
    return []
  word = headline . claim_words () [0]
  if not parent_reads_claims (edited, index, headline_of_handle):
    return [ f"carries '{word}', but its parent reads no claims (it is "
             "write-protected, or no editable node), so no child claims "
             "membership of it." ]
  claims = not metadata_has_atom (
    headline_of_handle [ headline . handle ] . metadata, "noClaim" )
  if word == "detach" and not claims:
    return [ "carries 'detach', but claims nothing already." ]
  if word == "attach" and claims:
    return [ "carries 'attach', but claims membership already." ]
  return []

def owned_skgrepos_text (ownership):
  return ", " . join ( name for name, owned in ownership . items () if owned ) or "none"

def repo_problems (headline, view_line, ownership):
  """Why the 'repo:NAME' of HEADLINE's own handle cannot move its node:
  NAME is no owned skgrepo, the occurrence is write-protected (the
  save would refuse its edit, as 'skg-set-repo' warns), or the node is
  there already. One a 'subtree' left out of the move is no error."""
  words = [ word for word in headline . repo_words () if word in headline . tokens ]
  if not words:
    return []
  skgrepo = word_parameter ( words [0] )
  if ownership . get (skgrepo) is not True:
    return [ f"carries '{words [0]}', but '{skgrepo}' is no owned skgrepo of "
             f"this server; its owned ones are: {owned_skgrepos_text (ownership)}." ]
  if metadata_has_atom (view_line . metadata, "writeProtected"):
    return [ f"carries '{words [0]}', but is write-protected here, so the save "
             "would refuse its edit; write it on the node's editable "
             "occurrence." ]
  if headline_node_repo (view_line . metadata) == skgrepo:
    return [ f"carries '{words [0]}', but its home skgrepo is '{skgrepo}' already." ]
  return []

def relRepo_problems (edited, index, headline_of_handle, ownership):
  """Why the 'relRepo:NAME' of the own handle of the headline at INDEX
  can set no relationship's skgrepo, as 'skg--rel-at-point' refuses:
  NAME is no owned skgrepo; the headline is a root; its parent is a
  folder whose relationships belong to the other end, a writable folder
  under a write-protected viewparent, a write-protected node, or
  neither a node nor a writable folder; or it is a member of the forks
  folder claiming nothing, which forks the folder's viewparent and
  records that in its own file. An alias sets its own relationship to
  its node."""
  headline = edited [index]
  words = [ word for word in headline . edit_request_words ()
            if word . startswith (RELREPO_WORD_PREFIX) and word in headline . tokens ]
  if not words:
    return []
  skgrepo = word_parameter ( words [0] )
  if ownership . get (skgrepo) is not True:
    return [ f"carries '{words [0]}', but '{skgrepo}' is no owned skgrepo of "
             f"this server; its owned ones are: {owned_skgrepos_text (ownership)}." ]
  if headline_kind ( headline_of_handle [ headline . handle ] . metadata ) == "alias":
    return []
  parent = headline . parent
  parent_metadata = outline_metadata (edited, parent, headline_of_handle)
  if parent_metadata is None:
    return [ f"carries '{words [0]}', but is a root: there is no relationship "
             "here to set." ]
  folder = folder_atom (parent_metadata)
  if folder in RELREPO_UNSUPPORTED_FOLDERS:
    return [ f"carries '{words [0]}', but stands in a {folder}, whose "
             "relationships belong to the other end." ]
  if folder in WRITABLE_FOLDER_RELATIONSHIP_KINDS:
    if metadata_has_atom (
        headline_of_handle [ headline . handle ] . metadata, "noClaim" ):
      return [ f"carries '{words [0]}', but this member of the {folder} "
               "claims nothing: either it forks the view-parent and "
               "records that in its own file, or it is a mirror of the "
               "view-parent only through others; attach it first to make "
               "the view-parent fork it." ]
    if not is_editable_node ( outline_metadata (
        edited, edited [parent] . parent, headline_of_handle )):
      return [ f"carries '{words [0]}', but the folder's viewparent is "
               "write-protected here, so the save would not write this "
               "relationship." ]
    return []
  if headline_kind (parent_metadata) != "node":
    return [ f"carries '{words [0]}', but its parent is neither a node nor a "
             "writable folder." ]
  if not is_editable_node (parent_metadata):
    return [ f"carries '{words [0]}', but its parent is write-protected here, "
             "so the save would not write this relationship." ]
  return []

def search_matching_problems (headline, view_line, ownership):
  """Why a flag word of HEADLINE's own handle cannot stand there, as
  'skg-set-flag-search-matching' refuses: a write-protected
  occurrence, or a node of a foreign skgrepo."""
  words = [ word for word in headline . edit_request_words ()
            if word in SEARCH_MATCHING_TOKENS and word in headline . tokens ]
  if not words:
    return []
  if metadata_has_atom (view_line . metadata, "writeProtected"):
    return [ f"carries '{words [0]}', but is write-protected here; write it on "
             "the node's editable occurrence." ]
  if ownership . get ( headline_node_repo (view_line . metadata) ) is not True:
    return [ f"carries '{words [0]}', but its node is foreign; search "
             "matching is set on nodes of owned skgrepos only." ]
  return []

# ------------------------------------------------------------
# 'subtree': the clients' prefix-argument forms
# ------------------------------------------------------------

def expand_subtree_words (edited, headline_of_handle, ownership, check):
  """For each headline whose handle carries 'subtree', give the
  headlines below it the words it applies to, as the clients'
  commands with a prefix argument mark them ('give_subtree_word'),
  dropping from the headline itself those that do not apply to it.
  What is left out, and why, goes to CHECK's notes, and a word
  'subtree' cannot apply from where it stands to its errors."""
  for index, root in enumerate (edited):
    if ( root . handle is None
         or root . handle not in headline_of_handle
         or headline_of_handle [ root . handle ] . metadata is None ):
      continue
    resolve_processed_words (edited, index, headline_of_handle, check)
    if not root . asks_subtree ():
      continue
    for word in words_with_a_subtree_form (root):
      if word in PROCESSED_TOKENS:
        pass # 'resolve_processed_words'
      elif word == "delete":
        expand_delete_subtree (edited, index, headline_of_handle)
      elif word . startswith (REPO_WORD_PREFIX):
        expand_repo_subtree (edited, index, word, headline_of_handle, check)
      elif word . startswith (RELREPO_WORD_PREFIX):
        expand_relRepo_subtree (edited, index, word, headline_of_handle, check)
      elif word in SEARCH_MATCHING_TOKENS:
        expand_search_matching_subtree (
          edited, index, word, headline_of_handle, ownership, check )
      # 'write-protect' asks the server for its subtree form.

def give_subtree_word (edited, target, word, root):
  """Give the headline at TARGET the WORD the 'subtree' on the headline
  at ROOT applies to it."""
  headline = edited [target]
  if word in headline . tokens or word in headline . subtree_words:
    return
  headline . subtree_words . append (word)
  headline . subtree_origin [word] = edited [root] . handle

def walk_below (outline, index, visit):
  """Call VISIT (LATER) on each headline below the one at INDEX, in
  order, skipping those below a headline for which it returns False."""
  pruned_level = None
  for later in descendant_indices (outline, index):
    if pruned_level is not None and outline [later] . level > pruned_level:
      continue
    pruned_level = None
    if not visit (later):
      pruned_level = outline [later] . level

def expand_delete_subtree (edited, root, headline_of_handle):
  """'delete subtree', the clients' 'skg-delete-recursive' ('C-u C-c
  <backspace>'): every node occurrence below is deleted too; the other
  headlines (folders, ghosts, unknown viewnodes) are skipped."""
  for later in descendant_indices (edited, root):
    metadata = handled_metadata (edited, later, headline_of_handle)
    if metadata is not None and headline_kind (metadata) == "node":
      give_subtree_word (edited, later, "delete", root)

def home_skgrepo_move (metadata):
  """( OLD, NEW ) of a home skgrepo move viewnode's
  '(homeSkgrepoMove (old OLD) (new NEW))', or None."""
  move = metadata_form (metadata, "homeSkgrepoMove")
  if move is None:
    return None
  return ( sexp_to_text ( ( metadata_form (move, "old") or [ None, "" ] ) [1] ),
           sexp_to_text ( ( metadata_form (move, "new") or [ None, "" ] ) [1] ))

def resolve_processed_words (edited, root, headline_of_handle, check):
  """Decide what the 'processed' or 'unprocessed' of the handle at ROOT
  marks, as the clients' 'C-c m' gestures do: which headlines get a
  processed request, and the objects each names ('processed_objects').
  The letter word ('node', 'rel', 'both') names objects; 'subtree'
  ('C-u') and 'outward' ('C-u C-u') walk the subtree
  ('processed_requests_of_gesture'). On a home skgrepo move, 'subtree'
  or 'outward' is the clients' 'C-u C-c m h': every move between the
  same two skgrepos in the moved node's subtree. A gesture that names
  nothing is an error in CHECK, as the clients refuse it."""
  headline = edited [root]
  words = [ word for word in headline . tokens if word in PROCESSED_TOKENS ]
  letters = headline . processed_letter_words ()
  if len (words) != 1 or len (letters) != 1:
    return # 'request_token_problems' reports it
  word = words [0]
  letter_objects = PROCESSED_LETTER_TOKENS [ letters [0] ]
  walk_object = ( WALK_OBJECT_OF_TOKEN [OUTWARD_TOKEN] if headline . asks_outward ()
                  else WALK_OBJECT_OF_TOKEN [SUBTREE_TOKEN] if headline . asks_subtree ()
                  else None )
  root_metadata = headline_of_handle [ headline . handle ] . metadata
  move = home_skgrepo_move (root_metadata)
  if move is not None and walk_object is not None:
    requests = []
    if "node" in letter_objects:
      moved_node = headline . parent
      for target in ( [ root ] if moved_node is None else
                      [ moved_node ] + descendant_indices (edited, moved_node) ):
        metadata = handled_metadata (edited, target, headline_of_handle)
        if metadata is not None and home_skgrepo_move (metadata) == move:
          requests . append ( ( target, [] ) )
  else:
    requests = processed_requests_of_gesture (
      edited, root, letter_objects, walk_object, headline_of_handle )
  if not requests:
    check . error_lines . append (
      f"Error: ⟨{headline . handle}⟩ on line {headline . compact_line_number} "
      f"of the compact view carries '{word} {letters [0]}', but "
      + nothing_to_mark_text (letter_objects) + "." )
    return
  if root not in [ target for target, _ in requests ]:
    headline . dropped_words . add (word)
  for target, objects in requests:
    if target != root:
      give_subtree_word (edited, target, word, root)
      if any ( own in edited [target] . tokens for own in PROCESSED_TOKENS ):
        continue # its own handle's word decides its objects
    edited [target] . processed_objects = objects

def nothing_to_mark_text (letter_objects):
  """What a gesture naming LETTER_OBJECTS says when it names nothing."""
  if letter_objects == [ "node" ]:
    return "its node has no change to mark here"
  if letter_objects == [ "rel" ]:
    return "its relationship to its viewparent has no change to mark here"
  return "neither its node nor its relationship has a change to mark here"

def processed_requests_of_gesture (edited, root, letter_objects, walk_object,
                                   headline_of_handle):
  """( INDEX, OBJECTS ) for each headline the clients' gesture with
  LETTER_OBJECTS on the headline at ROOT writes a processed request on
  ('skg--processed-requests-at-point' and
  'skg--processed-requests-for-subtree' in 'elisp/skg-processed.el').
  Without WALK_OBJECT: the headline, naming what
  'processed_objects_named_by_letters' says, or on a changed folder
  each child that names some of them. With it, 'newContent' or
  'newOutward': every headline in the subtree that names some of them,
  but at a new node in content position the letters and WALK_OBJECT,
  which the server expands from the graph, whatever the buffer shows;
  the rest of that node's subtree is skipped."""
  metadata_of = lambda index: handled_metadata (edited, index, headline_of_handle)
  root_metadata = metadata_of (root)
  if walk_object is None:
    named = processed_objects_named_by_letters (root_metadata, letter_objects, True)
    if named is not None:
      return [ ( root, named ) ]
    if leaves_fact (root_metadata) is not None or not folder_is_changed (
        edited, root, headline_of_handle ):
      return []
    children = [ index for index in descendant_indices (edited, root)
                 if edited [index] . parent == root ]
    requests = []
    for child in children:
      metadata = metadata_of (child)
      named = ( None if metadata is None else
                processed_objects_named_by_letters (metadata, letter_objects) )
      if named is not None:
        requests . append ( ( child, named ) )
    return requests
  requests = []
  def visit (index):
    metadata = metadata_of (index)
    if metadata is None:
      return True
    at_root = index == root
    parent = edited [index] . parent
    parent_metadata = None if parent is None else metadata_of (parent)
    handed_over = node_added (metadata) and (
      at_root or parent_metadata is None or folder_atom (parent_metadata) is None )
    named = ( letter_objects + [ walk_object ] if handed_over else
              processed_objects_named_by_letters (metadata, letter_objects, at_root) )
    if named is not None:
      requests . append ( ( index, named ) )
    return not handed_over
  if visit (root):
    walk_below (edited, root, visit)
  return requests

def processed_objects_named_by_letters (metadata, letter_objects, at_gesture = False):
  """What a gesture naming LETTER_OBJECTS names on a headline with
  METADATA: None when nothing, else the request's objects, [] for a
  bare request ('skg--processed-objects-named-by-letters'). On a
  headline that lists objects, the letters among them. On one that
  names a single leaf, the bare request when the leaf is of a kind a
  letter names: a relRepo move's and an unknown or restricted
  viewnode's are a relationship's, any other the node's own. On a
  changeset marker AT_GESTURE, the letters themselves:
  its 'node' names its members' own leaves, which its '(leaves ...)'
  does not list."""
  leaves = leaves_fact (metadata)
  if leaves is None:
    return None
  if not leaves [1]:
    kind = ( "rel" if ( metadata_form (metadata, "relRepoMove") is not None
                        or metadata_has_atom (metadata, "unknown")
                        or metadata_has_atom (metadata, "restricted") )
             else "node" )
    return [] if kind in letter_objects else None
  if at_gesture and metadata_form (metadata, "changesetMarker") is not None:
    return list (letter_objects)
  objects = [ obj for obj in letter_objects if obj in leaves [1] ]
  return objects or None

def node_added (metadata):
  """Whether METADATA's headline is a new node: '(diff (node added))'."""
  node = metadata_form ( metadata_form (metadata, "diff"), "node" )
  return node is not None and len (node) == 2 and is_symbol_named ( node [1], "added" )

def folder_is_changed (edited, folder, headline_of_handle):
  """Whether the folder headline at FOLDER is shown because a member's
  relationship to its viewparent changed ('folder_change_processed')."""
  if folder_atom ( handled_metadata (edited, folder, headline_of_handle) or [ None ] ) is None:
    return False
  members = [] # each ( METADATA, [CHILD METADATA] )
  for index in descendant_indices (edited, folder):
    metadata = handled_metadata (edited, index, headline_of_handle)
    if edited [index] . parent == folder:
      members . append ( ( metadata, [] ) )
    elif ( members and metadata is not None
           and edited [ edited [index] . parent ] . parent == folder ):
      members [-1] [1] . append (metadata)
  return any ( membership_change_processed (member, children) is not None
               for member, children in members if member is not None )

def expand_repo_subtree (edited, root, word, headline_of_handle, check):
  """'repo:NAME subtree', the clients' 'skg-set-repo-recursive' ('C-c s
  S'): the node and every node below it reached through member
  occurrences whose home skgrepo is the node's move to NAME
  ('skg--do-at-recursive-repo-move-occurrences'). A write-protected
  occurrence is not moved (the save would refuse its edit) and is
  noted, unless the move reaches the node's editable occurrence."""
  root_metadata = headline_of_handle [ edited [root] . handle ] . metadata
  old_skgrepo = headline_node_repo (root_metadata)
  moved_gnids = set ()
  write_protected = []
  def consider (target):
    metadata = handled_metadata (edited, target, headline_of_handle)
    if metadata_has_atom (metadata, "writeProtected"):
      write_protected . append (target)
      if target == root:
        edited [root] . dropped_words . add (word)
      return
    moved_gnids . add ( headline_gnid (metadata) )
    give_subtree_word (edited, target, word, root)
  consider (root)
  def visit (later):
    metadata = handled_metadata (edited, later, headline_of_handle)
    if ( metadata is None or headline_kind (metadata) != "node"
         or metadata_has_atom (metadata, "noClaim") ):
      return False
    if headline_node_repo (metadata) == old_skgrepo:
      consider (later)
    return True
  walk_below (edited, root, visit)
  for target in write_protected:
    gnid = headline_gnid ( handled_metadata (edited, target, headline_of_handle) )
    if gnid not in moved_gnids:
      check . note_lines . append (
        "Not moved: " + described_handle ( edited [target] . handle, headline_of_handle )
        + f", write-protected here, which the save would refuse to edit; "
        f"'{word} subtree' on ⟨{edited [root] . handle}⟩ leaves it." )

def expand_relRepo_subtree (edited, root, word, headline_of_handle, check):
  """'relRepo:NAME subtree', the clients' 'skg-set-relRepo-recursive'
  ('C-c s R'), for the kind of relationship its headline names: on a
  node its content relationships, on a forks folder the forks of its
  claiming members, the walk of
  'skg--relRepo-recursive-targets'. The headline itself takes it only
  when its own relationship is of that kind."""
  root_metadata = headline_of_handle [ edited [root] . handle ] . metadata
  root_folder = folder_atom (root_metadata)
  kind = ( "content" if headline_kind (root_metadata) == "node"
           else WRITABLE_FOLDER_RELATIONSHIP_KINDS . get (root_folder) )
  if kind is None:
    check . error_lines . append (
      f"Error: ⟨{edited [root] . handle}⟩ on line "
      f"{edited [root] . compact_line_number} of the compact view carries "
      f"'{word} subtree', which applies on a node (its content "
      "relationships) or a forks folder." )
    return
  def metadata_of (index):
    return outline_metadata (edited, index, headline_of_handle)
  def member_as_such (index):
    # an occurrence member of a forks folder, claiming or not: its
    # content is shown in part, and never collected as its list
    metadata = metadata_of (index)
    return ( metadata is not None and headline_kind (metadata) == "node"
             and folder_atom ( metadata_of ( edited [index] . parent ) or
                               [ Symbol ("skg") ] ) == "forksFolder" )
  def prune_below (index):
    return ( metadata_has_atom ( metadata_of (index), "writeProtected" )
             or member_as_such (index) )
  def relationship_matches (index):
    metadata = metadata_of (index)
    member_kind = headline_kind (metadata)
    if not ( ( member_kind == "node" and not metadata_has_atom (metadata, "noClaim") )
             or member_kind in ( "unknown", "restricted" )):
      return False
    parent = edited [index] . parent
    parent_metadata = metadata_of (parent)
    if parent_metadata is None:
      return False
    if headline_kind (parent_metadata) == "node":
      return ( kind == "content" and is_editable_node (parent_metadata)
               and not member_as_such (parent) )
    return ( WRITABLE_FOLDER_RELATIONSHIP_KINDS . get ( folder_atom (parent_metadata) )
             == kind
             and is_editable_node ( metadata_of ( edited [parent] . parent )) )
  if root_folder is not None or not relationship_matches (root):
    # a folder stands for no relationship; a node, for its own when
    # its parent's list holds it
    edited [root] . dropped_words . add (word)
  if root_folder is None and prune_below (root):
    return
  def visit (later):
    if edited [later] . handle is None:
      return False
    metadata = metadata_of (later)
    member_kind = headline_kind (metadata)
    if member_kind == "node":
      if metadata_has_atom (metadata, "noClaim"):
        return False
      if relationship_matches (later):
        give_subtree_word (edited, later, word, root)
      return not prune_below (later)
    if member_kind in ( "unknown", "restricted" ):
      if relationship_matches (later):
        give_subtree_word (edited, later, word, root)
      return True
    return folder_atom (metadata) in WRITABLE_FOLDER_RELATIONSHIP_KINDS
  walk_below (edited, root, visit)

def expand_search_matching_subtree (edited, root, word, headline_of_handle,
                                    ownership, check):
  """'noSearchMatching subtree' or 'searchMatching subtree', the
  clients' 'C-u C-c s x': the node and every member occurrence below
  it, skipping, as 'skg--stage-flag-search-matching-recursive' does, a
  second occurrence of a node, a write-protected occurrence, one with
  another edit request, and a foreign node; the skipped ones but the
  repeated are noted."""
  targets = [ root ]
  def visit (later):
    metadata = handled_metadata (edited, later, headline_of_handle)
    if ( metadata is None or headline_kind (metadata) != "node"
         or metadata_has_atom (metadata, "noClaim") ):
      return False
    targets . append (later)
    return True
  walk_below (edited, root, visit)
  seen_gnids = set ()
  skipped = []
  for target in targets:
    metadata = handled_metadata (edited, target, headline_of_handle)
    gnid = headline_gnid (metadata)
    other_requests = [ other for other in edited [target] . tokens
                       if is_edit_request_word (other) and other != word ]
    reason = (
      "repeated" if gnid in seen_gnids
      else "write-protected" if metadata_has_atom (metadata, "writeProtected")
      else "it has another edit request" if other_requests
      else "foreign" if ownership . get ( headline_node_repo (metadata) ) is not True
      else None )
    if reason is None:
      seen_gnids . add (gnid)
      give_subtree_word (edited, target, word, root)
      continue
    if target == root:
      edited [root] . dropped_words . add (word)
    if reason != "repeated":
      skipped . append ( described_handle ( edited [target] . handle,
                                            headline_of_handle )
                         + " (" + reason + ")" )
  if skipped:
    check . note_lines . append (
      f"Skipped: '{word} subtree' on ⟨{edited [root] . handle}⟩ leaves "
      + ", " . join (skipped) + "." )

def processed_request_text (headline, view_line):
  """'(processed OBJECT...)' or '(unprocessed OBJECT...)' for HEADLINE's
  processed word, naming the objects 'resolve_processed_words' gave it;
  where that has not run (a dirty buffer's text, 'extend''s check), the
  objects its own letter names on the headline alone. None when it
  names none."""
  objects = ( headline . processed_objects
              if headline . processed_objects is not None
              else own_processed_objects (headline, view_line) )
  if not headline . processed_words () or objects is None:
    return None
  return "(" + " " . join ( headline . processed_words () [ : 1 ] + objects ) + ")"

def own_processed_objects (headline, view_line):
  """The objects the letter word of HEADLINE's own handle names on the
  headline alone, as a one-level gesture there names them, or None."""
  letters = headline . processed_letter_words ()
  if len (letters) != 1 or view_line is None or view_line . metadata is None:
    return None
  return processed_objects_named_by_letters (
    view_line . metadata, PROCESSED_LETTER_TOKENS [ letters [0] ], True )

def edit_request_text (headline, view_line):
  """The '(editRequest ...)' HEADLINE's tokens ask for, or None."""
  parts = [ edit_request_part_of_word (word)
            for word in headline . edit_request_words () ]
  processed = processed_request_text (headline, view_line)
  if processed:
    parts = parts + [ processed ]
  return "(editRequest " + " " . join (parts) + ")" if parts else None

def unmarked_leaf_count (headline, view_line):
  """How many leaves an 'unprocessed' on HEADLINE unmarks, counted as a
  client counts them ('leaves_touched')."""
  if ( headline . processed_words () != [ "unprocessed" ]
       or headline . processed_objects is None ):
    return 0
  return leaves_touched ( leaves_fact (view_line . metadata),
                          headline . processed_objects )

def leaves_touched (leaves, objects):
  """How many leaves a request naming OBJECTS touches on a headline whose
  '(leaves N OBJECT...)' is LEAVES, ( N, LISTED ), as the server counts
  them ('skg--processed-leaves-touched'). Of the objects listed: N when
  OBJECTS name them all; one for 'rel' alone; N less one for 'node'
  alone. An object not listed ('node' on a changeset marker, a walk)
  counts two, so that unmarking it always asks: the client cannot count
  what the server will reach. No fact, or a bare request, counts one."""
  listed = leaves [1] if leaves else []
  named_listed = [ obj for obj in objects if obj in listed ]
  unlisted = len ( [ obj for obj in objects if obj not in listed ] )
  if not leaves or not objects:
    count = 1
  elif not named_listed:
    count = 0
  elif len (named_listed) == len (listed):
    count = leaves [0]
  elif named_listed == [ "rel" ]:
    count = 1
  else:
    count = leaves [0] - 1
  return 2 * unlisted + count

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

def described_handle (handle, headline_of_handle):
  """'⟨ID⟩ 'TITLE'', or for a headline without a title '⟨ID⟩ (KIND)',
  from the full view."""
  view_line = headline_of_handle [handle]
  if view_line . title:
    return f"⟨{handle}⟩ '{view_line . title}'"
  return f"⟨{handle}⟩ ({headline_kind (view_line . metadata)})"

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

def descendant_indices (outline, index):
  level = outline [index] . level
  descendants = []
  for later in range ( index + 1, len (outline) ):
    if outline [later] . level <= level:
      break
    descendants . append (later)
  return descendants

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

# The handle '⟨new⟩' marks a headline without a handle as a new node on
# purpose, whatever removed headline its title equals or resembles.
NEW_NODE_HANDLE_WORD = "new"
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

# ============================================================
# The daemon: one long-lived connection, holding the views its
# commands write
# ============================================================
# The messages that carry a view the server rerendered.
RERENDER_MESSAGE_TYPES = { "collateral-view", "rerender-view" }
RESPONSE_TYPE_PATTERN = re . compile ( r'^\(\(response-type ([^\s()]+)\)' )
# The commands that name a sidecar, whose port is the sidecar's.
SIDECAR_COMMANDS = ( "save", "extend", "close", "perspective" )
# The commands that always run in their own process: 'raw' speaks the
# protocol directly, 'import' binds its approval to a connection of
# its own, 'linkstack' keeps a file, asking the server at most for
# a title, 'chain' reads a view's files alone, and 'describe' asks
# the server only about text it reads from them. ('commit --all'
# runs here too unless '--yes' answers for it: only a commit of
# everything can ask for a typed yes.)
COMMANDS_NEVER_SENT_TO_A_DAEMON = ( "raw", "import", "linkstack", "chain",
                                    "describe" )

def command_daemon (arguments):
  """Run a daemon for the folder '--out-dir' until it is stopped, or
  with '--stop' stop the one running there."""
  folder = os . path . realpath (arguments . out_dir)
  if arguments . stop:
    stop_daemon (folder)
    return
  if arguments . port is None:
    raise SkgToolError (NO_PORT_MESSAGE)
  os . makedirs (folder, exist_ok = True)
  SkgForAiDaemon (folder, arguments . port) . run ()

def stop_daemon (folder):
  """Ask the daemon in FOLDER to stop, and print its answer, which it
  sends once it has released its views and removed its sockets."""
  socket_path = os . path . join (folder, DAEMON_SOCKET_NAME)
  if not os . path . lexists (socket_path):
    raise SkgToolError ( f"No daemon runs in {folder}." )
  try:
    reply = daemon_reply (socket_path, { "stop" : True })
  except ConnectionRefusedError:
    os . remove (socket_path)
    raise SkgToolError (
      f"No daemon runs in {folder}; removed the socket a stopped one left." )
  print_daemon_reply (reply)

def port_daemon_socket_path (port):
  """The socket on which the daemon for PORT, if one runs, takes every
  command for that port: in XDG_RUNTIME_DIR when that is set, else in
  /tmp."""
  return os . path . join ( os . environ . get ("XDG_RUNTIME_DIR") or "/tmp",
                            f"skg-for-ai-{port}.sock" )

def exit_code_of_command_sent_to_daemon (arguments, argv):
  """When a daemon takes the command ARGUMENTS (parsed from ARGV), send
  ARGV to it, print what it printed, and return its exit code.
  Otherwise None: the command runs here. The daemon for the command's
  port takes it, whatever folder it writes into. A command that names
  no port (by '--port' or SKG_PORT) and has no sidecar to take one
  from goes to the daemon in its folder, which talks to its own
  server; there is no default port to try after it. A socket nobody
  answers on is one a daemon left when it died."""
  if ( arguments . command in COMMANDS_NEVER_SENT_TO_A_DAEMON
       or ( arguments . command == "commit" and arguments . all
            and not arguments . yes )):
    return None
  named_port = port_named_by_command (arguments, argv)
  port = ( named_port if named_port is not None
           else port_of_sidecar_of_command (arguments) )
  if port is not None:
    socket_paths = [ port_daemon_socket_path (port) ]
  else:
    folder = daemon_folder_of_command (arguments)
    socket_paths = ( [ os . path . join (folder, DAEMON_SOCKET_NAME) ]
                     if folder is not None else [] )
  for socket_path in socket_paths:
    reply = daemon_reply_if_one_answers (
      socket_path, { "argv" : argv, "cwd" : os . getcwd (),
                     "named_port" : named_port } )
    if reply is not None:
      print_daemon_reply (reply)
      return reply [ "exit_code" ]
  return None

def port_named_by_command (arguments, argv):
  """The port the command line names by '--port', or, for a command
  without a sidecar, by SKG_PORT; else None."""
  if ( any ( token == "--port" or token . startswith ("--port=")
             for token in argv )
       or ( arguments . command not in SIDECAR_COMMANDS
            and "SKG_PORT" in os . environ )):
    return arguments . port
  return None

def port_of_sidecar_of_command (arguments):
  """The port the sidecar of a 'save', 'extend', 'close' or
  'perspective' records; None for other commands, or when the
  sidecar cannot be read (the command then says why, where it runs)."""
  if arguments . command not in SIDECAR_COMMANDS:
    return None
  try:
    with open ( sidecar_path_of ( daemon_sidecar_argument (arguments) ),
                encoding = "utf-8" ) as sidecar_file:
      return json . load (sidecar_file) [ "port" ]
  except ( OSError, ValueError, KeyError, TypeError, SkgToolError ):
    return None

def daemon_sidecar_argument (arguments):
  return ( arguments . sidecar if arguments . command == "close"
           else arguments . view_files )

def daemon_folder_of_command (arguments):
  """The folder whose daemon a command runs through: its '--out-dir',
  or for 'save', 'extend', 'close' and 'perspective' its
  sidecar's folder. None for the commands that write no view."""
  if arguments . command in SIDECAR_COMMANDS:
    path = daemon_sidecar_argument (arguments)
  else:
    return getattr (arguments, "out_dir", None)
  return ( path if os . path . isdir (path)
           else os . path . dirname (path) or "." )

def ask_daemon_of_port_to_rerender_diffView_buffers (port):
  """After a git-commit on a connection of this process's own, ask the
  daemon for PORT, if one runs, to rerender the buffers of the diff
  view it holds, which the server rerenders after a git-commit only
  for the committing connection."""
  reply = daemon_reply_if_one_answers (
    port_daemon_socket_path (port),
    { "rerender_diffView_buffers" : True,
      "cause" : "of a git-commit on another connection" } )
  if reply is not None:
    print_daemon_reply (reply)

class SkgForAiDaemon:
  """One connection to the server on PORT, kept until the daemon
  stops, holding every view its commands write, wherever they write
  it; a Unix socket in FOLDER and another for PORT, on which commands
  arrive. It runs them one at a time, on that connection. Between them
  it reads what the server sends unprompted, and handles what the
  commands' responses carried and the unprompted messages that
  arrived meanwhile: it rewrites the files of each view the server
  rerendered, asking for the rerender of the views a
  'repo-files-reloaded' message names, as the clients do, and logs
  each event to FOLDER's log."""

  def __init__ (self, folder, port):
    self . folder = folder
    self . port = port
    self . socket_path = os . path . join (folder, DAEMON_SOCKET_NAME)
    self . port_socket_path = port_daemon_socket_path (port)
    self . log_path = os . path . join (folder, DAEMON_LOG_NAME)
    self . connection = None
    self . listeners = [] # (LISTENING SOCKET, ITS PATH)
    self . stopping = False
    self . stopped = False

  def run (self):
    replaced_stale_sockets = [
      socket_path for socket_path, what_it_serves in [
        ( self . socket_path, f"in {self . folder}" ),
        ( self . port_socket_path, f"for port {self . port}" ) ]
      if remove_stale_daemon_socket (socket_path, what_it_serves) ]
    self . connection = DaemonConnection (self . port, self . folder, self . log)
    server_connection . RUNNING_DAEMON = self
    try:
      self . log ( f"started: process {os . getpid ()}, server port {self . port}"
                   + "" . join ( f"; replaced the socket a dead daemon left at "
                                 f"{socket_path}"
                                 for socket_path in replaced_stale_sockets ))
      self . log_startup_warnings ()
      for socket_path in ( self . socket_path, self . port_socket_path ):
        self . listeners . append (
          ( listening_unix_socket (socket_path), socket_path ))
      for signal_number in ( signal . SIGTERM, signal . SIGINT ):
        signal . signal (signal_number, self . stop_at_next_pause)
      print ( f"The daemon listens on {self . socket_path} and, for every "
              f"command for port {self . port}, on {self . port_socket_path}; "
              f"it holds the views its commands write and logs to "
              f"{self . log_path}.", flush = True )
      self . serve_until_stopped ()
    finally:
      self . shut_down ()
      server_connection . RUNNING_DAEMON = None

  def stop_at_next_pause (self, signal_number, frame):
    self . stopping = True

  def log (self, text):
    """Append TEXT to the log as one timestamped line."""
    with open (self . log_path, "a", encoding = "utf-8") as log_file:
      log_file . write ( time . strftime ("%Y-%m-%d %H:%M:%S") + " " +
                         " | " . join ( text . splitlines () ) + "\n" )

  def log_startup_warnings (self):
    """The warnings the server raised as it started, which a client
    asks for once per connection."""
    response = self . connection . request ( request_line ("server warnings") )
    require_response_type (response, "server-warnings")
    for warning in response_field (response, "warnings") or []:
      self . log ( "warning: " + sexp_to_text (warning) )

  def serve_until_stopped (self):
    while not self . stopping and not self . connection . broken:
      try:
        self . handle_waiting_messages ()
        if self . stopping or self . connection . broken:
          break
        if self . connection . buffered:
          self . read_unrequested_message ()
          continue
        readable, _, _ = select . select (
          [ self . connection . socket ] +
          [ listener for listener, _ in self . listeners ], [], [],
          DAEMON_IDLE_POLL_SECONDS )
        if self . connection . socket in readable:
          self . read_unrequested_message ()
        else:
          for listener, _ in list (self . listeners):
            if listener in readable and not self . stopping:
              self . serve_one_request (listener)
      except SkgToolError as error:
        self . log ( "error: " + str (error) )
    if self . connection . broken:
      self . log ( "error: the connection to the server is lost; the daemon stops" )

  def read_unrequested_message (self):
    """Read a message the server sent between commands: an unprompted
    one, queued with the rest."""
    payload = self . connection . read_one_message (FIRST_MESSAGE_TIMEOUT_SECONDS)
    if payload . startswith (UNPROMPTED_REPO_CHANGE_MESSAGE_START):
      self . connection . handle_unprompted_message (payload)
      return
    self . connection . observe_message (payload)
    if payload_response_type (payload) not in RERENDER_MESSAGE_TYPES:
      self . log ( "error: the server sent, unasked: " + payload [ : 300 ] )

  def serve_one_request (self, listener):
    """Read one request from a socket LISTENER accepts, and answer it.
    A connection that sends nothing asks nothing."""
    client, _ = listener . accept ()
    with client:
      try:
        client . settimeout (DAEMON_CLIENT_TIMEOUT_SECONDS)
        request_bytes = received_until_closed (client)
        if not request_bytes:
          return
        request = json . loads ( request_bytes . decode ("utf-8") )
        client . settimeout (None)
      except ( OSError, ValueError ) as error:
        self . log ( f"error: a command arrived unreadable: {error}" )
        return
      reply = self . reply_to_request (request)
      try:
        client . sendall ( json . dumps (reply) . encode ("utf-8") )
      except OSError as error:
        self . log ( f"error: could not answer a command: {error}" )

  def reply_to_request (self, request):
    """Do what REQUEST asks: stop (before answering); describe this
    daemon, as a second one asks before it refuses to start; rerender
    the held buffers of the diff view, as a commit on another
    connection asks; or run a command, '{argv, cwd, named_port}'."""
    if request . get ("stop"):
      self . shut_down ()
      return { "stdout" : f"Stopped the daemon in {self . folder}.\n",
               "stderr" : "", "exit_code" : 0 }
    if request . get ("describe"):
      return { "stdout" : "", "stderr" : "", "exit_code" : 0,
               "process" : os . getpid (), "folder" : self . folder,
               "port" : self . port }
    if request . get ("rerender_diffView_buffers"):
      self . rerender_held_diffView_buffers ( request [ "cause" ] )
      reply = { "stdout" : "", "stderr" : "", "exit_code" : 0 }
    else:
      reply = self . reply_to_command (
        request [ "argv" ], request [ "cwd" ], request . get ("named_port") )
    # Before the answer, so that the files a command's save rerendered
    # are rewritten by the time it returns, and not while its caller
    # edits them.
    self . handle_waiting_messages ()
    return reply

  def reply_to_command (self, argv, cwd, named_port):
    """Run ARGV in the folder CWD, the sender's current one, on the
    port NAMED_PORT when the sender named one; return what it printed
    and its exit code."""
    standard_output = io . StringIO ()
    standard_error = io . StringIO ()
    description = described_command (argv)
    daemon_cwd = os . getcwd ()
    self . connection . begin_command (description)
    try:
      os . chdir (cwd)
      with contextlib . redirect_stdout (standard_output), \
           contextlib . redirect_stderr (standard_error):
        exit_code = self . exit_code_of_command (argv, named_port, description)
    except OSError as error:
      standard_error . write ( f"skg_for_ai: the daemon cannot run in {cwd}: {error}\n" )
      exit_code = 1
    finally:
      os . chdir (daemon_cwd)
      self . connection . end_command ()
    return { "stdout" : standard_output . getvalue (),
             "stderr" : standard_error . getvalue (),
             "exit_code" : exit_code }

  def exit_code_of_command (self, argv, named_port, description):
    """Run ARGV as a command run here would run, but on the daemon's
    connection; print as it prints."""
    try:
      arguments = parse_command_line (argv)
      if arguments . command == "daemon":
        raise SkgToolError ( f"A daemon already runs in {self . folder}." )
      self . use_own_port_unless_named (arguments, named_port)
      run_command (arguments)
      return 0
    except SkgToolError as error:
      print ( "skg_for_ai: " + str (error), file = sys . stderr )
      self . log ( f"error: '{description}' failed: {error}" )
      return 1
    except SystemExit as exit_request:
      # argparse's, for '--help' or a bad command line
      return ( exit_request . code if isinstance (exit_request . code, int)
               else 0 if exit_request . code is None else 1 )
    except Exception as error:
      traceback . print_exc ()
      self . log ( f"error: '{description}' failed: {error!r}" )
      return 1

  def use_own_port_unless_named (self, arguments, named_port):
    """A command the daemon runs talks to the port its sender named
    (NAMED_PORT, by '--port' or SKG_PORT), else to the daemon's
    server; 'save', 'extend', 'close' and 'perspective' talk to
    their sidecar's."""
    if named_port is not None:
      arguments . port = named_port
    elif arguments . command not in SIDECAR_COMMANDS:
      arguments . port = self . port

  def handle_waiting_messages (self):
    if self . connection . waiting_messages:
      # fetched afresh for each batch, as for each command
      self . connection . herald_tables = None
    while self . connection . waiting_messages and not self . connection . broken:
      payload, cause = self . connection . waiting_messages . popleft ()
      try:
        if payload . startswith (UNPROMPTED_REPO_CHANGE_MESSAGE_START):
          self . handle_repo_files_reloaded ( parse_sexp (payload) )
        else:
          self . rewrite_rerendered_view ( parse_sexp (payload), cause )
      except SkgToolError as error:
        self . log ( "error: " + str (error) )

  def handle_repo_files_reloaded (self, message):
    """Log the counts and the warnings, and ask for the rerender of
    each view named that the daemon holds and whose compact view has
    no unsaved edits, as the clients do ('Repo files reloaded' in
    'docs/api-and-formats.org')."""
    count_clauses = repo_files_reloaded_count_clauses (message)
    warnings = [ sexp_to_text (warning)
                 for warning in response_field (message, "warnings") or [] ]
    if count_clauses:
      self . log ( "changed on disk: " + "; " . join (count_clauses) )
    for warning in warnings:
      self . log ( "warning: " + warning )
    clean_view_ids = []
    for view_id in response_field (message, "view-ids") or []:
      view_id = sexp_to_text (view_id)
      files_stem = self . connection . files_stem_of_held_view . get (view_id)
      saved_view = ( self . held_saved_view (view_id, files_stem)
                     if files_stem is not None else None )
      if saved_view is None:
        continue
      if compact_view_has_edits (saved_view):
        self . log ( f"kept {self . connection . described_view_files (files_stem)}: "
                     "files changed on disk change a node it shows, but its "
                     "compact view has unsaved edits; its save is refused if it "
                     "would write a node that changed" )
        continue
      clean_view_ids . append (view_id)
    if clean_view_ids:
      self . request_rerender (
        clean_view_ids,
        "files changed on disk" + (
          " (" + "; " . join (count_clauses + warnings) + ")"
          if count_clauses or warnings else "" ))

  def rerender_held_diffView_buffers (self, cause):
    """Ask for the rerender of every held buffer of the diff view,
    because of CAUSE."""
    diffView_ids = [ view_id for view_id in self . connection . files_stem_of_held_view
                     if view_id . startswith ("diff-view:") ]
    if diffView_ids:
      self . request_rerender (diffView_ids, cause)

  def request_rerender (self, view_ids, cause):
    """Send "rerender all views" excluding every held view but
    VIEW_IDS, and read its stream; the rerendered views wait, with
    CAUSE, among the connection's messages."""
    excluded = [ view_id for view_id in self . connection . files_stem_of_held_view
                 if view_id not in view_ids ]
    self . connection . cause = cause
    try:
      self . connection . send_line (
        "((request . \"rerender all views\")" +
        ( " (exclude-view-ids" + "" . join ( " " + quote_string (view_id)
                                             for view_id in excluded ) + ")"
          if excluded else "" ) + ")" )
      while True:
        message = parse_sexp (
          self . connection . read_message (FIRST_MESSAGE_TIMEOUT_SECONDS) )
        kind = response_type (message)
        if kind == "rerender-done":
          return
        if kind == "overPrivateText-telescope-confirmation":
          self . log ( "error: the server withholds a rerender until over-private "
                       "text is approved, which the daemon does not do: " +
                       sexp_to_text ( response_field (message, "prompt") or "" ))
    finally:
      self . connection . cause = None

  def rewrite_rerendered_view (self, message, cause):
    """Write the view MESSAGE carries over its files, unless its compact
    view has unsaved edits, and log it with CAUSE and what changed."""
    view_id = sexp_to_text ( response_field (message, "view-id") )
    files_stem = self . connection . files_stem_of_held_view . get (view_id)
    if files_stem is None:
      return
    saved_view = self . held_saved_view (view_id, files_stem)
    if saved_view is None:
      return
    described_files = self . connection . described_view_files (files_stem)
    if compact_view_has_edits (saved_view):
      self . log ( f"kept {described_files}: the server rerendered it because "
                   f"{cause}, but its compact view has unsaved edits; its save "
                   "is refused if it would write a node that changed" )
      return
    content = sexp_to_text ( response_field (message, "content") )
    sidecar = saved_view . sidecar
    written_paths = write_view_files (
      self . connection,
      RenderedView (
        view_id  = view_id,
        command  = sidecar [ "command" ],
        request  = sidecar [ "request" ],
        content  = content,
        errors   = [],
        warnings = [],
        ** perspective_fields_of_sidecar (sidecar) ),
      saved_view . directory, saved_view . base_name, sidecar [ "summary" ],
      unless_compact_differs_from = saved_view . compact_text )
    if written_paths is None:
      self . log ( f"kept {described_files}: the server rerendered it because "
                   f"{cause}, but its compact view was edited meanwhile; its "
                   "save is refused if it would write a node that changed" )
      return
    self . log ( f"re-rendered {described_files} because {cause}; " +
                 changed_nodes_text (saved_view . full_text, content) )

  def held_saved_view (self, view_id, files_stem):
    """The files of the held view VIEW_ID, read back; None, and the
    view closed, when they cannot be read."""
    try:
      return SavedView ( files_stem + ".view.json" )
    except ( OSError, ValueError ) as error:
      self . log ( f"error: the files of "
                   f"{self . connection . described_view_files (files_stem)} "
                   f"cannot be read ({error}); the daemon closes its view" )
      close_view_on_server (self . connection, view_id)
      self . connection . forget_view (view_id)
      return None

  def shut_down (self):
    """Close every held view, marking its sidecar closed, then the
    connection and the sockets."""
    if self . stopped:
      return
    self . stopped = True
    self . stopping = True
    released_count = 0
    if self . connection is not None:
      for view_id, files_stem in list (
          self . connection . files_stem_of_held_view . items () ):
        try:
          if not self . connection . broken:
            close_view_on_server (self . connection, view_id)
          mark_sidecar_closed ( files_stem + ".view.json" )
        except ( SkgToolError, OSError, ValueError ) as error:
          self . log ( f"error: releasing "
                       f"{self . connection . described_view_files (files_stem)}: "
                       f"{error}" )
        released_count += 1
      self . connection . files_stem_of_held_view . clear ()
      self . connection . disconnect ()
    for listener, socket_path in self . listeners:
      listener . close ()
      if os . path . lexists (socket_path):
        os . remove (socket_path)
    self . listeners = []
    self . log ( f"stopped: released {released_count} "
                 f"view{'' if released_count == 1 else 's'}" )

def daemon_reply (socket_path, request):
  """Send REQUEST, a JSON object, to the daemon listening at
  SOCKET_PATH; return its answer: '{"stdout", "stderr", "exit_code"}',
  and more for some requests."""
  with unix_socket_connected_at (socket_path) as client:
    client . sendall ( json . dumps (request) . encode ("utf-8") )
    client . shutdown (socket . SHUT_WR)
    reply = received_until_closed (client)
  if not reply:
    raise SkgToolError ( f"The daemon at {socket_path} stopped before answering." )
  return json . loads ( reply . decode ("utf-8") )

def daemon_reply_if_one_answers (socket_path, request):
  """'daemon_reply', or None when no daemon listens at SOCKET_PATH: no
  socket, or one a dead daemon left."""
  if not os . path . lexists (socket_path):
    return None
  try:
    return daemon_reply (socket_path, request)
  except ( ConnectionRefusedError, FileNotFoundError ):
    return None

def print_daemon_reply (reply):
  sys . stdout . write ( reply [ "stdout" ] )
  sys . stdout . flush ()
  sys . stderr . write ( reply [ "stderr" ] )

def unix_socket_connected_at (socket_path):
  """A socket connected to the daemon socket SOCKET_PATH. It connects
  by the socket's name from inside its folder, since the path of a
  Unix socket is limited to about a hundred bytes."""
  client = socket . socket ( socket . AF_UNIX, socket . SOCK_STREAM )
  try:
    in_folder ( os . path . dirname (socket_path) or ".",
                lambda : client . connect ( os . path . basename (socket_path) ))
  except BaseException:
    client . close ()
    raise
  return client

def listening_unix_socket (socket_path):
  """A socket listening at SOCKET_PATH. It is bound under a temporary
  name in the same folder and then renamed, so the socket's name
  exists only once it listens."""
  folder = os . path . dirname (socket_path) or "."
  temporary_name = ( "." + os . path . basename (socket_path) + "." +
                     str ( os . getpid () ))
  temporary_path = os . path . join (folder, temporary_name)
  listener = socket . socket ( socket . AF_UNIX, socket . SOCK_STREAM )
  try:
    if os . path . lexists (temporary_path):
      os . remove (temporary_path)
    in_folder ( folder, lambda : listener . bind (temporary_name) )
    listener . listen (16)
    os . rename (temporary_path, socket_path)
  except BaseException:
    listener . close ()
    raise
  return listener

def remove_stale_daemon_socket (socket_path, what_it_serves):
  """Remove the daemon socket a dead daemon left at SOCKET_PATH, and say
  whether there was one. A socket a daemon answers on is refused: one
  daemon holds a folder's views, and one takes a port's commands.
  WHAT_IT_SERVES ('in FOLDER', 'for port PORT') says which."""
  try:
    mode = os . lstat (socket_path) . st_mode
  except FileNotFoundError:
    return False
  if not stat . S_ISSOCK (mode):
    raise SkgToolError ( f"{socket_path} exists and is not a socket." )
  try:
    running = daemon_reply (socket_path, { "describe" : True })
  except ConnectionRefusedError:
    os . remove (socket_path)
    return True
  except FileNotFoundError:
    return False
  raise SkgToolError (
    f"A daemon already runs {what_it_serves}: process {running [ 'process' ]}, "
    f"started with '--out-dir {running [ 'folder' ]} --port {running [ 'port' ]}'; "
    f"stop it with 'daemon --stop --out-dir {running [ 'folder' ]}'." )

def in_folder (folder, action):
  """ACTION's value, called with FOLDER the current directory."""
  previous_folder = os . getcwd ()
  os . chdir (folder)
  try:
    return action ()
  finally:
    os . chdir (previous_folder)

def received_until_closed (connected_socket):
  received = b""
  while True:
    chunk = connected_socket . recv (65536)
    if not chunk:
      return received
    received += chunk

def payload_response_type (payload):
  match = RESPONSE_TYPE_PATTERN . match (payload)
  return match . group (1) if match else None

def described_command (argv):
  """ARGV as the log names a command: each path by its last part."""
  return " " . join ( os . path . basename (token) if os . sep in token else token
                      for token in argv )

def changed_nodes_text (old_full_text, new_full_text):
  """What a rerender changed, node by node, for the log: each node new
  to the view, gone from it, or changed in its title, body or
  metadata (its relationships, among others), by title."""
  old = node_renderings_by_gnid (old_full_text)
  new = node_renderings_by_gnid (new_full_text)
  descriptions = (
    [ "new " + quoted_title ( new [gnid] . title )
      for gnid in new if gnid not in old ] +
    [ "gone " + quoted_title ( old [gnid] . title )
      for gnid in old if gnid not in new ] +
    [ quoted_title ( new [gnid] . title ) + " changed: " +
      ", " . join ( changed_parts_of_rendering ( old [gnid], new [gnid] ))
      for gnid in new
      if gnid in old and changed_parts_of_rendering ( old [gnid], new [gnid] ) ])
  if not descriptions:
    return "no node changed"
  shown = descriptions [ : LOGGED_CHANGED_NODES_LIMIT ]
  hidden_count = len (descriptions) - len (shown)
  return "; " . join (shown) + (
    f"; and {hidden_count} more" if hidden_count else "" )

NodeRendering = collections . namedtuple (
  "NodeRendering", [ "title", "body", "metadata_text" ] )

def node_renderings_by_gnid (full_text):
  """{GNID: NodeRendering} of the first occurrence of each node in a
  full view, its metadata without the viewnode ID."""
  renderings = {}
  current_gnid = None
  body_lines = []
  def finish ():
    if current_gnid is not None:
      renderings [current_gnid] = renderings [current_gnid] . _replace (
        body = "\n" . join (body_lines) )
  for line in parse_full_view (full_text):
    if not line . is_headline:
      body_lines . append ( line . text )
      continue
    finish ()
    body_lines = []
    gnid = ( headline_gnid ( line . metadata )
             if line . metadata is not None
             and headline_kind ( line . metadata ) == "node" else None )
    current_gnid = gnid if gnid is not None and gnid not in renderings else None
    if current_gnid is not None:
      renderings [current_gnid] = NodeRendering (
        line . title, "",
        re . sub ( r'\(vnid [0-9a-z]+\) ?', "", line . metadata_text ))
  finish ()
  return renderings

def changed_parts_of_rendering (old, new):
  return ( ( [ "title, was " + quoted_title ( old . title ) ]
             if old . title != new . title else [] ) +
           ( [ "body" ] if old . body != new . body else [] ) +
           ( [ "metadata" ] if old . metadata_text != new . metadata_text else [] ))

def quoted_title (title):
  return "'" + title + "'"

class DaemonConnection (SkgConnection):
  """The daemon's connection, kept until the daemon stops. It holds
  every view written on it, wherever its files are, each view ID for
  one set of files, which it knows by their stem: their folder's real
  path joined to their base name. The messages that carry a rerendered view, and the
  unprompted ones, wait among 'waiting_messages' for the daemon to
  handle between commands; the warnings of save results and rerender
  streams are logged as they are read."""

  def __init__ (self, port, folder, log):
    super () . __init__ (port)
    self . folder = folder
    self . log = log
    self . files_stem_of_held_view = {}
    self . views_written_by_command = set ()
    self . waiting_messages = collections . deque () # (PAYLOAD, CAUSE)
    self . cause = None # why the server rerenders what it sends now
    self . broken = False # whether a response was cut off or the server left

  def close (self):
    """A command's close leaves the daemon's connection open."""
    pass

  def disconnect (self):
    self . socket . close ()

  def receive_more (self):
    try:
      super () . receive_more ()
    except ( SkgServerQuietError, SkgConnectionClosedError ):
      self . broken = True
      raise

  def holds_views_it_writes (self):
    return True

  def holds_view (self, view_id):
    return view_id in self . files_stem_of_held_view

  def held_views (self):
    return list ( self . files_stem_of_held_view . items () )

  def holds_view_for_other_files (self, view_id, directory, base_name):
    held_files_stem = self . files_stem_of_held_view . get (view_id)
    return ( held_files_stem is not None
             and held_files_stem != view_files_stem (directory, base_name) )

  def files_stem_of_view (self, view_id):
    """The stem of the files that show the held view VIEW_ID, or None."""
    return self . files_stem_of_held_view . get (view_id)

  def note_view_written (self, view_id, out_dir, base_name):
    """Hold VIEW_ID for the files BASE_NAME, written into OUT_DIR. A
    view ID shows one set of files, and a set of files one view: a
    later view under the same view ID releases the earlier files
    (their sidecar is marked closed), and a later view written over
    the same files closes the earlier view."""
    files_stem = view_files_stem (out_dir, base_name)
    self . views_written_by_command . add (view_id)
    previous_files_stem = self . files_stem_of_held_view . get (view_id)
    if previous_files_stem == files_stem:
      return
    if previous_files_stem is not None:
      self . mark_released (previous_files_stem)
      self . log ( f"released {self . described_view_files (previous_files_stem)}: "
                   f"its view ID now shows {self . described_view_files (files_stem)}" )
    for other_view_id, other_files_stem in list (
        self . files_stem_of_held_view . items () ):
      if other_files_stem == files_stem and other_view_id != view_id:
        close_view_on_server (self, other_view_id)
        del self . files_stem_of_held_view [other_view_id]
    self . files_stem_of_held_view [view_id] = files_stem
    self . log ( f"holding {self . described_view_files (files_stem)} "
                 f"(view ID {view_id})" )

  def forget_view (self, view_id):
    files_stem = self . files_stem_of_held_view . pop (view_id, None)
    if files_stem is not None:
      self . log ( "closed " + self . described_view_files (files_stem) )

  def release_view_to_reopen (self, view_id):
    """Close the held view VIEW_ID, which a command is about to open
    again; its files are no longer held until the command writes the
    new view."""
    close_view_on_server (self, view_id)
    self . mark_released ( self . files_stem_of_held_view . pop (view_id) )

  def mark_released (self, files_stem):
    """Mark the sidecar of FILES_STEM closed, if it is still there."""
    try:
      mark_sidecar_closed ( files_stem + ".view.json" )
    except ( OSError, ValueError ):
      pass

  def described_view_files (self, files_stem):
    """How the log names a view's files: by base name in the daemon's
    folder, by stem elsewhere."""
    return ( os . path . basename (files_stem)
             if os . path . dirname (files_stem) == self . folder
             else files_stem )

  def log_event (self, text):
    self . log (text)

  def log_view_event (self, verb, directory, base_name, suffix = ""):
    self . log ( verb + " " + self . described_view_files (
      view_files_stem (directory, base_name) ) + suffix )

  def handle_unprompted_message (self, payload):
    self . waiting_messages . append (( payload, None ))

  def other_views_text (self, saved_view_id):
    """Every held view but SAVED_VIEW_ID, as the clients list their
    other buffers ('skg--other-view-buffer-snapshots'): one whose
    compact view has unsaved edits is dirty, its baseline the full view
    and its current text the buffer its save would send; the server
    refuses a save that would change what a dirty view shows. One
    whose files cannot be read is left clean: the daemon closes it
    when it next rewrites it."""
    entries = []
    for view_id, files_stem in self . files_stem_of_held_view . items ():
      if view_id == saved_view_id:
        continue
      try:
        saved_view = SavedView ( files_stem + ".view.json" )
      except ( OSError, ValueError ):
        saved_view = None
      if saved_view is None or not compact_view_has_edits (saved_view):
        entries . append ( "((view-id " + quote_string (view_id) + ") (dirty false))" )
        continue
      entries . append (
        "((view-id " + quote_string (view_id) + ") (dirty true) (baseline (present "
        + quote_string (saved_view . full_text) + ")) (current "
        + quote_string ( current_buffer_of_edited_view (saved_view) ) + "))" )
    return "(" + " " . join (entries) + ")"

  def observe_message (self, payload):
    kind = payload_response_type (payload)
    if kind in RERENDER_MESSAGE_TYPES:
      self . waiting_messages . append ((
        payload, self . cause or "the server rerendered it unasked" ))
    elif kind in ( "save-result", "rerender-done" ):
      for warning in response_field (
          parse_sexp (payload), "warnings" ) or []:
        self . log ( "warning: " + sexp_to_text (warning) )

  def begin_command (self, description):
    self . views_written_by_command . clear ()
    self . herald_tables = None # fetched afresh, once per command, as on a command's own connection
    self . cause = f"of '{description}' on the daemon's connection"

  def end_command (self):
    """Drop the waiting rerenders of the views the command itself
    wrote: it wrote them from what it read."""
    self . waiting_messages = collections . deque (
      ( payload, cause ) for payload, cause in self . waiting_messages
      if not ( payload_response_type (payload) in RERENDER_MESSAGE_TYPES
               and sexp_to_text ( response_field (
                     parse_sexp (payload), "view-id" ))
                   in self . views_written_by_command ))
    self . views_written_by_command . clear ()
    self . cause = None

def view_files_stem (directory, base_name):
  """The real path of DIRECTORY joined to BASE_NAME: what names a
  view's three files wherever they are reached from."""
  return os . path . join ( os . path . realpath (directory), base_name )

if __name__ == "__main__":
  try:
    sys . exit ( main ( sys . argv [ 1 : ] ))
  except SkgToolError as error:
    print ( "skg_for_ai: " + str (error), file = sys . stderr )
    sys . exit (1)
