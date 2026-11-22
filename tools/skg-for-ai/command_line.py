"""The command line: 'parse_command_line' declares every command and
option, and 'run_command' runs a parsed command by its function, in
this process (which may be a daemon's)."""

import argparse
import os

from views import FOREST_VIEW_REQUESTS
from save import command_save
from view_commands import (
  command_close, command_new, command_perspective, run_view_command )
from diff_and_commit import command_commit, command_diff, command_extend
from receipts import command_receipt
from request_commands import (
  command_delete_references, command_import, command_raw,
  command_restrict )
from headline_commands import (
  command_chain, command_describe, command_linkstack )

# ============================================================
# Commands
# ============================================================

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
