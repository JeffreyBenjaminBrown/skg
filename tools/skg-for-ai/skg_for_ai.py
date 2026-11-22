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

import collections
import contextlib
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

import server_connection
from sexp import parse_sexp, quote_string, sexp_to_text
from server_connection import (
  FIRST_MESSAGE_TIMEOUT_SECONDS, NO_PORT_MESSAGE, SkgConnection,
  SkgConnectionClosedError, SkgServerQuietError, SkgToolError,
  UNPROMPTED_REPO_CHANGE_MESSAGE_START,
  repo_files_reloaded_count_clauses, request_line,
  require_response_type, response_field, response_type )
from full_view import headline_gnid, headline_kind, parse_full_view
from views import (
  RenderedView, SavedView, close_view_on_server, mark_sidecar_closed,
  perspective_fields_of_sidecar, write_view_files )
from save import compact_view_has_edits, current_buffer_of_edited_view
from daemon_client import (
  DAEMON_LOG_NAME, DAEMON_SOCKET_NAME, SIDECAR_COMMANDS, daemon_reply,
  exit_code_of_command_sent_to_daemon, in_folder,
  port_daemon_socket_path, received_until_closed, stop_daemon )
from command_line import parse_command_line, run_command

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

# How often the daemon's idle wait looks up from its sockets, to
# notice a stop signal.
DAEMON_IDLE_POLL_SECONDS = 0.5
# How long the daemon waits for a command's request to arrive whole.
DAEMON_CLIENT_TIMEOUT_SECONDS = 10
# How many changed nodes a log line names before it counts the rest.
LOGGED_CHANGED_NODES_LIMIT = 6

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

# ============================================================
# The daemon: one long-lived connection, holding the views its
# commands write
# ============================================================
# The messages that carry a view the server rerendered.
RERENDER_MESSAGE_TYPES = { "collateral-view", "rerender-view" }
RESPONSE_TYPE_PATTERN = re . compile ( r'^\(\(response-type ([^\s()]+)\)' )

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
