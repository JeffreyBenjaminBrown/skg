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

import sys

from server_connection import NO_PORT_MESSAGE, SkgToolError
from daemon_client import (
  SIDECAR_COMMANDS, exit_code_of_command_sent_to_daemon )
from command_line import parse_command_line, run_command
from daemon import command_daemon

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

if __name__ == "__main__":
  try:
    sys . exit ( main ( sys . argv [ 1 : ] ))
  except SkgToolError as error:
    print ( "skg_for_ai: " + str (error), file = sys . stderr )
    sys . exit (1)
