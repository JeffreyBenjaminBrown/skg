"""How a command reaches a running daemon: the daemon's socket paths,
which commands are sent to one ('exit_code_of_command_sent_to_daemon'),
and asking one to stop or to rerender. The daemon itself is in
'daemon'."""

import json
import os
import socket
import sys

from server_connection import SkgToolError
from views import sidecar_path_of

# The daemon's files, in the folder whose views it holds.
DAEMON_SOCKET_NAME = "skg-for-ai.sock"
DAEMON_LOG_NAME = "skg-for-ai.log"
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
