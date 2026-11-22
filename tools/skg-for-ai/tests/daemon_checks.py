"""The checks of 'test_skg_for_ai.py' on the sixth scratch server, a
gitrepo: 'daemon', what it rewrites and logs, 'close', 'daemon
--stop', a stale socket."""

import os

from harness import (
  DIFF_BASE_NAME, PORT, CaseFailure, daemon_log, edit_compact, git,
  port_socket_path, read_file, read_sidecar, require_equal,
  require_in_diff_compact, require_in_file, require_log_line,
  require_not_in_diff_compact, require_output, require_routed_run,
  require_tool_run, run_tool, save_through_second_server, skg_title,
  start_daemon, start_second_server )

# ============================================================
# The daemon: each case runs on the graph and the daemon the earlier
# ones left. A second server, on the same data root with its own port
# and search index, stands for a teammate's: its saves reach the
# daemon's server only through the files on disk.
# ============================================================

RELOADED_ONE_MAIN_FILE = "changed on disk: reloaded 1 file from repo main"

def failures_of_daemon (scratch, data_root):
  context = { "scratch"       : scratch,
              "data_root"     : data_root,
              "main_repo"     : os . path . join (data_root, "me", "main"),
              "daemon_folder" : os . path . join (scratch, "daemon"),
              "processes"     : [] }
  try:
    for case in [ check_daemon_starts, check_daemon_writes_what_a_command_writes,
                  check_daemon_rewrites_a_collateral_rerender,
                  check_daemon_rewrites_after_a_change_on_disk,
                  check_daemon_keeps_unsaved_edits,
                  check_daemon_logs_a_reload_warning,
                  check_daemon_declares_its_unsaved_views,
                  check_daemon_takes_a_command_for_its_port,
                  check_second_daemon_on_the_port_refused,
                  check_daemon_commits_and_rewrites_a_diffView_buffer,
                  check_interactive_commit_tells_the_daemon,
                  check_daemon_closes_a_view, check_daemon_stops,
                  check_daemon_replaces_a_stale_socket ]:
      try:
        case (context)
      except CaseFailure as failure:
        return [ case . __name__ + ": " + str (failure) +
                 "\nThe daemon's log:\n" + daemon_log (context) ]
    return []
  finally:
    for process in context [ "processes" ]:
      if process . poll () is None:
        process . kill ()
        process . wait (timeout = 30)

def check_daemon_starts (context):
  start_daemon (context)
  require_log_line (context, "started: process ")

def check_daemon_writes_what_a_command_writes (context):
  """Each command, run once through the daemon (with no '--port': it
  talks to the daemon's server) and once on its own connection (with
  no daemon's socket to find), writes
  the same full and compact views; the sidecars differ only in
  whether the view is still open, which the daemon's is."""
  plain_folder = os . path . join ( context [ "scratch" ], "daemon-plain" )
  for tool_arguments, base_name in [
      ( [ "view", "root-id" ], "view_root-id" ),
      ( [ "links-here", "child-b-id" ], "links-here_child-b-id" ),
      ( [ "search", "Fred" ], "search_Fred" ) ]:
    require_routed_run ( context, tool_arguments )
    require_tool_run ( tool_arguments + [ "--out-dir", plain_folder ],
                       bypassing_daemons = True )
    for suffix in ( ".full.org", ".compact.org" ):
      require_equal (
        base_name + suffix + " through the daemon",
        read_file ( os . path . join ( context [ "daemon_folder" ], base_name + suffix )),
        read_file ( os . path . join ( plain_folder, base_name + suffix )) )
    daemon_sidecar = read_sidecar ( context [ "daemon_folder" ], base_name )
    plain_sidecar = read_sidecar (plain_folder, base_name)
    require_equal ( base_name + "'s sidecars, open or not",
                    ( daemon_sidecar . pop ("closed"), plain_sidecar . pop ("closed") ),
                    ( False, True ) )
    require_equal ( base_name + "'s sidecar through the daemon",
                    daemon_sidecar, plain_sidecar )
  require_log_line ( context, "holding view_root-id (view ID content-view:root-id)" )

def check_daemon_rewrites_a_collateral_rerender (context):
  """A save through the daemon of one view retitles a node another
  held view shows: the server rerenders that view as collateral, and
  the daemon rewrites its files, before the save returns, and logs
  why."""
  folder = context [ "daemon_folder" ]
  require_routed_run ( context, [ "view", "child-a-id" ] )
  edit_compact ( folder, [ ( "* First child ⟨0a ",
                             "* First child, renamed through the daemon ⟨0a " ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_in_file ( folder, "view_root-id",
                    "** First child, renamed through the daemon ⟨0d aC1⟩\n" )
  require_log_line ( context, "saved view_child-a-id" )
  require_log_line (
    context,
    "re-rendered view_root-id because of 'save view_child-a-id.view.json --yes' "
    "on the daemon's connection; 'First child, renamed through the daemon' "
    "changed: title, was 'First child'" )

def check_daemon_rewrites_after_a_change_on_disk (context):
  """A save on the second server writes a file the daemon's server did
  not write; its watcher reloads it and tells the daemon's
  connection, naming the views that show the node; the daemon asks
  for their rerender, rewrites their files and logs why."""
  start_second_server (context)
  second_folder = save_through_second_server (
    context, "view_root-id",
    ( "** Second child, a forker ⟨0f ",
      "** Second child, renamed on the second server ⟨0f " ))
  require_log_line ( context, RELOADED_ONE_MAIN_FILE )
  require_log_line (
    context,
    "re-rendered view_root-id because files changed on disk (reloaded 1 "
    "file from repo main); 'Second child, renamed "
    "on the second server' changed: title, was 'Second child, a forker'" )
  require_log_line ( context, "re-rendered links-here_child-b-id because files changed on disk" )
  require_in_file ( context [ "daemon_folder" ], "view_root-id",
                    "** Second child, renamed on the second server ⟨0f " )
  require_in_file ( context [ "daemon_folder" ], "links-here_child-b-id",
                    "* Second child, renamed on the second server ⟨0a " )
  context [ "second_folder" ] = second_folder

def check_daemon_keeps_unsaved_edits (context):
  """A held view whose compact view has unsaved edits is not
  rerendered when a change on disk touches it: its edits stay, and
  the log says so."""
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** First child, renamed through the daemon ⟨0d ",
                             "** First child, my unsaved edit ⟨0d " ) ])
  save_through_second_server (
    context, "view_child-a-id",
    ( "* First child, renamed through the daemon ⟨0a ",
      "* First child, renamed on the second server ⟨0a " ))
  require_log_line ( context, "kept view_root-id: files changed on disk change "
                              "a node it shows, but its compact view has unsaved edits" )
  require_log_line ( context, "re-rendered view_child-a-id because files changed on disk" )
  require_in_file ( folder, "view_root-id", "** First child, my unsaved edit ⟨0d " )

def check_daemon_logs_a_reload_warning (context):
  """A file the watcher cannot read is a warning in the log."""
  bad_path = os . path . join ( context [ "main_repo" ], "bad-id.skg" )
  with open (bad_path, "w", encoding = "utf-8") as bad_file:
    bad_file . write ( "title: [unclosed\n" )
  require_log_line ( context, "warning: Node bad-id changed on disk but could "
                              "not be read, so the server keeps serving its "
                              "previous version" )
  os . remove (bad_path)

def check_daemon_declares_its_unsaved_views (context):
  """A save through the daemon names, in its envelope's 'other-views',
  every other view the daemon holds, with the edited text of those
  whose compact views have unsaved edits, as the clients name their
  other buffers: the server then refuses a save that changes a node
  such a view shows, naming the view, and writes nothing. Undoing the
  unsaved edit lets the next case's save through."""
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice ⟨",
                             "** Contained twice, retitled through the daemon ⟨" ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if ( completed . returncode == 0
       or "this save conflicts with unsaved dependencies in content-view:root-id"
          not in completed . stdout ):
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "shared-id's title after the refused save",
                  skg_title (context, "shared-id"), "Contained twice" )
  edit_compact ( folder, [ ( "** Contained twice, retitled through the daemon ⟨",
                             "** Contained twice ⟨" ) ], "view_child-a-id" )
  edit_compact ( folder, [ ( "** First child, my unsaved edit ⟨0d ",
                             "** First child, renamed through the daemon ⟨0d " ) ])

def check_daemon_takes_a_command_for_its_port (context):
  """A command for the daemon's port that writes into another folder
  runs through the daemon, which writes its files there and holds its
  view: a later save through the daemon of a node that view shows
  rewrites it."""
  other_folder = os . path . join ( context [ "scratch" ], "daemon-other" )
  context [ "other_folder" ] = other_folder
  completed = require_tool_run ([ "view", "shared-id", "--out-dir", other_folder ])
  require_equal ( "the files the routed command wrote", completed . stdout,
                  "" . join ( os . path . join ( other_folder, "view_shared-id" + suffix )
                              + "\n"
                              for suffix in ( ".full.org", ".compact.org",
                                              ".view.json" )))
  require_equal ( "the routed sidecar's 'closed'",
                  read_sidecar (other_folder, "view_shared-id") [ "closed" ], False )
  other_stem = os . path . join ( os . path . realpath (other_folder), "view_shared-id" )
  require_log_line ( context, f"holding {other_stem} (view ID content-view:shared-id)" )
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice ⟨",
                             "** Contained twice, retitled through the daemon ⟨" ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_log_line (
    context,
    f"re-rendered {other_stem} because of 'save view_child-a-id.view.json --yes' "
    "on the daemon's connection; 'Contained twice, retitled through the daemon' "
    "changed: title, was 'Contained twice'" )
  require_in_file ( other_folder, "view_shared-id",
                    "* Contained twice, retitled through the daemon ⟨" )

def check_second_daemon_on_the_port_refused (context):
  """A second daemon for the same port, in another folder, refuses to
  start, naming the first one's process; the first keeps its
  sockets."""
  second_folder = os . path . join ( context [ "scratch" ], "daemon-second" )
  completed = run_tool ([ "daemon", "--out-dir", second_folder ])
  expected = ( f"A daemon already runs for port {PORT}: process "
               f"{context [ 'daemon' ] . pid}, started with '--out-dir "
               f"{os . path . realpath ( context [ 'daemon_folder' ] )} --port {PORT}'" )
  if completed . returncode == 0 or expected not in completed . stderr:
    raise CaseFailure ( f"the second daemon exited {completed . returncode}, "
                        f"printed {completed . stdout!r}, {completed . stderr!r}" )
  for socket_path in ( os . path . join ( context [ "daemon_folder" ], "skg-for-ai.sock" ),
                       port_socket_path () ):
    require_equal ( "whether " + socket_path + " remains",
                    os . path . lexists (socket_path), True )
  require_equal ( "whether the second daemon left a socket",
                  os . path . lexists ( os . path . join (second_folder, "skg-for-ai.sock") ),
                  False )

# What the summary of a buffer of the diff view says once a git-commit
# leaves it showing no change.
NO_CHANGE_FACTS = "# Headlines with change facts 0, "

def check_daemon_commits_and_rewrites_a_diffView_buffer (context):
  """'commit --yes' runs through the daemon, so the server rerenders
  the buffer of the diff view the daemon holds, and the daemon
  rewrites it: after the git-commit the buffer's headlines carry no
  change facts."""
  diff_folder = os . path . join ( context [ "scratch" ], "daemon-diff" )
  context [ "diff_folder" ] = diff_folder
  require_tool_run ([ "diff", "--out-dir", diff_folder ])
  require_in_diff_compact ( diff_folder, "Contained twice, retitled through the daemon" )
  require_not_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )
  require_equal ( "the held diff buffer's 'closed'",
                  read_sidecar (diff_folder, DIFF_BASE_NAME) [ "closed" ], False )
  completed = require_tool_run ([ "commit", "Commit through the daemon.", "--yes" ])
  if "Committed 3 nodes, in main (" not in completed . stdout:
    raise CaseFailure ( f"the commit printed {completed . stdout!r}" )
  require_equal ( "the git log after the commit through the daemon",
                  git ( context [ "main_repo" ], [ "log", "--format=%s" ] ),
                  "Commit through the daemon.\nThe fixture\n" )
  diff_stem = os . path . join ( os . path . realpath (diff_folder), DIFF_BASE_NAME )
  require_log_line (
    context, f"re-rendered {diff_stem} because of 'commit Commit through the "
             f"daemon. --yes --port {PORT}' on the daemon's connection" )
  require_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )

def check_interactive_commit_tells_the_daemon (context):
  """A 'commit --all' that waits for a typed "yes" runs on its own
  connection; when it has committed, it asks the daemon to rerender
  its buffers of the diff view, which the daemon rewrites. (The new
  'diff' writes over the held buffer's files, which closes the view
  ID they showed.)"""
  diff_folder = context [ "diff_folder" ]
  folder = context [ "daemon_folder" ]
  edit_compact ( folder, [ ( "** Contained twice, retitled through the daemon ⟨",
                             "** Contained twice, committed interactively ⟨" ) ],
                 "view_child-a-id" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_child-a-id.view.json"),
                           "--yes" ], port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_tool_run ([ "diff", "--out-dir", diff_folder ])
  require_in_diff_compact ( diff_folder, "Contained twice, committed interactively" )
  require_not_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )
  completed = run_tool ( [ "commit", "Commit interactively.", "--all" ],
                         standard_input = "yes\n" )
  if completed . returncode != 0 or "Committed 1 node, in main (" not in completed . stdout:
    raise CaseFailure ( f"the commit printed {completed . stdout!r}, {completed . stderr!r}" )
  require_log_line (
    context, f"re-rendered {os . path . join ( os . path . realpath (diff_folder), DIFF_BASE_NAME )} "
             "because of a git-commit on another connection" )
  require_in_diff_compact ( diff_folder, NO_CHANGE_FACTS )

def check_daemon_closes_a_view (context):
  folder = context [ "daemon_folder" ]
  sidecar_path = os . path . join (folder, "view_child-a-id.view.json")
  require_output ( run_tool ( [ "close", sidecar_path ], port = None ),
                   sidecar_path + "\n" )
  require_equal ( "the closed sidecar's 'closed'",
                  read_sidecar (folder, "view_child-a-id") [ "closed" ], True )
  require_log_line ( context, "closed view_child-a-id" )

def check_daemon_stops (context):
  """'daemon --stop' releases every view (each sidecar says closed),
  removes the socket and ends the daemon. A command then runs on its
  own connection: a save of a view the daemon held is an ordinary
  unheld save, which keeps its handles."""
  folder = context [ "daemon_folder" ]
  require_output (
    run_tool ( [ "daemon", "--stop", "--out-dir", folder ], port = None ),
    f"Stopped the daemon in {os . path . realpath (folder)}.\n" )
  require_equal ( "the daemon's exit code",
                  context [ "daemon" ] . wait (timeout = 30), 0 )
  require_equal ( "whether the socket remains",
                  os . path . lexists ( os . path . join (folder, "skg-for-ai.sock") ),
                  False )
  for held_folder in ( folder, context [ "other_folder" ], context [ "diff_folder" ] ):
    for name in os . listdir (held_folder):
      if name . endswith (".view.json"):
        require_equal ( name + "'s 'closed'",
                        read_sidecar ( held_folder,
                                       name [ : - len (".view.json") ] ) [ "closed" ],
                        True )
  require_equal ( "whether the port's socket remains",
                  os . path . lexists ( port_socket_path () ), False )
  require_log_line ( context, "stopped: released 5 views" )
  edit_compact ( folder, [ ( "* Second child, renamed on the second server ⟨0a ",
                             "* Second child, saved after the daemon stopped ⟨0a " ) ],
                 "links-here_child-b-id" )
  completed = run_tool ([ "save", os . path . join (folder, "links-here_child-b-id.view.json"),
                          "--yes" ])
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_in_file ( folder, "links-here_child-b-id",
                    "* Second child, saved after the daemon stopped ⟨0a " )
  require_equal ( "child-b's title", skg_title (context, "child-b-id"),
                  "Second child, saved after the daemon stopped" )

def check_daemon_replaces_a_stale_socket (context):
  """A daemon killed outright leaves its socket. A command then runs
  on its own connection, and a new daemon replaces the socket; a
  signal stops it as '--stop' does."""
  folder = context [ "daemon_folder" ]
  start_daemon (context) . kill ()
  context [ "daemon" ] . wait (timeout = 30)
  socket_path = os . path . join (folder, "skg-for-ai.sock")
  require_equal ( "whether the dead daemon's socket remains",
                  os . path . lexists (socket_path), True )
  require_tool_run ([ "view", "shared-id", "--out-dir", folder ])
  require_equal ( "the sidecar written beside a dead daemon's socket, 'closed'",
                  read_sidecar (folder, "view_shared-id") [ "closed" ], True )
  start_daemon (context)
  require_log_line ( context, "replaced the socket a dead daemon left" )
  require_routed_run ( context, [ "view", "shared-id" ] )
  require_equal ( "the sidecar written through the new daemon, 'closed'",
                  read_sidecar (folder, "view_shared-id") [ "closed" ], False )
  context [ "daemon" ] . terminate ()
  require_equal ( "the exit code after SIGTERM",
                  context [ "daemon" ] . wait (timeout = 30), 0 )
  require_equal ( "whether the socket remains after SIGTERM",
                  os . path . lexists (socket_path), False )
  require_log_line ( context, "stopped: released 1 view" )
