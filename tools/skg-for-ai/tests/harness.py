"""What every group of checks of 'test_skg_for_ai.py' shares: where
the tool, the server binary and the fixtures are, and the test's port;
'failures_on_a_scratch_server', which starts a server on a fresh data
root and runs a group's checks against it; 'run_tool' and the
'require_*' checks that raise 'CaseFailure'; reading what a command
wrote ('read_compact', 'edit_compact', 'skg_title', ...), the diff
view's files, and starting a daemon or a second server. It imports
no group of checks."""

import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time

# ============================================================
# Where the tool, the server and the fixtures are
# ============================================================

TOOL_FOLDER = os . path . dirname (
  os . path . dirname ( os . path . abspath (__file__) ))
REPO_ROOT = os . path . dirname ( os . path . dirname (TOOL_FOLDER) )

FIXTURE_FOLDER = os . path . join (TOOL_FOLDER, "test-fixture")
SHARED_HERALD_CASES = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-rendering-cases.txt" )
# The rule table and label table the clients' tests render the cases
# with, as the server serves them.
SHARED_HERALD_RULES = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-rules.sexp" )
SHARED_SKGREPO_LABELS = os . path . join (
  REPO_ROOT, "tests", "shared", "herald-skgrepo-labels.sexp" )
SERVER_BINARY = os . environ . get (
  "SKG_BINARY", os . path . join (REPO_ROOT, "target", "debug", "skg") )

def port_for_this_checkout ():
  """1793 in the main checkout; in a linked worktree, the even port
  'skg_port_for_this_checkout' ('bash/test-runner-lib.sh') derives
  from its path, so that suites run at once from separate worktrees
  do not take the same port, nor the port after it."""
  return subprocess . run (
    [ "bash", "-c",
      'source "$1" && skg_port_for_this_checkout 1793 "$2"', "bash",
      os . path . join (REPO_ROOT, "bash", "test-runner-lib.sh"),
      REPO_ROOT ],
    capture_output = True, text = True, check = True ) . stdout . strip ()

# SKG_FOR_AI_TEST_PORT overrides the port; 'bash/all-tests.sh' sets it
# the same way.
PORT = int ( os . environ [ "SKG_FOR_AI_TEST_PORT" ]
             if "SKG_FOR_AI_TEST_PORT" in os . environ
             else port_for_this_checkout () )
SERVER_READY_TIMEOUT_SECONDS = 60
HANDLE_PATTERN = re . compile (r'⟨([0-9a-z]+)[ ⟩]')
VIEWNODE_ID_PATTERN = re . compile (r'^\*+ \(skg \(vnid ([0-9a-z]+)\)')

# ============================================================
# Scratch servers
# ============================================================

def failures_on_a_scratch_server (check, owned_skgrepo_in_git = False,
                                  with_private_skgrepo = False,
                                  make_data_root = None):
  """CHECK (SCRATCH, DATA_ROOT)'s failures, run against a server on a
  fresh copy of the fixture in the scratch folder SCRATCH, or on the
  data root MAKE_DATA_ROOT (DATA_ROOT) writes. With
  WITH_PRIVATE_SKGREPO, the copy gains a second owned skgrepo
  ('add_private_skgrepo'). With OWNED_SKGREPO_IN_GIT, each owned
  skgrepo is first made a gitrepo whose HEAD commit holds its files,
  as the diff view needs."""
  if port_is_listening (PORT):
    return [ f"port {PORT} is taken; set SKG_FOR_AI_TEST_PORT" ]
  scratch = tempfile . mkdtemp ( prefix = "skg-for-ai-test-" )
  data_root = os . path . join (scratch, "data")
  ( make_data_root or ( lambda root : shutil . copytree (FIXTURE_FOLDER, root) )
  ) (data_root)
  set_port_in_config ( os . path . join (data_root, "skgconfig.toml"), PORT )
  if with_private_skgrepo:
    add_private_skgrepo (data_root)
  if owned_skgrepo_in_git:
    for owned in [ "main" ] + ( [ "private" ] if with_private_skgrepo else [] ):
      commit_fixture_in_new_gitrepo ( os . path . join (data_root, "me", owned) )
  server_log_path = os . path . join (scratch, "server.log")
  with open (server_log_path, "w") as server_log:
    server = subprocess . Popen (
      [ SERVER_BINARY, "skgconfig.toml" ], cwd = data_root,
      stdout = server_log, stderr = subprocess . STDOUT )
  try:
    if not wait_for_server_ready (server_log_path, server):
      return [ f"the scratch server did not become ready; see {server_log_path}" ]
    failures = check (scratch, data_root)
  finally:
    server . terminate ()
    server . wait (timeout = 30)
  if failures:
    print ( f"Scratch files kept in {scratch}" )
  else:
    shutil . rmtree (scratch)
  return failures

def commit_fixture_in_new_gitrepo (folder):
  """Make FOLDER a gitrepo, with a local author, whose one commit
  holds its files."""
  for git_arguments in [ [ "init", "--quiet" ],
                         [ "config", "user.name", "Fixture Author" ],
                         [ "config", "user.email", "fixture@example.com" ],
                         [ "add", "--all" ],
                         [ "commit", "--quiet", "--message", "The fixture" ] ]:
    git (folder, git_arguments)

def git (folder, git_arguments):
  """Git's standard output, run in FOLDER; raises when git fails."""
  return subprocess . run (
    [ "git", "-C", folder ] + git_arguments,
    capture_output = True, text = True, check = True ) . stdout

# The files a second owned skgrepo, 'private', adds to the fixture,
# and those it adds to 'main' beside it ('add_private_skgrepo').
PRIVATE_SKGREPO_FILES = {
  # A telescope whose title sits below its home, 'main': overPrivateText.
  "me/main/tele-id.skg"      : "pid: tele-id\ncontains:\n- shared-id\n",
  "me/private/tele-id.skg"   : "pid: tele-id\ntitle: A title kept private\n",
  # A private node, contained by a private section of a public one, and
  # naming a public node that a save deletes.
  "me/private/secret-id.skg" : ( "pid: secret-id\ntitle: A private note\n"
                                 "contains:\n- shared-id\n- doomed-id\n" ),
  "me/private/child-a-id.skg" : "pid: child-a-id\ncontains:\n- secret-id\n",
  "me/main/doomed-id.skg"    : "pid: doomed-id\ntitle: A node to delete\n",
  # A private branch and its leaf, which move to 'main' together.
  "me/private/branch-id.skg" : ( "pid: branch-id\ntitle: A private branch\n"
                                 "contains:\n- leaf-id\n" ),
  "me/private/leaf-id.skg"   : "pid: leaf-id\ntitle: Its private leaf\n",
  # A node naming an ID nobody wrote, by membership and by a link.
  "me/main/refs-id.skg"      : ( "pid: refs-id\ntitle: Names an absent node\n"
                                 "body: |-\n  See [[id:absent-id][the absent one]].\n"
                                 "contains:\n- absent-id\n" ) }

def add_private_skgrepo (data_root):
  """Give the fixture at DATA_ROOT a second owned skgrepo, 'private',
  more private than 'main', and the files PRIVATE_SKGREPO_FILES
  lists."""
  with open ( os . path . join (data_root, "skgconfig.toml"), "a",
              encoding = "utf-8" ) as config_file:
    config_file . write ( '\n[[repos]]\nname = "private"\npath = "me/private"\n' )
  for relative_path, text in PRIVATE_SKGREPO_FILES . items ():
    path = os . path . join (data_root, relative_path)
    os . makedirs ( os . path . dirname (path), exist_ok = True )
    with open (path, "w", encoding = "utf-8") as skg_file:
      skg_file . write (text)

def set_port_in_config (config_path, port):
  text = read_file (config_path)
  with open (config_path, "w", encoding = "utf-8") as config_file:
    config_file . write (
      re . sub ( r'(?m)^port = [0-9]+', f"port = {port}", text ))

def wait_for_server_ready (server_log_path, server):
  deadline = time . time () + SERVER_READY_TIMEOUT_SECONDS
  while time . time () < deadline:
    if server . poll () is not None:
      return False
    if "Server ready." in read_file (server_log_path):
      return True
    time . sleep (0.2)
  return False

def port_is_listening (port):
  with socket . socket () as probe:
    return probe . connect_ex (( "127.0.0.1", port )) == 0

# ============================================================
# Running the tool, and checking what it printed and wrote
# ============================================================

class CaseFailure (Exception):
  pass

ROOT_BASE_NAME = "view_root-id"
REPORT_HEAD = ( "Changes in view_root-id.compact.org, against "
                "view_root-id.full.org (view ID content-view:root-id):\n" )
NOT_SAVED = "Not saved. Run again with '--yes' to save.\n"
DANGLING_LINK_WARNING = (
  "Server warning: Dangling link: node root-id links to absent id "
  "missing-id (label \"a node nobody wrote\").\n" )
UUID_PATTERN = re . compile (
  r'[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' )
DIFF_VIEW_ID_PATTERN = re . compile (
  r'diff-view:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' )

def fresh_root_view (context, name):
  folder = os . path . join ( context [ "scratch" ], "write", name )
  require_tool_run ([ "view", "root-id", "--out-dir", folder, "--no-summary" ])
  return folder

def require_tool_run (tool_arguments, bypassing_daemons = False):
  completed = run_tool ( tool_arguments, bypassing_daemons = bypassing_daemons )
  if completed . returncode != 0:
    raise CaseFailure (
      f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" )
  return completed

def run_save (folder, *flags):
  return run_tool ( [ "save", folder ] + list (flags) )

def require_save_succeeds (folder):
  completed = run_save (folder, "--yes")
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure (
      f"saving {folder} printed {completed . stdout!r}, {completed . stderr!r}" )
  return completed

def edit_compact (folder, replacements, base_name = ROOT_BASE_NAME):
  """Replace, in the compact view BASE_NAME in FOLDER, each OLD of
  REPLACEMENTS by its NEW, requiring that OLD occur once."""
  path = os . path . join (folder, base_name + ".compact.org")
  text = read_file (path)
  for old, new in replacements:
    if text . count (old) != 1:
      raise CaseFailure (
        f"{old!r} occurs {text . count (old)} times in {path}:\n{text}" )
    text = text . replace (old, new)
  with open (path, "w", encoding = "utf-8") as compact_file:
    compact_file . write (text)

def read_compact (folder, base_name = ROOT_BASE_NAME):
  return read_file ( os . path . join (folder, base_name + ".compact.org") )

def require_in_compact (folder, text):
  if text not in read_compact (folder):
    raise CaseFailure (
      f"the re-rendered compact view lacks {text!r}:\n{read_compact (folder)}" )

def id_of_compact_headline (folder, headline_start, base_name = ROOT_BASE_NAME):
  """The ID in the metadata of the full-view headline that the handle
  of the compact headline starting HEADLINE_START names, in the view
  BASE_NAME."""
  for line in read_compact (folder, base_name) . split ("\n"):
    if line . startswith (headline_start + " ⟨"):
      handle = HANDLE_PATTERN . search (line) . group (1)
      for full_line in read_file (
          os . path . join (folder, base_name + ".full.org") ) . split ("\n"):
        viewnode_id = VIEWNODE_ID_PATTERN . match (full_line)
        if viewnode_id and viewnode_id . group (1) == handle:
          match = UUID_PATTERN . search (full_line)
          if match is None:
            raise CaseFailure (
              f"the full view's headline ⟨{handle}⟩ has no UUID: {full_line!r}" )
          return match . group (0)
      raise CaseFailure ( f"the full view has no headline ⟨{handle}⟩" )
  raise CaseFailure (
    f"no headline {headline_start!r} in:\n{read_compact (folder, base_name)}" )

def compact_line_starting (folder, start, base_name = ROOT_BASE_NAME):
  """The one line of the compact file BASE_NAME (by default the root
  view's) that starts with START."""
  compact = read_file ( os . path . join (folder, base_name + ".compact.org") )
  lines = [ line for line in compact . split ("\n") if line . startswith (start) ]
  if len (lines) != 1:
    raise CaseFailure ( f"{len (lines)} lines start {start!r}:\n{compact}" )
  return lines [0]

def hit_titles (folder, base_name):
  """The titles of the top-level headlines of a search buffer's compact
  view: its hits."""
  return [ line [ 2 : line . index (" ⟨") ]
           for line in read_file ( os . path . join (
             folder, base_name + ".compact.org" )) . split ("\n")
           if line . startswith ("* ") ]

def require_output (completed, expected_stdout, expected_returncode = 0,
                    ignoring_view_ids = False):
  """IGNORING_VIEW_IDS: each diff view's view ID, a fresh UUID, is
  read as 'diff-view:ID'."""
  stdout = ( DIFF_VIEW_ID_PATTERN . sub ( "diff-view:ID", completed . stdout )
             if ignoring_view_ids else completed . stdout )
  if ( stdout != expected_stdout
       or completed . returncode != expected_returncode ):
    raise CaseFailure (
      f"exit {completed . returncode}, printed:\n{completed . stdout}"
      f"stderr: {completed . stderr}\nexpected exit {expected_returncode}, "
      f"printed:\n{expected_stdout}" )

def require_failure (completed, stdout_lines, stderr_text):
  """COMPLETED exited 1, printing every one of STDOUT_LINES (in order,
  among others) and STDERR_TEXT on standard error."""
  lines = completed . stdout . split ("\n")
  if ( completed . returncode != 1 or stderr_text not in completed . stderr
       or [ line for line in lines if line in stdout_lines ] != stdout_lines ):
    raise CaseFailure (
      f"exit {completed . returncode}, printed:\n{completed . stdout}stderr: "
      f"{completed . stderr}\nexpected exit 1, the lines {stdout_lines!r} and "
      f"on stderr {stderr_text!r}" )

def require_equal (what, actual, expected):
  if actual != expected:
    raise CaseFailure ( f"{what} is {actual!r}, not {expected!r}" )

def saved_line (warning_count):
  return ( f"Server: saved, 0 errors, {warning_count} warning"
           f"{'' if warning_count == 1 else 's'}.\n" )

def written_paths (folder):
  return "" . join ( os . path . join (folder, ROOT_BASE_NAME + suffix) + "\n"
                     for suffix in ( ".full.org", ".compact.org", ".view.json" ))

def skg_path (context, node_id):
  return os . path . join ( context [ "main_repo" ], node_id + ".skg" )

def skg_lines (context, node_id):
  return read_file ( skg_path (context, node_id) ) . split ("\n")

def skg_title (context, node_id):
  for line in skg_lines (context, node_id):
    if line . startswith ("title: "):
      return line [ len ("title: ") : ]
  return None

def skg_body (context, node_id):
  """The lines of a block-scalar body."""
  lines = skg_lines (context, node_id)
  for index, line in enumerate (lines):
    if line . startswith ("body: |"):
      body = []
      for body_line in lines [ index + 1 : ]:
        if not body_line . startswith ("  "):
          break
        body . append ( body_line [ 2 : ] )
      return body
  return None

def skg_list (context, node_id, key):
  lines = skg_lines (context, node_id)
  if key + ":" not in lines:
    return []
  members = []
  for line in lines [ lines . index (key + ":") + 1 : ]:
    if not line . startswith ("- "):
      break
    members . append ( line [ 2 : ] )
  return members

def skg_files (context):
  """{FILE NAME: TEXT} of the owned skgrepo."""
  return { name : read_file ( os . path . join ( context [ "main_repo" ], name ))
           for name in sorted ( os . listdir ( context [ "main_repo" ] ))
           if name . endswith (".skg") }

def run_tool (tool_arguments, standard_input = "", port = PORT,
              bypassing_daemons = False):
  """Run the tool; with PORT None, without '--port'. BYPASSING_DAEMONS:
  where it finds no daemon for its port."""
  environment = dict (os . environ)
  if bypassing_daemons:
    environment [ "XDG_RUNTIME_DIR" ] = tempfile . mkdtemp (
      prefix = "skg-for-ai-test-no-daemon-" )
  try:
    return subprocess . run (
      [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py") ]
      + tool_arguments + ( [ "--port", str (port) ] if port is not None else [] ),
      input = standard_input, capture_output = True, text = True,
      env = environment )
  finally:
    if bypassing_daemons:
      os . rmdir ( environment [ "XDG_RUNTIME_DIR" ] )

def read_file (path):
  with open (path, encoding = "utf-8") as input_file:
    return input_file . read ()

# ============================================================
# The diff view's files
# ============================================================

DIFF_BASE_NAME = "diff_all-changes"

def require_in_diff_compact (folder, text):
  compact = read_diff_compact (folder)
  if text not in compact:
    raise CaseFailure ( f"the diff view's compact buffer lacks {text!r}:\n{compact}" )

def require_not_in_diff_compact (folder, text):
  compact = read_diff_compact (folder)
  if text in compact:
    raise CaseFailure ( f"the diff view's compact buffer still has {text!r}:\n{compact}" )

def sidecar_of_diff (folder):
  return os . path . join (folder, DIFF_BASE_NAME + ".view.json")

def read_diff_compact (folder):
  return read_file ( os . path . join (folder, DIFF_BASE_NAME + ".compact.org") )

def diff_report_head ():
  return ( "Changes in " + DIFF_BASE_NAME + ".compact.org, against " +
           DIFF_BASE_NAME + ".full.org (view ID diff-view:ID):\n" )

def diff_written_paths (folder):
  return "" . join ( os . path . join (folder, DIFF_BASE_NAME + suffix) + "\n"
                     for suffix in ( ".full.org", ".compact.org", ".view.json" ))

# ============================================================
# Daemons, and a second server over the same data root
# ============================================================

DAEMON_READY_TIMEOUT_SECONDS = 30
# How long a case waits for the watcher to reload a change on disk
# and the daemon to log it.
DAEMON_LOG_TIMEOUT_SECONDS = 20
SECOND_SERVER_PORT = PORT + 1
# What a daemon prints once its socket answers.
DAEMON_READY_LINE = "The daemon listens on"

def start_daemon (context):
  """Start a daemon for the daemon folder, on the scratch server, and
  wait until its socket answers; return its process."""
  folder = context [ "daemon_folder" ]
  output_path = os . path . join ( context [ "scratch" ], "daemon-output.txt" )
  ready_lines_before = ( read_file (output_path) . count (DAEMON_READY_LINE)
                         if os . path . exists (output_path) else 0 )
  with open (output_path, "a") as output:
    process = subprocess . Popen (
      [ sys . executable, os . path . join (TOOL_FOLDER, "skg_for_ai.py"),
        "daemon", "--out-dir", folder, "--port", str (PORT) ],
      stdout = output, stderr = subprocess . STDOUT )
  context [ "processes" ] . append (process)
  context [ "daemon" ] = process
  deadline = time . time () + DAEMON_READY_TIMEOUT_SECONDS
  while time . time () < deadline:
    if process . poll () is not None:
      raise CaseFailure ( f"the daemon exited: {read_file (output_path)}" )
    if read_file (output_path) . count (DAEMON_READY_LINE) > ready_lines_before:
      return process
    time . sleep (0.1)
  raise CaseFailure ( f"the daemon did not start: {read_file (output_path)}" )

def start_second_server (context):
  """A second server over the scratch data root, with its own port and
  search index, as a teammate's server reads the same skgrepos."""
  if port_is_listening (SECOND_SERVER_PORT):
    raise CaseFailure ( f"port {SECOND_SERVER_PORT} is taken" )
  config_name = "skgconfig-second.toml"
  config_text = read_file ( os . path . join ( context [ "data_root" ], "skgconfig.toml" ))
  with open ( os . path . join ( context [ "data_root" ], config_name ), "w",
              encoding = "utf-8" ) as config_file:
    config_file . write (
      re . sub ( r'(?m)^tantivy_folder = .*$',
                 'tantivy_folder = ".index-second.tantivy"',
                 re . sub ( r'(?m)^port = [0-9]+', f"port = {SECOND_SERVER_PORT}",
                            config_text )))
  log_path = os . path . join ( context [ "scratch" ], "second-server.log" )
  with open (log_path, "w") as server_log:
    server = subprocess . Popen (
      [ SERVER_BINARY, config_name ], cwd = context [ "data_root" ],
      stdout = server_log, stderr = subprocess . STDOUT )
  context [ "processes" ] . append (server)
  if not wait_for_server_ready (log_path, server):
    raise CaseFailure ( f"the second server did not become ready; see {log_path}" )

def save_through_second_server (context, base_name, replacement):
  """View the node BASE_NAME names afresh on the second server, make
  the one REPLACEMENT in its compact view, and save it there; return
  the folder."""
  folder = os . path . join ( context [ "scratch" ], "second-server", base_name )
  node_id = base_name . split ("_", 1) [1]
  require_tool_run_on ( [ "view", node_id, "--out-dir", folder ], SECOND_SERVER_PORT )
  edit_compact ( folder, [ replacement ], "view_" + node_id )
  completed = run_tool ( [ "save", os . path . join ( folder, "view_" + node_id + ".view.json" ),
                           "--yes" ], port = SECOND_SERVER_PORT )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the second server's save printed {completed . stdout!r}, "
                        f"{completed . stderr!r}" )
  return folder

def port_socket_path ():
  return os . path . join ( os . environ [ "XDG_RUNTIME_DIR" ],
                            f"skg-for-ai-{PORT}.sock" )

def require_routed_run (context, tool_arguments):
  """Run the command with the daemon folder as its '--out-dir' and no
  '--port', so that it runs through the daemon."""
  completed = run_tool (
    tool_arguments + [ "--out-dir", context [ "daemon_folder" ] ], port = None )
  if completed . returncode != 0:
    raise CaseFailure (
      f"'{' ' . join (tool_arguments)}' through the daemon failed: "
      f"{completed . stdout}{completed . stderr}" )
  return completed

def require_tool_run_on (tool_arguments, port):
  completed = run_tool (tool_arguments, port = port)
  if completed . returncode != 0:
    raise CaseFailure (
      f"'{' ' . join (tool_arguments)}' failed: {completed . stderr}" )
  return completed

def require_log_line (context, text):
  """Wait until a line of the daemon's log contains TEXT."""
  deadline = time . time () + DAEMON_LOG_TIMEOUT_SECONDS
  while time . time () < deadline:
    if text in daemon_log (context):
      return
    time . sleep (0.1)
  raise CaseFailure ( f"the daemon's log has no line with {text!r}" )

def daemon_log (context):
  path = os . path . join ( context [ "daemon_folder" ], "skg-for-ai.log" )
  return read_file (path) if os . path . exists (path) else ""

def require_in_file (folder, base_name, text):
  compact = read_file ( os . path . join (folder, base_name + ".compact.org") )
  if text not in compact:
    raise CaseFailure ( f"{base_name}.compact.org lacks {text!r}:\n{compact}" )

def read_sidecar (folder, base_name):
  return json . loads ( read_file ( os . path . join (folder, base_name + ".view.json") ))
