"""The checks of 'test_skg_for_ai.py' on the eighth scratch server,
over the perspective integration test's data: 'perspective', on a
command's own connection and through a daemon."""

import os
import re
import shutil

from harness import (
  REPO_ROOT, CaseFailure, daemon_log, edit_compact, read_compact,
  read_sidecar, require_equal, require_failure, require_in_file,
  require_log_line, require_output, require_routed_run,
  require_save_succeeds, require_tool_run, run_tool, skg_body,
  start_daemon )

# ============================================================
# A view's perspective: the data of the perspective integration
# test, where R, in foreign skgrepo "foreign", contains M, in foreign
# skgrepo "origin", and K; A, in "foreign", U, in owned "main", and B,
# in foreign "third", each fork M; and P, in "main", contains M. A
# view of R is drawn from its root's home, "foreign", A in M's place;
# from main's, U; from third's, B; from origin's, M itself. The cases
# run in order.
# ============================================================

PERSPECTIVE_DATA = os . path . join (
  REPO_ROOT, "tests", "integration", "perspective", "data" )
FOLLOWS_MAINS_LINE = "Showing the perspective of main.\n"
FOLLOWS_THIRDS_LINE = "Showing the perspective of third.\n"
FOLLOWS_ORIGINS_LINE = "Showing the perspective of origin.\n"
FOLLOWS_ROOT_HOMES_LINE = (
  "Showing the perspective of each root's home skgrepo (foreign).\n" )
FOLLOWS_MAINS_SUMMARY = (
  "# Showing the perspective of main (each root's home skgrepo is the default).\n" )
FOLLOWS_THIRDS_SUMMARY = (
  "# Showing the perspective of third (each root's home skgrepo is the default).\n" )
R_SHOWING_A = "* R ⟨0a C2 ⌂:foreign⟩\n** A ⟨0b Kĥ1⟩\n*** ⟨Its mirrors are these.⟩\n**** M ⟨2•C 3bK ⌂:origin⟩\n***** ⟨☮ These also contain it.⟩\n****** P ⟨☮ Cb ⌂:main folded⟩\n**** U ⟨K1 ⌂:main⟩\n**** B ⟨K1 ⌂:third⟩\n** K ⟨0i⟩\n"
R_SHOWING_U = "* R ⟨0a C2 ⌂:foreign⟩\n** U ⟨0b Kĥ1 ⌂:main⟩\n*** ⟨Its mirrors are these.⟩\n**** M ⟨2•C 3bK ⌂:origin⟩\n***** ⟨☮ These also contain it.⟩\n****** P ⟨☮ Cb ⌂:main folded⟩\n**** A ⟨K1 ⌂:foreign⟩\n**** B ⟨K1 ⌂:third⟩\n** K ⟨0i⟩\n"
R_SHOWING_B = "* R ⟨0a C2 ⌂:foreign⟩\n** B ⟨0b Kĥ1 ⌂:third⟩\n*** ⟨Its mirrors are these.⟩\n**** M ⟨2•C 3bK ⌂:origin⟩\n***** ⟨☮ These also contain it.⟩\n****** P ⟨☮ Cb ⌂:main folded⟩\n**** A ⟨K1 ⌂:foreign⟩\n**** U ⟨K1 ⌂:main⟩\n** K ⟨0i⟩\n"

def copy_perspective_fixture (data_root):
  """The perspective data, with a config naming its three foreign
  skgrepos and its owned one."""
  for folder in ( "foreign", "owned", "third", "origin" ):
    shutil . copytree ( os . path . join (PERSPECTIVE_DATA, folder),
                        os . path . join (data_root, folder) )
  with open ( os . path . join (data_root, "skgconfig.toml"), "w",
              encoding = "utf-8" ) as config_file:
    config_file . write (
      'tantivy_folder = ".index.tantivy"\nport = 0\ntiming_log = false\n'
      'beep_when_server_becomes_available = false\n\n'
      '[[repos]]\nname = "foreign"\npath = "foreign"\nforeign = true\n\n'
      '[[repos]]\nname = "main"\npath = "owned/main"\n\n'
      '[[repos]]\nname = "third"\npath = "third"\nforeign = true\n\n'
      '[[repos]]\nname = "origin"\npath = "origin"\nforeign = true\n' )

def failures_of_perspective (scratch, data_root):
  context = { "scratch"       : scratch,
              "data_root"     : data_root,
              "main_repo"     : os . path . join (data_root, "owned", "main"),
              "daemon_folder" : os . path . join (scratch, "daemon"),
              "processes"     : [] }
  try:
    for case in [ check_switching_perspective_on_a_command_s_own_connection,
                  check_saving_a_view_that_follows_mains,
                  check_following_the_root_homes_again,
                  check_the_root_too,
                  check_switching_perspective_through_a_daemon,
                  check_viewing_a_root_a_perspective_put_in_place ]:
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

def r_tree (folder):
  """The headlines of the view of R in FOLDER, and the summary's line
  naming its perspective, if it has one. A headline three or more
  levels deep loses its viewnode ID: those under R's child stand in
  the forks folder of the mirror shown there, drawn afresh, with new
  viewnode IDs, whenever a switch re-points that child."""
  return "" . join ( without_deep_vnid (line) + "\n"
                     for line in read_compact (folder, "view_R") . split ("\n")
                     if line . startswith ("*") or line . startswith ("# Showing") )

def without_deep_vnid (line):
  """LINE without its handle's viewnode ID, if it is a headline three
  or more levels deep."""
  if not line . startswith ("***"):
    return line
  return re . sub ( r"⟨[0-9a-z]+( |⟩)", lambda m: "⟨" + ( "" if m . group (1) == " " else "⟩" ),
                    line, count = 1 )

def check_switching_perspective_on_a_command_s_own_connection (context):
  """With no daemon, 'perspective' registers the view on its
  own connection (by saving it back unchanged) and switches it to the
  next perspective, main's: U stands in M's place, the summary says
  so, and the sidecar records it. A view of P, in "main", is drawn
  from main's already, and the server refuses a switch that names
  it."""
  folder = os . path . join ( context [ "scratch" ], "own" )
  context [ "own_folder" ] = folder
  require_tool_run ([ "view", "R", "--out-dir", folder ])
  require_equal ( "R's view", r_tree (folder), R_SHOWING_A )
  require_equal ( "a fresh sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ],
                  "root homes" )
  sidecar_path = os . path . join (folder, "view_R.view.json")
  require_output (
    run_tool ([ "perspective", sidecar_path ]),
    FOLLOWS_MAINS_LINE + "" . join (
      os . path . join (folder, "view_R" + suffix) + "\n"
      for suffix in ( ".full.org", ".compact.org", ".view.json" )))
  require_equal ( "R's view, following main's", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U )
  require_equal ( "the sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ], "skgrepo main" )
  require_tool_run ([ "view", "P", "--out-dir", folder ])
  require_failure (
    run_tool ([ "perspective", os . path . join (folder, "view_P.view.json"),
                "skgrepo", "main" ]),
    [], "The server refused: The perspective of main would show this buffer "
        "as the perspective of each root's home skgrepo does, so it is not "
        "one of its perspectives." )

def check_saving_a_view_that_follows_mains (context):
  """An edited view is not switched. Its save names the choice: the
  server checks U's '(inPlaceOf M)' and rerenders the view from
  main's perspective, and the rerendered view still shows U, its
  sidecar still recording the choice."""
  folder = context [ "own_folder" ]
  edit_compact ( folder, [ ( "** U ⟨0b Kĥ1 ⌂:main⟩\n",
                             "** U ⟨0b Kĥ1 ⌂:main⟩\nU's new body.\n" ) ], "view_R" )
  require_failure (
    run_tool ([ "perspective", os . path . join (folder, "view_R.view.json"),
                "root homes" ]),
    [], "Nothing was switched: the compact view has edits" )
  require_save_succeeds ( os . path . join (folder, "view_R.view.json") )
  require_equal ( "R's view, saved", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U )
  require_in_file ( folder, "view_R", "** U ⟨0b Kĥ1 ⌂:main⟩\nU's new body.\n" )
  require_equal ( "U's body", skg_body (context, "U"), [ "U's new body." ] )
  require_equal ( "the saved sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ], "skgrepo main" )

def check_following_the_root_homes_again (context):
  """Named, third's perspective shows B in M's place; the next after
  it is origin's, M's own home, which shows M itself; the next after
  that, the last, is the root homes again."""
  folder = context [ "own_folder" ]
  completed = run_tool ([ "perspective", os . path . join (folder, "view_R.view.json"),
                          "skgrepo third" ])
  if completed . returncode != 0 or not completed . stdout . startswith (
      FOLLOWS_THIRDS_LINE ):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's view, following third's", r_tree (folder),
                  FOLLOWS_THIRDS_SUMMARY + R_SHOWING_B )
  require_equal ( "the sidecar's perspective",
                  { key : read_sidecar (folder, "view_R") [key]
                    for key in ( "perspective", "perspective_name" ) },
                  { "perspective" : "skgrepo third",
                    "perspective_name" : "the perspective of third" } )
  completed = run_tool ([ "perspective", os . path . join (folder, "view_R.view.json") ])
  if completed . returncode != 0 or not completed . stdout . startswith (
      FOLLOWS_ORIGINS_LINE ):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's child, from origin's perspective",
                  r_tree (folder) . split ("\n") [0 : 2],
                  [ "# Showing the perspective of origin (each root's home skgrepo is the default).",
                    "* R ⟨0a C2 ⌂:foreign⟩" ] )
  child = r_tree (folder) . split ("\n") [2]
  if not ( child . startswith ("** M ⟨") and "ĥ" not in child ):
    raise CaseFailure ( f"from origin's, M stands as itself:\n{r_tree (folder)}" )
  completed = run_tool ([ "perspective", os . path . join (folder, "view_R.view.json") ])
  if completed . returncode != 0 or not completed . stdout . startswith (
      FOLLOWS_ROOT_HOMES_LINE ):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's view, following its root's home", r_tree (folder),
                  # A is re-pointed where M was reinstated, so keeps
                  # the viewnode ID that occurrence was given.
                  R_SHOWING_A . replace ("** A ⟨0b ", "** A ⟨0v ") )
  require_equal ( "the sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ],
                  "root homes" )

def check_the_root_too (context):
  """A view of M, which U, in "main", forks, shows U as its root from
  main's perspective, standing for M ('ĥ' after its 'K'), and M again
  from the root homes."""
  folder = context [ "own_folder" ]
  require_tool_run ([ "view", "M", "--out-dir", folder ])
  root = lambda: read_compact (folder, "view_M") . split ("\n* ") [1] . split ("\n") [0]
  fresh = root ()
  require_equal ( "M's fresh root", ( fresh . startswith ("M ⟨0a "), "ĥ" in fresh ), ( True, False ) )
  require_tool_run ([ "perspective", os . path . join (folder, "view_M.view.json"),
                      "skgrepo main" ])
  switched = root ()
  require_equal ( "M's root from main's perspective",
                  ( switched . startswith ("U ⟨0a K"), "ĥ" in switched ), ( True, True ) )
  require_tool_run ([ "perspective", os . path . join (folder, "view_M.view.json"),
                      "root homes" ])
  require_equal ( "M's root from the root homes again", root (), fresh )

def check_switching_perspective_through_a_daemon (context):
  """Through a daemon, which holds the view, the switch needs no
  registering save, and is logged. The daemon's rewrite of the view,
  when another view's save retitles U, keeps the choice; so does
  stopping the daemon, after which a save on the command's own
  connection names it, and the switch back registers it again."""
  folder = context [ "daemon_folder" ]
  start_daemon (context)
  require_routed_run ( context, [ "view", "R" ] )
  sidecar_path = os . path . join (folder, "view_R.view.json")
  completed = run_tool ( [ "perspective", sidecar_path ], port = None )
  if completed . returncode != 0 or not completed . stdout . startswith (FOLLOWS_MAINS_LINE):
    raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
  require_equal ( "R's view, following main's", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U )
  require_equal ( "the held sidecar", { key : read_sidecar (folder, "view_R") [key]
                                        for key in ( "perspective", "closed" ) },
                  { "perspective" : "skgrepo main", "closed" : False } )
  require_log_line ( context, "perspective of view_R: the perspective of main" )
  require_routed_run ( context, [ "view", "U" ] )
  edit_compact ( folder, [ ( "* U ⟨0a ", "* U, retitled ⟨0a " ) ], "view_U" )
  completed = run_tool ( [ "save", os . path . join (folder, "view_U.view.json"), "--yes" ],
                         port = None )
  if completed . returncode != 0 or "Server: saved," not in completed . stdout:
    raise CaseFailure ( f"the save printed {completed . stdout!r}, {completed . stderr!r}" )
  require_log_line ( context, "re-rendered view_R because of 'save view_U.view.json --yes'" )
  require_equal ( "R's view, rewritten by the daemon", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U . replace ("** U ", "** U, retitled ") )
  require_equal ( "the rewritten sidecar's choice",
                  read_sidecar (folder, "view_R") [ "perspective" ], "skgrepo main" )
  require_tool_run ([ "daemon", "--stop", "--out-dir", folder ])
  context [ "daemon" ] . wait (timeout = 30)
  require_equal ( "the released sidecar", { key : read_sidecar (folder, "view_R") [key]
                                            for key in ( "perspective", "closed" ) },
                  { "perspective" : "skgrepo main", "closed" : True } )
  edit_compact ( folder, [ ( "\n** U, retitled ⟨", "\n** U, retitled again ⟨" ) ], "view_R" )
  require_save_succeeds (sidecar_path)
  require_equal ( "R's view, saved after the daemon stopped", r_tree (folder),
                  FOLLOWS_MAINS_SUMMARY + R_SHOWING_U . replace ("** U ", "** U, retitled again ") )
  require_tool_run ([ "perspective", sidecar_path, "root homes" ])
  require_equal ( "R's view, following its root's home", r_tree (folder),
                  R_SHOWING_A . replace (
                    "U ⟨K1", "U, retitled again ⟨K1" ) )

def check_viewing_a_root_a_perspective_put_in_place (context):
  """Through a daemon, a view of M switched to main's perspective
  has U as its root, so the server answers a view of U on that
  connection by naming M's view, as it lands a client there: 'view U'
  prints the files of M's view and says why. Switched back, 'view U'
  draws U by itself."""
  folder = context [ "daemon_folder" ]
  start_daemon (context)
  require_routed_run ( context, [ "view", "M" ] )
  sidecar_path = os . path . join (folder, "view_M.view.json")
  for perspective in ( "skgrepo main", "root homes" ):
    completed = run_tool ( [ "perspective", sidecar_path, perspective ],
                           port = None )
    if completed . returncode != 0:
      raise CaseFailure ( f"the switch printed {completed . stdout!r}, {completed . stderr!r}" )
    viewed = require_routed_run ( context, [ "view", "U" ] )
    if perspective == "skgrepo main":
      require_equal ( "what 'view U' printed, U being the root of M's view",
                      viewed . stdout . split (),
                      [ os . path . join ( os . path . realpath (folder), "view_M" + suffix )
                        for suffix in ( ".full.org", ".compact.org", ".view.json" ) ] )
      if "U is already a root of the view content-view:M" not in viewed . stderr:
        raise CaseFailure ( f"'view U' said {viewed . stderr!r}" )
    else:
      require_equal ( "what 'view U' wrote, M's view showing M again",
                      [ os . path . basename (path) for path in viewed . stdout . split () ],
                      [ "view_U.full.org", "view_U.compact.org", "view_U.view.json" ] )
  require_tool_run ([ "daemon", "--stop", "--out-dir", folder ])
  context [ "daemon" ] . wait (timeout = 30)
