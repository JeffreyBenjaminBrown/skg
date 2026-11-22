"""The commands that write a view: 'view', 'search', 'forest' and its
aliases 'contains' and 'links-here' ('run_view_command'); 'new';
'perspective'; and 'close', which closes one."""

import contextlib
import json
import os
import re
import sys
import uuid

from sexp import sexp_to_text
from server_connection import (
  SkgToolError, SkgViewAlreadyOpen, approved_overPrivateText_field,
  connect_to_server, overPrivateText_answer, request_line,
  require_response_type, response_field, response_text_field,
  response_type )
from full_view import headline_gnid, parse_full_view
from compact_view import fetch_repo_ownership
from views import (
  RenderedView, SEARCH_OPTIONS, close_view_on_server, fetch_forest_view,
  fetch_search_view, fetch_single_root_view, load_saved_view,
  mark_sidecar_closed, release_view, require_port_of_sidecar,
  require_view_has_headlines, write_view_files )
from save import (
  compact_view_has_edits, connection_for_saved_view,
  register_view_by_saving_it_unchanged )
from save_dialogue import rerendered_view_of_save

# Commands that name one forest kind.
FOREST_COMMAND_ALIASES = { "contains" : "containers",
                           "links-here" : "mentioners" }

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
