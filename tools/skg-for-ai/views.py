"""A view as files: fetching one from the server ('fetch_*'),
writing its three files ('RenderedView', 'write_view_files'), and
reading them back ('SavedView', 'load_saved_view' and the sidecar's
fields)."""

import json
import os
import sys
import uuid

from sexp import (
  Symbol, quote_string, sexp_to_text, top_level_elements_with_offsets )
from server_connection import (
  DEFAULT_PERSPECTIVE, SkgToolError, SkgViewAlreadyOpen,
  overPrivateText_answer, request_line,
  request_releasing_overPrivateText, require_response_type,
  response_field, response_text_field, response_type )
from full_view import (
  HEADLINE_PATTERN, headline_viewnode_id, metadata_span_of_headline,
  parse_full_view )
from compact_view import (
  compact_tree_lines, fetch_herald_tables, fetch_link_statuses,
  fetch_repo_ownership, summary_lines )

# The folders a client can request from a node ('C-c l' in Emacs; the
# 'skg-show-folderOf-*' commands in 'elisp/skg-request-views.el'), each
# by its '(viewRequests ...)' request: 'forest KIND ID'. None for the
# containers folder, which a fresh content view already draws under
# its root.
FOREST_VIEW_REQUESTS = {
  "containers"    : None,
  "mentioners"    : "(folder mentioners)",
  "forks"         : "(folder forks)",
  "hides"         : "(folder hides)",
  "aliases"       : "(folder aliases)",
  "flags"         : "flags" }

# ============================================================
# Fetching views
# ============================================================

def require_view_has_headlines (view):
  """A search with no match answers with a message, and a view of an
  ID the graph lacks with an empty buffer: neither is a view."""
  content = view . content or ""
  if not any ( HEADLINE_PATTERN . match (line)
               for line in content . split ("\n") ):
    raise SkgToolError (
      ( "The server returned no view: " + content . strip ()
        if content . strip ()
        else "The server returned an empty view; the graph may lack that ID." )
      + ( " Errors: " + json . dumps ( view . errors, ensure_ascii = False )
          if view . errors else "" ))

def fetch_single_root_view (connection, node_id, show_overPrivateText = False):
  view_id = "content-view:" + node_id
  if connection . holds_view (view_id):
    # A connection asking for a view ID it holds is answered by name,
    # '(switch-to-view ID)', not drawn afresh.
    connection . release_view_to_reopen (view_id)
  response = request_releasing_overPrivateText (
    connection,
    request_line ( "single root content view",
                   ( "id", node_id ), ( "view-id", view_id ) ),
    show_overPrivateText, "viewed" )
  require_response_type (response, "content-view")
  open_view_id = response_field (response, "switch-to-view")
  if open_view_id is not None:
    raise SkgViewAlreadyOpen ( node_id, sexp_to_text (open_view_id) )
  return RenderedView (
    view_id  = view_id,
    request  = { "id" : node_id },
    content  = response_field (response, "content"),
    errors   = response_field (response, "errors") or [],
    warnings = response_field (response, "warnings") or [] )

# The search options of 'C-c f i', each a flag of 'search'.
SEARCH_OPTIONS = ( "regex", "body", "operators" )

def fetch_search_view (connection, terms, options = (),
                       show_overPrivateText = False):
  """The search buffer for TERMS, searched with the OPTIONS named
  (among SEARCH_OPTIONS), as 'skg-search-interactive' asks. Under a
  skgrepo restriction the server may first ask whether to include
  overPrivateText telescopes: SHOW_OVERPRIVATETEXT includes them, and
  otherwise they are excluded, as a client's "no" does; a line on
  standard error says which."""
  line = request_line (
    "text search", ( "terms", terms ),
    * [ ( option, "true" if option in options else "false" )
        for option in SEARCH_OPTIONS ] )
  response = connection . request (line)
  if response_type (response) == "overPrivateText-telescope-confirmation":
    choice = "include" if show_overPrivateText else "exclude"
    print ( "skg server: " + response_text_field (response, "prompt") + " The "
            "search " + choice + "s them" + (
              "." if show_overPrivateText else
              "; '--show-overPrivateText' includes them." ),
            file = sys . stderr )
    response = connection . request (
      line [ : -1 ] + " (overPrivateText-telescopes . " + quote_string (choice)
      + "))" )
  require_response_type (response, "search-results")
  if response_field (response, "warnings") is None:
    # no match, or an error: a message, not a view, which has no warnings
    raise SkgToolError ( "The server returned no view: " +
                         response_text_field (response, "content") )
  return RenderedView (
    view_id  = "search:" + terms,
    request  = dict ( { "terms" : terms },
                      ** { option : True for option in options } ),
    content  = response_field (response, "content") or "",
    errors   = [],
    warnings = response_field (response, "warnings") or [] )

def fetch_diff_view_buffer (connection, buffer_name):
  """The diff view's buffer BUFFER_NAME, registered on CONNECTION under
  a fresh view ID, or None when the server answers that it would be
  empty. A refusal raises: it applies to every buffer."""
  view_id = "diff-view:" + str ( uuid . uuid4 () )
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
  return RenderedView (
    view_id  = view_id,
    command  = "diff",
    request  = { "buffer" : buffer_name },
    content  = content,
    errors   = [],
    warnings = response_field (response, "warnings") or [],
    processed_summary = response_field (response, "processed-summary") )

def fetch_forest_view (connection, kind, node_id, show_overPrivateText = False):
  """The node's view with the folder KIND under its root, as the
  clients' 'C-c l' command for KIND draws it. A fresh content view
  already holds the root's containers folder, so for 'containers' it
  is that view, and no save is sent."""
  view_request = FOREST_VIEW_REQUESTS [kind]
  if view_request is None:
    return fetch_single_root_view (connection, node_id, show_overPrivateText)
  return fetch_view_with_root_folder_request (
    connection, node_id, view_request, show_overPrivateText )

def fetch_view_with_root_folder_request (connection, node_id, view_request,
                                         show_overPrivateText = False):
  """Open the node's view, then save it unchanged but for a
  '(viewRequests VIEW_REQUEST)' on its root, as the clients' 'C-c l'
  commands do. The save's re-rendered buffer is the view. Nothing in
  the buffer differs from the graph, so the save writes no file."""
  fresh_view = fetch_single_root_view (connection, node_id, show_overPrivateText)
  # A view of an ID no node has is empty, with no headline to ask on:
  # refused as 'view' refuses it.
  require_view_has_headlines (fresh_view)
  requesting_buffer = buffer_with_view_request_on_first_headline (
    fresh_view . content, view_request )
  def send_save ():
    return connection . save_buffer (
      fresh_view . view_id, requesting_buffer, fresh_view . content )
  terminal = send_save () [-1]
  if response_type (terminal) == "overPrivateText-telescope-confirmation":
    overPrivateText_answer (
      terminal, show_overPrivateText, "",
      "Nothing was viewed: the server withholds the folder's text from "
      "overPrivateText telescopes under the skgrepo restriction; run again "
      "with '--show-overPrivateText' to release it.", to_stderr = True,
      approval = "the withheld rerender is asked for" )
    terminal = release_withheld_save_rerender (
      connection, terminal, fresh_view . view_id )
  require_response_type (terminal, "save-result")
  content = response_field (terminal, "content")
  errors = response_field (terminal, "errors") or []
  if not content:
    raise SkgToolError ( "The save that requests the folder failed:\n" +
                         "\n" . join (errors) )
  return RenderedView (
    view_id  = fresh_view . view_id,
    request  = { "id" : node_id, "view_request" : view_request },
    content  = content,
    errors   = errors,
    warnings = response_field (terminal, "warnings") or [] )

def release_withheld_save_rerender (connection, confirmation, view_id):
  """Release the rerender a save withheld: the server's CONFIRMATION
  names the views withheld, and lists the PIDs whose text they would
  show. Send "rerender views" for them, approving those PIDs, as the
  clients' approval does ('skg--request-rerender-views' in
  'elisp/skg-request-rerender-all-views.el'), and return a save-result
  of the rerender: the content of its view of VIEW_ID,
  the errors and warnings of its 'rerender-done'. Raise when the
  server asks again, or sends no view of VIEW_ID."""
  view_ids = [ sexp_to_text (withheld)
               for withheld in response_field (confirmation, "view-ids") or [] ]
  pids = [ sexp_to_text (pid)
           for pid in response_field (confirmation, "pids") or [] ]
  messages = connection . rerender_views (view_ids, pids)
  done = messages [-1]
  rerendered = [ message for message in messages
                 if response_type (message) == "rerender-view"
                 and sexp_to_text ( response_field (message, "view-id") ) == view_id ]
  if ( response_type ( messages [0] ) == "overPrivateText-telescope-confirmation"
       or not rerendered ):
    print ( "Server: " + "; " . join (
      [ response_text_field ( messages [0], "prompt" ) ] +
      [ sexp_to_text (error) for error in response_field (done, "errors") or [] ] ))
    raise SkgToolError (
      "The save succeeded, but the server did not release its re-rendered "
      "view (see above); the files are out of date. View the node again "
      "with '--show-overPrivateText'." )
  return [ [ Symbol ("response-type"), Symbol ("save-result") ],
           [ Symbol ("content"), response_field ( rerendered [0], "content" ) ],
           [ Symbol ("errors"), response_field (done, "errors") or [] ],
           [ Symbol ("warnings"), response_field (done, "warnings") or [] ] ]

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
  server writes it: before the first part headed by, or the first atom
  named, one of FOLLOWING_PART_HEADS, or else last."""
  for element_start, element in top_level_elements_with_offsets (metadata_text):
    if ( ( isinstance (element, list) and element
           and element [0] in following_part_heads )
         or ( isinstance (element, Symbol)
              and element in following_part_heads )):
      return ( metadata_text [ : element_start ] + part_text + " " +
               metadata_text [ element_start : ] )
  return metadata_text [ : -1 ] + " " + part_text + ")"

def close_view_on_server (connection, view_id):
  connection . request (
    request_line ( "close view", ( "view-id", view_id ) ))

def release_view (connection, view_id):
  """Close the view a command wrote, as a client closes a killed
  buffer's, unless the connection holds it: a daemon's connection
  keeps every view written into its folder."""
  if not connection . holds_view (view_id):
    close_view_on_server (connection, view_id)

# ============================================================
# Writing the files
# ============================================================

class RenderedView:
  """A view as the server rendered it, which 'write_view_files' writes
  as three files. 'view_id' is the server's view ID; 'command' the
  command the sidecar records ('view', 'search', 'diff', ...), which
  a view command sets after fetching; 'request' a dict of what it
  asked for; 'content' the full view's text; 'errors' and 'warnings'
  the server's; 'processed_summary' the server's, for a buffer of the
  diff view; 'perspective' and 'perspective_name' the view's
  perspective, by the server's wire name and its display name."""
  def __init__ (self, view_id, request, content, errors, warnings,
                command = None, processed_summary = None,
                perspective = DEFAULT_PERSPECTIVE, perspective_name = None):
    self . view_id = view_id
    self . command = command
    self . request = request
    self . content = content
    self . errors = errors
    self . warnings = warnings
    self . processed_summary = processed_summary
    self . perspective = perspective
    self . perspective_name = perspective_name

def write_view_files (connection, view, out_dir, base_name, show_summary,
                      unless_compact_differs_from = None):
  """Write the RenderedView VIEW's three files; return their paths. The
  sidecar records the view's perspective: VIEW's
  'perspective' and 'perspective_name', which a rewrite of a
  view carries from its sidecar ('perspective_fields_of_sidecar'), or
  the root homes, a fresh view's. Given
  UNLESS_COMPACT_DIFFERS_FROM, write nothing, and return None, when
  the compact view on disk is no longer that text: it was edited
  since it was read."""
  os . makedirs (out_dir, exist_ok = True)
  full_path    = os . path . join (out_dir, base_name + ".full.org")
  compact_path = os . path . join (out_dir, base_name + ".compact.org")
  sidecar_path = os . path . join (out_dir, base_name + ".view.json")
  full_text = view . content
  parsed_view = parse_full_view (full_text)
  link_statuses = fetch_link_statuses (connection, parsed_view)
  repo_ownership = fetch_repo_ownership (connection)
  herald_tables = fetch_herald_tables (connection)
  compact_lines = []
  if show_summary:
    compact_lines += summary_lines (
      parsed_view, link_statuses, repo_ownership, view,
      os . path . basename (full_path) )
  compact_lines += compact_tree_lines (
    parsed_view, link_statuses, herald_tables )
  held = connection . holds_views_it_writes ()
  if ( unless_compact_differs_from is not None
       and read_text_file (compact_path) != unless_compact_differs_from ):
    return None
  write_text_file (full_path, full_text)
  write_text_file (compact_path, "\n" . join (compact_lines) + "\n")
  write_text_file (
    sidecar_path,
    json . dumps (
      { "view_id"      : view . view_id,
        "command"      : view . command,
        "request"      : view . request,
        "perspective" : view . perspective,
        "perspective_name"   : view . perspective_name,
        "port"         : connection . port,
        "full_view"    : os . path . basename (full_path),
        "compact_view" : os . path . basename (compact_path),
        "errors"       : view . errors,
        "warnings"     : view . warnings,
        "summary"      : show_summary,
        "closed"       : not held },
      indent = 2, ensure_ascii = False ) + "\n" )
  connection . note_view_written ( view . view_id, out_dir, base_name )
  return [ full_path, compact_path, sidecar_path ]

def write_text_file (path, text):
  with open (path, "w", encoding = "utf-8") as output_file:
    output_file . write (text)

def read_text_file (path):
  with open (path, encoding = "utf-8") as input_file:
    return input_file . read ()

# ============================================================
# Reading a view's files back, and its sidecar
# ============================================================
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
    self . headline_of_handle = headlines_by_viewnode_id (self . parsed_view)

def headlines_by_viewnode_id (parsed_view):
  """{VIEWNODE ID: VIEWLINE} for each headline of the full view."""
  return { headline_viewnode_id ( line . metadata ) : line
           for line in parsed_view
           if line . is_headline
           and headline_viewnode_id ( line . metadata ) is not None }

def load_saved_view (path):
  """PATH is a sidecar, one of the other two files of its view, or a
  directory holding exactly one sidecar."""
  return SavedView ( sidecar_path_of (path) )

# The other two files of a view, each named as its sidecar is but for
# its suffix.
VIEW_ORG_FILE_SUFFIXES = ( ".compact.org", ".full.org" )

def sidecar_path_of (path):
  """PATH, a sidecar; or the sidecar beside PATH, the compact or the
  full view of the same view; or the one sidecar in the directory
  PATH. Any other path is refused, saying what is wanted."""
  wanted = ( "name a view's BASE.view.json (or its BASE.compact.org or "
             "BASE.full.org), or a directory holding only one view" )
  if not os . path . isdir (path):
    for suffix in VIEW_ORG_FILE_SUFFIXES:
      if path . endswith (suffix):
        sidecar = path [ : - len (suffix) ] + ".view.json"
        if not os . path . isfile (sidecar):
          raise SkgToolError (
            f"{path} has no {os . path . basename (sidecar)} beside it, "
            "which records the view it shows; " + wanted + "." )
        return sidecar
    if not path . endswith (".view.json"):
      raise SkgToolError ( f"{path} is not a view's file; " + wanted + "." )
    if not os . path . isfile (path):
      raise SkgToolError ( f"{path} does not exist; " + wanted + "." )
    return path
  sidecars = sorted ( name for name in os . listdir (path)
                      if name . endswith (".view.json") )
  if len (sidecars) != 1:
    raise SkgToolError (
      f"{path} holds {len (sidecars)} '.view.json' files"
      + ( ": " + ", " . join (sidecars) if sidecars else "" )
      + "; name the one to save." )
  return os . path . join ( path, sidecars [0] )

def mark_sidecar_closed (sidecar_path):
  with open (sidecar_path, encoding = "utf-8") as sidecar_file:
    sidecar = json . load (sidecar_file)
  sidecar [ "closed" ] = True
  write_text_file ( sidecar_path,
                    json . dumps (sidecar, indent = 2, ensure_ascii = False)
                    + "\n" )

def perspective_of_sidecar (sidecar):
  """The perspective of the view a SIDECAR records, by the server's
  wire name: 'skgrepo NAME', or 'root homes', as every fresh view
  does, and a sidecar that names none or names one in an older form
  ('user', 'owner of NAME', 'root authors')."""
  recorded = sidecar . get ( "perspective", DEFAULT_PERSPECTIVE )
  if isinstance (recorded, str) and recorded . startswith ("skgrepo "):
    return recorded
  return DEFAULT_PERSPECTIVE

def perspective_fields_of_sidecar (sidecar):
  """The fields of a 'RenderedView' that carry the perspective a
  SIDECAR records into a rewrite of its view."""
  return { "perspective" : perspective_of_sidecar (sidecar),
           "perspective_name"   : sidecar . get ("perspective_name") }

def require_port_of_sidecar (port, sidecar):
  """A command on a sidecar talks to the server the sidecar records;
  a '--port' (PORT, unless None) naming another is refused."""
  if port is not None and port != sidecar [ "port" ]:
    raise SkgToolError (
      f"The view came from the server on port {sidecar [ 'port' ]}, "
      f"not {port}." )
