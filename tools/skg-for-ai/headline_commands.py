"""The commands about one headline or link, as the clients' 'C-c U',
'C-c v ?' and 'C-c v s' do: 'chain' (a headline's ancestry as links),
'describe' (what its heralds mean) and 'linkstack' (the clients' stack
of links, kept in a file)."""

import os
import re

from sexp import metadata_form, sexp_to_text
from server_connection import (
  SkgConnection, SkgToolError, request_line, require_response_type,
  response_field )
from full_view import headline_gnid, headline_viewnode_id, relation_side
from compact_view import fetch_link_statuses_of_ids, herald_contexts
from views import (
  load_saved_view, read_text_file, require_port_of_sidecar,
  write_text_file )
from edited_compact_view import (
  PLACED_NODE_HANDLE_WORD, WORD_PARAMETER_PATTERN )

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
