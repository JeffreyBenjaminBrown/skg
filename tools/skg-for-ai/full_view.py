"""A full view, parsed into lines ('parse_full_view', 'ViewLine'),
and the facts one headline's '(skg ...)' metadata states: its kind,
its IDs, its repo, its folder. Every other reader of a view stands
on this."""

import json
import os
import re

from sexp import (
  Symbol, end_of_balanced_sexp, metadata_form, parse_sexp, sexp_to_text )

REPO_ROOT =os . path . dirname ( os . path . dirname (
  os . path . dirname ( os . path . abspath (__file__) )))
SHARED_FOLDER = os . path . join (REPO_ROOT, "shared")
HEADLINE_PATTERN = re . compile (r'^(\*+) (.*)$')

# ============================================================
# The full view, parsed
# ============================================================

class ViewLine:
  """One line of the full view. A headline has 'stars', 'title' (the
  text after its metadata), 'metadata' (the parsed '(skg ...)', or
  None) and 'metadata_text' (its text); a body line has only 'text'."""
  def __init__ (self, line_number, text):
    self . line_number = line_number
    self . text = text
    self . is_headline = False
    self . stars = None
    self . title = None
    self . metadata = None
    self . metadata_text = None

def parse_full_view (full_text):
  lines = full_text . split ("\n")
  if lines and lines [-1] == "":
    lines = lines [ : -1 ]
  parsed = []
  for index, text in enumerate (lines):
    view_line = ViewLine (index + 1, text)
    match = HEADLINE_PATTERN . match (text)
    if match:
      view_line . is_headline = True
      view_line . stars = match . group (1)
      span = metadata_span_of_headline (text)
      if span is None:
        view_line . title = match . group (2)
      else:
        view_line . metadata_text = text [ span [0] : span [1] ]
        view_line . metadata = parse_sexp (view_line . metadata_text)
        view_line . title = text [ span [1] : ] . lstrip (" ")
    parsed . append (view_line)
  return parsed

def metadata_span_of_headline (headline_text):
  """The [start, end) offsets of the '(skg ...)' right after the stars
  and a space, or None."""
  match = HEADLINE_PATTERN . match (headline_text)
  if not match:
    return None
  start = match . start (2)
  if headline_text [ start : start + 5 ] not in ( "(skg ", "(skg)" ):
    # As the clients read it: a title may start "(skgrepos ...".
    return None
  end = end_of_balanced_sexp (headline_text, start)
  return ( start, end ) if end is not None else None

# ============================================================
# Facts from one headline's metadata
# ============================================================

def metadata_fields_table ():
  with open ( os . path . join (SHARED_FOLDER, "metadata-fields.json"),
              encoding = "utf-8" ) as table_file:
    return json . load (table_file)

def metadata_kinds ():
  return metadata_fields_table () [ "kinds" ]

KIND_ATOMS = { atom for kind in metadata_kinds () for atom in kind [ "atoms" ] }
# The parts of '(skg ...)' in the order the server writes them.
METADATA_PARTS = metadata_fields_table () [ "parts" ]
FOLDER_ATOMS = { atom for kind in metadata_kinds () if kind [ "kind" ] == "folder"
                 for atom in kind [ "atoms" ] }

def headline_kind (metadata):
  """The kind atom's text, with any parameter ('flag hadId'), or
  'node' when the metadata names none."""
  for element in metadata [ 1 : ]:
    if isinstance (element, Symbol) and element in KIND_ATOMS:
      return str (element)
    if ( isinstance (element, list) and element
         and isinstance (element [0], Symbol) and element [0] in KIND_ATOMS ):
      return " " . join ( sexp_to_text (part) for part in element )
  return "node"

def is_folder_kind (kind):
  return kind . endswith ("Folder")

def folder_atom (metadata):
  """The folder atom of a folder headline's metadata, or None."""
  return next ( ( str (element) for element in metadata [ 1 : ]
                  if isinstance (element, Symbol) and element in FOLDER_ATOMS ),
                None )

def metadata_has_atom (metadata, atom_name):
  return any ( isinstance (element, Symbol) and element == atom_name
               for element in metadata [ 1 : ] )

def headline_viewnode_id (metadata):
  """The viewnode ID of '(vnid X)', the first part of every headline's
  metadata the server renders, or None."""
  form = metadata_form (metadata, "vnid") if metadata is not None else None
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def headline_gnid (metadata):
  form = metadata_form (metadata, "gnid")
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def headline_node_repo (metadata):
  skgrepos = metadata_form (metadata, "skgRepos")
  form = metadata_form (skgrepos, "nodeRepo")
  return sexp_to_text ( form [1] ) if form and len (form) > 1 else None

def relation_side (metadata, relation_name, side_name):
  """( COUNT, ANCESTOR_GENERATIONS ) of a relation side in the
  headline's '(view (rels ...))', or None."""
  view = metadata_form (metadata, "view")
  rels = metadata_form (view, "rels")
  side = metadata_form ( metadata_form (rels, relation_name), side_name )
  if side is None:
    return None
  return ( first_number (side), ancestor_generations (side) )

def first_number (sexp):
  for element in sexp [ 1 : ]:
    if isinstance (element, int):
      return element
  return 0

def ancestor_generations (side):
  ancestors = metadata_form (side, "ancestors")
  if ancestors is None:
    return []
  return [ element for element in ancestors [ 1 : ]
           if isinstance (element, int) ]
