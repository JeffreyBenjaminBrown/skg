"""The compact view ('compact_tree_lines'): each headline's metadata
replaced by a handle of its heralds, each link followed by its
target's title, and the summary above the tree ('summary_lines').
Also the heralds themselves: relationship heralds, every herald by the
lens, what a headline's heralds need from the headlines around it; and
change facts and repo ownership."""

import json
import os
import re

from sexp import (
  Symbol, metadata_form, parse_sexp, quote_string, render_sexp,
  sexp_to_text )
from server_connection import (
  DEFAULT_PERSPECTIVE, SkgToolError, request_line,
  require_response_type, response_field, response_type )
from full_view import (
  SHARED_FOLDER, ancestor_generations, first_number, folder_atom,
  headline_gnid, headline_kind, headline_node_repo,
  headline_viewnode_id, is_folder_kind, metadata_has_atom,
  relation_side )

LINK_PATTERN = re . compile (
  r'\[\[id:([^\]\[]+)\](?:\[[^\]]*\])?\]')

# ============================================================
# Relationship heralds, as the clients render them, styles dropped.
# The logic mirrors 'render_rel_facts' in 'nvim/lua/skg/heralds.lua'
# and 'heralds--render-rel-facts' in 'elisp/heralds-minor-mode.el';
# 'docs/heralds.org', "Relationship heralds", states the rules.
# ============================================================

def relations_in_display_order ():
  with open ( os . path . join (SHARED_FOLDER, "relations.json"),
              encoding = "utf-8" ) as relations_file:
    relations = json . load (relations_file) [ "relations" ]
  return [ ( relation [ "name" ], relation [ "letter" ] )
           for relation in sorted (
               relations, key = lambda r : r [ "display_position" ] ) ]

RELATIONS_IN_DISPLAY_ORDER = relations_in_display_order ()
# Ordinary content: the parent contains it, and nothing else does. A
# handle leaves this token out; every other form stays.
BARE_PARENT_CONTAINS_TOKEN = "aC"
PROPERTY_COUNT_LETTERS = [ ( "aliases", "A" ), ( "extraIds", "I" ),
                           ( "flags", "F" ) ]

def relationship_heralds_text (metadata):
  """The headline's relationship and property-count tokens,
  space-separated, or '' when it has none."""
  view = metadata_form (metadata, "view")
  if view is None:
    return ""
  rels = metadata_form (view, "rels")
  props = metadata_form (view, "props")
  in_place_of = metadata_form (metadata, "inPlaceOf") is not None
  tokens = []
  for relation_name, letter in RELATIONS_IN_DISPLAY_ORDER:
    form = metadata_form (rels, relation_name)
    if form is not None or ( relation_name == "forks" and in_place_of ):
      token = relation_token (relation_name, letter, form, in_place_of)
      if token:
        tokens . append (token)
  for property_name, letter in PROPERTY_COUNT_LETTERS:
    form = metadata_form (props, property_name)
    if form is not None:
      tokens . append ( letter + str ( first_number (form) ))
  return " " . join (tokens)

def relation_token (relation_name, letter, form, in_place_of):
  in_side = relation_side_text (relation_name, "in", form)
  out_side = relation_side_text (relation_name, "out", form)
  shows_in_place_of = relation_name == "forks" and in_place_of
  if not in_side and not out_side and not shows_in_place_of:
    return ""
  return in_side + letter + ( "ĥ" if shows_in_place_of else "" ) + out_side

def relation_side_text (relation_name, side_name, form):
  side = metadata_form (form, side_name)
  if side is None:
    return ""
  count = first_number (side)
  generations = ancestor_generations (side)
  subset_name = ( "unintegrated"
                  if ( relation_name, side_name ) == ( "contains", "out" )
                  else "substantive"
                  if ( relation_name, side_name ) == ( "linksTo", "in" )
                  else None )
  subset = metadata_form (side, subset_name) if subset_name else None
  if subset is not None:
    return fraction_text ( count, generations, first_number (subset),
                           ancestor_generations (subset) )
  return side_text (count, generations)

def side_text (count, generations):
  if count == 0 and not generations:
    return ""
  return part_text (count, generations)

def part_text (count, generations, member_generations = None):
  """COUNT, unless the ancestor flags account for every member, then
  the flags for GENERATIONS."""
  members = distinct_generations (
    generations if member_generations is None else member_generations )
  numeral = "" if count == len (members) else str (count)
  return numeral + "" . join (
    ancestor_flag (generation)
    for generation in distinct_generations (generations) )

def fraction_text (total, total_generations, numerator, numerator_generations):
  if numerator > total:
    raise SkgToolError (
      f"herald subset {numerator} exceeds total {total}" )
  if total == 0:
    return ""
  if numerator == 0:
    return side_text (total, total_generations)
  total_generations = distinct_generations (total_generations)
  numerator_generations = distinct_generations (numerator_generations)
  remaining_generations = [ generation for generation in total_generations
                            if generation not in numerator_generations ]
  text = part_text (numerator, numerator_generations) + "/"
  if numerator != total:
    text += part_text (total, remaining_generations, total_generations)
  return text

def ancestor_flag (generation):
  if 1 <= generation <= 26:
    return chr ( ord ("a") + generation - 1 )
  return "{" + str (generation) + "}"

def distinct_generations (generations):
  return sorted ( set (generations) )

# ============================================================
# Links
# ============================================================

def link_target_ids_in_order (parsed_view):
  ids = []
  for view_line in parsed_view:
    for match in LINK_PATTERN . finditer ( visible_text_of_line (view_line) ):
      if match . group (1) not in ids:
        ids . append ( match . group (1) )
  return ids

def visible_text_of_line (view_line):
  """What a reader sees of the line: a headline's title, a body
  line's text."""
  return view_line . title if view_line . is_headline else view_line . text

def fetch_link_statuses (connection, parsed_view):
  """The statuses of the view's link targets: see
  'fetch_link_statuses_of_ids'."""
  return fetch_link_statuses_of_ids (
    connection, link_target_ids_in_order (parsed_view) )

def fetch_link_statuses_of_ids (connection, ids):
  """{ID: {'status': 'resolved'|'restricted'|'missing', 'pid', 'label',
  'title'}} for each of IDS: the 'link statuses' request the clients'
  link annotations send, then 'titles by ids' for the resolved
  targets' titles."""
  if not ids:
    return {}
  response = connection . request (
    "((request . \"link statuses\") (request-id . \"skg-for-ai\") (ids " +
    " " . join ( quote_string (target) for target in ids ) + "))" )
  require_response_type (response, "link-statuses")
  statuses = {}
  for row in response_field (response, "results") or []:
    target = sexp_to_text ( row [0] )
    status = sexp_to_text ( row [1] )
    statuses [target] = { "status" : status }
    if status == "resolved":
      statuses [target] [ "pid" ] = sexp_to_text ( row [2] )
      statuses [target] [ "label" ] = sexp_to_text ( row [3] )
  titles = fetch_titles (
    connection, sorted ({ entry [ "pid" ] for entry in statuses . values ()
                          if entry [ "status" ] == "resolved" }) )
  for entry in statuses . values ():
    if entry [ "status" ] == "resolved":
      entry [ "title" ] = titles . get ( entry [ "pid" ] )
  return statuses

def fetch_titles (connection, ids):
  if not ids:
    return {}
  response = connection . request (
    "((request . \"titles by ids\") (ids " +
    " " . join ( quote_string (target) for target in ids ) + "))" )
  if response_type (response) != "titles-by-ids":
    return {}
  titles = {}
  for pair in response_field (response, "content") or []:
    # each pair is ("ID" . "TITLE")
    titles [ sexp_to_text ( pair [0] ) ] = sexp_to_text ( pair [-1] )
  return titles

def annotate_links (text, link_statuses):
  def annotated (match):
    return match . group (0) + link_annotation (
      link_statuses . get ( match . group (1) ))
  return LINK_PATTERN . sub (annotated, text)

# What follows a link whose target this server holds no node of (the
# server's status 'missing'): the node may be deleted, never written,
# or in a skgrepo this server does not load, so no fault is claimed.
NOT_HELD_ANNOTATION = "⟦not held here⟧"
# What follows a link whose status the server did not give back.
LOOKUP_FAILED_ANNOTATION = "⟦✗ lookup failed⟧"

def link_annotation (entry):
  if entry is None:
    return LOOKUP_FAILED_ANNOTATION
  if entry [ "status" ] == "missing":
    return NOT_HELD_ANNOTATION
  if entry [ "status" ] != "resolved":
    return "⟦✗ " + annotation_safe ( entry [ "status" ] ) + "⟧"
  title = entry . get ( "title" ) or "(title not found)"
  return ( "⟦→ " + annotation_safe (title) + " ⌂:"
           + annotation_safe ( entry [ "label" ] ) + "⟧" )

# An annotation's brackets, and the look-alikes that replace them in
# the text it quotes, so that 'ANNOTATED_LINK_PATTERN' ends it at its
# own '⟧' and a save strips exactly what the view wrote.
ANNOTATION_SAFE_BRACKETS = str . maketrans ({ "⟦" : "⦗", "⟧" : "⦘" })

def annotation_safe (text):
  return text . translate (ANNOTATION_SAFE_BRACKETS)

# ============================================================
# Repo ownership: whether each configured skgrepo is owned or foreign,
# as the server's 'repos' request reports it.
# ============================================================

def fetch_repo_ownership (connection):
  """{REPO: True (owned) | False (foreign)}, for every skgrepo of the
  config. A skgrepo it lacks is of unknown ownership."""
  response = connection . request ( request_line ("repos") )
  require_response_type (response, "repos")
  ownership = {}
  for entry in response_field (response, "repos") or []:
    entry = [ None ] + entry
    name = sexp_to_text ( metadata_form (entry, "name") [1] )
    owned = sexp_to_text ( metadata_form (entry, "owned") [1] )
    ownership [name] = owned == "true"
  return ownership

# ============================================================
# The compact view
# ============================================================

def compact_tree_lines (parsed_view, link_statuses, herald_tables):
  """The compact view's lines of PARSED_VIEW: each line of it, but
  the body of a forks-folder member that restates its note's
  ('restatements')."""
  contexts = herald_contexts (parsed_view)
  restated, omitted_line_numbers = restatements (parsed_view)
  return [ compact_line ( view_line, link_statuses, herald_tables,
                          contexts . get ( view_line . line_number ),
                          restated . get ( view_line . line_number, () ))
           for view_line in parsed_view
           if view_line . line_number not in omitted_line_numbers ]

def compact_line (view_line, link_statuses, herald_tables, context,
                  restated = ()):
  if not view_line . is_headline:
    return annotate_links (view_line . text, link_statuses)
  title = annotate_links (view_line . title, link_statuses)
  handle = handle_text (view_line, herald_tables, context, restated)
  return ( view_line . stars + " " + title +
           ( " " if title else "" ) + handle )

# The word a handle carries for each field a forks-folder member
# restates of its note, b, the folder's viewparent: its title, its
# body ('restatements').
RESTATED_WORDS = { "title" : "title=b", "body" : "body=b" }

def restatements (parsed_view):
  """({LINE NUMBER: FIELDS}, OMITTED): for each headline of PARSED_VIEW
  that is a node standing in a forks folder whose title, or nonblank
  body, equals that of the folder's viewparent node (its note), the
  fields it restates, 'title' and 'body'; and the line numbers of the
  bodies it restates, which the compact view leaves out. The clients
  draw such a member with its title and body covered
  ('elisp/skg-fork-dedup.el'); a save reads a left-out body back from
  the full view ('edited_outline')."""
  headlines = []
  for view_line in parsed_view:
    if view_line . is_headline:
      headlines . append (( view_line, [] ))
    elif headlines:
      headlines [-1] [1] . append (view_line)
  parents = parent_indices_by_level (
    [ len ( view_line . stars ) for view_line, _ in headlines ] )
  def body_text (body_lines):
    return "\n" . join ( line . text for line in body_lines ) . rstrip ()
  restated = {}
  omitted = set ()
  for index, (view_line, body_lines) in enumerate (headlines):
    folder = parents [index]
    note = None if folder is None else parents [folder]
    def has_atom (line, atom):
      return line . metadata is not None and metadata_has_atom (line . metadata, atom)
    if ( note is None
         or not has_atom ( headlines [folder] [0], "forksFolder" )
         or not has_atom ( view_line, "node" )
         or not has_atom ( headlines [note] [0], "node" )):
      continue
    note_line, note_body = headlines [note]
    fields = []
    if view_line . title . strip () and view_line . title == note_line . title:
      fields . append ("title")
    body = body_text (body_lines)
    if body and body == body_text (note_body):
      fields . append ("body")
      omitted . update ( line . line_number for line in body_lines )
    if fields:
      restated [ view_line . line_number ] = tuple (fields)
  return restated, omitted

def handle_text (view_line, herald_tables, context, restated = ()):
  """'⟨ID HERALDS title=b body=b folded⟩': ID the headline's viewnode
  ID, HERALDS every herald a client shows for it but a bare 'aC', a
  word for each field it restates of its note ('restatements'), and
  'folded' when a client shows it collapsed."""
  viewnode_id = headline_viewnode_id (view_line . metadata)
  if viewnode_id is None:
    raise SkgToolError (
      f"Line {view_line . line_number} of the view is a headline without "
      "a viewnode ID, so it can have no handle: " + view_line . text )
  parts = [ viewnode_id ]
  heralds = herald_text (
    view_line . metadata, herald_tables, context,
    omitted_relationship_tokens = ( BARE_PARENT_CONTAINS_TOKEN, ))
  if heralds:
    parts . append (heralds)
  parts += [ RESTATED_WORDS [field] for field in restated ]
  if metadata_has_atom (view_line . metadata, "folded"):
    parts . append ("folded")
  return "⟨" + " " . join (parts) + "⟩"

# ============================================================
# Every herald, as the clients render a headline's metadata, styles
# dropped. The server serves the rule table ('herald rules'), which
# the lens below applies to the metadata; it mirrors
# 'nvim/lua/skg/sexpr/lens.lua', the port of
# 'elisp/skg-sexpr/skg-lens.el', whose docstrings give the rule
# grammar. The rule table only positions the relationship heralds,
# with a sentinel that 'relationship_heralds_text' fills. Three
# heralds need what one headline's metadata cannot say, which a
# client supplies from the headlines around it and the label table
# ('docs/heralds.org', "What the server serves, and what the clients
# add"): a 'repoBoundary' gets its skgrepo's label, a pending
# detachment the atom 'pendingDetachment', and a changed folder its
# changed label in place of the lens's. 'chunks_from_metadata' in
# 'nvim/lua/skg/heralds.lua' and 'heralds-from-metadata' in
# 'elisp/heralds-minor-mode.el' do the same.
# ============================================================

def herald_styles ():
  with open ( os . path . join (SHARED_FOLDER, "herald-styles.json"),
              encoding = "utf-8" ) as styles_file:
    return json . load (styles_file)

HERALD_STYLES = herald_styles ()
# The style keywords a rule may open with: each style's name in
# capitals, as the served rule table spells it.
STYLE_KEYWORDS = { name . upper () for name in HERALD_STYLES [ "styles" ] }
FOLDER_HERALDS = HERALD_STYLES [ "folder_heralds" ]
# What the rule table emits where the relationship heralds go
# ('RELS_SPANS_SENTINEL' in 'server/heralds.rs').
RELATIONSHIP_HERALDS_SENTINEL = "__RELS_SPANS__"

class HeraldTables:
  """The two tables the server serves over 'herald rules': the rule
  table, parsed, and {REPO: LABEL}, each configured skgrepo's herald
  label."""
  def __init__ (self, rules, skgrepo_labels):
    self . rules = rules
    self . skgrepo_labels = skgrepo_labels

def fetch_herald_tables (connection):
  """The connection's HeraldTables, fetched on its first use, so once
  per command."""
  if connection . herald_tables is None:
    response = connection . request ( request_line ("herald rules") )
    require_response_type (response, "herald-rules")
    connection . herald_tables = herald_tables_of_response (response)
  return connection . herald_tables

def herald_tables_of_response (response):
  """The HeraldTables of a 'herald rules' response: its
  '(content "RULES")' and '(skgrepoLabels ("REPO" "LABEL")...)'."""
  labels = metadata_form ( [ None ] + response, "skgrepoLabels" ) or [ None ]
  return HeraldTables (
    parse_sexp ( response_field (response, "content") ),
    { sexp_to_text ( entry [0] ) : sexp_to_text ( entry [1] )
      for entry in labels [ 1 : ] } )

class HeraldContext:
  """What a headline's heralds need from the headlines around it.
  'folder_change' is None, or, for a folder a member of which carries
  a change of its relationship to the folder's viewparent, whether
  every such change is processed; 'pending_detachment' whether the
  headline is a pending detachment."""
  def __init__ (self, folder_change = None, pending_detachment = False):
    self . folder_change = folder_change
    self . pending_detachment = pending_detachment

def herald_text (metadata, herald_tables, context = HeraldContext (),
                 omitted_relationship_tokens = ()):
  """The heralds a client shows for METADATA, the parsed '(skg ...)',
  as text: tokens separated by a space, but those the rule table
  marks ABUT, which join the one before. OMITTED_RELATIONSHIP_TOKENS
  are relationship tokens left out."""
  folder = folder_atom (metadata)
  if ( context . folder_change is not None and folder is not None
       and folder in FOLDER_HERALDS [ "changed_labels" ] ):
    return changed_folder_herald_text (folder, context . folder_change)
  lensed = metadata_with_repoBoundary_label (
    metadata, herald_tables . skgrepo_labels )
  if context . pending_detachment:
    lensed = lensed + [ Symbol ("pendingDetachment") ]
  relationship_text = " " . join (
    token for token in relationship_heralds_text (metadata) . split ()
    if token not in omitted_relationship_tokens )
  return lens_tokens_as_text (
    lens_tokens (lensed, herald_tables . rules), relationship_text )

def changed_folder_herald_text (folder, processed):
  """A changed folder's herald: its ☮ when it is write-protected, its
  changed label, then ✓ when PROCESSED."""
  return ( ( "☮ " if folder in FOLDER_HERALDS [ "write_protected" ] else "" )
           + FOLDER_HERALDS [ "changed_labels" ] [folder]
           + ( "✓" if processed else "" ))

def metadata_with_repoBoundary_label (metadata, skgrepo_labels):
  """METADATA with the atom 'repoBoundary' in its '(view ...)'
  replaced by '(repoBoundary LABEL)', LABEL the label SKGREPO_LABELS
  gives its nodeRepo, or that nodeRepo when the table lacks it. On an
  attributed fork, whose '(view ...)' holds '(attribution NAME
  [unverified])', LABEL is "<HOME/ORIGINAL>": HOME that label,
  ORIGINAL NAME's label, followed by "?" when unverified. A headline
  naming no nodeRepo keeps the bare atom, which renders "⌂"."""
  repo = headline_node_repo (metadata)
  if repo is None:
    return metadata
  label = skgrepo_labels . get (repo, repo)
  for part in metadata:
    if is_list_headed_by (part, "view"):
      for fact in part:
        if is_list_headed_by (fact, "attribution") and len (fact) > 1:
          original = str (fact [1])
          unverified = any ( is_symbol_named (x, "unverified") for x in fact [2:] )
          label = ( "<" + label + "/" + skgrepo_labels . get (original, original)
                    + ( "?" if unverified else "" ) + ">" )
  return [ [ [ Symbol ("repoBoundary"), label ]
             if is_symbol_named (fact, "repoBoundary") else fact
             for fact in part ]
           if is_list_headed_by (part, "view") else part
           for part in metadata ]

def lens_tokens_as_text (tokens, relationship_text):
  """TOKENS joined as a client shows them: separated by a space, but a
  token marked abut, which joins the one before; structural colons
  stripped. The first sentinel token becomes RELATIONSHIP_TEXT, and
  any later one is dropped (the rule table emits one for '(rels ...)'
  and one for '(props ...)')."""
  pieces = []
  relationship_text_placed = False
  for text, abut in tokens:
    if text == RELATIONSHIP_HERALDS_SENTINEL:
      if not relationship_text or relationship_text_placed:
        continue
      text = relationship_text
      relationship_text_placed = True
    else:
      text = without_structural_colons (text)
      if not text:
        continue
    if pieces and not abut:
      pieces . append (" ")
    pieces . append (text)
  return "" . join (pieces)

def without_structural_colons (text):
  """TEXT without the colons the lens joins atoms with where they
  mean nothing: a colon is structural when the character before or
  after it is not alphanumeric, '+', '-' or a space, except a colon
  after '⌂' ("⌂:LABEL") or before '~' ("request:~NAME").
  ('strip_structural_colons' in 'nvim/lua/skg/heralds.lua'.)"""
  def keeps_adjacent_colon (character):
    return character in "+ -" or character . isalnum ()
  kept = []
  index = 0
  while index < len (text):
    previous = text [ index - 1 ] if index > 0 else None
    this = text [index]
    following = text [ index + 1 ] if index + 1 < len (text) else None
    if ( following == ":" and this not in ":⌂"
         and not keeps_adjacent_colon (this) ):
      kept . append (this)
      index += 2
    elif ( this == ":" and following is not None and previous != "⌂"
           and following != "~" and not keeps_adjacent_colon (following) ):
      kept . append (following)
      index += 2
    else:
      kept . append (this)
      index += 1
  return "" . join (kept)

# ------------------------------------------------------------
# The lens: a rule table applied to a sexp. A token is ( TEXT, ABUT ).
# ------------------------------------------------------------

def lens_tokens (sexp, rules):
  """RULES applied to SEXP, when RULES' label is SEXP's head."""
  if not isinstance (sexp, list) or not isinstance (rules, list):
    return []
  header = lens_rule_header (rules)
  if not sexp or not sexp_values_equal ( sexp [0], header [ "label" ] ):
    return []
  return lens_transform_from (sexp, header)

def lens_rule_header (rule):
  """RULE parsed:
    RULE        ::= WHEN-BODY | [STYLE] (INTERC-BODY | SIMPLE-BODY)
    WHEN-BODY   ::= WHEN LABEL... CHILDREN...
    INTERC-BODY ::= INTERC SEP [LABEL] CHILDREN...
    SIMPLE-BODY ::= LABEL [ABUT] CHILDREN...
  A simple body is 'non_leaf' when a child is a list, else 'any_leaf'
  when its label is ANY, else 'atomic_leaf'."""
  index = 0
  if is_symbol_named ( rule [index], "WHEN" ):
    index += 1
    labels = []
    while index < len (rule) and isinstance ( rule [index], Symbol ):
      labels . append ( rule [index] )
      index += 1
    return { "kind" : "when", "labels" : labels, "label" : None,
             "abut" : False, "children" : rule [ index : ] }
  if isinstance ( rule [index], Symbol ) and rule [index] in STYLE_KEYWORDS:
    index += 1
  if is_symbol_named ( rule [index], "INTERC" ):
    separator = rule [ index + 1 ]
    index += 2
    label = None
    if ( index < len (rule) and isinstance ( rule [index], Symbol )
         and rule [index] not in STYLE_KEYWORDS ):
      label = rule [index]
      index += 1
    return { "kind" : "interc", "separator" : separator, "label" : label,
             "abut" : False, "children" : rule [ index : ] }
  label = rule [index]
  index += 1
  abut = index < len (rule) and is_symbol_named ( rule [index], "ABUT" )
  if abut:
    index += 1
  children = rule [ index : ]
  kind = ( "non_leaf" if any ( isinstance (child, list) for child in children )
           else "any_leaf" if is_symbol_named (label, "ANY")
           else "atomic_leaf" )
  return { "kind" : kind, "label" : label, "abut" : abut,
           "children" : children }

def lens_transform_from (sexp, header):
  """The tokens of HEADER's list children applied to SEXP, each after
  the concatenated string children; the string children alone when
  no list child gives a token."""
  prefix = lens_string_children (header [ "children" ])
  results = [ token for child in header [ "children" ]
              if isinstance (child, list)
              for token in lens_dispatch (sexp, child) ]
  if not prefix:
    return results
  if not results:
    return [ ( prefix, False ) ]
  return [ ( prefix + text, False ) for text, abut in results ]

def lens_dispatch (sexp, rule_child):
  header = lens_rule_header (rule_child)
  kind = header [ "kind" ]
  if kind == "when":
    # The children apply to SEXP itself when an element of SEXP is
    # one of the labels, or a list headed by one.
    return ( lens_transform_from (sexp, header)
             if any ( lens_atomic_matches (sexp, label)
                      or lens_children_with_label (sexp, label)
                      for label in header [ "labels" ] )
             else [] )
  if kind == "interc":
    return lens_interc_tokens (sexp, header)
  if kind == "any_leaf":
    return lens_any_leaf_tokens (sexp, header)
  if kind == "atomic_leaf":
    if not header [ "children" ]:
      return []
    return [ ( lens_joined_text ( header [ "children" ] ), header [ "abut" ] )
             for _ in lens_atomic_matches ( sexp, header [ "label" ] ) ]
  return [ token
           for match in lens_children_with_label ( sexp, header [ "label" ] )
           for token in lens_transform_from (match, header) ]

def lens_interc_tokens (sexp, header):
  """With a label, a token per child of SEXP headed by it; without, at
  most one, of SEXP itself. A token joins with the separator the
  nonempty outputs of the list sub-rules, after the string children;
  it is left out when every output is empty."""
  prefix = lens_string_children (header [ "children" ])
  separator = lens_display_text ( header [ "separator" ] )
  targets = ( lens_children_with_label ( sexp, header [ "label" ] )
              if header [ "label" ] is not None else [ sexp ] )
  results = []
  for target in targets:
    slots = [ "" . join ( text for text, abut in lens_dispatch (target, child) )
              for child in header [ "children" ] if isinstance (child, list) ]
    if any (slots):
      results . append (
        ( prefix + separator . join ( slot for slot in slots if slot ),
          False ) )
  return results

def lens_any_leaf_tokens (sexp, header):
  """'(ANY CHILD...)': one token of the children, or, when a child is
  IT, a token per element of SEXP after its head, that element in
  IT's place."""
  children = header [ "children" ]
  if not children:
    return []
  if not any ( is_symbol_named (child, "IT") for child in children ):
    return [ ( lens_joined_text (children), header [ "abut" ] ) ]
  return [ ( lens_joined_text (
               [ element if is_symbol_named (child, "IT") else child
                 for child in children ] ),
             header [ "abut" ] )
           for element in sexp [ 1 : ] ]

def lens_joined_text (values):
  return ":" . join ( lens_display_text (value) for value in values )

def lens_display_text (value):
  return render_sexp (value) if isinstance (value, list) else str (value)

def lens_string_children (children):
  return "" . join ( child for child in children
                     if isinstance (child, str)
                     and not isinstance (child, Symbol) )

def lens_children_with_label (sexp, label):
  return [ element for element in sexp [ 1 : ]
           if isinstance (element, list) and element
           and sexp_values_equal ( element [0], label ) ]

def lens_atomic_matches (sexp, label):
  return [ element for element in sexp [ 1 : ]
           if not isinstance (element, list)
           and sexp_values_equal (element, label) ]

def sexp_values_equal (left, right):
  """Equality of parsed sexps, a symbol never equal to a string."""
  if isinstance (left, list) or isinstance (right, list):
    return ( isinstance (left, list) and isinstance (right, list)
             and len (left) == len (right)
             and all ( sexp_values_equal (l, r)
                       for l, r in zip (left, right) ))
  return type (left) is type (right) and left == right

def is_symbol_named (value, name):
  return isinstance (value, Symbol) and value == name

def is_list_headed_by (value, name):
  return isinstance (value, list) and bool (value) and is_symbol_named (
    value [0], name )

# ------------------------------------------------------------
# What a headline's heralds need from the headlines around it, read
# from the full view as a client reads it from its buffer
# ('folder_change_processed_in_lines' and
# 'pending_detachment_in_lines' in 'nvim/lua/skg/metadata.lua').
# ------------------------------------------------------------

def herald_contexts (parsed_view):
  """{LINE_NUMBER: HeraldContext} for each headline of the full view
  that has metadata."""
  headlines = [ line for line in parsed_view if line . is_headline ]
  parents = parent_indices_by_level (
    [ len ( line . stars ) for line in headlines ] )
  def metadata_at (index):
    return None if index is None else headlines [index] . metadata
  contexts = {}
  for index, line in enumerate (headlines):
    if line . metadata is None:
      continue
    parent = parents [index]
    contexts [ line . line_number ] = HeraldContext (
      folder_change_processed (headlines, index),
      is_pending_detachment (
        line . metadata, metadata_at (parent),
        metadata_at ( None if parent is None else parents [parent] )))
  return contexts

def parent_indices_by_level (levels):
  """The index of each headline's parent, by the outline LEVELS, or
  None for a top-level one."""
  parents = []
  open_ancestors = [] # indices, outermost first
  for index, level in enumerate (levels):
    while open_ancestors and levels [ open_ancestors [-1] ] >= level:
      open_ancestors . pop ()
    parents . append ( open_ancestors [-1] if open_ancestors else None )
    open_ancestors . append (index)
  return parents

def folder_change_processed (headlines, index):
  """For a folder headline, None when none of its members (its child
  headlines) carries a change of its relationship to the folder's
  viewparent, else whether every such change is processed; None for any
  other headline."""
  if ( headlines [index] . metadata is None
       or folder_atom ( headlines [index] . metadata ) is None ):
    return None
  level = len ( headlines [index] . stars )
  members = [] # each ( METADATA, [CHILD METADATA] )
  for later in headlines [ index + 1 : ]:
    later_level = len ( later . stars )
    if later_level <= level:
      break
    if later_level == level + 1:
      members . append ( ( later . metadata, [] ) )
    elif ( later_level == level + 2 and members
           and later . metadata is not None ):
      members [-1] [1] . append ( later . metadata )
  processed_flags = [ membership_change_processed (member, children)
                  for member, children in members if member is not None ]
  processed_flags = [ processed for processed in processed_flags if processed is not None ]
  return all (processed_flags) if processed_flags else None

def membership_change_processed (member, children):
  """None when the folder member MEMBER carries no change of its
  relationship to the folder's viewparent (a '(rel ...)' in its
  '(diff ...)', or a 'relRepoMove' child), else whether it is processed."""
  changed, processed = False, True
  diff = metadata_form (member, "diff")
  if metadata_form (diff, "rel") is not None:
    changed = True
    leaves = leaves_fact (member)
    processed = objects_processed (
      member, [ "rel" ] if leaves and "rel" in leaves [1] else [] )
  for child in children:
    if metadata_form (child, "relRepoMove") is not None:
      changed = True
      processed = processed and metadata_form (
        metadata_form (child, "diff"), "processed" ) is not None
  return processed if changed else None

def objects_processed (metadata, objects):
  """Whether the headline's '(processed ...)' names every one of OBJECTS;
  with none listed, whether it has a '(processed ...)'."""
  processed = metadata_form ( metadata_form (metadata, "diff"), "processed" )
  if processed is None:
    return False
  return set (objects) <= { str (element) for element in processed [ 1 : ] }

def is_pending_detachment (metadata, parent, grandparent):
  """Whether a headline with METADATA, under a parent with PARENT
  whose own parent has GRANDPARENT, carries 'noClaim', is warranted by
  the graph (no 'noWarrant' or 'ghostWarrant', no '(rel removed)'),
  and stands under a parent that reads its children's claims: a node
  that is not write-protected, or a folder that is not write-protected
  whose viewparent is such a node. In a forks folder, where a forker
  the viewparent does not fork claims nothing too, it must also be a
  note the viewparent still forks ('is_forked_by_its_folders_viewparent'):
  a detached forkee or mutual ('skg--pending-detachment-p')."""
  view = metadata_form (metadata, "view") or []
  rel = metadata_form ( metadata_form (metadata, "diff"), "rel" )
  if ( not metadata_has_atom (metadata, "noClaim")
       or metadata_has_atom (view, "noWarrant")
       or metadata_has_atom (view, "ghostWarrant")
       or ( rel is not None and len (rel) == 2
            and is_symbol_named ( rel [1], "removed" ))):
    return False
  if ( parent is not None and folder_atom (parent) == "forksFolder"
       and not is_forked_by_its_folders_viewparent (metadata) ):
    return False
  return reads_claims (parent, grandparent)

def is_forked_by_its_folders_viewparent (metadata):
  """Whether METADATA, a member of a folder, names its folder's
  viewparent (ancestor generation 2) among the notes forking it:
  '(view (rels (forks (in N (ancestors ... 2 ...)))))'
  ('skg--forked-by-its-folders-viewparent-p')."""
  form = metadata
  for key in ( "view", "rels", "forks", "in", "ancestors" ):
    form = metadata_form (form, key)
    if form is None:
      return False
  return 2 in form [ 1 : ]

def is_editable_node (metadata):
  """Whether METADATA is an occurrence's that is not write-protected
  ('skg--occurrence-sexp-p', 'skg--node-write-protected-p')."""
  return ( metadata is not None and metadata_has_atom (metadata, "node")
           and not metadata_has_atom (metadata, "writeProtected") )

def reads_claims (parent, grandparent):
  """Whether a parent with the metadata PARENT, whose own parent has
  GRANDPARENT, reads its children's claims: a node that is not
  write-protected, or a folder that is not write-protected whose
  viewparent is such a node ('skg--reads-claims-p')."""
  if parent is not None and folder_atom (parent) is not None:
    return ( folder_atom (parent) not in FOLDER_HERALDS [ "write_protected" ]
             and is_editable_node (grandparent) )
  return is_editable_node (parent)

# ============================================================
# Change facts: a headline's '(diff (leaves ...) (processed ...))', which
# the summary of a buffer of the diff view counts and a processed
# request names ('docs/api-and-formats.org', "Metadata of the diff
# view").
# ============================================================

def leaves_fact (metadata):
  """( N, OBJECTS ) of the headline's '(diff (leaves N OBJECT...))',
  or None when it names no leaf."""
  leaves = metadata_form ( metadata_form (metadata, "diff"), "leaves" )
  if leaves is None:
    return None
  return ( first_number (leaves),
           [ str (element) for element in leaves [ 1 : ]
             if not isinstance (element, int) ] )

def headline_is_processed (metadata):
  """Whether a headline that names leaves is processed: its '(processed ...)'
  names every object its '(leaves ...)' lists, or, listing none, is
  there."""
  processed = metadata_form ( metadata_form (metadata, "diff"), "processed" )
  if processed is None:
    return False
  processed_objects = { str (element) for element in processed [ 1 : ] }
  return set ( leaves_fact (metadata) [1] ) <= processed_objects

# ============================================================
# The summary
# ============================================================

def summary_lines (parsed_view, link_statuses, repo_ownership, view,
                   full_file_name):
  """Org comment lines, each a few 'NAME COUNT' facts of the view,
  after a line naming its perspective when that is not its root
  homes."""
  headlines = [ line . metadata for line in parsed_view
                if line . is_headline and line . metadata is not None ]
  occurrences = [ metadata for metadata in headlines
                  if headline_kind (metadata) == "node" ]
  other_kind_counts = {}
  for metadata in headlines:
    kind = headline_kind (metadata)
    if kind != "node":
      kind = "folders" if is_folder_kind (kind) else kind
      other_kind_counts [kind] = other_kind_counts . get (kind, 0) + 1
  repo_counts = {}
  for metadata in occurrences:
    repo = headline_node_repo (metadata) or "(none)"
    repo_counts [repo] = repo_counts . get (repo, 0) + 1
  def count_where (predicate, among = headlines):
    return sum ( 1 for metadata in among if predicate (metadata) )
  link_count = sum (
    len ( LINK_PATTERN . findall ( visible_text_of_line (line) ))
    for line in parsed_view )
  statuses = [ entry [ "status" ] for entry in link_statuses . values () ]
  resolved_count = statuses . count ("resolved")
  unresolved_detail = ", " . join (
    status + " " + str ( statuses . count (status) )
    for status in sorted ( set (statuses) - { "resolved" } ))
  def facts (*pairs):
    return "# " + ", " . join (
      name + " " + str (count) for name, count in pairs ) + "."
  return ( diff_summary_lines (parsed_view, view)
           if view . command == "diff" else [] ) + [
    "# skg-for-ai compact view of " + full_file_name +
      " (view ID " + view . view_id + ")" ] + (
    [ "# Showing " + ( view . perspective_name or view . perspective )
      + " (each root's home skgrepo is the default)." ]
    if view . perspective != DEFAULT_PERSPECTIVE else [] ) + [
    "# Each headline ends in its handle ⟨ID HERALDS⟩: ID its viewnode ID,",
    "# the '(vnid ID)' of its metadata in the full view, and HERALDS what a",
    "# client shows for it, but a bare 'aC' (only the parent contains it);",
    "# 'folded' marks a headline a client shows collapsed.",
    "# What the heralds mean: \"Start here\" in tools/skg-for-ai/README.org.",
    facts ( ( "Headlines", len (headlines) ),
            ( "node occurrences", len (occurrences) ),
            ( "distinct nodes",
              len ({ headline_gnid (metadata) for metadata in occurrences }) ),
            * sorted ( other_kind_counts . items () )),
    "# Skgrepos: " + ( ", " . join (
      repo + " (" + ownership_word ( repo_ownership . get (repo) ) + ") " +
      str (count)
      for repo, count in sorted ( repo_counts . items () ))
      or "none" ) + ".",
    facts ( ( "Foreign",
              count_where ( lambda metadata : repo_ownership . get (
                headline_node_repo (metadata) ) is False, occurrences )),
            ( "write-protected",
              count_where ( lambda metadata : metadata_has_atom (
                metadata, "writeProtected" ))),
            ( "folded",
              count_where ( lambda metadata : metadata_has_atom (
                metadata, "folded" )))),
    facts ( ( "Contained elsewhere too",
              count_where (contained_by_a_node_outside_its_ancestry) ),
            ( "forked",
              count_where ( lambda metadata :
                side_count (metadata, "forks", "in") > 0 )),
            ( "forking",
              count_where ( lambda metadata :
                side_count (metadata, "forks", "out") > 0 ))),
    facts ( ( "Links", link_count ),
            ( "distinct targets", len (statuses) ),
            ( "resolved", resolved_count ),
            ( "unresolved", len (statuses) - resolved_count )) [ : -1 ] +
      ( " (" + unresolved_detail + ")." if unresolved_detail else "." ),
    facts ( ( "Server errors", len ( view . errors )),
            ( "warnings", len ( view . warnings ))) [ : -1 ] +
      ( " (listed in the .view.json file)."
        if view . errors or view . warnings else "." ) ]

def diff_summary_lines (parsed_view, view):
  """The processed summary the server sent with a buffer of the diff
  view, and the count of its headlines with change facts, processed and
  not."""
  changed_headlines = [ line . metadata for line in parsed_view
                        if line . is_headline and line . metadata is not None
                        and leaves_fact (line . metadata) is not None ]
  processed_count = sum ( 1 for metadata in changed_headlines
                      if headline_is_processed (metadata) )
  summary = view . processed_summary
  return [
    "# Processed: " + ( processed_summary_text (summary) if summary
                        else "the server sent no processed summary." ),
    f"# Headlines with change facts {len (changed_headlines)}, processed "
    f"{processed_count}, unprocessed {len (changed_headlines) - processed_count}." ]

def processed_summary_text (summary):
  """The sentence a client shows for the processed summary SUMMARY,
  '((processed N) (changed M) (foreign F) (order-only O) (waiting W)
  (notEditable K))' ('skg--processed-summary-message' in
  'elisp/skg-processed.el'). It counts nodes, the unit 'commit' commits;
  W of the N processed would wait. "Commit ready" means 'commit' would
  commit every changed node."""
  counts = { str ( entry [0] ) : first_number ( [ None ] + entry [ 1 : ] )
             for entry in summary or [] }
  processed = counts . get ("processed", 0)
  changed = counts . get ("changed", 0)
  waiting = counts . get ("waiting", 0)
  not_editable = counts . get ("notEditable", 0)
  foreign = counts . get ("foreign", 0)
  complete = processed == changed and waiting == 0
  return (
    ( f"Commit ready. All {processed} of {changed} changed nodes are processed"
      if complete else f"{processed} of {changed} changed nodes are processed" ) +
    ( f" ({waiting} of them " + ( "waits)" if waiting == 1 else "wait)" )
      if waiting else "" ) +
    ( f", {not_editable} changes are not editable in this buffer"
      if not_editable else "" ) +
    ( f", {count_phrase (foreign, 'node')} changed in foreign skgrepos"
      if foreign else "" ) +
    "." )

def count_phrase (count, noun):
  """COUNT and NOUN, made plural by an "s" unless COUNT is 1."""
  return f"{count} {noun}" + ( "" if count == 1 else "s" )

def ownership_word (owned):
  return { True : "owned", False : "foreign", None : "ownership unknown" } [owned]

def contained_by_a_node_outside_its_ancestry (metadata):
  """Whether the 'contains' in-side counts a container its ancestor
  flags do not name: one not above this occurrence in the view."""
  side = relation_side (metadata, "contains", "in")
  if side is None:
    return False
  count, generations = side
  return count > len ( distinct_generations (generations) )

def side_count (metadata, relation_name, side_name):
  side = relation_side (metadata, relation_name, side_name)
  return side [0] if side else 0
