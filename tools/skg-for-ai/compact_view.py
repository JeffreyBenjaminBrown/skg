"""The compact view ('compact_tree_lines'): each headline's metadata
replaced by a handle of its heralds, each link followed by its
target's title, and the summary above the tree ('summary_lines').
Also repo ownership. The heralds themselves are in 'heralds'."""

import re

from sexp import metadata_form, quote_string, sexp_to_text
from server_connection import (
  DEFAULT_PERSPECTIVE, SkgToolError, request_line,
  require_response_type, response_field, response_type )
from full_view import (
  first_number, headline_gnid, headline_kind, headline_node_repo,
  headline_viewnode_id, is_folder_kind, metadata_has_atom, relation_side )
from heralds import (
  BARE_PARENT_CONTAINS_TOKEN, distinct_generations, headline_is_processed,
  herald_contexts, herald_text, leaves_fact, parent_indices_by_level )

LINK_PATTERN = re . compile (
  r'\[\[id:([^\]\[]+)\](?:\[[^\]]*\])?\]')

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
