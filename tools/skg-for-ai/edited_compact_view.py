"""An edited compact view, read for a save: its headlines matched to
the full view by their handles ('parse_edited_compact_view',
'edited_outline', 'OutlineHeadline'); and the words of each handle,
checked headline by headline ('handle_error_lines'). 'outline_words'
reads the words against the outline and expands 'subtree' into the
clients' prefix-argument forms. 'save' saves what both read."""

import re

from server_connection import SkgToolError
from full_view import (
  HEADLINE_PATTERN, folder_atom, headline_kind, headline_viewnode_id )
from compact_view import NOT_HELD_ANNOTATION, RESTATED_WORDS
from heralds import parent_indices_by_level

# ============================================================
# Saving an edited compact view
# ============================================================

# A compact headline's handle, at the end of the line: its viewnode
# ID, then any display tokens.
COMPACT_HANDLE_PATTERN = re . compile (
  r'^(.*?) ?⟨([0-9a-z]+)((?: [^⟨⟩]*)?)⟩\s*$' )
# The handle tokens that ask for edit requests. The rebuild writes
# them into the headline's '(editRequest ...)': one of these
# ('stripAttribution' the clients' 'skg-strip-attribution', which
# removes a fork's attribution),
# the request of that name, and after it one processed request,
# '(processed OBJECT...)' or '(unprocessed OBJECT...)'.
EDIT_REQUEST_TOKENS = [ "delete", "restoreNode", "restoreRel", "stripAttribution" ]
PROCESSED_TOKENS = [ "processed", "unprocessed" ]
# With 'processed' or 'unprocessed', one of these says what it marks,
# as the clients' 'C-c m n', 'C-c m r' and 'C-c m b' do: each word with
# the objects it names.
PROCESSED_LETTER_TOKENS = { "node" : [ "node" ], "rel" : [ "rel" ],
                            "both" : [ "node", "rel" ] }
# With 'processed' or 'unprocessed': walk the subtree as 'subtree' does,
# but at a new node name the outward walk, as the clients' C-u C-u does.
OUTWARD_TOKEN = "outward"
# The walk each word asks the server for at a new node in content
# position.
WALK_OBJECT_OF_TOKEN = { "subtree" : "newContent", "outward" : "newOutward" }
# The handle words that ask for the edit requests with a parameter,
# each a prefix and its parameter: 'merge:ID', the clients'
# 'skg-set-merge-request' ('C-c s m'), whose node acquires ID by a
# nodeMerge; 'relRepo:NAME', their 'skg-set-relRepo' ('C-c s r'), the
# skgrepo NAME recording the headline's relationship to its parent.
MERGE_WORD_PREFIX = "merge:"
RELREPO_WORD_PREFIX = "relRepo:"
# The handle words that ask for the flag request, the clients'
# 'skg-set-flag-search-matching' ('C-c s x'), each with the flag's
# desired value.
SEARCH_MATCHING_TOKENS = { "noSearchMatching" : "true",
                           "searchMatching"   : "false" }
# The handle tokens that ask for view requests, each with the request
# the rebuild writes into the headline's '(viewRequests ...)':
# 'editable' the clients' 'skg-set-editable' ('C-c s e'), 'fork' their
# 'skg-fork-node' ('C-c m f'), 'write-protect' their
# 'skg-request-write-protected' ('C-c s w'; with 'subtree', 'C-c s W'
# and its request 'writeProtectedSubtreeView').
VIEW_REQUEST_TOKENS = [ ( "editable", "editableView" ), ( "fork", "fork" ),
                        ( "write-protect", "writeProtectedView" ) ]
WRITE_PROTECT_SUBTREE_REQUEST = "writeProtectedSubtreeView"
# The handle words that change whether a headline claims membership of
# its parent's list: 'detach' writes 'noClaim', as the clients'
# 'skg-detach' ('C-c s t') does, 'attach' removes it, as their
# 'skg-attach' ('C-c s a') does.
CLAIM_TOKENS = [ "detach", "attach" ]
# 'repo:NAME' moves the node to the owned skgrepo NAME, as the clients'
# 'skg-set-repo' ('C-c s s') does.
REPO_WORD_PREFIX = "repo:"
# 'subtree' asks for the subtree form of the other words of its handle,
# as a prefix argument (C-u) does in the clients: 'delete', 'processed',
# 'unprocessed', 'write-protect', 'repo:NAME', 'relRepo:NAME',
# 'noSearchMatching' and 'searchMatching'.
SUBTREE_TOKEN = "subtree"
# What a word may name: a skgrepo or an ID, written into metadata bare.
WORD_PARAMETER_PATTERN = re . compile ( r'[^\s()";⟨⟩]+' )
# A headline ending in '⟨node:ID⟩' places the existing node ID there,
# as the clients' 'skg-paste-node' ('C-c o n') does.
PLACED_NODE_HANDLE_WORD = "node"
PLACED_NODE_HANDLE_PATTERN = re . compile (
  r'^(.*?) ?⟨' + PLACED_NODE_HANDLE_WORD + r':([^\s⟨⟩]+)((?: [^⟨⟩]*)?)⟩\s*$' )

# The handle '⟨new⟩' marks a headline without a handle as a new node on
# purpose, whatever removed headline its title equals or resembles.
NEW_NODE_HANDLE_WORD = "new"
# A link followed by the annotation 'annotate_links' appended to it:
# '⟦→ ...⟧', '⟦✗ ...⟧' (which also matches the '⟦✗ missing⟧' views
# written before 'NOT_HELD_ANNOTATION' still hold), or that one.
ANNOTATED_LINK_PATTERN = re . compile (
  r'(\[\[id:[^\]\[]+\](?:\[[^\]]*\])?\])(?:⟦[→✗][^⟧]*⟧|' +
  re . escape (NOT_HELD_ANNOTATION) + r')' )

class OutlineHeadline:
  """A headline of the full view or of the edited compact view.
  'handle' is its viewnode ID (None for a new headline),
  'marked_new' whether a new headline says it is new on purpose, with
  the handle '⟨new⟩',
  'parent' the index of its parent in the same outline (None at the
  top level), 'tokens' the display tokens of its handle, 'body' its
  body lines, link annotations stripped. A headline ending in
  '⟨node:ID⟩' has no handle and 'placed_node_id' ID: it places that
  existing node there. 'resolve_placed_nodes' then sets 'placed_pid'
  and 'placed_title', what the server calls the node, and
  'assign_placed_view_requests' 'placed_requests_editable', whether
  its metadata asks for an editable view."""
  def __init__ (self, handle, level, title, tokens, compact_line_number,
                marked_new = False, placed_node_id = None):
    self . handle = handle
    self . marked_new = marked_new
    self . placed_node_id = placed_node_id
    self . placed_pid = None
    self . placed_title = None
    self . placed_requests_editable = False
    self . level = level
    self . title = title
    self . tokens = tokens
    self . compact_line_number = compact_line_number
    self . body = []
    self . parent = None
    # The words a 'subtree' above gives it, each with the handle of
    # the headline carrying that 'subtree' ('expand_subtree_words').
    self . subtree_words = []
    self . subtree_origin = {}
    # The objects its processed request names, as the processed word
    # that reaches it decides ('resolve_processed_words'); None until
    # then.
    self . processed_objects = None
    # The words of its own handle a 'subtree' on it does not apply to
    # it: its relationship is not of the kind the gesture sets, say.
    self . dropped_words = set ()
    self . merge_title = None # the acquiree's, once resolved

  def words (self):
    """The words of its handle, but those a 'subtree' on it drops, and
    those a 'subtree' gives it."""
    return [ word for word in dict . fromkeys ( self . tokens + self . subtree_words )
             if word not in self . dropped_words ]

  def edit_request_words (self):
    return [ word for word in self . words () if is_edit_request_word (word) ]

  def processed_words (self):
    return [ word for word in PROCESSED_TOKENS if word in self . words () ]

  def processed_letter_words (self):
    return [ word for word in self . tokens if word in PROCESSED_LETTER_TOKENS ]

  def asks_outward (self):
    return OUTWARD_TOKEN in self . tokens

  def view_request_words (self):
    return [ word for word, _ in VIEW_REQUEST_TOKENS if word in self . words () ]

  def claim_words (self):
    return [ word for word in CLAIM_TOKENS if word in self . words () ]

  def repo_words (self):
    return [ word for word in self . words ()
             if word . startswith (REPO_WORD_PREFIX) ]

  def asks_subtree (self):
    return SUBTREE_TOKEN in self . tokens

  def merge_acquiree (self):
    """The ID its 'merge:ID' names, or None."""
    return next ( ( word [ len (MERGE_WORD_PREFIX) : ]
                    for word in self . edit_request_words ()
                    if word . startswith (MERGE_WORD_PREFIX) ), None )

  def has_requests (self):
    return bool ( self . edit_request_words () or self . processed_words ()
                  or self . processed_letter_words () or self . asks_outward ()
                  or self . view_request_words ()
                  or self . claim_words () or self . repo_words ()
                  or self . asks_subtree () )

  def is_placed (self):
    return self . placed_node_id is not None

  def report_title (self):
    """The title the report gives the headline: for a placed node, the
    one the server has, once resolved."""
    return self . placed_title if self . placed_title is not None else self . title

def edited_outline (saved_view):
  """The edited compact view of SAVED_VIEW, parsed, each body the
  compact view left out because it restates its note's ('restatements')
  read back from the full view: a headline whose handle still carries
  'body=b' and that has no body there gets the body the full view gave
  it. Text written under it is its body instead; to empty the body,
  take the word away."""
  edited = parse_edited_compact_view (saved_view . compact_text)
  original_body = { headline . handle : headline . body
                    for headline in outline_of_full_view (saved_view . parsed_view) }
  for headline in edited:
    if ( RESTATED_WORDS [ "body" ] in headline . tokens
         and headline . handle in original_body
         and not any ( line . strip () for line in headline . body )):
      headline . body = list ( original_body [ headline . handle ] )
  return edited

def parse_edited_compact_view (compact_text):
  """The edited compact view's headlines, in order. The summary's
  comment lines before the first headline are dropped."""
  lines = compact_text . split ("\n")
  if lines and lines [-1] == "":
    lines = lines [ : -1 ]
  headlines = []
  for index, text in enumerate (lines):
    match = HEADLINE_PATTERN . match (text)
    if match:
      headlines . append ( edited_headline (
        index + 1, len ( match . group (1) ), match . group (2) ))
    elif headlines:
      headlines [-1] . body . append ( strip_link_annotations (text) )
    elif text . strip () and not text . startswith ("#"):
      raise SkgToolError (
        f"Line {index + 1} of the compact view is text before its first "
        f"headline: {text!r}" )
  assign_parents (headlines)
  return headlines

# The heralds whose words include a handle word, which a handle's words
# leave out: the labels of the hider and hidden folders hold 'forks',
# a word for the fork operation, which no folder takes.
HERALD_PHRASES_HOLDING_HANDLE_WORDS = (
  "It hides these from its forks.",
  "These hide it from their forks.",
  "hides these, but no fork partner shown contains them." )

def handle_words (display_text):
  """The words of a handle's display text: its tokens, the heralds of
  'HERALD_PHRASES_HOLDING_HANDLE_WORDS' left out."""
  for phrase in HERALD_PHRASES_HOLDING_HANDLE_WORDS:
    display_text = display_text . replace (phrase, " ")
  return display_text . split ()

def edited_headline (compact_line_number, level, text_after_stars):
  placed = PLACED_NODE_HANDLE_PATTERN . match (text_after_stars)
  if placed:
    return OutlineHeadline (
      None, level, strip_link_annotations ( placed . group (1) ),
      handle_words ( placed . group (3) ), compact_line_number,
      placed_node_id = placed . group (2) )
  match = COMPACT_HANDLE_PATTERN . match (text_after_stars)
  if not match:
    return OutlineHeadline (
      None, level, strip_link_annotations (text_after_stars) . rstrip (),
      [], compact_line_number )
  marked_new = match . group (2) == NEW_NODE_HANDLE_WORD
  return OutlineHeadline (
    None if marked_new else match . group (2), level,
    strip_link_annotations ( match . group (1) ),
    handle_words ( match . group (3) ), compact_line_number, marked_new )

def strip_link_annotations (text):
  return ANNOTATED_LINK_PATTERN . sub ( r'\1', text )

def outline_of_full_view (parsed_view):
  headlines = []
  for view_line in parsed_view:
    if view_line . is_headline:
      headlines . append ( OutlineHeadline (
        headline_viewnode_id ( view_line . metadata ),
        len (view_line . stars), view_line . title, [], None ))
    elif headlines:
      headlines [-1] . body . append (view_line . text)
  assign_parents (headlines)
  return headlines

def assign_parents (headlines):
  for headline, parent in zip (
      headlines, parent_indices_by_level (
        [ headline . level for headline in headlines ] )):
    headline . parent = parent

def handle_error_lines (edited, headline_of_handle):
  """A line per handle that names no headline of the full view, and
  per handle whose request tokens make no one '(editRequest ...)',
  and per '⟨new⟩' or '⟨node:ID⟩' carrying another word, and per
  placed node with a body or children. HEADLINE_OF_HANDLE maps each
  viewnode ID of the full view to its headline. Whether the server
  accepts a request is the server's to say."""
  errors = []
  for index, headline in enumerate (edited):
    # '⟨new⟩' takes one 'repo:NAME', its home skgrepo; '⟨node:ID⟩' no word.
    other_words = (
      headline . tokens [ 1 : ] if ( headline . marked_new
                                    and headline . repo_words () == headline . tokens [ : 1 ] )
      else headline . tokens )
    if ( headline . marked_new or headline . is_placed () ) and other_words:
      errors . append (
        "Error: " + compact_handle_text (headline) + " on line "
        f"{headline . compact_line_number} of the compact view carries "
        + ", " . join ( "'" + token + "'" for token in other_words )
        + "; it takes no other word" +
        ( " but one 'repo:NAME'" if headline . marked_new else "" ) + "." )
    elif headline . marked_new and headline . repo_words () and not (
        WORD_PARAMETER_PATTERN . fullmatch (
          word_parameter ( headline . repo_words () [0] ))):
      errors . append (
        "Error: " + compact_handle_text (headline) + " on line "
        f"{headline . compact_line_number} of the compact view carries "
        f"'{headline . repo_words () [0]}', which names nothing." )
    if headline . is_placed ():
      errors += placed_node_shape_error_lines (edited, index)
    if headline . handle is None:
      continue
    where = ( f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
              "of the compact view" )
    view_line = headline_of_handle . get ( headline . handle )
    if view_line is None:
      errors . append (
        f"Error: {where} names no headline of the full view." )
    elif headline . has_requests ():
      errors += [ f"Error: {where} {problem}"
                  for problem in request_token_problems (headline, view_line) ]
  return errors

# A handle's brackets. A title may not gain them: the compact view
# would read what follows as a handle (or a handle as part of the
# title), so the save would not be what was typed.
HANDLE_BRACKETS = ( "⟨", "⟩" )

def handle_bracket_title_error_lines (edited, headline_of_handle):
  """An error per new headline whose title holds a handle's bracket,
  and per headline whose title is edited to hold one. A title the full
  view already has is left as it is; a placed node takes its title
  from the server. (A title's '⟦' and '⟧' are free: only the link
  annotations write them, quoting a title's as look-alikes.)"""
  errors = []
  for headline in edited:
    if headline . is_placed ():
      continue
    brackets = [ bracket for bracket in HANDLE_BRACKETS
                 if bracket in headline . title ]
    if not brackets:
      continue
    view_line = ( None if headline . handle is None
                  else headline_of_handle . get ( headline . handle ) )
    if view_line is not None and view_line . title == headline . title:
      continue
    where = (
      f"the new headline on line {headline . compact_line_number} "
      "of the compact view has the title" if view_line is None
      else f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
           "of the compact view has its title edited to" )
    errors . append (
      f"Error: {where} '{headline . title}': " + quoted_words (brackets)
      + ( " are the brackets" if len (brackets) > 1 else " is a bracket" )
      + " of a handle, and the compact view can misread a title holding "
      "one. Write the title without '⟨' and '⟩' "
      "('〈' and '〉', or '<' and '>', look alike)." )
  return errors

def compact_handle_text (headline):
  """The handle a headline without a viewnode ID was written with:
  '⟨new⟩' or '⟨node:ID⟩'."""
  if headline . is_placed ():
    return "⟨" + PLACED_NODE_HANDLE_WORD + ":" + headline . placed_node_id + "⟩"
  return "⟨" + NEW_NODE_HANDLE_WORD + "⟩"

def placed_node_shape_error_lines (edited, index):
  """Errors for the placed node at INDEX of the edited outline that has
  body lines or children. The headline is saved write-protected, as the
  clients' paste writes it, and a write-protected occurrence saves
  nothing of its own: its body would not be written, and no child of
  it claims membership, so a new child would be an orphan graphnode
  and a moved one would leave its parent for nowhere. Both wait for
  the save that places it, after which it is editable."""
  headline = edited [index]
  where = ( compact_handle_text (headline) + " on line " +
            f"{headline . compact_line_number} of the compact view" )
  errors = []
  if any ( line . strip () for line in headline . body ):
    errors . append (
      f"Error: {where} has a body; a placed node is saved write-protected, "
      "which saves no body. Save the placement first, then edit its body." )
  if any ( other . parent == index for other in edited ):
    errors . append (
      f"Error: {where} has children; a placed node is saved write-protected, "
      "under which nothing claims membership. Save the placement first, "
      "then add them." )
  return errors

def is_edit_request_word (word):
  """Whether WORD asks for an edit request: 'delete', 'restoreNode',
  'restoreRel', 'stripAttribution', 'merge:ID', 'relRepo:NAME', 'noSearchMatching',
  'searchMatching'."""
  return ( word in EDIT_REQUEST_TOKENS or word in SEARCH_MATCHING_TOKENS
           or word . startswith (MERGE_WORD_PREFIX)
           or word . startswith (RELREPO_WORD_PREFIX) )

def edit_request_part_of_word (word):
  """The part of '(editRequest ...)' an edit-request WORD asks for."""
  if word in SEARCH_MATCHING_TOKENS:
    return "(flag noSearchMatching " + SEARCH_MATCHING_TOKENS [word] + ")"
  if word . startswith (MERGE_WORD_PREFIX):
    return "(merge " + word [ len (MERGE_WORD_PREFIX) : ] + ")"
  if word . startswith (RELREPO_WORD_PREFIX):
    return "(relRepo " + word [ len (RELREPO_WORD_PREFIX) : ] + ")"
  return word

def word_parameter (word):
  """The parameter of a word 'PREFIX:PARAMETER' ('repo:', 'relRepo:',
  'merge:')."""
  return word . split (":", 1) [1]

def quoted_words (words):
  return " and " . join ( "'" + word + "'" for word in words )

# The kinds that may carry each kind of word: what the clients'
# commands accept ('shared/metadata-fields.json' says which kinds take
# 'noClaim').
RELREPO_KINDS = ( "node", "unknown", "restricted", "alias" )
CLAIM_KINDS = ( "node", "ghost", "unknown", "restricted", "alias", "id" )
# The writable partner folders, each with the kind of relationship
# 'relRepo:NAME subtree' sets from it ('skg--writable-folder-relations'
# in 'elisp/skg-request-relRepo-info.el'). The folder's viewparent records the
# relationship to each member that claims; a member of the forks folder
# claiming nothing either forks the viewparent, recording that in its
# own file, or is a mirror of it only through others.
WRITABLE_FOLDER_RELATIONSHIP_KINDS = { "forksFolder" : "forkee" }

def request_token_problems (headline, view_line):
  """Why HEADLINE's request tokens make no one '(editRequest ...)'
  for VIEW_LINE, the headline its handle names, or ask for what its
  kind does not take."""
  problems = []
  if view_line . metadata is None:
    problems . append ( "carries a request, but the headline has no metadata." )
    return problems
  kind = headline_kind (view_line . metadata)
  if len ( headline . edit_request_words () ) > 1:
    problems . append (
      f"carries {quoted_words ( headline . edit_request_words () )}"
      + subtree_origin_text (headline, headline . edit_request_words ())
      + "; a headline takes one edit request." )
  if len ( headline . processed_words () ) > 1:
    problems . append ( "carries both 'processed' and 'unprocessed'"
                        + subtree_origin_text (headline, PROCESSED_TOKENS) + "." )
  own_processed_words = [ word for word in headline . tokens if word in PROCESSED_TOKENS ]
  marking_words = headline . processed_letter_words () + (
    [ OUTWARD_TOKEN ] if headline . asks_outward () else [] )
  if own_processed_words and len ( headline . processed_letter_words () ) != 1:
    problems . append (
      f"carries '{own_processed_words [0]}', which takes one of 'node', 'rel' "
      "and 'both', saying what it marks"
      + ( f", but carries {quoted_words ( headline . processed_letter_words () )}."
          if headline . processed_letter_words () else "." ))
  if marking_words and not own_processed_words:
    problems . append (
      f"carries {quoted_words (marking_words)} without 'processed' or 'unprocessed'." )
  only_on_a_node = (
    headline . view_request_words () + headline . repo_words () +
    [ word for word in headline . edit_request_words ()
      if word . startswith (MERGE_WORD_PREFIX) or word in SEARCH_MATCHING_TOKENS
      or word == "stripAttribution" ] )
  if only_on_a_node and kind != "node":
    problems . append (
      f"carries {quoted_words (only_on_a_node)}, but is not a node "
      "occurrence; only a node takes it." )
  relRepo_words = [ word for word in headline . edit_request_words ()
                    if word . startswith (RELREPO_WORD_PREFIX) ]
  if relRepo_words and kind not in RELREPO_KINDS and not (
      # 'relRepo:NAME subtree' on a writable folder sets its members'
      headline . asks_subtree () and folder_atom (view_line . metadata)
      in WRITABLE_FOLDER_RELATIONSHIP_KINDS ):
    problems . append (
      f"carries {quoted_words (relRepo_words)}, but is no node, alias, "
      "unknown viewnode or restricted viewnode; only those stand for a "
      "relationship whose skgrepo can be set." )
  if headline . claim_words () and kind not in CLAIM_KINDS:
    problems . append (
      f"carries {quoted_words ( headline . claim_words () )}, but a headline of "
      f"this kind ({kind}) claims no membership." )
  if len ( headline . claim_words () ) > 1:
    problems . append ( "carries both 'detach' and 'attach'." )
  if len ( headline . repo_words () ) > 1:
    problems . append ( f"carries {quoted_words ( headline . repo_words () )}; "
                        "a node has one home skgrepo." )
  if ( "write-protect" in headline . view_request_words ()
       and "editable" in headline . view_request_words () ):
    problems . append ( "carries both 'write-protect' and 'editable'." )
  for word in ( headline . edit_request_words () + headline . repo_words () ):
    if ( ":" in word and
         not WORD_PARAMETER_PATTERN . fullmatch ( word_parameter (word) )):
      problems . append ( f"carries '{word}', which names nothing." )
  if headline . asks_subtree () and not words_with_a_subtree_form (headline):
    problems . append (
      "carries 'subtree' with no word it applies to; 'subtree' applies to "
      "'delete', 'processed', 'unprocessed', 'write-protect', 'repo:NAME', "
      "'relRepo:NAME', 'noSearchMatching' and 'searchMatching'." )
  return problems

def subtree_origin_text (headline, words):
  """' (WORD from 'subtree' on ⟨ID⟩, ...)' for those of WORDS a
  'subtree' above gave HEADLINE; empty when none."""
  origins = [ f"'{word}' from 'subtree' on ⟨{headline . subtree_origin [word]}⟩"
              for word in words if word in headline . subtree_origin
              and word not in headline . tokens ]
  return " (" + ", " . join (origins) + ")" if origins else ""

def words_with_a_subtree_form (headline):
  """The words of HEADLINE's own handle that 'subtree' applies to."""
  return [ word for word in headline . tokens
           if word in ( "delete", "processed", "unprocessed", "write-protect" )
           or word in SEARCH_MATCHING_TOKENS
           or word . startswith (REPO_WORD_PREFIX)
           or word . startswith (RELREPO_WORD_PREFIX) ]
