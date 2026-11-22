"""An edited compact view, read for a save: its headlines matched to
the full view by their handles ('parse_edited_compact_view',
'edited_outline', 'OutlineHeadline'); the words of each handle, checked
against the outline ('handle_error_lines', 'outline_word_check'); and
'subtree', expanded into the clients' prefix-argument forms. 'save'
saves what this reads."""

import re

from sexp import Symbol, metadata_form, sexp_to_text
from server_connection import SkgToolError
from full_view import (
  HEADLINE_PATTERN, folder_atom, headline_gnid, headline_kind,
  headline_node_repo, headline_viewnode_id, metadata_has_atom )
from compact_view import NOT_HELD_ANNOTATION, RESTATED_WORDS
from heralds import (
  is_editable_node, is_symbol_named, leaves_fact,
  membership_change_processed, parent_indices_by_level, reads_claims )

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

# ------------------------------------------------------------
# Words read against the outline: what a 'subtree' gives the
# headlines below it, and what 'detach', 'attach', 'repo:NAME',
# 'relRepo:NAME' and the flag words need of where they stand
# ------------------------------------------------------------

# The writable partner folders, each with the kind of relationship
# 'relRepo:NAME subtree' sets from it ('skg--writable-folder-relations'
# in 'elisp/skg-request-relRepo-info.el'). The folder's viewparent records the
# relationship to each member that claims; a member of the forks folder
# claiming nothing either forks the viewparent, recording that in its
# own file, or is a mirror of it only through others.
WRITABLE_FOLDER_RELATIONSHIP_KINDS = { "forksFolder" : "forkee" }
# The partner folders whose members' relationships belong to the other
# end, so that no relRepo can be set from there
# ('skg--relRepo-unsupported-folder-atoms').
RELREPO_UNSUPPORTED_FOLDERS = (
  "hiderFolder", "hiddenFolder",
  "containerFolder", "mentionerFolder", "acquiredByFolder",
  "acquiredFolder", "illegalEditFolder", "integratedElsewhereFolder",
  "hiddenInForkPartnerFolder", "hiddenByForkerFolder",
  "hiddenOutsideOfForkPartnersFolder" )

class OutlineWordCheck:
  """What reading the handles' words against the outline found:
  'error_lines', which stop the save, and 'note_lines', what a
  'subtree' leaves out and why, which the report prints with the
  changes."""
  def __init__ (self):
    self . error_lines = []
    self . note_lines = []

def outline_word_check (edited, headline_of_handle, ownership):
  """Give each headline the words a 'subtree' above it gives it
  ('expand_subtree_words'); then check what the words ask of where
  they stand: a headline given words must still make one edit
  request; 'detach' and 'attach' need a parent that reads claims;
  'repo:NAME' an owned skgrepo and an editable node; 'relRepo:NAME' an
  owned skgrepo and a relationship the save writes; a flag word an
  editable node of an owned skgrepo. OWNERSHIP is {SKGREPO: OWNED}."""
  check = OutlineWordCheck ()
  expand_subtree_words (edited, headline_of_handle, ownership, check)
  for index, headline in enumerate (edited):
    if headline . marked_new and headline . repo_words ():
      skgrepo = word_parameter ( headline . repo_words () [0] )
      if ownership . get (skgrepo) is not True:
        check . error_lines . append (
          f"Error: ⟨new⟩ on line {headline . compact_line_number} of the "
          f"compact view carries '{headline . repo_words () [0]}', but "
          f"'{skgrepo}' is no owned skgrepo of this server; its owned ones "
          f"are: {owned_skgrepos_text (ownership)}." )
    if headline . handle is None or headline . handle not in headline_of_handle:
      continue
    view_line = headline_of_handle [ headline . handle ]
    if view_line . metadata is None:
      continue
    where = ( f"⟨{headline . handle}⟩ on line {headline . compact_line_number} "
              "of the compact view" )
    problems = (
      ( request_token_problems (headline, view_line)
        if headline . subtree_words else [] ) +
      claim_problems (edited, index, headline_of_handle) +
      repo_problems (headline, view_line, ownership) +
      relRepo_problems (edited, index, headline_of_handle, ownership) +
      search_matching_problems (headline, view_line, ownership) )
    check . error_lines += [ f"Error: {where} {problem}" for problem in problems ]
  return check

def outline_metadata (edited, index, headline_of_handle):
  """The metadata the headline at INDEX of the edited outline is read
  with by the save, as far as its kind and editability go: its full
  view headline's; a placed node's, a write-protected occurrence; a
  new headline's, an editable node. None for no headline, or a handle
  the full view lacks."""
  if index is None:
    return None
  headline = edited [index]
  if headline . is_placed ():
    return [ Symbol ("skg"), Symbol ("node"), Symbol ("writeProtected") ]
  if headline . handle is None:
    return [ Symbol ("skg"), Symbol ("node") ]
  view_line = headline_of_handle . get ( headline . handle )
  return None if view_line is None else view_line . metadata

def handled_metadata (edited, index, headline_of_handle):
  """The full view's metadata of the headline at INDEX, which a word can
  be written into; None for a new or placed headline."""
  if edited [index] . handle is None:
    return None
  return outline_metadata (edited, index, headline_of_handle)

def parent_reads_claims (edited, index, headline_of_handle):
  parent = edited [index] . parent
  return reads_claims (
    outline_metadata (edited, parent, headline_of_handle),
    None if parent is None else outline_metadata (
      edited, edited [parent] . parent, headline_of_handle ))

def claim_problems (edited, index, headline_of_handle):
  """Why 'detach' or 'attach' cannot stand on the headline at INDEX:
  its parent reads no claims, or it already claims (or does not) what
  the word asks."""
  headline = edited [index]
  if not headline . claim_words ():
    return []
  word = headline . claim_words () [0]
  if not parent_reads_claims (edited, index, headline_of_handle):
    return [ f"carries '{word}', but its parent reads no claims (it is "
             "write-protected, or no editable node), so no child claims "
             "membership of it." ]
  claims = not metadata_has_atom (
    headline_of_handle [ headline . handle ] . metadata, "noClaim" )
  if word == "detach" and not claims:
    return [ "carries 'detach', but claims nothing already." ]
  if word == "attach" and claims:
    return [ "carries 'attach', but claims membership already." ]
  return []

def owned_skgrepos_text (ownership):
  return ", " . join ( name for name, owned in ownership . items () if owned ) or "none"

def repo_problems (headline, view_line, ownership):
  """Why the 'repo:NAME' of HEADLINE's own handle cannot move its node:
  NAME is no owned skgrepo, the occurrence is write-protected (the
  save would refuse its edit, as 'skg-set-repo' warns), or the node is
  there already. One a 'subtree' left out of the move is no error."""
  words = [ word for word in headline . repo_words () if word in headline . tokens ]
  if not words:
    return []
  skgrepo = word_parameter ( words [0] )
  if ownership . get (skgrepo) is not True:
    return [ f"carries '{words [0]}', but '{skgrepo}' is no owned skgrepo of "
             f"this server; its owned ones are: {owned_skgrepos_text (ownership)}." ]
  if metadata_has_atom (view_line . metadata, "writeProtected"):
    return [ f"carries '{words [0]}', but is write-protected here, so the save "
             "would refuse its edit; write it on the node's editable "
             "occurrence." ]
  if headline_node_repo (view_line . metadata) == skgrepo:
    return [ f"carries '{words [0]}', but its home skgrepo is '{skgrepo}' already." ]
  return []

def relRepo_problems (edited, index, headline_of_handle, ownership):
  """Why the 'relRepo:NAME' of the own handle of the headline at INDEX
  can set no relationship's skgrepo, as 'skg--rel-at-point' refuses:
  NAME is no owned skgrepo; the headline is a root; its parent is a
  folder whose relationships belong to the other end, a writable folder
  under a write-protected viewparent, a write-protected node, or
  neither a node nor a writable folder; or it is a member of the forks
  folder claiming nothing, which forks the folder's viewparent and
  records that in its own file. An alias sets its own relationship to
  its node."""
  headline = edited [index]
  words = [ word for word in headline . edit_request_words ()
            if word . startswith (RELREPO_WORD_PREFIX) and word in headline . tokens ]
  if not words:
    return []
  skgrepo = word_parameter ( words [0] )
  if ownership . get (skgrepo) is not True:
    return [ f"carries '{words [0]}', but '{skgrepo}' is no owned skgrepo of "
             f"this server; its owned ones are: {owned_skgrepos_text (ownership)}." ]
  if headline_kind ( headline_of_handle [ headline . handle ] . metadata ) == "alias":
    return []
  parent = headline . parent
  parent_metadata = outline_metadata (edited, parent, headline_of_handle)
  if parent_metadata is None:
    return [ f"carries '{words [0]}', but is a root: there is no relationship "
             "here to set." ]
  folder = folder_atom (parent_metadata)
  if folder in RELREPO_UNSUPPORTED_FOLDERS:
    return [ f"carries '{words [0]}', but stands in a {folder}, whose "
             "relationships belong to the other end." ]
  if folder in WRITABLE_FOLDER_RELATIONSHIP_KINDS:
    if metadata_has_atom (
        headline_of_handle [ headline . handle ] . metadata, "noClaim" ):
      return [ f"carries '{words [0]}', but this member of the {folder} "
               "claims nothing: either it forks the view-parent and "
               "records that in its own file, or it is a mirror of the "
               "view-parent only through others; attach it first to make "
               "the view-parent fork it." ]
    if not is_editable_node ( outline_metadata (
        edited, edited [parent] . parent, headline_of_handle )):
      return [ f"carries '{words [0]}', but the folder's viewparent is "
               "write-protected here, so the save would not write this "
               "relationship." ]
    return []
  if headline_kind (parent_metadata) != "node":
    return [ f"carries '{words [0]}', but its parent is neither a node nor a "
             "writable folder." ]
  if not is_editable_node (parent_metadata):
    return [ f"carries '{words [0]}', but its parent is write-protected here, "
             "so the save would not write this relationship." ]
  return []

def search_matching_problems (headline, view_line, ownership):
  """Why a flag word of HEADLINE's own handle cannot stand there, as
  'skg-set-flag-search-matching' refuses: a write-protected
  occurrence, or a node of a foreign skgrepo."""
  words = [ word for word in headline . edit_request_words ()
            if word in SEARCH_MATCHING_TOKENS and word in headline . tokens ]
  if not words:
    return []
  if metadata_has_atom (view_line . metadata, "writeProtected"):
    return [ f"carries '{words [0]}', but is write-protected here; write it on "
             "the node's editable occurrence." ]
  if ownership . get ( headline_node_repo (view_line . metadata) ) is not True:
    return [ f"carries '{words [0]}', but its node is foreign; search "
             "matching is set on nodes of owned skgrepos only." ]
  return []

# ------------------------------------------------------------
# 'subtree': the clients' prefix-argument forms
# ------------------------------------------------------------

def expand_subtree_words (edited, headline_of_handle, ownership, check):
  """For each headline whose handle carries 'subtree', give the
  headlines below it the words it applies to, as the clients'
  commands with a prefix argument mark them ('give_subtree_word'),
  dropping from the headline itself those that do not apply to it.
  What is left out, and why, goes to CHECK's notes, and a word
  'subtree' cannot apply from where it stands to its errors."""
  for index, root in enumerate (edited):
    if ( root . handle is None
         or root . handle not in headline_of_handle
         or headline_of_handle [ root . handle ] . metadata is None ):
      continue
    resolve_processed_words (edited, index, headline_of_handle, check)
    if not root . asks_subtree ():
      continue
    for word in words_with_a_subtree_form (root):
      if word in PROCESSED_TOKENS:
        pass # 'resolve_processed_words'
      elif word == "delete":
        expand_delete_subtree (edited, index, headline_of_handle)
      elif word . startswith (REPO_WORD_PREFIX):
        expand_repo_subtree (edited, index, word, headline_of_handle, check)
      elif word . startswith (RELREPO_WORD_PREFIX):
        expand_relRepo_subtree (edited, index, word, headline_of_handle, check)
      elif word in SEARCH_MATCHING_TOKENS:
        expand_search_matching_subtree (
          edited, index, word, headline_of_handle, ownership, check )
      # 'write-protect' asks the server for its subtree form.

def give_subtree_word (edited, target, word, root):
  """Give the headline at TARGET the WORD the 'subtree' on the headline
  at ROOT applies to it."""
  headline = edited [target]
  if word in headline . tokens or word in headline . subtree_words:
    return
  headline . subtree_words . append (word)
  headline . subtree_origin [word] = edited [root] . handle

def walk_below (outline, index, visit):
  """Call VISIT (LATER) on each headline below the one at INDEX, in
  order, skipping those below a headline for which it returns False."""
  pruned_level = None
  for later in descendant_indices (outline, index):
    if pruned_level is not None and outline [later] . level > pruned_level:
      continue
    pruned_level = None
    if not visit (later):
      pruned_level = outline [later] . level

def expand_delete_subtree (edited, root, headline_of_handle):
  """'delete subtree', the clients' 'skg-delete-recursive' ('C-u C-c
  <backspace>'): every node occurrence below is deleted too; the other
  headlines (folders, ghosts, unknown viewnodes) are skipped."""
  for later in descendant_indices (edited, root):
    metadata = handled_metadata (edited, later, headline_of_handle)
    if metadata is not None and headline_kind (metadata) == "node":
      give_subtree_word (edited, later, "delete", root)

def home_skgrepo_move (metadata):
  """( OLD, NEW ) of a home skgrepo move viewnode's
  '(homeSkgrepoMove (old OLD) (new NEW))', or None."""
  move = metadata_form (metadata, "homeSkgrepoMove")
  if move is None:
    return None
  return ( sexp_to_text ( ( metadata_form (move, "old") or [ None, "" ] ) [1] ),
           sexp_to_text ( ( metadata_form (move, "new") or [ None, "" ] ) [1] ))

def resolve_processed_words (edited, root, headline_of_handle, check):
  """Decide what the 'processed' or 'unprocessed' of the handle at ROOT
  marks, as the clients' 'C-c m' gestures do: which headlines get a
  processed request, and the objects each names ('processed_objects').
  The letter word ('node', 'rel', 'both') names objects; 'subtree'
  ('C-u') and 'outward' ('C-u C-u') walk the subtree
  ('processed_requests_of_gesture'). On a home skgrepo move, 'subtree'
  or 'outward' is the clients' 'C-u C-c m h': every move between the
  same two skgrepos in the moved node's subtree. A gesture that names
  nothing is an error in CHECK, as the clients refuse it."""
  headline = edited [root]
  words = [ word for word in headline . tokens if word in PROCESSED_TOKENS ]
  letters = headline . processed_letter_words ()
  if len (words) != 1 or len (letters) != 1:
    return # 'request_token_problems' reports it
  word = words [0]
  letter_objects = PROCESSED_LETTER_TOKENS [ letters [0] ]
  walk_object = ( WALK_OBJECT_OF_TOKEN [OUTWARD_TOKEN] if headline . asks_outward ()
                  else WALK_OBJECT_OF_TOKEN [SUBTREE_TOKEN] if headline . asks_subtree ()
                  else None )
  root_metadata = headline_of_handle [ headline . handle ] . metadata
  move = home_skgrepo_move (root_metadata)
  if move is not None and walk_object is not None:
    requests = []
    if "node" in letter_objects:
      moved_node = headline . parent
      for target in ( [ root ] if moved_node is None else
                      [ moved_node ] + descendant_indices (edited, moved_node) ):
        metadata = handled_metadata (edited, target, headline_of_handle)
        if metadata is not None and home_skgrepo_move (metadata) == move:
          requests . append ( ( target, [] ) )
  else:
    requests = processed_requests_of_gesture (
      edited, root, letter_objects, walk_object, headline_of_handle )
  if not requests:
    check . error_lines . append (
      f"Error: ⟨{headline . handle}⟩ on line {headline . compact_line_number} "
      f"of the compact view carries '{word} {letters [0]}', but "
      + nothing_to_mark_text (letter_objects) + "." )
    return
  if root not in [ target for target, _ in requests ]:
    headline . dropped_words . add (word)
  for target, objects in requests:
    if target != root:
      give_subtree_word (edited, target, word, root)
      if any ( own in edited [target] . tokens for own in PROCESSED_TOKENS ):
        continue # its own handle's word decides its objects
    edited [target] . processed_objects = objects

def nothing_to_mark_text (letter_objects):
  """What a gesture naming LETTER_OBJECTS says when it names nothing."""
  if letter_objects == [ "node" ]:
    return "its node has no change to mark here"
  if letter_objects == [ "rel" ]:
    return "its relationship to its viewparent has no change to mark here"
  return "neither its node nor its relationship has a change to mark here"

def processed_requests_of_gesture (edited, root, letter_objects, walk_object,
                                   headline_of_handle):
  """( INDEX, OBJECTS ) for each headline the clients' gesture with
  LETTER_OBJECTS on the headline at ROOT writes a processed request on
  ('skg--processed-requests-at-point' and
  'skg--processed-requests-for-subtree' in 'elisp/skg-processed.el').
  Without WALK_OBJECT: the headline, naming what
  'processed_objects_named_by_letters' says, or on a changed folder
  each child that names some of them. With it, 'newContent' or
  'newOutward': every headline in the subtree that names some of them,
  but at a new node in content position the letters and WALK_OBJECT,
  which the server expands from the graph, whatever the buffer shows;
  the rest of that node's subtree is skipped."""
  metadata_of = lambda index: handled_metadata (edited, index, headline_of_handle)
  root_metadata = metadata_of (root)
  if walk_object is None:
    named = processed_objects_named_by_letters (root_metadata, letter_objects, True)
    if named is not None:
      return [ ( root, named ) ]
    if leaves_fact (root_metadata) is not None or not folder_is_changed (
        edited, root, headline_of_handle ):
      return []
    children = [ index for index in descendant_indices (edited, root)
                 if edited [index] . parent == root ]
    requests = []
    for child in children:
      metadata = metadata_of (child)
      named = ( None if metadata is None else
                processed_objects_named_by_letters (metadata, letter_objects) )
      if named is not None:
        requests . append ( ( child, named ) )
    return requests
  requests = []
  def visit (index):
    metadata = metadata_of (index)
    if metadata is None:
      return True
    at_root = index == root
    parent = edited [index] . parent
    parent_metadata = None if parent is None else metadata_of (parent)
    handed_over = node_added (metadata) and (
      at_root or parent_metadata is None or folder_atom (parent_metadata) is None )
    named = ( letter_objects + [ walk_object ] if handed_over else
              processed_objects_named_by_letters (metadata, letter_objects, at_root) )
    if named is not None:
      requests . append ( ( index, named ) )
    return not handed_over
  if visit (root):
    walk_below (edited, root, visit)
  return requests

def processed_objects_named_by_letters (metadata, letter_objects, at_gesture = False):
  """What a gesture naming LETTER_OBJECTS names on a headline with
  METADATA: None when nothing, else the request's objects, [] for a
  bare request ('skg--processed-objects-named-by-letters'). On a
  headline that lists objects, the letters among them. On one that
  names a single leaf, the bare request when the leaf is of a kind a
  letter names: a relRepo move's and an unknown or restricted
  viewnode's are a relationship's, any other the node's own. On a
  changeset marker AT_GESTURE, the letters themselves:
  its 'node' names its members' own leaves, which its '(leaves ...)'
  does not list."""
  leaves = leaves_fact (metadata)
  if leaves is None:
    return None
  if not leaves [1]:
    kind = ( "rel" if ( metadata_form (metadata, "relRepoMove") is not None
                        or metadata_has_atom (metadata, "unknown")
                        or metadata_has_atom (metadata, "restricted") )
             else "node" )
    return [] if kind in letter_objects else None
  if at_gesture and metadata_form (metadata, "changesetMarker") is not None:
    return list (letter_objects)
  objects = [ obj for obj in letter_objects if obj in leaves [1] ]
  return objects or None

def node_added (metadata):
  """Whether METADATA's headline is a new node: '(diff (node added))'."""
  node = metadata_form ( metadata_form (metadata, "diff"), "node" )
  return node is not None and len (node) == 2 and is_symbol_named ( node [1], "added" )

def folder_is_changed (edited, folder, headline_of_handle):
  """Whether the folder headline at FOLDER is shown because a member's
  relationship to its viewparent changed ('folder_change_processed')."""
  if folder_atom ( handled_metadata (edited, folder, headline_of_handle) or [ None ] ) is None:
    return False
  members = [] # each ( METADATA, [CHILD METADATA] )
  for index in descendant_indices (edited, folder):
    metadata = handled_metadata (edited, index, headline_of_handle)
    if edited [index] . parent == folder:
      members . append ( ( metadata, [] ) )
    elif ( members and metadata is not None
           and edited [ edited [index] . parent ] . parent == folder ):
      members [-1] [1] . append (metadata)
  return any ( membership_change_processed (member, children) is not None
               for member, children in members if member is not None )

def expand_repo_subtree (edited, root, word, headline_of_handle, check):
  """'repo:NAME subtree', the clients' 'skg-set-repo-recursive' ('C-c s
  S'): the node and every node below it reached through member
  occurrences whose home skgrepo is the node's move to NAME
  ('skg--do-at-recursive-repo-move-occurrences'). A write-protected
  occurrence is not moved (the save would refuse its edit) and is
  noted, unless the move reaches the node's editable occurrence."""
  root_metadata = headline_of_handle [ edited [root] . handle ] . metadata
  old_skgrepo = headline_node_repo (root_metadata)
  moved_gnids = set ()
  write_protected = []
  def consider (target):
    metadata = handled_metadata (edited, target, headline_of_handle)
    if metadata_has_atom (metadata, "writeProtected"):
      write_protected . append (target)
      if target == root:
        edited [root] . dropped_words . add (word)
      return
    moved_gnids . add ( headline_gnid (metadata) )
    give_subtree_word (edited, target, word, root)
  consider (root)
  def visit (later):
    metadata = handled_metadata (edited, later, headline_of_handle)
    if ( metadata is None or headline_kind (metadata) != "node"
         or metadata_has_atom (metadata, "noClaim") ):
      return False
    if headline_node_repo (metadata) == old_skgrepo:
      consider (later)
    return True
  walk_below (edited, root, visit)
  for target in write_protected:
    gnid = headline_gnid ( handled_metadata (edited, target, headline_of_handle) )
    if gnid not in moved_gnids:
      check . note_lines . append (
        "Not moved: " + described_handle ( edited [target] . handle, headline_of_handle )
        + f", write-protected here, which the save would refuse to edit; "
        f"'{word} subtree' on ⟨{edited [root] . handle}⟩ leaves it." )

def expand_relRepo_subtree (edited, root, word, headline_of_handle, check):
  """'relRepo:NAME subtree', the clients' 'skg-set-relRepo-recursive'
  ('C-c s R'), for the kind of relationship its headline names: on a
  node its content relationships, on a forks folder the forks of its
  claiming members, the walk of
  'skg--relRepo-recursive-targets'. The headline itself takes it only
  when its own relationship is of that kind."""
  root_metadata = headline_of_handle [ edited [root] . handle ] . metadata
  root_folder = folder_atom (root_metadata)
  kind = ( "content" if headline_kind (root_metadata) == "node"
           else WRITABLE_FOLDER_RELATIONSHIP_KINDS . get (root_folder) )
  if kind is None:
    check . error_lines . append (
      f"Error: ⟨{edited [root] . handle}⟩ on line "
      f"{edited [root] . compact_line_number} of the compact view carries "
      f"'{word} subtree', which applies on a node (its content "
      "relationships) or a forks folder." )
    return
  def metadata_of (index):
    return outline_metadata (edited, index, headline_of_handle)
  def member_as_such (index):
    # an occurrence member of a forks folder, claiming or not: its
    # content is shown in part, and never collected as its list
    metadata = metadata_of (index)
    return ( metadata is not None and headline_kind (metadata) == "node"
             and folder_atom ( metadata_of ( edited [index] . parent ) or
                               [ Symbol ("skg") ] ) == "forksFolder" )
  def prune_below (index):
    return ( metadata_has_atom ( metadata_of (index), "writeProtected" )
             or member_as_such (index) )
  def relationship_matches (index):
    metadata = metadata_of (index)
    member_kind = headline_kind (metadata)
    if not ( ( member_kind == "node" and not metadata_has_atom (metadata, "noClaim") )
             or member_kind in ( "unknown", "restricted" )):
      return False
    parent = edited [index] . parent
    parent_metadata = metadata_of (parent)
    if parent_metadata is None:
      return False
    if headline_kind (parent_metadata) == "node":
      return ( kind == "content" and is_editable_node (parent_metadata)
               and not member_as_such (parent) )
    return ( WRITABLE_FOLDER_RELATIONSHIP_KINDS . get ( folder_atom (parent_metadata) )
             == kind
             and is_editable_node ( metadata_of ( edited [parent] . parent )) )
  if root_folder is not None or not relationship_matches (root):
    # a folder stands for no relationship; a node, for its own when
    # its parent's list holds it
    edited [root] . dropped_words . add (word)
  if root_folder is None and prune_below (root):
    return
  def visit (later):
    if edited [later] . handle is None:
      return False
    metadata = metadata_of (later)
    member_kind = headline_kind (metadata)
    if member_kind == "node":
      if metadata_has_atom (metadata, "noClaim"):
        return False
      if relationship_matches (later):
        give_subtree_word (edited, later, word, root)
      return not prune_below (later)
    if member_kind in ( "unknown", "restricted" ):
      if relationship_matches (later):
        give_subtree_word (edited, later, word, root)
      return True
    return folder_atom (metadata) in WRITABLE_FOLDER_RELATIONSHIP_KINDS
  walk_below (edited, root, visit)

def expand_search_matching_subtree (edited, root, word, headline_of_handle,
                                    ownership, check):
  """'noSearchMatching subtree' or 'searchMatching subtree', the
  clients' 'C-u C-c s x': the node and every member occurrence below
  it, skipping, as 'skg--stage-flag-search-matching-recursive' does, a
  second occurrence of a node, a write-protected occurrence, one with
  another edit request, and a foreign node; the skipped ones but the
  repeated are noted."""
  targets = [ root ]
  def visit (later):
    metadata = handled_metadata (edited, later, headline_of_handle)
    if ( metadata is None or headline_kind (metadata) != "node"
         or metadata_has_atom (metadata, "noClaim") ):
      return False
    targets . append (later)
    return True
  walk_below (edited, root, visit)
  seen_gnids = set ()
  skipped = []
  for target in targets:
    metadata = handled_metadata (edited, target, headline_of_handle)
    gnid = headline_gnid (metadata)
    other_requests = [ other for other in edited [target] . tokens
                       if is_edit_request_word (other) and other != word ]
    reason = (
      "repeated" if gnid in seen_gnids
      else "write-protected" if metadata_has_atom (metadata, "writeProtected")
      else "it has another edit request" if other_requests
      else "foreign" if ownership . get ( headline_node_repo (metadata) ) is not True
      else None )
    if reason is None:
      seen_gnids . add (gnid)
      give_subtree_word (edited, target, word, root)
      continue
    if target == root:
      edited [root] . dropped_words . add (word)
    if reason != "repeated":
      skipped . append ( described_handle ( edited [target] . handle,
                                            headline_of_handle )
                         + " (" + reason + ")" )
  if skipped:
    check . note_lines . append (
      f"Skipped: '{word} subtree' on ⟨{edited [root] . handle}⟩ leaves "
      + ", " . join (skipped) + "." )

def processed_request_text (headline, view_line):
  """'(processed OBJECT...)' or '(unprocessed OBJECT...)' for HEADLINE's
  processed word, naming the objects 'resolve_processed_words' gave it;
  where that has not run (a dirty buffer's text, 'extend''s check), the
  objects its own letter names on the headline alone. None when it
  names none."""
  objects = ( headline . processed_objects
              if headline . processed_objects is not None
              else own_processed_objects (headline, view_line) )
  if not headline . processed_words () or objects is None:
    return None
  return "(" + " " . join ( headline . processed_words () [ : 1 ] + objects ) + ")"

def own_processed_objects (headline, view_line):
  """The objects the letter word of HEADLINE's own handle names on the
  headline alone, as a one-level gesture there names them, or None."""
  letters = headline . processed_letter_words ()
  if len (letters) != 1 or view_line is None or view_line . metadata is None:
    return None
  return processed_objects_named_by_letters (
    view_line . metadata, PROCESSED_LETTER_TOKENS [ letters [0] ], True )

def edit_request_text (headline, view_line):
  """The '(editRequest ...)' HEADLINE's tokens ask for, or None."""
  parts = [ edit_request_part_of_word (word)
            for word in headline . edit_request_words () ]
  processed = processed_request_text (headline, view_line)
  if processed:
    parts = parts + [ processed ]
  return "(editRequest " + " " . join (parts) + ")" if parts else None

def unmarked_leaf_count (headline, view_line):
  """How many leaves an 'unprocessed' on HEADLINE unmarks, counted as a
  client counts them ('leaves_touched')."""
  if ( headline . processed_words () != [ "unprocessed" ]
       or headline . processed_objects is None ):
    return 0
  return leaves_touched ( leaves_fact (view_line . metadata),
                          headline . processed_objects )

def leaves_touched (leaves, objects):
  """How many leaves a request naming OBJECTS touches on a headline whose
  '(leaves N OBJECT...)' is LEAVES, ( N, LISTED ), as the server counts
  them ('skg--processed-leaves-touched'). Of the objects listed: N when
  OBJECTS name them all; one for 'rel' alone; N less one for 'node'
  alone. An object not listed ('node' on a changeset marker, a walk)
  counts two, so that unmarking it always asks: the client cannot count
  what the server will reach. No fact, or a bare request, counts one."""
  listed = leaves [1] if leaves else []
  named_listed = [ obj for obj in objects if obj in listed ]
  unlisted = len ( [ obj for obj in objects if obj not in listed ] )
  if not leaves or not objects:
    count = 1
  elif not named_listed:
    count = 0
  elif len (named_listed) == len (listed):
    count = leaves [0]
  elif named_listed == [ "rel" ]:
    count = 1
  else:
    count = leaves [0] - 1
  return 2 * unlisted + count

def described_handle (handle, headline_of_handle):
  """'⟨ID⟩ 'TITLE'', or for a headline without a title '⟨ID⟩ (KIND)',
  from the full view."""
  view_line = headline_of_handle [handle]
  if view_line . title:
    return f"⟨{handle}⟩ '{view_line . title}'"
  return f"⟨{handle}⟩ ({headline_kind (view_line . metadata)})"

def descendant_indices (outline, index):
  level = outline [index] . level
  descendants = []
  for later in range ( index + 1, len (outline) ):
    if outline [later] . level <= level:
      break
    descendants . append (later)
  return descendants
