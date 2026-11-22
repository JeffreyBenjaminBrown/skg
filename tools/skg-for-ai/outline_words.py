"""'outline_words': the words of an edited compact view's handles, read
against the outline ('outline_word_check'): what a 'subtree' gives the
headlines below it ('expand_subtree_words', and the processed words
it resolves), and what 'detach', 'attach', 'repo:NAME',
'relRepo:NAME' and the flag words need of where they stand. Also the
request text the words become ('edit_request_text',
'processed_request_text'). 'edited_compact_view' reads the handles
and their words; 'save' saves what both find."""

from sexp import Symbol, metadata_form, sexp_to_text
from full_view import (
  folder_atom, headline_gnid, headline_kind, headline_node_repo,
  metadata_has_atom )
from heralds import (
  is_editable_node, is_symbol_named, leaves_fact,
  membership_change_processed, reads_claims )
from edited_compact_view import (
  OUTWARD_TOKEN, PROCESSED_LETTER_TOKENS, PROCESSED_TOKENS,
  RELREPO_WORD_PREFIX, REPO_WORD_PREFIX, SEARCH_MATCHING_TOKENS,
  SUBTREE_TOKEN, WALK_OBJECT_OF_TOKEN, WRITABLE_FOLDER_RELATIONSHIP_KINDS,
  edit_request_part_of_word, is_edit_request_word, request_token_problems,
  word_parameter, words_with_a_subtree_form )

# ------------------------------------------------------------
# Words read against the outline: what a 'subtree' gives the
# headlines below it, and what 'detach', 'attach', 'repo:NAME',
# 'relRepo:NAME' and the flag words need of where they stand
# ------------------------------------------------------------

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
