use super::misc::{ID, SkgrepoName};
use crate::diff_view::readiness_requests::NamedLeafSet;
use super::viewnode::ViewnodeId;
use std::error::Error;
use std::fmt;
use std::io;

/// What a message says of one headline: its title, the graphnode ID it
/// carries, and its viewnode ID. A message names it as an org
/// subheadline ('org_subheadline'). No message invents a viewnode ID.
#[derive (Debug, Clone, PartialEq, Eq, Hash)]
pub struct HeadlineName {
  pub title        : String, // empty when the headline has none
  pub graphnode_id : Option<ID>, // None for a folder, a property, a tombstone, or a typed headline without one
  pub viewnode_id  : Option<ViewnodeId>, // None for a headline typed, or copied, since the last render
}

#[derive (Debug)]
pub enum LinkParseError {
  InvalidFormat,
  MissingDivider,
}

#[derive (Debug)]
pub enum SaveError {
  ParseError (String),
  DatabaseError(Box<dyn Error>),
  IoError (io::Error),
  // A failed save carries, alongside its errors, the nonfatal
  // warnings collected before the abort (e.g. discarded folder-headline
  // text). Warnings always accompany errors.
  BufferValidationErrors {
    errors   : Vec<BufferValidationError>,
    warnings : Vec<String>, }, }

/// If the user attempts to save a buffer
/// with any of these flags, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
pub enum BufferValidationError {
  Body_of_NonVognode               (String,   // Title from buffer
                                  String,   // Non-vognode kind (e.g. "aliasFolder", "alias")
                                  HeadlineName),
  Multiple_Defining_Viewnodes     (ID, Vec<HeadlineName>), // For any given ID, at most one occurrence can be editable. (Its contents are intended to define those of the node.) The names are the editable occurrences'.
  AmbiguousDeletion              (ID, Vec<HeadlineName>), // The names are the editable occurrences', with and without the delete request.
  ContradictoryReadinessRequests (NamedLeafSet, Vec<HeadlineName>), // The change leaves that one headline requests ready and another unready. The names are those headlines'.
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  ModifiedForeignNode            (ID, SkgrepoName), // Attempted to modify a node from a foreign skgrepo - (gnid, repo_name)
  CreatedForeignNode             (ID, SkgrepoName), // Attempted to create a node in a foreign skgrepo - (gnid, repo_name)
  CannotMoveToOrFromForeignSkgrepo (ID,
                                   SkgrepoName, // disk skgrepo
                                   SkgrepoName), // buffer skgrepo
  CannotMoveAndMergeSimultaneously (ID),
  SkgrepoNotInConfig              (ID, SkgrepoName),
  // Fork errors. Editing a foreign node N is read as a request to
  // clone it (the clone C lives in an owned skgrepo, subscribes to and
  // overrides N). These are the ways that request can be refused.
  ForkSkgrepoUnresolved           (ID), // N's pid: no owned vognode ancestor in the view to inherit C's skgrepo from, and the user set none in the confirmation buffer.
  ForkAlreadyExists              (ID,   // N's pid
                                  ID),  // the existing owned clone that already overrides N (monogamy: a node may have at most one owned overrider)
  ForkSkgrepoRestricted             (ID,           // N's pid
                                  SkgrepoName),  // C's resolved owned skgrepo, which is restricted under the skgrepo restriction
  ForkSkgrepoNotOwned             (ID,           // N's pid
                                  SkgrepoName),  // C's chosen skgrepo, which the user does not own (a typed or hand-edited skgrepo the rotation would never offer)
  ForkRequestOnUnknownNode       (ID, HeadlineName),  // An explicit 'skg-fork-node' request on a node whose id is not in the graph (an unsaved headline): nothing exists to override.
  ForkRequestMultiple            (ID, Vec<HeadlineName>),  // Two headlines for the same id both carry an explicit fork request; at most one is allowed.
  OverrideInvariantViolation     (String),
  MultipleEditableViewRequestsForSameId    (ID, Vec<HeadlineName>), // Multiple editable view requests for the same ID
  WriteProtectAndEditableViewRequestsOnOneOccurrence (ID, HeadlineName), // One occurrence asks to become both write-protected and the editable one. (On different occurrences of a node, the two requests move editability, and are allowed.)
  EmptyTitle                             (ID),
  LocalStructureViolation        (String, ID, HeadlineName), // (error message, nearest ancestor ID, the headline violating)
  EditRequestOnWriteProtectedOccurrence      (ID, HeadlineName), // Write-protected nodes -- ghosts and unknowns in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit an editable view of the node first.
  EditedWriteProtectedOccurrence {
    gnid    : ID,
    title    : String,
    changes  : Vec<String>,
    headline : HeadlineName,
  }, // This occurrence differs from the graph, but a write-protected occurrence emits no fieldIntent for the differing data.
  FlagsSurfaceEdited {
    recorder_gnid : ID,
    recorder_title : String,
    changes        : Vec<String>,
    headline       : HeadlineName, // the recorder's occurrence
  },
  FlagEditOnForeignNode                 (ID, SkgrepoName),
  FlagEditOnUnknownNode                 (ID),
  IDFolder_Edited                   (ID,       // recorder of the idFolder
                                  Vec<ID>,  // ids the buffer's idFolder claims
                                  Vec<ID>,  // the recorder's real ids (pid + extra_ids); empty if the recorder is not in the graph
                                  HeadlineName), // the recorder's occurrence
  OverridesHere_Mismatch         (Option<ID>, // the carrier's own ID
                                  ID,         // the original the fact claims
                                  Option<ID>, // who the graph says substitutes for that original; None if the graph was unavailable
                                  HeadlineName), // the carrier
  Other                          (String),
}


//
// Implementations
//

impl HeadlineName {
  pub fn of (
    viewnode_id  : Option<&ViewnodeId>,
    graphnode_id : Option<&ID>,
    title        : &str,
  ) -> HeadlineName {
    HeadlineName {
      title        : title . to_string (),
      graphnode_id : graphnode_id . cloned (),
      viewnode_id  : viewnode_id . cloned (), }}

  /// The org subheadline naming this headline under a message's
  /// headline, which the clients put at level 2 (see
  /// 'docs/api-and-formats.org', "How a message names a headline"):
  /// the title (or '(untitled)'), then 'graphnode ID: X' when it
  /// carries one, then 'viewnode ID: X' with the command that finds
  /// it, or 'not yet saved'.
  pub fn org_subheadline (
    &self
  ) -> String {
    let viewnode_line : String = match &self . viewnode_id {
      Some (viewnode_id) => format! (
        "- viewnode ID: {}. To find it: C-c g v {}\n",
        viewnode_id, viewnode_id ),
      None => "- not yet saved\n" . to_string (), };
    format! ( "{}{}", self . org_subheadline_of_a_node_this_save_created (),
              viewnode_line ) }

  /// The org subheadline naming a headline from which the save that
  /// sends the message created a node: the title (or '(untitled)')
  /// and 'graphnode ID: X', with no viewnode line, since the headline
  /// is saved and its viewnode ID is the rerender's to give.
  pub fn org_subheadline_of_a_node_this_save_created (
    &self
  ) -> String {
    let heading : &str =
      if self . title . is_empty () { "(untitled)" }
      else { &self . title };
    let graphnode_line : String =
      self . graphnode_id . as_ref ()
      . map ( |gnid| format! ("- graphnode ID: {}\n", gnid . 0) )
      . unwrap_or_default ();
    format! ( "*** {}\n{}", heading, graphnode_line ) }
}

impl fmt::Display for HeadlineName {
  /// 'viewnode ID X', or 'headline "T"': for a one-line description,
  /// as in a log. A message shown to the user names its headlines as
  /// subheadlines instead ('org_subheadline').
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match &self . viewnode_id {
      Some (viewnode_id) =>
        write! (f, "viewnode ID {}", viewnode_id),
      None =>
        write! (f, "headline {:?}", self . title), }}}

/// The subheadlines naming 'names', one after another.
pub fn org_subheadlines (
  names : &[HeadlineName],
) -> String {
  names . iter () . map ( HeadlineName::org_subheadline )
    . collect::<Vec<String>> () . concat () }

/// A message's text followed by the subheadlines naming 'names': the
/// text ends its line, and the subheadlines come after it, so that
/// nothing of the message falls under one of them.
pub fn with_org_subheadlines (
  text  : &str,
  names : &[HeadlineName],
) -> String {
  format! ( "{}\n{}", text . trim_end_matches ('\n'),
            org_subheadlines (names) ) }

/// Several occurrences' names on one line: 'viewnode IDs X, Y' when
/// every one has a viewnode ID, else each named as 'HeadlineName'
/// displays it.
pub fn name_headlines (
  names : &[HeadlineName],
) -> String {
  match viewnode_ids_of_all (names) {
    Some (viewnode_ids) if names . len () > 1 =>
      format! ("viewnode IDs {}", viewnode_ids),
    _ =>
      names . iter () . map ( |name| name . to_string () )
      . collect::<Vec<String>> () . join (", "), }}

/// The viewnode IDs, comma-separated, if every name has one.
fn viewnode_ids_of_all (
  names : &[HeadlineName],
) -> Option<String> {
  let viewnode_ids : Vec<&str> =
    names . iter ()
    . map ( |name| name . viewnode_id . as_ref ()
            . map ( |viewnode_id| viewnode_id . 0 . as_str () ))
    . collect::<Option<Vec<&str>>> () ?;
  Some ( viewnode_ids . join (", ") ) }

impl std::fmt::Display for LinkParseError {
  fn fmt (
    &self,
    f: &mut std::fmt::Formatter <'_>
  ) -> std::fmt::Result {
    match self {
      LinkParseError::InvalidFormat =>
        write! (
          f, "Invalid link format. Expected [[id:ID][LABEL]]" ),
      LinkParseError::MissingDivider =>
        write! (
          f, "Missing divider between ID and label. Expected ][" ),
    } } }

impl Error for LinkParseError {}

impl std::fmt::Display for BufferValidationError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>
  ) -> std::fmt::Result {
    match self {
      BufferValidationError::Body_of_NonVognode(title, kind, headline) =>
        write!(f, "{} node ({}) should not have a body. Node title: '{}'",
               kind, headline, title),
      BufferValidationError::Multiple_Defining_Viewnodes (gnid, headlines) =>
        write!(f, "Multiple occurrences of node {:?} are editable ({})",
               gnid, name_headlines (headlines)),
      BufferValidationError::AmbiguousDeletion (gnid, headlines) =>
        write!(f, "Ambiguous deletion request for ID {:?} ({})",
               gnid, name_headlines (headlines)),
      BufferValidationError::ContradictoryReadinessRequests (leaf, headlines) =>
        write!(f, "Contradictory readiness requests for {} ({})",
               leaf . describe (), name_headlines (headlines)),
      BufferValidationError::DuplicatedContent (gnid) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", gnid),
      BufferValidationError::ModifiedForeignNode(gnid, skgrepo) =>
        write!(f, "Cannot modify node {:?} from foreign repo '{}'", gnid, skgrepo),
      BufferValidationError::CreatedForeignNode(gnid, skgrepo) =>
        write!(f, "Cannot create node {:?} in foreign repo '{}'", gnid, skgrepo),
      BufferValidationError::CannotMoveToOrFromForeignSkgrepo(gnid, disk_skgrepo, buffer_skgrepo) =>
        write!(f, "Cannot move node {:?} between repos '{}' and '{}': one or both are foreign", gnid, disk_skgrepo, buffer_skgrepo),
      BufferValidationError::CannotMoveAndMergeSimultaneously(gnid) =>
        write!(f, "Cannot move and merge node {:?} in the same save", gnid),
      BufferValidationError::SkgrepoNotInConfig(gnid, skgrepo) =>
        write!(f, "Node {:?} references repo '{}' which does not exist in config", gnid, skgrepo),
      BufferValidationError::ForkSkgrepoUnresolved(gnid) =>
        write!(f, "Cannot fork node {:?}: no owned repo to put the clone in. It has no owned ancestor in the view to inherit a repo from; set the clone's repo in the confirmation buffer (C-c s s).", gnid),
      BufferValidationError::ForkAlreadyExists(original, existing) =>
        write!(f, "Cannot fork node {:?}: you have already forked it. Your clone is {:?}. Edit that clone instead (a node may have at most one owned override).", original, existing),
      BufferValidationError::ForkSkgrepoRestricted(gnid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is restricted under the current repo-set. Activate it first; an invisible clone is never created silently.", gnid, skgrepo),
      BufferValidationError::ForkSkgrepoNotOwned(gnid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is not one you own. Choose an owned repo for the clone (C-c s s in the confirmation buffer).", gnid, skgrepo),
      BufferValidationError::ForkRequestOnUnknownNode(gnid, headline) =>
        write!(f, "Cannot fork node {:?} ({}): it is not in the graph. Only a saved node can be forked; save it first, then fork.", gnid, headline),
      BufferValidationError::ForkRequestMultiple(gnid, headlines) =>
        write!(f, "Multiple fork requests for the same node {:?} ({}). At most one fork request per node is allowed.", gnid, name_headlines (headlines)),
      BufferValidationError::OverrideInvariantViolation(msg) =>
        write!(f, "{}", msg),
      BufferValidationError::MultipleEditableViewRequestsForSameId (gnid, headlines) =>
        write!(f, "Multiple editable view requests for the same ID {:?} ({}). At most one request per ID is allowed.", gnid, name_headlines (headlines)),
      BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (gnid, headline) =>
        write!(f, "An occurrence of node {:?} ({}) requests both write-protection and an editable view. Request one; to move editability, request write-protection on one occurrence (C-c s w) and an editable view on another (C-c s d).", gnid, headline),
      BufferValidationError::EmptyTitle(gnid) =>
        write!(f, "Node {:?} has an empty title. Every editable node must have a non-empty title.", gnid),
      BufferValidationError::LocalStructureViolation(msg, gnid, headline) =>
        write!(f, "Local structure violation at ID {:?} ({}): {}", gnid, headline, msg),
      BufferValidationError::IDFolder_Edited(recorder, buffer_gnids, real_gnids, headline) =>
        write!(f, "The idFolder under node {:?} ({}) was edited (buffer claims {:?}; real ids are {:?}). Reordering is fine, but IDs cannot be added, removed or edited through the buffer; edit the .skg file directly.", recorder, headline, buffer_gnids, real_gnids),
      BufferValidationError::EditRequestOnWriteProtectedOccurrence (gnid, headline) =>
        write!(f, "Edit request on write-protected node {:?} ({}). Ghosts and unknowns are write-protected; write-protected nodes cannot carry write instructions. Visit an editable view of the node first (C-c g RET).", gnid, headline),
      BufferValidationError::EditedWriteProtectedOccurrence {
        gnid, title, changes, headline } =>
        write!(f, "The write-protected occurrence of node {:?} ({:?}, {}) differs from the graph ({}). A write-protected occurrence saves nothing of its own; edit an editable occurrence instead, or request this one editable (C-c s d).",
               gnid, title, headline, changes . join ("; ")),
      BufferValidationError::FlagsSurfaceEdited {
        recorder_gnid, recorder_title, changes, headline } =>
        write!(f, "The flags surface under node {:?} ({:?}, {}) was edited: {}. It is server-owned and no changes were saved. Use skg-set-flag-search-matching for noSearchMatching; provenance flags have no setter.",
               recorder_gnid, recorder_title, headline, changes . join ("; ")),
      BufferValidationError::FlagEditOnForeignNode (gnid, skgrepo) =>
        write! (f, "Cannot change flags of node {:?} from foreign repo '{}'; this gesture never creates an implicit fork.", gnid, skgrepo),
      BufferValidationError::FlagEditOnUnknownNode (gnid) =>
        write! (f, "Cannot change flags of unsaved or unknown node {:?}; save the node first.", gnid),
      BufferValidationError::OverridesHere_Mismatch(carrier, original, effective, headline) =>
        write!(f, "Node {:?} ({}) carries the fact (overridesHere {:?}), but it is not on the override chain of that original (which resolves to {:?}). The fact looks hand-edited or stale; saving it would rewrite a contains list. Re-render the view and retry.", carrier, headline, original, effective),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl BufferValidationError {
  /// The headlines the error concerns, which its org form names as
  /// subheadlines.
  pub fn named_headlines (
    &self
  ) -> Vec<&HeadlineName> {
    match self {
      BufferValidationError::Body_of_NonVognode (_, _, headline)
        | BufferValidationError::ForkRequestOnUnknownNode (_, headline)
        | BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (_, headline)
        | BufferValidationError::LocalStructureViolation (_, _, headline)
        | BufferValidationError::EditRequestOnWriteProtectedOccurrence (_, headline)
        | BufferValidationError::EditedWriteProtectedOccurrence { headline, .. }
        | BufferValidationError::FlagsSurfaceEdited { headline, .. }
        | BufferValidationError::IDFolder_Edited (_, _, _, headline)
        | BufferValidationError::OverridesHere_Mismatch (_, _, _, headline) =>
        vec! [ headline ],
      BufferValidationError::Multiple_Defining_Viewnodes (_, headlines)
        | BufferValidationError::AmbiguousDeletion (_, headlines)
        | BufferValidationError::ContradictoryReadinessRequests (_, headlines)
        | BufferValidationError::ForkRequestMultiple (_, headlines)
        | BufferValidationError::MultipleEditableViewRequestsForSameId (_, headlines) =>
        headlines . iter () . collect (),
      BufferValidationError::DuplicatedContent (_)
        | BufferValidationError::ModifiedForeignNode (..)
        | BufferValidationError::CreatedForeignNode (..)
        | BufferValidationError::CannotMoveToOrFromForeignSkgrepo (..)
        | BufferValidationError::CannotMoveAndMergeSimultaneously (_)
        | BufferValidationError::SkgrepoNotInConfig (..)
        | BufferValidationError::ForkSkgrepoUnresolved (_)
        | BufferValidationError::ForkAlreadyExists (..)
        | BufferValidationError::ForkSkgrepoRestricted (..)
        | BufferValidationError::ForkSkgrepoNotOwned (..)
        | BufferValidationError::OverrideInvariantViolation (_)
        | BufferValidationError::EmptyTitle (_)
        | BufferValidationError::FlagEditOnForeignNode (..)
        | BufferValidationError::FlagEditOnUnknownNode (_)
        | BufferValidationError::Other (_) =>
        Vec::new (), }}
}

impl Error for BufferValidationError {}
