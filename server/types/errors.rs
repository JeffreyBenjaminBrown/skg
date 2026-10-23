use super::misc::{ID, SkgrepoName};
use super::viewnode::Vid;
use std::error::Error;
use std::fmt;
use std::io;

/// What a message calls one headline: its view id, or, for a headline
/// without one (the user typed it, or copied one, and has not saved),
/// its title. No message invents a view id.
#[derive (Debug, Clone, PartialEq, Eq, Hash)]
pub enum HeadlineName {
  ViewId (Vid),
  Title (String),
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
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  ModifiedForeignNode            (ID, SkgrepoName), // Attempted to modify a node from a foreign skgrepo - (skgid, repo_name)
  CreatedForeignNode             (ID, SkgrepoName), // Attempted to create a node in a foreign skgrepo - (skgid, repo_name)
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
  EditRequestOnWriteProtectedOccurrence      (ID, HeadlineName), // Write-protected nodes -- phantoms in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit an editable view of the node first.
  EditedWriteProtectedOccurrence {
    skgid    : ID,
    title    : String,
    changes  : Vec<String>,
    headline : HeadlineName,
  }, // This occurrence differs from the graph, but a write-protected occurrence emits no fieldIntent for the differing data.
  MemberChildOfWriteProtectedOccurrence {
    skgid    : ID,
    title    : String,
    headline : HeadlineName,
  }, // A write-protected occurrence has a child that affects its parent, but a write-protected occurrence cannot write its content.
  FlagsSurfaceEdited {
    recorder_skgid : ID,
    recorder_title : String,
    changes        : Vec<String>,
    headline       : HeadlineName, // the recorder's occurrence
  },
  FlagEditOnForeignNode                 (ID, SkgrepoName),
  FlagEditOnUnknownNode                 (ID),
  IDFolder_Edited                   (ID,       // recorder of the IDFolder
                                  Vec<ID>,  // ids the buffer's IDFolder claims
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
  /// The view id when the headline carries one, else the title.
  pub fn of (
    vid   : Option<&Vid>,
    title : &str,
  ) -> HeadlineName {
    match vid {
      Some (vid) => HeadlineName::ViewId (vid . clone ()),
      None       => HeadlineName::Title (title . to_string ()), }}

  /// The line naming the headline in a message's org form.
  pub fn org_line (
    &self
  ) -> String {
    match self {
      HeadlineName::ViewId (vid) =>
        format! ("- view id: {}\n", vid),
      HeadlineName::Title (title) =>
        format! ("- headline: {:?}\n", title), }}
}

impl fmt::Display for HeadlineName {
  /// 'view id X', or 'headline "T"'.
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      HeadlineName::ViewId (vid) =>
        write! (f, "view id {}", vid),
      HeadlineName::Title (title) =>
        write! (f, "headline {:?}", title), }}}

/// ' (view id X)', or nothing for a headline without a view id: for
/// a message that already names the headline some other way.
pub fn view_id_suffix (
  vid : Option<&Vid>,
) -> String {
  vid . map ( |vid| format! (" (view id {})", vid) )
    . unwrap_or_default () }

/// Several occurrences' names: 'view ids X, Y' when every one has a
/// view id, else each named as 'HeadlineName' names it.
pub fn name_headlines (
  names : &[HeadlineName],
) -> String {
  match view_ids_of_all (names) {
    Some (vids) if names . len () > 1 =>
      format! ("view ids {}", vids),
    _ =>
      names . iter () . map ( |name| name . to_string () )
      . collect::<Vec<String>> () . join (", "), }}

/// The line naming several occurrences in a message's org form.
pub fn headline_names_org_line (
  names : &[HeadlineName],
) -> String {
  match view_ids_of_all (names) {
    Some (vids) => format! ("- view ids: {}\n", vids),
    None        => format! ("- occurrences: {}\n",
                            name_headlines (names)), }}

/// The view ids, comma-separated, if every name is one.
fn view_ids_of_all (
  names : &[HeadlineName],
) -> Option<String> {
  let vids : Vec<&str> =
    names . iter ()
    . map ( |name| match name {
      HeadlineName::ViewId (vid) => Some (vid . 0 . as_str ()),
      HeadlineName::Title (_)    => None } )
    . collect::<Option<Vec<&str>>> () ?;
  Some ( vids . join (", ") ) }

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
      BufferValidationError::Multiple_Defining_Viewnodes (skgid, headlines) =>
        write!(f, "Multiple occurrences of node {:?} are editable ({})",
               skgid, name_headlines (headlines)),
      BufferValidationError::AmbiguousDeletion (skgid, headlines) =>
        write!(f, "Ambiguous deletion request for ID {:?} ({})",
               skgid, name_headlines (headlines)),
      BufferValidationError::DuplicatedContent (skgid) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", skgid),
      BufferValidationError::ModifiedForeignNode(skgid, skgrepo) =>
        write!(f, "Cannot modify node {:?} from foreign repo '{}'", skgid, skgrepo),
      BufferValidationError::CreatedForeignNode(skgid, skgrepo) =>
        write!(f, "Cannot create node {:?} in foreign repo '{}'", skgid, skgrepo),
      BufferValidationError::CannotMoveToOrFromForeignSkgrepo(skgid, disk_skgrepo, buffer_skgrepo) =>
        write!(f, "Cannot move node {:?} between repos '{}' and '{}': one or both are foreign", skgid, disk_skgrepo, buffer_skgrepo),
      BufferValidationError::CannotMoveAndMergeSimultaneously(skgid) =>
        write!(f, "Cannot move and merge node {:?} in the same save", skgid),
      BufferValidationError::SkgrepoNotInConfig(skgid, skgrepo) =>
        write!(f, "Node {:?} references repo '{}' which does not exist in config", skgid, skgrepo),
      BufferValidationError::ForkSkgrepoUnresolved(skgid) =>
        write!(f, "Cannot fork node {:?}: no owned repo to put the clone in. It has no owned ancestor in the view to inherit a repo from; set the clone's repo in the confirmation buffer (C-c s s).", skgid),
      BufferValidationError::ForkAlreadyExists(original, existing) =>
        write!(f, "Cannot fork node {:?}: you have already forked it. Your clone is {:?}. Edit that clone instead (a node may have at most one owned override).", original, existing),
      BufferValidationError::ForkSkgrepoRestricted(skgid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is restricted under the current repo-set. Activate it first; an invisible clone is never created silently.", skgid, skgrepo),
      BufferValidationError::ForkSkgrepoNotOwned(skgid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is not one you own. Choose an owned repo for the clone (C-c s s in the confirmation buffer).", skgid, skgrepo),
      BufferValidationError::ForkRequestOnUnknownNode(skgid, headline) =>
        write!(f, "Cannot fork node {:?} ({}): it is not in the graph. Only a saved node can be forked; save it first, then fork.", skgid, headline),
      BufferValidationError::ForkRequestMultiple(skgid, headlines) =>
        write!(f, "Multiple fork requests for the same node {:?} ({}). At most one fork request per node is allowed.", skgid, name_headlines (headlines)),
      BufferValidationError::OverrideInvariantViolation(msg) =>
        write!(f, "{}", msg),
      BufferValidationError::MultipleEditableViewRequestsForSameId (skgid, headlines) =>
        write!(f, "Multiple editable view requests for the same ID {:?} ({}). At most one request per ID is allowed.", skgid, name_headlines (headlines)),
      BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (skgid, headline) =>
        write!(f, "An occurrence of node {:?} ({}) requests both write-protection and an editable view. Request one; to move editability, request write-protection on one occurrence (C-c s w) and an editable view on another (C-c s d).", skgid, headline),
      BufferValidationError::EmptyTitle(skgid) =>
        write!(f, "Node {:?} has an empty title. Every editable node must have a non-empty title.", skgid),
      BufferValidationError::LocalStructureViolation(msg, skgid, headline) =>
        write!(f, "Local structure violation at ID {:?} ({}): {}", skgid, headline, msg),
      BufferValidationError::IDFolder_Edited(recorder, buffer_skgids, real_skgids, headline) =>
        write!(f, "The idFolder under node {:?} ({}) was edited (buffer claims {:?}; real ids are {:?}). Reordering is fine, but IDs cannot be added, removed or edited through the buffer; edit the .skg file directly.", recorder, headline, buffer_skgids, real_skgids),
      BufferValidationError::EditRequestOnWriteProtectedOccurrence (skgid, headline) =>
        write!(f, "Edit request on write-protected (phantom) node {:?} ({}). Phantoms are write-protected; write-protected nodes cannot carry write instructions. Visit a editable view of the node first (C-c g RET).", skgid, headline),
      BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes, headline } =>
        write!(f, "The write-protected occurrence of node {:?} ({:?}, {}) differs from the graph ({}). A write-protected occurrence saves nothing of its own; edit an editable occurrence instead, or request this one editable (C-c s d).",
               skgid, title, headline, changes . join ("; ")),
      BufferValidationError::MemberChildOfWriteProtectedOccurrence {
        skgid, title, headline } =>
        write!(f, "A child typed under the write-protected occurrence of {} ({}, {}) cannot become its content. Request an editable occurrence of it (C-c s d), or mark the child as not affecting its parent.",
               title, skgid . 0, headline),
      BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid, recorder_title, changes, headline } =>
        write!(f, "The flags surface under node {:?} ({:?}, {}) was edited: {}. It is server-owned and no changes were saved. Use skg-set-flag-search-matching for noSearchMatching; provenance flags have no setter.",
               recorder_skgid, recorder_title, headline, changes . join ("; ")),
      BufferValidationError::FlagEditOnForeignNode (skgid, skgrepo) =>
        write! (f, "Cannot change flags of node {:?} from foreign repo '{}'; this gesture never creates an implicit fork.", skgid, skgrepo),
      BufferValidationError::FlagEditOnUnknownNode (skgid) =>
        write! (f, "Cannot change flags of unsaved or unknown node {:?}; save the node first.", skgid),
      BufferValidationError::OverridesHere_Mismatch(carrier, original, effective, headline) =>
        write!(f, "Node {:?} ({}) carries the fact (overridesHere {:?}), but it is not on the override chain of that original (which resolves to {:?}). The fact looks hand-edited or stale; saving it would rewrite a contains list. Re-render the view and retry.", carrier, headline, original, effective),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
