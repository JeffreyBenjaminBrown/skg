use super::misc::{ID, SkgRepoName};
use std::error::Error;
use std::io;
use std::collections::HashSet;

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
  // text). Decided 2026-06-12: warnings always accompany errors.
  BufferValidationErrors {
    errors   : Vec<BufferValidationError>,
    warnings : Vec<String>, }, }

/// If the user attempts to save a buffer
/// with any of these flags, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
pub enum BufferValidationError {
  Body_of_NonVognode               (String,   // Title from buffer
                                  String),  // Non-vognode kind (e.g. "aliasFolder", "alias")
  Multiple_Defining_Viewnodes     (ID), // For any given ID, at most one occurrence can be editable. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  InconsistentSkgRepos            (ID, HashSet<SkgRepoName>), // Multiple viewnodes with same ID have different skgrepos
  ModifiedForeignNode            (ID, SkgRepoName), // Attempted to modify a node from a foreign skgrepo - (node_id, repo_name)
  CreatedForeignNode             (ID, SkgRepoName), // Attempted to create a node in a foreign skgrepo - (node_id, repo_name)
  CannotMoveToOrFromForeignSkgRepo (ID,
                                   SkgRepoName, // disk skgrepo
                                   SkgRepoName), // buffer skgrepo
  CannotMoveAndMergeSimultaneously (ID),
  SkgRepoNotInConfig              (ID, SkgRepoName),
  // Fork errors. Editing a foreign node N is read as a request to
  // clone it (the clone C lives in an owned skgrepo, subscribes to and
  // overrides N). These are the ways that request can be refused.
  ForkSkgRepoUnresolved           (ID), // N's pid: no OWNED vognode ancestor in the view to inherit C's skgrepo from, and the user set none in the confirmation buffer.
  ForkAlreadyExists              (ID,   // N's pid
                                  ID),  // the existing owned clone that already overrides N (monogamy: a node may have at most one owned overrider)
  ForkSkgRepoRestricted             (ID,           // N's pid
                                  SkgRepoName),  // C's resolved owned skgrepo, which is RESTRICTED under the skgrepo restriction
  ForkSkgRepoNotOwned             (ID,           // N's pid
                                  SkgRepoName),  // C's chosen skgrepo, which the user does NOT own (a typed or hand-edited skgrepo the rotation would never offer)
  ForkRequestOnUnknownNode       (ID),  // An explicit 'skg-fork-node' request on a node whose id is not in the graph (an unsaved headline): nothing exists to override.
  ForkRequestMultiple            (ID),  // Two headlines for the same id both carry an explicit fork request; at most one is allowed.
  OverrideInvariantViolation     (String),
  EditableViewRequestOnEditableNode      (ID), // An editable view request on a node that is already editable
  EditableViewRequestOnNodeWithContentChildren (ID), // An editable view request on a node that has content (affectsParent=Container) children. Non-content children (e.g. containerward role tree stubs) don't trigger this.
  MultipleEditableViewRequestsForSameId    (ID), // Multiple editable view requests for the same ID
  EmptyTitle                             (ID),
  LocalStructureViolation        (String, ID), // (error message, nearest ancestor ID)
  EditRequestOnWriteProtectedOccurrence      (ID), // Write-protected nodes -- phantoms in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit an editable view of the node first.
  EditedWriteProtectedOccurrence {
    skgid   : ID,
    title   : String,
    changes : Vec<String>,
  }, // This occurrence was changed since the server rendered it, but a write-protected occurrence emits no fieldIntent for the changed data.
  FlagsSurfaceEdited {
    recorder_skgid : ID,
    recorder_title : String,
    changes        : Vec<String>,
  },
  FlagEditOnForeignNode                 (ID, SkgRepoName),
  FlagEditOnUnknownNode                 (ID),
  IDFolder_Edited                   (ID,       // recorder of the IDFolder
                                  Vec<ID>,  // ids the buffer's IDFolder claims
                                  Vec<ID>), // the recorder's real ids (pid + extra_ids); empty if the recorder is not in the graph
  OverridesHere_Mismatch         (Option<ID>, // the carrier's own ID
                                  ID,         // the original the fact claims
                                  Option<ID>), // who the graph says substitutes for that original; None if the graph was unavailable
  Other                          (String),
}


//
// Implementations
//

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
      BufferValidationError::Body_of_NonVognode(title, kind) =>
        write!(f, "{} node should not have a body. Node title: '{}'",
               kind, title),
      BufferValidationError::Multiple_Defining_Viewnodes (skgid) =>
        write!(f, "Multiple occurrences of node {:?} are editable", skgid),
      BufferValidationError::AmbiguousDeletion (skgid) =>
        write!(f, "Ambiguous deletion request for ID {:?}", skgid),
      BufferValidationError::DuplicatedContent (skgid) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", skgid),
      BufferValidationError::InconsistentSkgRepos(skgid, skgrepos) => {
        let skgrepo_list: Vec<&SkgRepoName> = skgrepos . iter() . collect();
        write!(f, "Multiple viewnodes with ID {:?} have inconsistent repos: {:?}", skgid, skgrepo_list) },
      BufferValidationError::ModifiedForeignNode(skgid, skgrepo) =>
        write!(f, "Cannot modify node {:?} from foreign repo '{}'", skgid, skgrepo),
      BufferValidationError::CreatedForeignNode(skgid, skgrepo) =>
        write!(f, "Cannot create node {:?} in foreign repo '{}'", skgid, skgrepo),
      BufferValidationError::CannotMoveToOrFromForeignSkgRepo(skgid, disk_skgrepo, buffer_skgrepo) =>
        write!(f, "Cannot move node {:?} between repos '{}' and '{}': one or both are foreign", skgid, disk_skgrepo, buffer_skgrepo),
      BufferValidationError::CannotMoveAndMergeSimultaneously(skgid) =>
        write!(f, "Cannot move and merge node {:?} in the same save", skgid),
      BufferValidationError::SkgRepoNotInConfig(skgid, skgrepo) =>
        write!(f, "Node {:?} references repo '{}' which does not exist in config", skgid, skgrepo),
      BufferValidationError::ForkSkgRepoUnresolved(skgid) =>
        write!(f, "Cannot fork node {:?}: no owned repo to put the clone in. It has no owned ancestor in the view to inherit a repo from; set the clone's repo in the confirmation buffer (C-c s s).", skgid),
      BufferValidationError::ForkAlreadyExists(original, existing) =>
        write!(f, "Cannot fork node {:?}: you have already forked it. Your clone is {:?}. Edit that clone instead (a node may have at most one owned override).", original, existing),
      BufferValidationError::ForkSkgRepoRestricted(skgid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is restricted under the current repo-set. Activate it first; an invisible clone is never created silently.", skgid, skgrepo),
      BufferValidationError::ForkSkgRepoNotOwned(skgid, skgrepo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is not one you own. Choose an owned repo for the clone (C-c s s in the confirmation buffer).", skgid, skgrepo),
      BufferValidationError::ForkRequestOnUnknownNode(skgid) =>
        write!(f, "Cannot fork node {:?}: it is not in the graph. Only a saved node can be forked; save it first, then fork.", skgid),
      BufferValidationError::ForkRequestMultiple(skgid) =>
        write!(f, "Multiple fork requests for the same node {:?}. At most one fork request per node is allowed.", skgid),
      BufferValidationError::OverrideInvariantViolation(msg) =>
        write!(f, "{}", msg),
      BufferValidationError::EditableViewRequestOnEditableNode (skgid) =>
        write!(f, "Editable view request on a node that is already editable (ID {:?}). The node already shows its content; no expansion needed.", skgid),
      BufferValidationError::EditableViewRequestOnNodeWithContentChildren (skgid) =>
        write!(f, "Editable view request on a node with content children (ID {:?}). The expansion would clobber those children. Save without the request first, then delete children and retry.", skgid),
      BufferValidationError::MultipleEditableViewRequestsForSameId (skgid) =>
        write!(f, "Multiple editable view requests for the same ID {:?}. At most one request per ID is allowed.", skgid),
      BufferValidationError::EmptyTitle(skgid) =>
        write!(f, "Node {:?} has an empty title. Every editable node must have a non-empty title.", skgid),
      BufferValidationError::LocalStructureViolation(msg, skgid) =>
        write!(f, "Local structure violation at ID {:?}: {}", skgid, msg),
      BufferValidationError::IDFolder_Edited(recorder, buffer_skgids, real_skgids) =>
        write!(f, "The idFolder under node {:?} was edited (buffer claims {:?}; real ids are {:?}). Reordering is fine, but IDs cannot be added, removed or edited through the buffer; edit the .skg file directly.", recorder, buffer_skgids, real_skgids),
      BufferValidationError::EditRequestOnWriteProtectedOccurrence (skgid) =>
        write!(f, "Edit request on write-protected (phantom) node {:?}. Phantoms are write-protected; write-protected nodes cannot carry write instructions. Visit a editable view of the node first (C-c g RET).", skgid),
      BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes } =>
        write!(f, "The write-protected occurrence of node {:?} ({:?}) was edited ({}) but is write-protected. Re-render, then edit a editable occurrence instead.",
               skgid, title, changes . join ("; ")),
      BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid, recorder_title, changes } =>
        write!(f, "The flags surface under node {:?} ({:?}) was edited: {}. It is server-owned and no changes were saved. Use skg-set-flag-search-matching for noSearchMatching; provenance flags have no setter.",
               recorder_skgid, recorder_title, changes . join ("; ")),
      BufferValidationError::FlagEditOnForeignNode (skgid, skgrepo) =>
        write! (f, "Cannot change flags of node {:?} from foreign repo '{}'; this gesture never creates an implicit fork.", skgid, skgrepo),
      BufferValidationError::FlagEditOnUnknownNode (skgid) =>
        write! (f, "Cannot change flags of unsaved or unknown node {:?}; save the node first.", skgid),
      BufferValidationError::OverridesHere_Mismatch(carrier, original, effective) =>
        write!(f, "Node {:?} carries the fact (overridesHere {:?}), but it is not on the override chain of that original (which resolves to {:?}). The fact looks hand-edited or stale; saving it would rewrite a contains list. Re-render the view and retry.", carrier, original, effective),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
