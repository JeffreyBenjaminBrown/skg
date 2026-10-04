use super::misc::{ID, RepoName};
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
  Body_of_Scaffold               (String,   // Title from buffer
                                  String),  // Scaffold kind (e.g. "aliasFolder", "alias")
  Multiple_Defining_Viewnodes     (ID), // For any given ID, at most one occurrence can be definitive. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  InconsistentRepos            (ID, HashSet<RepoName>), // Multiple viewnodes with same ID have different repos
  ModifiedForeignNode            (ID, RepoName), // Attempted to modify a node from a foreign (write-protected) repo - (node_id, repo_name)
  CreatedForeignNode             (ID, RepoName), // Attempted to create a node in a foreign (write-protected) repo - (node_id, repo_name)
  CannotMoveToOrFromForeignRepo (ID,
                                   RepoName, // disk repo
                                   RepoName), // buffer repo
  CannotMoveAndMergeSimultaneously (ID),
  RepoNotInConfig              (ID, RepoName),
  // Fork errors. Editing a foreign node N is read as a request to
  // clone it (the clone C lives in an owned repo, subscribes to and
  // overrides N). These are the ways that request can be refused.
  ForkRepoUnresolved           (ID), // N's pid: no OWNED vognode ancestor in the view to inherit C's repo from, and the user set none in the confirmation buffer.
  ForkAlreadyExists              (ID,   // N's pid
                                  ID),  // the existing user-owned clone that already overrides N (monogamy: a node may have at most one user-owned overrider)
  ForkRepoInactive             (ID,           // N's pid
                                  RepoName),  // C's resolved owned repo, which is INACTIVE under the active repo-set
  ForkRepoNotOwned             (ID,           // N's pid
                                  RepoName),  // C's chosen repo, which the user does NOT own (a typed or hand-edited repo the rotation would never offer)
  ForkRequestOnUnknownNode       (ID),  // An explicit 'skg-fork-node' request on a node whose id is not in the graph (an unsaved headline): nothing exists to override.
  ForkRequestMultiple            (ID),  // Two headlines for the same id both carry an explicit fork request; at most one is allowed.
  OverrideInvariantViolation     (String),
  DefinitiveRequestOnDefinitiveNode      (ID), // A definitive view request on a node that is already definitive
  DefinitiveRequestOnNodeWithContentChildren (ID), // A definitive view request on a node that has content (affectsParent=Container) children. Non-content children (e.g. containerward ancestry stubs) don't trigger this.
  MultipleDefinitiveRequestsForSameId    (ID), // Multiple definitive view requests for the same ID
  EmptyTitle                             (ID),
  LocalStructureViolation        (String, ID), // (error message, nearest ancestor ID)
  EditRequestOnWriteProtectedOccurrence      (ID), // Write-protected nodes -- phantoms in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit a definitive view of the node first.
  EditedWriteProtectedOccurrence {
    id      : ID,
    title   : String,
    changes : Vec<String>,
  }, // This occurrence was changed since the server rendered it, but a write-protected occurrence emits no save instruction for the changed data.
  FlagsSurfaceEdited {
    owner_id    : ID,
    owner_title : String,
    changes     : Vec<String>,
  },
  FlagEditOnForeignNode                 (ID, RepoName),
  FlagEditOnUnknownNode                 (ID),
  IDFolder_Edited                   (ID,       // owner of the IDFolder
                                  Vec<ID>,  // ids the buffer's IDFolder claims
                                  Vec<ID>), // the owner's real ids (pid + extra_ids); empty if the owner is not in the graph
  OverridesHere_Mismatch         (Option<ID>, // the carrier's own ID
                                  ID,         // the original the marker claims
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
      BufferValidationError::Body_of_Scaffold(title, kind) =>
        write!(f, "{} node should not have a body. Node title: '{}'",
               kind, title),
      BufferValidationError::Multiple_Defining_Viewnodes (id) =>
        write!(f, "Multiple occurrences of node {:?} are definitive", id),
      BufferValidationError::AmbiguousDeletion (id) =>
        write!(f, "Ambiguous deletion request for ID {:?}", id),
      BufferValidationError::DuplicatedContent (id) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", id),
      BufferValidationError::InconsistentRepos(id, repos) => {
        let repo_list: Vec<&RepoName> = repos . iter() . collect();
        write!(f, "Multiple viewnodes with ID {:?} have inconsistent repos: {:?}", id, repo_list) },
      BufferValidationError::ModifiedForeignNode(id, repo) =>
        write!(f, "Cannot modify node {:?} from foreign (write-protected) repo '{}'", id, repo),
      BufferValidationError::CreatedForeignNode(id, repo) =>
        write!(f, "Cannot create node {:?} in foreign (write-protected) repo '{}'", id, repo),
      BufferValidationError::CannotMoveToOrFromForeignRepo(id, disk_repo, buffer_repo) =>
        write!(f, "Cannot move node {:?} between repos '{}' and '{}': one or both are foreign (write-protected)", id, disk_repo, buffer_repo),
      BufferValidationError::CannotMoveAndMergeSimultaneously(id) =>
        write!(f, "Cannot move and merge node {:?} in the same save", id),
      BufferValidationError::RepoNotInConfig(id, repo) =>
        write!(f, "Node {:?} references repo '{}' which does not exist in config", id, repo),
      BufferValidationError::ForkRepoUnresolved(id) =>
        write!(f, "Cannot fork node {:?}: no owned repo to put the clone in. It has no owned ancestor in the view to inherit a repo from; set the clone's repo in the confirmation buffer (C-c s s).", id),
      BufferValidationError::ForkAlreadyExists(original, existing) =>
        write!(f, "Cannot fork node {:?}: you have already forked it. Your clone is {:?}. Edit that clone instead (a node may have at most one user-owned override).", original, existing),
      BufferValidationError::ForkRepoInactive(id, repo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is inactive under the current repo-set. Activate it first; an invisible clone is never created silently.", id, repo),
      BufferValidationError::ForkRepoNotOwned(id, repo) =>
        write!(f, "Cannot fork node {:?}: the clone's repo '{}' is not one you own. Choose an owned repo for the clone (C-c s s in the confirmation buffer).", id, repo),
      BufferValidationError::ForkRequestOnUnknownNode(id) =>
        write!(f, "Cannot fork node {:?}: it is not in the graph. Only a saved node can be forked; save it first, then fork.", id),
      BufferValidationError::ForkRequestMultiple(id) =>
        write!(f, "Multiple fork requests for the same node {:?}. At most one fork request per node is allowed.", id),
      BufferValidationError::OverrideInvariantViolation(msg) =>
        write!(f, "{}", msg),
      BufferValidationError::DefinitiveRequestOnDefinitiveNode (id) =>
        write!(f, "Definitive view request on a node that is already definitive (ID {:?}). The node already shows its content; no expansion needed.", id),
      BufferValidationError::DefinitiveRequestOnNodeWithContentChildren (id) =>
        write!(f, "Definitive view request on a node with content children (ID {:?}). The expansion would clobber those children. Save without the request first, then delete children and retry.", id),
      BufferValidationError::MultipleDefinitiveRequestsForSameId (id) =>
        write!(f, "Multiple definitive view requests for the same ID {:?}. At most one request per ID is allowed.", id),
      BufferValidationError::EmptyTitle(id) =>
        write!(f, "Node {:?} has an empty title. Every definitive node must have a non-empty title.", id),
      BufferValidationError::LocalStructureViolation(msg, id) =>
        write!(f, "Local structure violation at ID {:?}: {}", id, msg),
      BufferValidationError::IDFolder_Edited(owner, buffer_ids, real_ids) =>
        write!(f, "The idFolder under node {:?} was edited (buffer claims {:?}; real ids are {:?}). Reordering is fine, but IDs cannot be added, removed or edited through the buffer; edit the .skg file directly.", owner, buffer_ids, real_ids),
      BufferValidationError::EditRequestOnWriteProtectedOccurrence (id) =>
        write!(f, "Edit request on write-protected (phantom) node {:?}. Phantoms are write-protected; write-protected nodes cannot carry write instructions. Visit a definitive view of the node first (C-c g RET).", id),
      BufferValidationError::EditedWriteProtectedOccurrence {
        id, title, changes } =>
        write!(f, "The write-protected occurrence of node {:?} ({:?}) was edited ({}) but is write-protected. Re-render, then edit a definitive occurrence instead.",
               id, title, changes . join ("; ")),
      BufferValidationError::FlagsSurfaceEdited {
        owner_id, owner_title, changes } =>
        write!(f, "The flags surface under node {:?} ({:?}) was edited: {}. It is server-owned and no changes were saved. Use skg-set-flag-search-matching for noSearchMatching; provenance flags have no setter.",
               owner_id, owner_title, changes . join ("; ")),
      BufferValidationError::FlagEditOnForeignNode (id, repo) =>
        write! (f, "Cannot change flags of node {:?} from foreign repo '{}'; this gesture never creates an implicit fork.", id, repo),
      BufferValidationError::FlagEditOnUnknownNode (id) =>
        write! (f, "Cannot change flags of unsaved or unknown node {:?}; save the node first.", id),
      BufferValidationError::OverridesHere_Mismatch(carrier, original, effective) =>
        write!(f, "Node {:?} carries the marker (overridesHere {:?}), but it is not on the override chain of that original (which resolves to {:?}). The marker looks hand-edited or stale; saving it would rewrite a contains list. Re-render the view and retry.", carrier, original, effective),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
