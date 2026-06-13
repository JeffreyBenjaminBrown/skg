use super::misc::{ID, SourceName};
use std::error::Error;
use std::io;
use std::collections::HashSet;

#[derive (Debug)]
pub enum TextLinkParseError {
  InvalidFormat,
  MissingDivider,
}

#[derive (Debug)]
pub enum SaveError {
  ParseError (String),
  DatabaseError(Box<dyn Error>),
  IoError (io::Error),
  // A failed save carries, alongside its errors, the nonfatal
  // warnings collected before the abort (e.g. discarded col-headline
  // text). Decided 2026-06-12: warnings always accompany errors.
  BufferValidationErrors {
    errors   : Vec<BufferValidationError>,
    warnings : Vec<String>, }, }

/// If the user attempts to save a buffer
/// with any of these properties, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
pub enum BufferValidationError {
  Body_of_Scaffold               (String,   // Title from buffer
                                  String),  // Scaffold kind (e.g. "aliasCol", "alias")
  Multiple_Defining_Viewnodes     (ID), // For any given ID, at most one node with that ID can have indefinitive=false. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  InconsistentSources            (ID, HashSet<SourceName>), // Multiple viewnodes with same ID have different sources
  ModifiedForeignNode            (ID, SourceName), // Attempted to modify a node from a foreign (read-only) source - (node_id, source_name)
  CreatedForeignNode             (ID, SourceName), // Attempted to create a node in a foreign (read-only) source - (node_id, source_name)
  CannotMoveToOrFromForeignSource (ID,
                                   SourceName, // disk source
                                   SourceName), // buffer source
  CannotMoveAndMergeSimultaneously (ID),
  SourceNotInConfig              (ID, SourceName),
  // Fork errors. Editing a foreign node N is read as a request to
  // clone it (the clone C lives in an owned source, subscribes to and
  // overrides N). These are the ways that request can be refused.
  ForkSourceUnresolved           (ID), // N's pid: no OWNED vognode ancestor in the view to inherit C's source from, and the user set none in the confirmation buffer.
  ForkAlreadyExists              (ID,   // N's pid
                                  ID),  // the existing user-owned clone that already overrides N (monogamy: a node may have at most one user-owned overrider)
  ForkSourceInactive             (ID,           // N's pid
                                  SourceName),  // C's resolved owned source, which is INACTIVE under the active source-set
  ForkSourceNotOwned             (ID,           // N's pid
                                  SourceName),  // C's chosen source, which the user does NOT own (a typed or hand-edited source the rotation would never offer)
  OverrideInvariantViolation     (String),
  DefinitiveRequestOnDefinitiveNode      (ID), // A definitive view request on a node that is already definitive
  DefinitiveRequestOnNodeWithContentChildren (ID), // A definitive view request on a node that has content (parentIs=Container) children. Non-content children (e.g. containerward ancestry stubs) don't trigger this.
  MultipleDefinitiveRequestsForSameId    (ID), // Multiple definitive view requests for the same ID
  EmptyTitle                             (ID),
  LocalStructureViolation        (String, ID), // (error message, nearest ancestor ID)
  EditRequestOnIndefinitive      (ID), // Indefinitive (read-only) nodes -- phantoms in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit a definitive view of the node first.
  IDCol_Edited                   (ID,       // owner of the IDCol
                                  Vec<ID>,  // ids the buffer's IDCol claims
                                  Vec<ID>), // the owner's real ids (pid + extra_ids); empty if the owner is not in the graph
  OverridesHere_Mismatch         (Option<ID>, // the carrier's own ID
                                  ID,         // the original the marker claims
                                  Option<ID>), // who the graph says substitutes for that original; None if the graph was unavailable
  Other                          (String),
}


//
// Implementations
//

impl std::fmt::Display for TextLinkParseError {
  fn fmt (
    &self,
    f: &mut std::fmt::Formatter <'_>
  ) -> std::fmt::Result {
    match self {
      TextLinkParseError::InvalidFormat =>
        write! (
          f, "Invalid textlink format. Expected [[id:ID][LABEL]]" ),
      TextLinkParseError::MissingDivider =>
        write! (
          f, "Missing divider between ID and label. Expected ][" ),
    } } }

impl Error for TextLinkParseError {}

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
        write!(f, "Multiple nodes with ID {:?} are marked as defining (indefinitive=false)", id),
      BufferValidationError::AmbiguousDeletion (id) =>
        write!(f, "Ambiguous deletion request for ID {:?}", id),
      BufferValidationError::DuplicatedContent (id) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", id),
      BufferValidationError::InconsistentSources(id, sources) => {
        let source_list: Vec<&SourceName> = sources . iter() . collect();
        write!(f, "Multiple viewnodes with ID {:?} have inconsistent sources: {:?}", id, source_list) },
      BufferValidationError::ModifiedForeignNode(id, source) =>
        write!(f, "Cannot modify node {:?} from foreign (read-only) source '{}'", id, source),
      BufferValidationError::CreatedForeignNode(id, source) =>
        write!(f, "Cannot create node {:?} in foreign (read-only) source '{}'", id, source),
      BufferValidationError::CannotMoveToOrFromForeignSource(id, disk_source, buffer_source) =>
        write!(f, "Cannot move node {:?} between sources '{}' and '{}': one or both are foreign (read-only)", id, disk_source, buffer_source),
      BufferValidationError::CannotMoveAndMergeSimultaneously(id) =>
        write!(f, "Cannot move and merge node {:?} in the same save", id),
      BufferValidationError::SourceNotInConfig(id, source) =>
        write!(f, "Node {:?} references source '{}' which does not exist in config", id, source),
      BufferValidationError::ForkSourceUnresolved(id) =>
        write!(f, "Cannot fork node {:?}: no owned source to put the clone in. It has no owned ancestor in the view to inherit a source from; set the clone's source in the confirmation buffer (C-c s s).", id),
      BufferValidationError::ForkAlreadyExists(original, existing) =>
        write!(f, "Cannot fork node {:?}: you have already forked it. Your clone is {:?}. Edit that clone instead (a node may have at most one user-owned override).", original, existing),
      BufferValidationError::ForkSourceInactive(id, source) =>
        write!(f, "Cannot fork node {:?}: the clone's source '{}' is inactive under the current source-set. Activate it first; an invisible clone is never created silently.", id, source),
      BufferValidationError::ForkSourceNotOwned(id, source) =>
        write!(f, "Cannot fork node {:?}: the clone's source '{}' is not one you own. Choose an owned source for the clone (C-c s s in the confirmation buffer).", id, source),
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
      BufferValidationError::IDCol_Edited(owner, buffer_ids, real_ids) =>
        write!(f, "The idCol under node {:?} was edited (buffer claims {:?}; real ids are {:?}). Reordering is fine, but IDs cannot be added, removed or edited through the buffer; edit the .skg file directly.", owner, buffer_ids, real_ids),
      BufferValidationError::EditRequestOnIndefinitive (id) =>
        write!(f, "Edit request on indefinitive (phantom) node {:?}. Phantoms are indefinitive; indefinitive nodes cannot carry write instructions. Visit a definitive view of the node first (C-c g RET).", id),
      BufferValidationError::OverridesHere_Mismatch(carrier, original, effective) =>
        write!(f, "Node {:?} carries the marker (overridesHere {:?}), but the server would draw {:?} in place of that original. The marker looks hand-edited or stale; saving it would rewrite a contains list. Re-render the view and retry.", carrier, original, effective),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
