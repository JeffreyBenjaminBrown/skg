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
  BufferValidationErrors ( Vec<BufferValidationError> ), }

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
  OverrideInvariantViolation     (String),
  DefinitiveRequestOnDefinitiveNode      (ID), // A definitive view request on a node that is already definitive
  DefinitiveRequestOnNodeWithContentChildren (ID), // A definitive view request on a node that has content (parentIs=Container) children. Non-content children (e.g. containerward ancestry stubs) don't trigger this.
  MultipleDefinitiveRequestsForSameId    (ID), // Multiple definitive view requests for the same ID
  EmptyTitle                             (ID),
  LocalStructureViolation        (String, ID), // (error message, nearest ancestor ID)
  EditRequestOnIndefinitive      (ID), // Indefinitive (read-only) nodes -- phantoms in particular -- cannot carry write instructions like (editRequest delete) or (editRequest (merge X)). The user must visit a definitive view of the node first.
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
      BufferValidationError::EditRequestOnIndefinitive (id) =>
        write!(f, "Edit request on indefinitive (phantom) node {:?}. Phantoms are indefinitive; indefinitive nodes cannot carry write instructions. Visit a definitive view of the node first (C-c g RET).", id),
      BufferValidationError::Other (msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
