use super::misc::{ID, SourceNickname};
use std::error::Error;
use std::io;
use std::collections::HashSet;

#[derive(Debug)]
pub enum TextLinkParseError {
  InvalidFormat,
  MissingDivider,
}

#[derive(Debug)]
pub enum SaveError {
  ParseError(String),
  DatabaseError(Box<dyn Error>),
  IoError(io::Error),
  BufferValidationErrors ( Vec<BufferValidationError> ), }

/// If the user attempts to save a buffer
/// with any of these properties, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum BufferValidationError {
  Body_of_Scaffold               (String,   // Title from buffer
                                  String),  // Scaffold kind (e.g. "aliasCol", "alias")
  Multiple_Defining_Orgnodes     (ID), // For any given ID, at most one node with that ID can have indefinitive=false. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  InconsistentSources            (ID, HashSet<SourceNickname>), // Multiple orgnodes with same ID have different sources
  ModifiedForeignNode            (ID, SourceNickname), // Attempted to modify a node from a foreign (read-only) source - (node_id, source_nickname)
  DiskSourceBufferSourceConflict (ID,
                                  SourceNickname, // disk source
                                  SourceNickname), // buffer source
  SourceNotInConfig              (ID, SourceNickname),
  DefinitiveRequestOnDefinitiveNode      (ID), // A definitive view request on a node that is already definitive
  DefinitiveRequestOnNodeWithChildren    (ID), // A definitive view request on a node that has children
  MultipleDefinitiveRequestsForSameId    (ID), // Multiple definitive view requests for the same ID
  LocalStructureViolation        (String, ID), // (error message, nearest ancestor ID)
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
      BufferValidationError::Multiple_Defining_Orgnodes(id) =>
        write!(f, "Multiple nodes with ID {:?} are marked as defining (indefinitive=false)", id),
      BufferValidationError::AmbiguousDeletion(id) =>
        write!(f, "Ambiguous deletion request for ID {:?}", id),
      BufferValidationError::DuplicatedContent(id) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", id),
      BufferValidationError::InconsistentSources(id, sources) => {
        let source_list: Vec<&SourceNickname> = sources.iter().collect();
        write!(f, "Multiple orgnodes with ID {:?} have inconsistent sources: {:?}", id, source_list) },
      BufferValidationError::ModifiedForeignNode(id, source) =>
        write!(f, "Cannot modify node {:?} from foreign (read-only) source '{}'", id, source),
      BufferValidationError::DiskSourceBufferSourceConflict(id, disk_source, buffer_source) =>
        write!(f, "Source mismatch for node {:?}: disk has '{}', buffer specifies '{}'", id, disk_source, buffer_source),
      BufferValidationError::SourceNotInConfig(id, source) =>
        write!(f, "Node {:?} references source '{}' which does not exist in config", id, source),
      BufferValidationError::DefinitiveRequestOnDefinitiveNode(id) =>
        write!(f, "Definitive view request on a node that is already definitive (ID {:?}). The node already shows its content; no expansion needed.", id),
      BufferValidationError::DefinitiveRequestOnNodeWithChildren(id) =>
        write!(f, "Definitive view request on a node with children (ID {:?}). The expansion would clobber those children. Save without the request first, then delete children and retry.", id),
      BufferValidationError::MultipleDefinitiveRequestsForSameId(id) =>
        write!(f, "Multiple definitive view requests for the same ID {:?}. At most one request per ID is allowed.", id),
      BufferValidationError::LocalStructureViolation(msg, id) =>
        write!(f, "Local structure violation at ID {:?}: {}", id, msg),
      BufferValidationError::Other(msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
