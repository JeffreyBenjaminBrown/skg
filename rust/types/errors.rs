use super::{ID, OrgNode, SourceNickname};
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
/// with any of these properties,, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum BufferValidationError {
  Body_of_AliasCol               (OrgNode),
  Child_of_AliasCol_with_ID      (OrgNode),
  Body_of_Alias                  (OrgNode),
  Child_of_Alias                 (OrgNode),
  Alias_with_no_AliasCol_Parent  (OrgNode),
  Multiple_AliasCols_in_Children (OrgNode),
  Multiple_DefiningContainers    (ID), // For any given ID, at most one node with that ID can have indefinitive=false. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
  InconsistentSources            (ID, HashSet<SourceNickname>), // Multiple orgnodes with same ID have different sources
  RootWithoutSource              (OrgNode), // Root node (top-level in forest) has no source
  ModifiedForeignNode            (ID, SourceNickname), // Attempted to modify a node from a foreign (read-only) source - (node_id, source_nickname)
  DiskSourceBufferSourceConflict (ID,
                                  SourceNickname, // disk source
                                  SourceNickname), // buffer source
  SourceNotInConfig              (ID, SourceNickname),
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
      BufferValidationError::Body_of_AliasCol(node) =>
        write!(f, "AliasCol node should not have a body. Node: {:?}",
               node.metadata.id),
      BufferValidationError::Child_of_AliasCol_with_ID(node) =>
        write!(f, "Children of AliasCol should not have IDs. Node: {:?}",
               node.metadata.id),
      BufferValidationError::Body_of_Alias(node) =>
        write!(f, "Alias node should not have a body. Node: {:?}",
               node.metadata.id),
      BufferValidationError::Child_of_Alias(node) =>
        write!(f, "Alias nodes should not have children. Node: {:?}",
               node.metadata.id),
      BufferValidationError::Alias_with_no_AliasCol_Parent(node) =>
        write!(f, "Alias node must have an AliasCol parent. Node: {:?}",
               node.metadata.id),
      BufferValidationError::Multiple_AliasCols_in_Children(node) =>
        write!(f, "Node has multiple AliasCol children (max 1 allowed). Node: {:?}",
               node.metadata.id),
      BufferValidationError::Multiple_DefiningContainers(id) =>
        write!(f, "Multiple nodes with ID {:?} are marked as defining (indefinitive=false)", id),
      BufferValidationError::AmbiguousDeletion(id) =>
        write!(f, "Ambiguous deletion request for ID {:?}", id),
      BufferValidationError::DuplicatedContent(id) =>
        write!(f, "Node has multiple Content children with the same ID {:?}", id),
      BufferValidationError::InconsistentSources(id, sources) => {
        let source_list: Vec<&SourceNickname> = sources.iter().collect();
        write!(f, "Multiple orgnodes with ID {:?} have inconsistent sources: {:?}", id, source_list) },
      BufferValidationError::RootWithoutSource(node) =>
        write!(f, "Root node (top-level in forest) must have a source. Node ID: {:?}, title: '{}'",
               node.metadata.id, node.title),
      BufferValidationError::ModifiedForeignNode(id, source) =>
        write!(f, "Cannot modify node {:?} from foreign (read-only) source '{}'", id, source),
      BufferValidationError::DiskSourceBufferSourceConflict(id, disk_source, buffer_source) =>
        write!(f, "Source mismatch for node {:?}: disk has '{}', buffer specifies '{}'", id, disk_source, buffer_source),
      BufferValidationError::SourceNotInConfig(id, source) =>
        write!(f, "Node {:?} references source '{}' which does not exist in config", id, source),
      BufferValidationError::Other(msg) =>
        write!(f, "{}", msg), }} }

impl Error for BufferValidationError {}
