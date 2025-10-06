use super::{ID, OrgNode};
use std::error::Error;
use std::io;

/// Tells Rust what to do with a node.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeSaveAction {
  // PITFALL: It's nonsense if both of these are true.
  // The server will in that case delete,
  // so the mightContainMore has no effect.
  pub mightContainMore: bool, // An exception from normal treatment. Uusually, an org-node's content is taken to be equal to the corresponding node's conent. But if this field is true, the org-node's content is merely a (potentially improper, potentially empty) subset of the node's content.
  pub toDelete: bool,
}

#[derive(Debug)]
pub enum SaveError {
  ParseError(String),
  DatabaseError(Box<dyn Error>),
  IoError(io::Error),
  InconsistentInstructions {
    inconsistent_deletions: Vec<ID>,
    multiple_definers: Vec<ID>, }, }

/// If the user attempts to save a buffer
/// with any of these properties,, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Buffer_Cannot_Be_Saved {
  Body_of_AliasCol               (OrgNode),
  Child_of_AliasCol_with_ID      (OrgNode),
  Body_of_Alias                  (OrgNode),
  Child_of_Alias                 (OrgNode),
  Alias_with_no_AliasCol_Parent  (OrgNode),
  Multiple_AliasCols_in_Children (OrgNode),
  Multiple_DefiningContainers    (ID), // For any given ID, at most one node with that ID can have repeated=false and mightContainMore=false. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
}

impl std::fmt::Display for SaveError {
  fn fmt ( &self,
            f: &mut std::fmt::Formatter<'_>
  ) -> std::fmt::Result {
    match self {
      SaveError::ParseError(msg) =>
        write!(f, "Parse error: {}", msg),
      SaveError::DatabaseError(err) =>
        write!(f, "Database error: {}", err),
      SaveError::IoError(err) =>
        write!(f, "IO error: {}", err),
      SaveError::InconsistentInstructions {
        inconsistent_deletions, multiple_definers } => {
        write!(
          f,
          "Inconsistent deletions: {:?} or multiple definers: {:?}",
          inconsistent_deletions,
          multiple_definers) }} }}

impl std::error::Error for SaveError {
  fn source(&self
  ) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      SaveError::DatabaseError(err) => Some(err.as_ref()),
      SaveError::IoError(err) => Some(err),
      _ => None, }} }
