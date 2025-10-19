use super::{ID, OrgNode};
use std::error::Error;
use std::io;


/////////////////
/// Types
/////////////////

/// Tells Rust what to do with a node.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeSaveAction {
  // PITFALL: It's nonsense if both of these are true.
  // The server will in that case delete,
  // so the indefinitive has no effect.
  pub indefinitive: bool, // An exception from normal treatment. Uusually, an org-node's content is taken to be equal to the corresponding node's conent. But if this field is true, the org-node's content is merely a (potentially improper, potentially empty) subset of the node's content.
  pub toDelete: bool,
}

#[derive(Debug)]
pub enum SaveError {
  ParseError(String),
  DatabaseError(Box<dyn Error>),
  IoError(io::Error),
  InconsistentInstructions {
    inconsistent_deletions: Vec<ID>,
    multiple_definers: Vec<ID>, },
  BufferValidationErrors ( Vec<Buffer_Cannot_Be_Saved> ), }

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
  Multiple_DefiningContainers    (ID), // For any given ID, at most one node with that ID can have repeated=false and indefinitive=false. (Its contents are intended to define those of the node.)
  AmbiguousDeletion              (ID),
  DuplicatedContent              (ID), // A node has multiple Content children with the same ID
}


/////////////////
/// Functions
/////////////////

impl std::fmt::Display for SaveError {
  fn fmt (
    &self,
    f : &mut std::fmt::Formatter<'_>
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
          multiple_definers) },
      SaveError::BufferValidationErrors(errors) => {
        write!(f, "Buffer validation errors: {} error(s) found", errors.len()) }} }}

impl std::error::Error for SaveError {
  fn source (
    &self
  ) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      SaveError::DatabaseError(err) => Some(err.as_ref()),
      SaveError::IoError(err) => Some(err),
      _ => None, }} }

/// Formats a SaveError as an org-mode buffer content for the client.
pub fn format_save_error_as_org (
  error : &SaveError
) -> String {
  match error {
    SaveError::ParseError(msg) => {
      format!("* NOTHING WAS SAVED\n\nParse error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}", msg)
    },
    SaveError::DatabaseError(err) => {
      format!("* NOTHING WAS SAVED\n\nDatabase error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::IoError(err) => {
      format!("* NOTHING WAS SAVED\n\nI/O error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::InconsistentInstructions {
      inconsistent_deletions, multiple_definers }
    => {
      let mut content : String =
        String::from("* NOTHING WAS SAVED\n\nInconsistencies found when interpreting buffer text as save instructions.\n\n");
      if !inconsistent_deletions.is_empty() {
        content.push_str("** Inconsistent Deletion Instructions\n");
        content.push_str("The following IDs have conflicting toDelete instructions:\n");
        for id in inconsistent_deletions {
          content.push_str(&format!("- {}\n", id.0)); }
        content.push('\n'); }
      if !multiple_definers.is_empty() {
        content.push_str("** Multiple Defining Containers\n");
        content.push_str("The following IDs are defined by multiple containers:\n");
        for id in multiple_definers {
          content.push_str(&format!("- {}\n", id.0)); }
        content.push('\n'); }
      content.push_str("** Resolution\n");
      content.push_str(
        "Please fix these inconsistencies and try saving again.\n");
      content },
    SaveError::BufferValidationErrors(errors) => {
      let mut content : String =
        String::from("* NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n");
      for (i, error) in errors.iter().enumerate() {
        content.push_str(&format!("** Error {}\n", i + 1));
        content.push_str(&format_buffer_validation_error(error));
        content.push('\n'); }
      content.push_str("** Resolution\n");
      content.push_str("Please fix these errors and try saving again.\n");
      content }} }

fn format_buffer_validation_error (
  error : &Buffer_Cannot_Be_Saved
) -> String {
  match error {
    Buffer_Cannot_Be_Saved::Body_of_AliasCol(node) => {
      format!("AliasCol node has a body (not allowed):\n- Title: {}\n", node.title)
    },
    Buffer_Cannot_Be_Saved::Child_of_AliasCol_with_ID(node) => {
      format!("Child of AliasCol has an ID (not allowed):\n- Title: {}\n", node.title)
    },
    Buffer_Cannot_Be_Saved::Body_of_Alias(node) => {
      format!("Alias node has a body (not allowed):\n- Title: {}\n", node.title)
    },
    Buffer_Cannot_Be_Saved::Child_of_Alias(node) => {
      format!("Alias node has children (not allowed):\n- Title: {}\n", node.title)
    },
    Buffer_Cannot_Be_Saved::Alias_with_no_AliasCol_Parent(node) => {
      format!("Alias node must have an AliasCol parent:\n- Title: {}\n", node.title)
    },
    Buffer_Cannot_Be_Saved::Multiple_AliasCols_in_Children(node) => {
      format!("Node has multiple AliasCol children (only one allowed):\n- Title: {}\n", node.title)
    },
    Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(id) => {
      format!("ID has multiple defining containers:\n- ID: {}\n", id.0)
    },
    Buffer_Cannot_Be_Saved::AmbiguousDeletion(id) => {
      format!("ID has ambiguous deletion instructions:\n- ID: {}\n", id.0)
    },
    Buffer_Cannot_Be_Saved::DuplicatedContent(id) => {
      format!("Node has multiple Content children with the same ID:\n- ID: {}\n", id.0)
    },
  }
}
