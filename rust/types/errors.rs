use super::{ID, OrgNode};
use std::error::Error;
use std::io;

#[derive(Debug)]
pub enum HyperlinkParseError {
  InvalidFormat,
  MissingDivider,
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
  Other                          (String),
}


//
// Implementations
//

impl std::fmt::Display for HyperlinkParseError {
  fn fmt (
    &self,
    f: &mut std::fmt::Formatter <'_>
  ) -> std::fmt::Result {
    match self {
      HyperlinkParseError::InvalidFormat =>
        write! (
          f, "Invalid hyperlink format. Expected [[id:ID][LABEL]]" ),
      HyperlinkParseError::MissingDivider =>
        write! (
          f, "Missing divider between ID and label. Expected ][" ),
    } } }

impl Error for HyperlinkParseError {}
