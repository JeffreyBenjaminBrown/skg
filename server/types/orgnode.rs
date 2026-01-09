/// Types used by OrgNode (the new unified type in orgnode_new.rs).
///
/// OrgnodeViewData, // cycle, focused, folded, relationships
///   OrgnodeRelationships, // 2 bools, 3 opt ints
/// EditRequest, // merge | delete (mutually exclusive)
/// ViewRequest, // aliases | containerward | sourceward | definitive
///              // (NOT mutually exclusive)

use super::misc::ID;
use std::fmt;
use std::str::FromStr;

/// View-related metadata. It dictates only how the node is shown.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeViewData {
  // PITFALL: One could reasonably describe 'focused' and 'folded' as code
  // rather than data. They tell Emacs what to do. Once Emacs has done that,
  // it deletes them from the metadata. The other fields in this type are
  // only acted on to the extent that Emacs displays them.

  pub cycle: bool, // True if a node is in its own org-precedessors.
  pub focused: bool, // Where the cursor is. True for only one node.
  pub folded: bool, // folded in the Emacs org-mode sense
  pub relationships: OrgnodeRelationships,
}

/// These data only influence how the node is shown.
/// Editing them and then saving the buffer leaves the graph unchanged,
/// and the edits would be immediately lost,
/// as this data is regenerated each time the view is rebuilt.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeRelationships {
  pub parentIsContainer: bool,
  pub parentIsContent: bool,
  pub numContainers: Option<usize>,
  pub numContents: Option<usize>,
  pub numLinksIn: Option<usize>,
}

/// Requests for editing operations on a node.
/// Only one edit request is allowed per node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditRequest {
  Merge(ID), // The node with this request is the acquirer. The node with the ID that this request specifies is the acquiree.
  Delete, // request to delete this node
}

/// Requests for additional views related to a node.
/// Multiple view requests can be active simultaneously.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ViewRequest {
  Aliases,
  Containerward,
  Sourceward,
  Definitive,
}


//
// Implementations
//

impl fmt::Display for EditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      EditRequest::Merge(id) => write!(f, "(merge {})", id.0),
      EditRequest::Delete    => write!(f, "toDelete"),
    } } }

impl FromStr for EditRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "toDelete" => Ok ( EditRequest::Delete ),
      _ => {
        // Try to parse as "merge <id>"
        if let Some(id_str) = s.strip_prefix("merge ") {
          Ok ( EditRequest::Merge ( ID::from(id_str) ) )
        } else {
          Err ( format! ( "Unknown EditRequest value: {}", s ))
        }} }} }

impl fmt::Display for ViewRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      ViewRequest::Aliases       => write!(f, "aliases"),
      ViewRequest::Containerward => write!(f, "containerwardView"),
      ViewRequest::Sourceward    => write!(f, "sourcewardView"),
      ViewRequest::Definitive    => write!(f, "definitiveView"),
    } } }

impl FromStr for ViewRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "aliases"           => Ok ( ViewRequest::Aliases ),
      "containerwardView" => Ok ( ViewRequest::Containerward ),
      "sourcewardView"    => Ok ( ViewRequest::Sourceward ),
      "definitiveView"    => Ok ( ViewRequest::Definitive ),
      _ => Err ( format! ( "Unknown ViewRequest value: {}", s )),
    }} }

impl Default for OrgnodeRelationships {
  fn default () -> Self {
    OrgnodeRelationships {
      parentIsContainer : true,
      parentIsContent   : false,
      numContainers : Some ( 1 ),
      numContents   : Some ( 0 ),
      numLinksIn    : Some ( 0 ),
    }} }

impl Default for OrgnodeViewData {
  fn default () -> Self {
    OrgnodeViewData {
      cycle : false,
      focused : false,
      folded : false,
      relationships : OrgnodeRelationships::default (),
    }} }
