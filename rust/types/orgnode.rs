use serde::{Serialize, Deserialize};

use super::skgnode::NodeWithEphem;

/// Raw text from Emacs is first loaded as a forest of these.
#[derive(Clone, Debug, PartialEq)]
pub struct OrgNode {
  pub title    : String,         // The title part of the headline (after asterisks and metadata). 'Headline' is a term from org-mode.
  pub body     : Option<String>, // "body" is a term fron org-mode
  pub branches : Vec<OrgNode>, 
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum OrgNodeInterp {
  // Tells Rust how to interpret -- what to do with -- an OrgNode.
  // Each org node's relationship to its org-container is determined by which of these it is. Thus org-container can relate differently to its different org-children.
  Content(NodeWithEphem),
  Aliases(Vec<String>),
}

/// Enums (safer than strings) corresponding to
/// the constructors of OrgNodeInterp.
#[derive(Debug, Clone, PartialEq)]
pub enum OrgNodeInterpEnum {
  NodeWithEphem,
  Aliases, 
}

//
// Implementations
//

impl Default for OrgNodeInterpEnum {
  fn default() -> Self {
    OrgNodeInterpEnum::NodeWithEphem 
  }
}

impl OrgNodeInterp {
  pub fn content (content_node: NodeWithEphem) -> Self {
    OrgNodeInterp::Content (content_node) 
  }
  pub fn aliases (alias_list: Vec<String>) -> Self {
    OrgNodeInterp::Aliases (alias_list) 
  }
  pub fn is_content (&self) -> bool {
    matches! (self, OrgNodeInterp::Content(_)) 
  }
  pub fn is_aliases (&self) -> bool {
    matches! (self, OrgNodeInterp::Aliases(_)) 
  }
}