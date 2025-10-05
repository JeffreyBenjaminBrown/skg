use super::{ID,SkgNode};

pub type SaveInstruction = (SkgNode, NodeSaveAction);

/// Tells Rust what to do with a node.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeSaveAction {
  // PITFALL: It's nonsense if both of these are true.
  // The server will in that case delete,
  // so the mightContainMore has no effect.
  pub mightContainMore: bool, // An exception from normal treatment. Uusually, an org-node's content is taken to be equal to the corresponding node's conent. But if this field is true, the org-node's content is merely a (potentially improper, potentially empty) subset of the node's content.
  pub toDelete: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OrgNode2 {
  pub metadata: HeadlineMd2,
  pub title: String, // does not re-state the metadata
  pub body: Option<String>,
}

/* Each org headline corresponds to a node.
This is the metadata necessary to interpret the headline. */
#[derive(Debug, Clone, PartialEq)]
pub struct HeadlineMd2 {
  pub id: Option<ID>,
  pub relToOrgParent: RelToOrgParent2,
  pub cycle: bool,
  pub focused: bool,
  pub folded: bool,
  pub mightContainMore: bool,
  pub repeat: bool,
  pub toDelete: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelToOrgParent2 {
  Content, // The default relationship.
  Container, // For looking 'backward': The node contains its parent.
  AliasCol, // The node collects aliases for its parent.
  Alias, // The node is an alias for its grandparent.
  None, // The node bears no relationship to its parent.
}
