use super::ID;

/// Tells Rust what to do with a node.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeSaveAction {
  pub mightContainMore: bool,
  pub to_delete: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OrgNode2 {
  pub metadata: OrgNodeMd2,
  pub title: String, // does not re-state the metadata
  pub body: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OrgNodeMd2 {
  pub id: Option<ID>,
  pub relToOrgParent: RelToOrgParent2,
  pub cycle: bool,
  pub focused: bool,
  pub folded: bool,
  pub mightContainMore: bool,
  pub repeat: bool,
  pub to_delete: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelToOrgParent2 {
  Content,
  Container,
  AliasCol,
  Alias,
  None,
}
