use super::{ID, SkgNode, NodeSaveAction};
use std::fmt;
use std::str::FromStr;

pub type SaveInstruction = (SkgNode, NodeSaveAction);

#[derive(Debug, Clone, PartialEq)]
pub struct OrgNode {
  pub metadata: OrgnodeMetadata,
  pub title: String, // does not re-state the metadata
  pub body: Option<String>,
}

/* Each org headline corresponds to a node.
This is the metadata necessary to interpret the headline. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeMetadata {
  pub id: Option<ID>,
  pub treatment: Treatment,
  pub cycle: bool,
  pub focused: bool,
  pub folded: bool,
  pub mightContainMore: bool,
  pub repeat: bool,
  pub toDelete: bool,
  pub relationships: OrgnodeRelationships,
}

/* Relationship metadata about a node's connections in the graph. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeRelationships {
  pub parentIsContainer: bool,
  pub parentIsContent: bool,
  pub numContainers: Option<usize>,
  pub numContents: Option<usize>,
  pub numLinksIn: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Treatment {
  Content, // The default relationship.
  AliasCol, // The node collects aliases for its parent.
  Alias, // The node is an alias for its grandparent.
  ParentIgnores, // This node is not used to update its parent. (That does *not* mean it is ignored when the buffer is saved. It and its recursive org-content are processed normally. It only means it has no impact on its parent.)
}

//
// Implementations
//

impl fmt::Display for Treatment {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    let s : &str =
      match self {
        Treatment::Content => "content",
        Treatment::AliasCol => "aliasCol",
        Treatment::Alias => "alias",
        Treatment::ParentIgnores => "parentIgnores",
      };
    write! ( f, "{}", s ) } }

impl FromStr for Treatment {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "content" => Ok ( Treatment::Content ),
      "aliasCol" => Ok ( Treatment::AliasCol ),
      "alias" => Ok ( Treatment::Alias ),
      "parentIgnores" => Ok ( Treatment::ParentIgnores ),
      _ => Err ( format! ( "Unknown Treatment value: {}", s )),
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

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    treatment : Treatment::Content,
    cycle : false,
    focused : false,
    folded : false,
    mightContainMore : false,
    repeat : false,
    toDelete : false,
    relationships : OrgnodeRelationships::default (),
  }}
