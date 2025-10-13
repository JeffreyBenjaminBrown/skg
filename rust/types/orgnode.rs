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
  pub relToOrgParent: RelToOrgParent,
  pub cycle: bool,
  pub focused: bool,
  pub folded: bool,
  pub mightContainMore: bool,
  pub repeat: bool,
  pub toDelete: bool,
  pub parentIsContainer: bool,
  pub parentIsContent: bool,
  pub numContainers: Option<usize>,
  pub numContents: Option<usize>,
  pub numLinksIn: Option<usize>,
}

/* PITFALL:
RelToOrgParent is not necessarily the only relationship
bewteen a node and its parent.
It has two uses: to make clear to the user one way they relate,
and to instruct the server what to do when the buffer is saved. */
#[derive(Debug, Clone, PartialEq)]
pub enum RelToOrgParent {
  Content, // The default relationship.
  Container, // For looking 'backward': The node contains its parent.
  AliasCol, // The node collects aliases for its parent.
  Alias, // The node is an alias for its grandparent.
  SearchResult, // When the user searches for title/alias text, each hit is one of these. If, somehow, Rust finds a SearchResult in a saved org buffer, it ignores it, including all of its recursive content.
  None, // The node bears no relationship to its parent.
}

//
// Implementations
//

impl fmt::Display for RelToOrgParent {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    let s : &str =
      match self {
        RelToOrgParent::Content => "content",
        RelToOrgParent::Container => "container",
        RelToOrgParent::AliasCol => "aliasCol",
        RelToOrgParent::Alias => "alias",
        RelToOrgParent::SearchResult => "searchResult",
        RelToOrgParent::None => "none",
      };
    write! ( f, "{}", s ) } }

impl FromStr for RelToOrgParent {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "content" => Ok ( RelToOrgParent::Content ),
      "container" => Ok ( RelToOrgParent::Container ),
      "aliasCol" => Ok ( RelToOrgParent::AliasCol ),
      "alias" => Ok ( RelToOrgParent::Alias ),
      "searchResult" => Ok ( RelToOrgParent::SearchResult ),
      "none" => Ok ( RelToOrgParent::None ),
      _ => Err ( format! ( "Unknown RelToOrgParent value: {}", s )),
    }} }

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    relToOrgParent : RelToOrgParent::Content,
    cycle : false,
    focused : false,
    folded : false,
    mightContainMore : false,
    repeat : false,
    toDelete : false,
    parentIsContainer : false,
    parentIsContent : false,
    numContainers : None,
    numContents : None,
    numLinksIn : None,
  } }
