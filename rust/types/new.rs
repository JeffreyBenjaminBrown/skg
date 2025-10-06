use super::{ID,SkgNode};
use std::fmt;
use std::str::FromStr;

pub type SaveInstruction = (SkgNode, NodeSaveAction);

/// Tells Rust what to do with a node.
#[derive(Clone, Copy, Debug, PartialEq)]
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
  SearchResult, // When the user searches for title/alias text, each hit is one of these. If, somehow, Rust finds a SearchResult in a saved org buffer, it ignores it, including all of its recursive content.
  None, // The node bears no relationship to its parent.
}

//
// Implementations
//

impl fmt::Display for RelToOrgParent2 {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    let s : &str =
      match self {
        RelToOrgParent2::Content => "content",
        RelToOrgParent2::Container => "container",
        RelToOrgParent2::AliasCol => "aliasCol",
        RelToOrgParent2::Alias => "alias",
        RelToOrgParent2::SearchResult => "searchResult",
        RelToOrgParent2::None => "none",
      };
    write! ( f, "{}", s ) } }

impl FromStr for RelToOrgParent2 {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "content" => Ok ( RelToOrgParent2::Content ),
      "container" => Ok ( RelToOrgParent2::Container ),
      "aliasCol" => Ok ( RelToOrgParent2::AliasCol ),
      "alias" => Ok ( RelToOrgParent2::Alias ),
      "searchResult" => Ok ( RelToOrgParent2::SearchResult ),
      "none" => Ok ( RelToOrgParent2::None ),
      _ => Err ( format! ( "Unknown RelToOrgParent2 value: {}", s )),
    }} }

/// Renders HeadlineMd2 as a metadata string suitable for org-mode display.
/// This is the inverse of parse_metadata_to_orgNodeMd2.
/// Returns string like "id:abc123,repeated,focused" etc.
pub fn headlinemd2_to_string (
  metadata : &HeadlineMd2
) -> String {
  let mut parts : Vec<String> =
    Vec::new ();
  if let Some ( ref id ) = metadata.id {
    parts.push ( format! ( "id:{}", id.0 )); }
  if metadata.relToOrgParent != RelToOrgParent2::Content {
    parts.push ( format! ( "relToOrgParent:{}", metadata.relToOrgParent )); }
  if metadata.repeat {
    parts.push ( "repeated".to_string () ); }
  if metadata.folded {
    parts.push ( "folded".to_string () ); }
  if metadata.focused {
    parts.push ( "focused".to_string () ); }
  if metadata.cycle {
    parts.push ( "cycle".to_string () ); }
  if metadata.mightContainMore {
    parts.push ( "mightContainMore".to_string () ); }
  if metadata.toDelete {
    parts.push ( "toDelete".to_string () ); }
  parts.join ( "," ) }
