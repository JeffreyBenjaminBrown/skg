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

/// Each org headline corresponds to a node.
/// This is the metadata necessary to interpret the headline.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeMetadata {
  // TODO ? These fields could be absorbed into the definition of OrgNode. On the other hand, it's nice to be able to talk about 'the metadata'.
  pub id: Option<ID>,
  pub viewData: OrgnodeViewData,
  pub code: OrgnodeCode,
}

/* View-related metadata: fields that dictate only how the node is shown. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeViewData {
  pub cycle: bool, // True if a node is in its own org-precedessors.
  pub focused: bool, // Where the cursor is. True for only one node.
  pub folded: bool, // folded in the Emacs org-mode sense
  pub relationships: OrgnodeRelationships,
}

/// These data only influence how the node is shown.
/// Editing them and saving has no effect on the graph.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeRelationships {
  pub parentIsContainer: bool,
  pub parentIsContent: bool,
  pub numContainers: Option<usize>,
  pub numContents: Option<usize>,
  pub numLinksIn: Option<usize>,
}

/// These data determine how the node is treated when saved.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeCode {
  pub relToParent: RelToParent,
  pub indefinitive: bool, // When a definitive org node is saved, its content determines the content of the corresponding node in the graph. When an *in*definitive node is saved, any of its org-content not currently in the graph are appended to the graph, but no content is removed. (I record the negative 'indefinitive', rather than the positive default 'definitive', to save characters in the buffer, because the default is omitted from metadata strings, and is much more common.)
  pub repeat: bool, /* Implies the same treatment as 'indefinitive', but also implies that the node already appears elsewhere in this buffer.
PITFALL: We treat a node as indefinitive if 'repeat' OR 'indefinitive' is true. But the only time it is marked as 'repeat' is when Rust builds a content view and discovers the node has already been rendered. By contrast, when a node marked 'repeat' is saved, since the user may have edited it, the 'repeat' label is replaced with 'indefinitive'. And 'repeat' detection is skipped entirely for 'indefinitive' nodes, so it will thereafter appear without the 'repeat' marker.
TODO : Is this ugly? I don't want to have to keep 'repeat' and 'indefinitive' in sync. In Rust, the function 'change_repeated_to_indefinitive', which is called each time a buffer is saved, ensures that the server knows to treat each repeated node as indefinitive. But in the Emacs client, nothing encodes the relationship between the two fields. */
  pub toDelete: bool,
}

/// 'RelToParent' describes how a node relates to its parent.
/// It influences both how the user should read it,
/// and how Rust should treat the data when it is saved.
/// PITFALL: It does not describe every potential relationship
/// between the node and its parent.
#[derive(Debug, Clone, PartialEq)]
pub enum RelToParent {
  Content, // The default relationship: parents 'contain' most children.
  AliasCol, // The node collects aliases for its parent.
  Alias, // The node is an alias for its grandparent.
  ParentIgnores, // This node is not used to update its parent. (That does *not* mean it is ignored when the buffer is saved. It and its recursive org-content are processed normally. It only means it has no impact on its parent.)
}

//
// Implementations
//

impl fmt::Display for RelToParent {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    let s : &str =
      match self {
        RelToParent::Content => "content",
        RelToParent::AliasCol => "aliasCol",
        RelToParent::Alias => "alias",
        RelToParent::ParentIgnores => "parentIgnores",
      };
    write! ( f, "{}", s ) } }

impl FromStr for RelToParent {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "content"       => Ok ( RelToParent::Content ),
      "aliasCol"      => Ok ( RelToParent::AliasCol ),
      "alias"         => Ok ( RelToParent::Alias ),
      "parentIgnores" => Ok ( RelToParent::ParentIgnores ),
      _ => Err ( format! ( "Unknown RelToParent value: {}", s )),
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

impl Default for OrgnodeCode {
  fn default () -> Self {
    OrgnodeCode {
      relToParent : RelToParent::Content,
      indefinitive : false,
      repeat : false,
      toDelete : false,
    }} }

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    viewData : OrgnodeViewData::default (),
    code : OrgnodeCode::default (),
  }}
