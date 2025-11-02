use super::{ID, SkgNode, NodeSaveAction};
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

pub type SaveInstruction = (SkgNode, NodeSaveAction);

/// When an 'acquiree' merges into an 'acquirer',
/// we need three SaveInstructions.
#[derive(Debug, Clone)]
pub struct MergeInstructionTriple {
  pub acquiree_text_preserver : SaveInstruction, // new node with acquiree's title and body
  pub updated_acquirer        : SaveInstruction, // acquirer with acquiree's IDs, contents, and relationships merged in. (This is complex; see 'three_merged_skgnodes'.)
  pub acquiree_to_delete      : SaveInstruction,
}

impl MergeInstructionTriple {
  pub fn to_vec (
    &self
  ) -> Vec<SaveInstruction> {
    vec![
      self.acquiree_text_preserver.clone(),
      self.updated_acquirer.clone(),
      self.acquiree_to_delete.clone(),
    ] }

  pub fn acquirer_id (
    &self
  ) -> &ID {
    &self.updated_acquirer.0.ids[0] }

  pub fn acquiree_id (
    &self
  ) -> &ID {
    &self.acquiree_to_delete.0.ids[0] }

  pub fn preserver_id (
    &self
  ) -> &ID {
    &self.acquiree_text_preserver.0.ids[0] }
}

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
  pub nodeRequests: HashSet<NodeRequest>,
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

/// Requests for additional information or views related to a node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeRequest {
  ContainerwardView,
  SourcewardView,
  Merge(ID),  // Request to merge another node into this one
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

impl fmt::Display for NodeRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      NodeRequest::ContainerwardView => write!(f, "containerwardView"),
      NodeRequest::SourcewardView    => write!(f, "sourcewardView"),
      NodeRequest::Merge(id)         => write!(f, "(merge {})", id.0),
    } } }

impl FromStr for NodeRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "containerwardView" => Ok ( NodeRequest::ContainerwardView ),
      "sourcewardView"    => Ok ( NodeRequest::SourcewardView ),
      _ => {
        // Try to parse as "merge <id>"
        if let Some(id_str) = s.strip_prefix("merge ") {
          Ok ( NodeRequest::Merge ( ID::from(id_str) ) )
        } else {
          Err ( format! ( "Unknown NodeRequest value: {}", s ))
        }} }} }

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
      nodeRequests : HashSet::new (),
    }} }

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    viewData : OrgnodeViewData::default (),
    code : OrgnodeCode::default (),
  }}
