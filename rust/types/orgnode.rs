use super::ID;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

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
  pub id: Option<ID>,
  pub source: Option<String>,
  pub viewData: OrgnodeViewData,
  pub code: OrgnodeCode,
}

/* View-related metadata: fields that dictate only how the node is shown. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeViewData {
  // PITFALL: One could reasonably describe 'focused' and 'folded' as code rather than data. They tell Emacs what to do. Once Emacs has done that, it deletes them from the metadata. The other fields in this type are only acted on to the extent that Emacs displays them.

  pub cycle: bool, // True if a node is in its own org-precedessors.
  pub focused: bool, // Where the cursor is. True for only one node.
  pub folded: bool, // folded in the Emacs org-mode sense
  pub repeat: bool, // Implies that the node already appears elsewhere in this buffer.
    // PITFALL: repeated => indefinitive. (At least it should, and I think the code adheres to that.)
  pub relationships: OrgnodeRelationships,
}

/* These data only influence how the node is shown.
Editing them and then saving the buffer leaves the graph unchanged,
and the edits would be immediately lost,
as this data is regenerated each time the view is rebuilt. */
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
  pub indefinitive: bool, // A definitive node defines the title, body and initial contents, if present. Otherwise those things are taken from disk. The indefinitive nodes can append content, if they have any novel contents (where novel = not already contained by the target node).
  // Thus 'indefinitive' defines, among other things, a node's relationship to its children -- at least, to those children that are content (i.e. with relToParent=Content). (Any orgnode, indefinitive or otherwise, can define aliases. I haven't decided whether that will be true of the three sharing relationsips -- subscription, hides and overrides.)
  // (On word choice: I record the negative 'indefinitive', rather than the positive default 'definitive', to save characters in the buffer, because the default is omitted from metadata strings, and is much more common.)
  pub editRequest: Option<EditRequest>,
  pub viewRequests: HashSet<ViewRequest>,
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
  ContainerwardView,
  SourcewardView,
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
      ViewRequest::ContainerwardView => write!(f, "containerwardView"),
      ViewRequest::SourcewardView    => write!(f, "sourcewardView"),
    } } }

impl FromStr for ViewRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "containerwardView" => Ok ( ViewRequest::ContainerwardView ),
      "sourcewardView"    => Ok ( ViewRequest::SourcewardView ),
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
      repeat : false,
      relationships : OrgnodeRelationships::default (),
    }} }

impl Default for OrgnodeCode {
  fn default () -> Self {
    OrgnodeCode {
      relToParent : RelToParent::Content,
      indefinitive : false,
      editRequest : None,
      viewRequests : HashSet::new (),
    }} }

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    source : None,
    viewData : OrgnodeViewData::default (),
    code : OrgnodeCode::default (),
  }}
