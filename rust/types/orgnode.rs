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
  pub id: Option<ID>,

  /* Fields that dictate only how it is shown. */

  pub cycle: bool, // True if a node is in its own org-precedessors.
  pub focused: bool, // Where the cursor is. True for only one node.
  pub folded: bool, // folded in the Emacs org-mode sense
  pub relationships: OrgnodeRelationships,

  /* Fields that determine how it is treated. */

  pub treatment: Treatment,
  pub indefinitive: bool, // When a definitive org node is saved, its content determines the content of the corresponding node in the graph. When an *in*definitive node is saved, any of its org-content not currently in the graph are appended to the graph, but no content is removed. (I record the negative 'indefinitive', rather than the positive default 'definitive', to save characters in the buffer, because the default is omitted from metadata strings, and is much more common.)
  pub repeat: bool, /* The node already appears elsewhere in this buffer. PITFALL: We treat a node as indefinitive if 'repeat' OR 'indefinitive' is true.
TODO : Is this ugly? I don't want to have to keep 'repeat' and 'indefinitive' in sync. In Rust, the function 'change_repeated_to_indefinitive', which is called each time a buffer is saved, ensures that the server knows to treat each repeated node as indefinitive. But in the Emacs client, nothing encodes the relationship between the two fields. */
  pub toDelete: bool,
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

/// 'Treatment' describes how a node relates to its parent.
/// This influences both how the user should read it,
/// and how Rust should treat the data when it is saved.
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
    cycle : false,
    focused : false,
    folded : false,
    relationships : OrgnodeRelationships::default (),
    treatment : Treatment::Content,
    indefinitive : false,
    repeat : false,
    toDelete : false,
  }}
