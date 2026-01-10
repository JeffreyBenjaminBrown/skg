/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'SkgNode' type.
/// Nodes of the tree are represented via the 'OrgNode' type.
///   (That name might change once there are more clients. The only client so far is written in Emacs org-mode; hence the name.)
/// Some 'OrgNode's correspond to SkgNodes; these are 'TrueNode's.
/// Others do not so correspond, but rather encode information about neighboring tree nodes. These are 'Scaffold' nodes.

use super::misc::ID;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

//
// Type declarations
//

/// Corresponds to an Emacs headline-body pair.
#[derive( Debug, Clone, PartialEq )]
pub struct OrgNode {
  pub focused : bool,
  pub folded  : bool,
  pub kind    : OrgNodeKind,
}

#[derive( Debug, Clone, PartialEq )]
pub enum OrgNodeKind {
  True  ( TrueNode ),
  Scaff ( Scaffold ),
}

/// An OrgNode that corresponds to a SkgNode.
#[derive( Debug, Clone, PartialEq )]
pub struct TrueNode {
  pub title            : String,
  pub body             : Option < String >,
  pub id_opt           : Option < ID >,
  pub source_opt       : Option < String >,
  pub effect_on_parent : EffectOnParent,
  pub indefinitive     : bool,
  pub cycle            : bool,
  pub relationships    : OrgnodeRelationships,
  pub edit_request     : Option < EditRequest >,
  pub view_requests    : HashSet < ViewRequest >,
}

/// Describes how a TrueNode affects its parent when saved.
#[derive( Debug, Clone, PartialEq, Eq )]
pub enum EffectOnParent {
  Content,               // Normal content relationship
  Subscribee,            // Subscription relationship
  ParentIgnores,         // No effect on parent (containerward views)
  HiddenFromSubscribees, // No effect on parent (hidden from subscriptions)
}

/// These data only influence how the node is shown.
/// Editing them and then saving the buffer leaves the graph unchanged,
/// and the edits would be immediately lost,
/// as this data is regenerated each time the view is rebuilt.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeRelationships {
  pub parentIsContainer: bool,
  pub parentIsContent: bool,
  pub numContainers: Option<usize>,
  pub numContents: Option<usize>,
  pub numLinksIn: Option<usize>,
}

/// Scaffold nodes are display-only structures
/// that don't correspond per se to nodes in the graph,
/// but encode information about the OrgNodes around them.
#[derive( Debug, Clone, PartialEq, Eq )]
pub enum Scaffold {
  Alias (String), // The node is an alias for its grandparent.
  AliasCol, // The node collects (as children) aliases for its parent.
  ForestRoot, // Not rendered. Makes forests easier to process. Its children are the level-1 headlines of the org buffer.
  HiddenInSubscribeeCol, // Child of a Subscribee. Collects nodes that the subscriber hides from its subscriptions, and that are top-level content of this subscribee.
  HiddenOutsideOfSubscribeeCol, // Child of SubscribeeCol. Collects nodes that the subscriber hides from its subscriptions, but that are not top-level content of any of its subscribees.
  SubscribeeCol, // Collects subscribees for its parent.
}

impl Scaffold {
  /// Compare scaffold kinds. For Alias, compares variant only (ignoring string content).
  pub fn matches_kind ( &self, other : &Scaffold ) -> bool {
    match ( self, other ) {
      ( Scaffold::Alias ( _ ), Scaffold::Alias ( _ ) ) => true,
      _ => std::mem::discriminant ( self )
           == std::mem::discriminant ( other ),
    }} }

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
  Aliases,
  Containerward,
  Sourceward,
  Definitive,
}

//
// Implementations
//

impl Scaffold {
  pub fn title ( &self ) -> &str {
    match self {
      Scaffold::Alias ( s ) => s,
      Scaffold::AliasCol => "its aliases",
      Scaffold::ForestRoot => "",
      Scaffold::HiddenInSubscribeeCol => "hidden from this subscription",
      Scaffold::HiddenOutsideOfSubscribeeCol => "hidden from all subscriptions",
      Scaffold::SubscribeeCol => "it subscribes to these",
    }}

  /// For serialization.
  pub fn interp_str ( &self ) -> &str {
    match self {
      Scaffold::Alias ( _ ) => "alias",
      Scaffold::AliasCol => "aliasCol",
      Scaffold::ForestRoot => "forestRoot",
      Scaffold::HiddenInSubscribeeCol => "hiddenInSubscribeeCol",
      Scaffold::HiddenOutsideOfSubscribeeCol => "hiddenOutsideOfSubscribeeCol",
      Scaffold::SubscribeeCol => "subscribeeCol",
    }} }

impl OrgNode {
  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn id_opt ( &self ) -> Option<&ID> {
    match &self . kind {
      OrgNodeKind::True ( t ) => t . id_opt . as_ref (),
      OrgNodeKind::Scaff ( _ ) => None,
    }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn title ( &self ) -> &str {
    match &self . kind {
      OrgNodeKind::True ( t ) => &t . title,
      OrgNodeKind::Scaff ( s ) => s . title (),
    }}

  /// Returns true if this is a TrueNode and is indefinitive.
  pub fn is_indefinitive ( &self ) -> bool {
    match &self . kind {
      OrgNodeKind::True ( t ) => t . indefinitive,
      OrgNodeKind::Scaff ( _ ) => false,
    }}

  //
  // Mutation helpers (for TrueNodes only; no-op for Scaffolds)
  //

  /// Set the indefinitive flag. No-op for Scaffolds.
  pub fn set_indefinitive ( &mut self, value : bool ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . indefinitive = value;
    }}

  /// Clear the body (set to None). No-op for Scaffolds.
  pub fn clear_body ( &mut self ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . body = None;
    }}

  /// Set the title. No-op for Scaffolds.
  pub fn set_title ( &mut self, title : String ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . title = title;
    }}

  /// Set the source. No-op for Scaffolds.
  pub fn set_source ( &mut self, source : String ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . source_opt = Some ( source );
    }}

  /// Set the cycle flag. No-op for Scaffolds.
  pub fn set_cycle ( &mut self, value : bool ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . cycle = value;
    }}

  /// Get mutable access to view_requests. Returns None for Scaffolds.
  pub fn view_requests_mut ( &mut self ) -> Option < &mut HashSet < ViewRequest > > {
    match &mut self . kind {
      OrgNodeKind::True ( t ) => Some ( &mut t . view_requests ),
      OrgNodeKind::Scaff ( _ ) => None,
    }}

  /// Set the edit_request. No-op for Scaffolds.
  pub fn set_edit_request ( &mut self, edit_request : Option < EditRequest > ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . edit_request = edit_request;
    }}

  /// Set numContainers in relationships. No-op for Scaffolds.
  pub fn set_num_containers ( &mut self, value : Option < usize > ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . relationships . numContainers = value;
    }}

  /// Set numContents in relationships. No-op for Scaffolds.
  pub fn set_num_contents ( &mut self, value : Option < usize > ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . relationships . numContents = value;
    }}

  /// Set numLinksIn in relationships. No-op for Scaffolds.
  pub fn set_num_links_in ( &mut self, value : Option < usize > ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . relationships . numLinksIn = value;
    }}

  /// Set parentIsContainer in relationships. No-op for Scaffolds.
  pub fn set_parent_is_container ( &mut self, value : bool ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . relationships . parentIsContainer = value;
    }}

  /// Set parentIsContent in relationships. No-op for Scaffolds.
  pub fn set_parent_is_content ( &mut self, value : bool ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . relationships . parentIsContent = value;
    }}

  /// Set effect_on_parent. No-op for Scaffolds.
  pub fn set_effect_on_parent ( &mut self, effect : EffectOnParent ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . effect_on_parent = effect;
    }}

  /// Convert a TrueNode to an Alias scaffold.
  /// Uses the TrueNode's title as the alias text.
  /// No-op if already a Scaffold.
  pub fn convert_to_alias ( &mut self ) {
    if let OrgNodeKind::True ( true_node ) = &self . kind {
      self . kind = OrgNodeKind::Scaff (
        Scaffold::Alias ( true_node . title . clone () ) );
    }}

  /// Set the id. No-op for Scaffolds.
  pub fn set_id ( &mut self, id : Option < ID > ) {
    if let OrgNodeKind::True ( t ) = &mut self . kind {
      t . id_opt = id;
    }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body ( &self ) -> Option < &String > {
    match &self . kind {
      OrgNodeKind::True ( t ) => t . body . as_ref (),
      OrgNodeKind::Scaff ( _ ) => None,
    }}
}

impl fmt::Display for EditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      EditRequest::Merge(id) => write!(f, "(merge {})", id.0),
      EditRequest::Delete    => write!(f, "toDelete"),
    }} }

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
      ViewRequest::Aliases       => write!(f, "aliases"),
      ViewRequest::Containerward => write!(f, "containerwardView"),
      ViewRequest::Sourceward    => write!(f, "sourcewardView"),
      ViewRequest::Definitive    => write!(f, "definitiveView"),
    }} }

impl FromStr for ViewRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "aliases"           => Ok ( ViewRequest::Aliases ),
      "containerwardView" => Ok ( ViewRequest::Containerward ),
      "sourcewardView"    => Ok ( ViewRequest::Sourceward ),
      "definitiveView"    => Ok ( ViewRequest::Definitive ),
      _ => Err ( format! ( "Unknown ViewRequest value: {}", s )),
    }} }

//
// Defaults
//

impl Default for OrgnodeRelationships {
  fn default () -> Self {
    OrgnodeRelationships {
      parentIsContainer : true,
      parentIsContent   : false,
      numContainers : Some ( 1 ),
      numContents   : Some ( 0 ),
      numLinksIn    : Some ( 0 ),
    }} }

impl Default for TrueNode {
  fn default () -> Self {
    TrueNode {
      title            : String::new (),
      body             : None,
      id_opt           : None,
      source_opt       : None,
      effect_on_parent : EffectOnParent::Content,
      indefinitive     : false,
      cycle            : false,
      relationships    : OrgnodeRelationships::default (),
      edit_request     : None,
      view_requests    : HashSet::new (),
    }} }

impl Default for OrgNode {
  fn default () -> Self {
    OrgNode {
      focused : false,
      folded  : false,
      kind    : OrgNodeKind::True ( TrueNode::default () ),
    }} }

//
// Constructor functions
//

pub fn mk_definitive_orgnode (
  id     : ID,
  source : String,
  title  : String,
  body   : Option < String >,
) -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( TrueNode {
      title,
      body,
      id_opt           : Some ( id ),
      source_opt       : Some ( source ),
      effect_on_parent : EffectOnParent::Content,
      indefinitive     : false,
      cycle            : false,
      relationships    : OrgnodeRelationships::default (),
      edit_request     : None,
      view_requests    : HashSet::new (),
    }),
  }}

/// Create an indefinitive OrgNode from disk data with a specific effect.
/// This is used for subscription-related nodes (Subscribee, HiddenFromSubscribees).
/// Body is always None since indefinitive nodes don't have editable content.
pub fn mk_indefinitive_orgnode (
  id               : ID,
  source           : String,
  title            : String,
  effect_on_parent : EffectOnParent,
) -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( TrueNode {
      title,
      body             : None,
      id_opt           : Some ( id ),
      source_opt       : Some ( source ),
      effect_on_parent,
      indefinitive     : true,
      cycle            : false,
      relationships    : OrgnodeRelationships::default (),
      edit_request     : None,
      view_requests    : HashSet::new (),
    }),
  }}

/// Create a OrgNode with *nearly* full metadata control.
/// The exception is that the 'OrgnodeRelationships' is intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the OrgNode tree.
pub fn mk_orgnode (
  id               : ID,
  source           : String,
  title            : String,
  body             : Option < String >,
  effect_on_parent : EffectOnParent,
  indefinitive     : bool,
  edit_request     : Option < EditRequest >,
  view_requests    : HashSet < ViewRequest >,
) -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( TrueNode {
      title,
      body,
      id_opt           : Some ( id ),
      source_opt       : Some ( source ),
      effect_on_parent,
      indefinitive,
      cycle            : false,
      relationships    : OrgnodeRelationships::default (),
      edit_request,
      view_requests,
    }),
  }}

/// Create a Scaffold OrgNode from a Scaffold.
pub fn orgnode_from_scaffold ( scaffold : Scaffold ) -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::Scaff ( scaffold ),
  }}

/// Helper to create a ForestRoot OrgNode.
pub fn forest_root_orgnode () -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::Scaff ( Scaffold::ForestRoot ),
  }}
