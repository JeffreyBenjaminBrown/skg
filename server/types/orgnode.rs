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
  pub title          : String,
  pub body           : Option < String >,
  pub id_opt         : Option < ID >,
  pub source_opt     : Option < String >,
  pub parent_ignores : bool, // When true, if the buffer is saved, this node has no effect on its parent. It is effectively a new tree root, but it does not have to be located at the top of the buffer tree with the other roots.
  pub indefinitive   : bool,
  pub cycle          : bool,
  pub stats          : TrueNodeStats,
  pub edit_request   : Option < EditRequest >,
  pub view_requests  : HashSet < ViewRequest >,
}

/// These data only influence how the node is shown.
/// Editing them and then saving the buffer leaves the graph unchanged,
/// and the edits would be immediately lost,
/// as this data is regenerated each time the view is rebuilt.
#[derive(Debug, Clone, PartialEq)]
pub struct TrueNodeStats {
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
  Alias (String), // The string is an alias for the node's grandparent.
  AliasCol, // The node collects (as children) aliases for its parent.
  ForestRoot, // Not rendered. Makes forests easier to process. Its children are the level-1 headlines of the org buffer.
  HiddenInSubscribeeCol, // Child of a Subscribee. Collects nodes that the subscriber hides from its subscriptions, and that are top-level content of this subscribee.
  HiddenOutsideOfSubscribeeCol, // Child of SubscribeeCol. Collects nodes that the subscriber hides from its subscriptions, but that are not top-level content of any of its subscribees.
  SubscribeeCol, // Collects subscribees for its parent.
}

/// A discriminant (i.e. some labels) for the Scaffold variants.
/// (We can't simply use the Scaffold variants themselves,
/// because of the Alias payload.)
/// Used for the bijective Emacs string mapping.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScaffoldKind { Alias,
                        AliasCol,
                        ForestRoot,
                        HiddenInSubscribeeCol,
                        HiddenOutsideOfSubscribeeCol,
                        SubscribeeCol, }

/// Requests for editing operations on a node.
/// Only one edit request is allowed per node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditRequest {
  Merge(ID), // The node with this request is the acquirer. The node with the ID that this request specifies is the acquiree.
  Delete, // request to delete this node
}

/// Requests for additional views related to a node.
/// Multiple view requests can be active simultaneously.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
  /// Single source of truth for Scaffold <-> Emacs string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, ScaffoldKind)] = &[
    ("alias",                        ScaffoldKind::Alias),
    ("aliasCol",                     ScaffoldKind::AliasCol),
    ("forestRoot",                   ScaffoldKind::ForestRoot),
    ("hiddenInSubscribeeCol",        ScaffoldKind::HiddenInSubscribeeCol),
    ("hiddenOutsideOfSubscribeeCol", ScaffoldKind::HiddenOutsideOfSubscribeeCol),
    ("subscribeeCol",                ScaffoldKind::SubscribeeCol),
  ];

  /// Get the kind (discriminant) of this Scaffold.
  pub fn kind ( &self ) -> ScaffoldKind {
    match self {
      Scaffold::Alias ( _ )                  => ScaffoldKind::Alias,
      Scaffold::AliasCol                     => ScaffoldKind::AliasCol,
      Scaffold::ForestRoot                   => ScaffoldKind::ForestRoot,
      Scaffold::HiddenInSubscribeeCol        => ScaffoldKind::HiddenInSubscribeeCol,
      Scaffold::HiddenOutsideOfSubscribeeCol => ScaffoldKind::HiddenOutsideOfSubscribeeCol,
      Scaffold::SubscribeeCol                => ScaffoldKind::SubscribeeCol,
    }}

  /// Compare scaffold kinds. For Alias, compares variant only (ignoring string content).
  pub fn matches_kind ( &self, other : &Scaffold ) -> bool {
    self.kind() == other.kind() }

  /// String representation as used in Emacs metadata sexps.
  pub fn repr_in_client ( &self ) -> String {
    let kind : ScaffoldKind = self.kind();
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, k)| *k == kind )
      .map ( |(s, _)| s.to_string() )
      .expect ( "REPRS_IN_CLIENT should cover all ScaffoldKinds" ) }

  /// Parse an Emacs string to a Scaffold. For Alias, uses the provided title.
  pub fn from_repr_in_client ( s: &str, title: &str
                             ) -> Option<Scaffold>
  { Self::REPRS_IN_CLIENT.iter()
      .find ( |(es, _)| *es == s )
      .map ( |(_, kind)| kind.to_scaffold ( title ) ) }

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

impl ScaffoldKind {
  /// Construct a Scaffold from this kind. For Alias, uses the provided title.
  pub fn to_scaffold ( &self, title: &str ) -> Scaffold {
    match self {
      ScaffoldKind::Alias                     => Scaffold::Alias ( title.to_string() ),
      ScaffoldKind::AliasCol                  => Scaffold::AliasCol,
      ScaffoldKind::ForestRoot                => Scaffold::ForestRoot,
      ScaffoldKind::HiddenInSubscribeeCol     => Scaffold::HiddenInSubscribeeCol,
      ScaffoldKind::HiddenOutsideOfSubscribeeCol => Scaffold::HiddenOutsideOfSubscribeeCol,
      ScaffoldKind::SubscribeeCol             => Scaffold::SubscribeeCol,
    }}
}

impl ViewRequest {
  /// Single source of truth for ViewRequest <-> client string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, ViewRequest)] = &[
    ("aliases",          ViewRequest::Aliases),
    ("containerwardView", ViewRequest::Containerward),
    ("sourcewardView",   ViewRequest::Sourceward),
    ("definitiveView",   ViewRequest::Definitive),
  ];

  /// String representation as used in client metadata.
  pub fn repr_in_client ( &self ) -> &'static str {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, vr)| vr == self )
      .map ( |(s, _)| *s )
      .expect ( "REPRS_IN_CLIENT should cover all ViewRequest variants" ) }

  /// Parse a client string to a ViewRequest.
  pub fn from_client_string ( s: &str ) -> Option<ViewRequest> {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(cs, _)| *cs == s )
      .map ( |(_, vr)| *vr ) }
}

impl OrgNode {
  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn title ( &self ) -> &str {
    match &self . kind {
      OrgNodeKind::True ( t ) => &t . title,
      OrgNodeKind::Scaff ( s ) => s . title (),
    }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body ( &self ) -> Option < &String > {
    match &self . kind {
      OrgNodeKind::True ( t ) => t . body . as_ref (),
      OrgNodeKind::Scaff ( _ ) => None,
    }}

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn id_opt ( &self ) -> Option<&ID> {
    match &self . kind {
      OrgNodeKind::True ( t ) => t . id_opt . as_ref (),
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
    write!(f, "{}", self.repr_in_client()) } }

impl FromStr for ViewRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    Self::from_client_string ( s )
      .ok_or_else ( || format! ( "Unknown ViewRequest value: {}", s ) ) } }

//
// Defaults
//

impl Default for TrueNodeStats {
  fn default () -> Self {
    TrueNodeStats {
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
      parent_ignores   : false,
      indefinitive     : false,
      cycle            : false,
      stats            : TrueNodeStats::default (),
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
) -> OrgNode { mk_orgnode ( id,
                            source,
                            title,
                            body,
                            false,              // parent_ignores
                            false,              // indefinitive
                            None,               // edit_request
                            HashSet::new () ) } // view_requests

/// Create an indefinitive OrgNode from disk data.
/// Body is always None since indefinitive nodes don't have editable content.
pub fn mk_indefinitive_orgnode (
  id             : ID,
  source         : String,
  title          : String,
  parent_ignores : bool,
) -> OrgNode { mk_orgnode ( id,
                            source,
                            title,
                            None, // body
                            parent_ignores,
                            true, // indefinitive
                            None, // edit_request
                            HashSet::new ( )) } // view_requests

/// Create a OrgNode with *nearly* full metadata control.
/// The exception is that the 'TrueNodeStats' is intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the OrgNode tree.
pub fn mk_orgnode (
  id             : ID,
  source         : String,
  title          : String,
  body           : Option < String >,
  parent_ignores : bool,
  indefinitive   : bool,
  edit_request   : Option < EditRequest >,
  view_requests  : HashSet < ViewRequest >,
) -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( TrueNode {
      title,
      body,
      id_opt           : Some ( id ),
      source_opt       : Some ( source ),
      parent_ignores,
      indefinitive,
      cycle            : false,
      stats            : TrueNodeStats::default (),
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
