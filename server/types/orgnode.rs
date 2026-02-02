/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'SkgNode' type.
/// Nodes of the tree are represented via the 'OrgNode' type.
///   (That name might change once there are more clients. The only client so far is written in Emacs org-mode; hence the name.)
/// Some 'OrgNode's correspond to SkgNodes; these are 'TrueNode's.
/// Others do not so correspond, but rather encode information about neighboring tree nodes. These are 'Scaffold' nodes.

use super::git::{NodeDiffStatus, FieldDiffStatus};
use super::misc::{ID, SourceName};
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
  pub id             : ID,
  pub source         : SourceName,
  pub parent_ignores : bool, // When true, if the buffer is saved, this node has no effect on its parent. It is effectively a new tree root, but it does not have to be located at the top of the buffer tree with the other roots.
  // PITFALL : Don't move parent_ignores to ViewNodeStats. Doing so might seem tidy, because parent_ignores describes another relationship between the node and its view-ancestry. But parent_ignores is different because the user can in some cases reasonably change its value. That is, parent_ignores is not dictated solely by the view, but instead by some combination of the view and the user's intentions.

  pub indefinitive  : bool, // When the user saves a buffer, an 'indefinitive' orgnode representing node N will not affect N in the graph. It is just a view of N, not a way to edit N. However, its presence as a content-child of some other node P will still cause P's content to be updated in the graph.

  // The next two *Stats fields only influence how the node is shown. Editing them and saving the buffer leaves the graph unchanged, and those edits will be immediately lost, as this data is regenerated each time the view is rebuilt.
  pub graphStats    : GraphNodeStats,
  pub viewStats     : ViewNodeStats,

  pub edit_request  : Option < EditRequest >,
  pub view_requests : HashSet < ViewRequest >,

  pub diff          : Option < NodeDiffStatus >,
}

/// Graph-level statistics about a node.
/// These are derived from the graph database and are the same
/// regardless of where/how the node appears in a view.
#[derive(Debug, Clone, PartialEq)]
pub struct GraphNodeStats {
  pub numContainers : Option<usize>,
  pub numContents   : Option<usize>,
  pub numLinksIn    : Option<usize>,
}

/// View-specific statistics about a node.
/// These depend on the node's position in the current view tree.
/// `cycle` depends on ancestors; `parentIs*` depends on the specific parent.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewNodeStats {
  pub cycle             : bool,
  pub parentIsContainer : bool,
  pub parentIsContent   : bool,
}

/// Scaffold nodes are display-only structures
/// that don't correspond per se to nodes in the graph,
/// but encode information about the OrgNodes around them.
#[derive( Debug, Clone, PartialEq, Eq )]
pub enum Scaffold {
  Alias { text: String, // an alias for the node's grandparent
          diff: Option<FieldDiffStatus> },
  AliasCol, // The node collects (as children) aliases for its parent.
  BufferRoot, // Not rendered. Makes forests easier to process. Its children are the level-1 headlines of the org buffer.
  HiddenInSubscribeeCol, // Child of a Subscribee. Collects nodes that the subscriber hides from its subscriptions, and that are top-level content of this subscribee.
  HiddenOutsideOfSubscribeeCol, // Child of SubscribeeCol. Collects nodes that the subscriber hides from its subscriptions, but that are not top-level content of any of its subscribees.
  ID { id: String, // an ID of the node's grandparent.
       diff: Option<FieldDiffStatus> },
  IDCol, // Collects (as children) Scaffold::IDs for its parent.
  SubscribeeCol, // Collects subscribees for its parent.
  TextChanged, // Indicates title/body changed between disk and HEAD. Visible in 'git diff mode'.
}

/// A discriminant (i.e. some labels) for the Scaffold variants.
/// (We can't simply use the Scaffold variants themselves,
/// because of the Alias/ID payloads.)
/// Used for the bijective Emacs string mapping.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScaffoldKind { Alias,
                        AliasCol,
                        BufferRoot,
                        HiddenInSubscribeeCol,
                        HiddenOutsideOfSubscribeeCol,
                        SubscribeeCol,
                        TextChanged,
                        IDCol,
                        ID, }

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

/// Diff status for TrueNodes in git diff view mode.
//
// Implementations
//

impl Scaffold {
  /// Single source of truth for Scaffold <-> Emacs string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, ScaffoldKind)] = &[
    ("alias",                        ScaffoldKind::Alias),
    ("aliasCol",                     ScaffoldKind::AliasCol),
    ("forestRoot",                   ScaffoldKind::BufferRoot),
    ("hiddenInSubscribeeCol",        ScaffoldKind::HiddenInSubscribeeCol),
    ("hiddenOutsideOfSubscribeeCol", ScaffoldKind::HiddenOutsideOfSubscribeeCol),
    ("subscribeeCol",                ScaffoldKind::SubscribeeCol),
    ("textChanged",                  ScaffoldKind::TextChanged),
    ("idCol",                        ScaffoldKind::IDCol),
    ("id",                           ScaffoldKind::ID),
  ];

  /// Get the kind (discriminant) of this Scaffold.
  pub fn kind ( &self ) -> ScaffoldKind {
    match self {
      Scaffold::Alias { .. }                 => ScaffoldKind::Alias,
      Scaffold::AliasCol                     => ScaffoldKind::AliasCol,
      Scaffold::BufferRoot                   => ScaffoldKind::BufferRoot,
      Scaffold::HiddenInSubscribeeCol        => ScaffoldKind::HiddenInSubscribeeCol,
      Scaffold::HiddenOutsideOfSubscribeeCol => ScaffoldKind::HiddenOutsideOfSubscribeeCol,
      Scaffold::SubscribeeCol                => ScaffoldKind::SubscribeeCol,
      Scaffold::TextChanged                  => ScaffoldKind::TextChanged,
      Scaffold::IDCol                        => ScaffoldKind::IDCol,
      Scaffold::ID { .. }                    => ScaffoldKind::ID,
    }}

  /// Compare scaffold kinds. For Alias/ID, compares variant only (ignoring payload).
  pub fn matches_kind ( &self, other : &Scaffold ) -> bool {
    self.kind() == other.kind() }

  /// String representation as used in Emacs metadata sexps.
  pub fn repr_in_client ( &self ) -> String {
    let kind : ScaffoldKind = self.kind();
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, k)| *k == kind )
      .map ( |(s, _)| s.to_string() )
      .expect ( "REPRS_IN_CLIENT should cover all ScaffoldKinds" ) }

  pub fn title ( &self ) -> &str {
    match self {
      Scaffold::Alias { text, .. } => text,
      Scaffold::AliasCol => "its aliases",
      Scaffold::BufferRoot => "",
      Scaffold::HiddenInSubscribeeCol => "hidden from this subscription",
      Scaffold::HiddenOutsideOfSubscribeeCol => "hidden from all subscriptions",
      Scaffold::SubscribeeCol => "it subscribes to these",
      Scaffold::TextChanged => "Text changed. See git diff.",
      Scaffold::IDCol => "its IDs",
      Scaffold::ID { id, .. } => id,
    }} }

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

impl Default for GraphNodeStats {
  fn default () -> Self {
    GraphNodeStats {
      numContainers : Some ( 1 ),
      numContents   : Some ( 0 ),
      numLinksIn    : Some ( 0 ),
    }} }

impl Default for ViewNodeStats {
  fn default () -> Self {
    ViewNodeStats {
      cycle             : false,
      parentIsContainer : true,
      parentIsContent   : false,
    }} }

//
// Constructor functions
//

/// Create a TrueNode with default values for all fields except id, source, and title.
/// Useful when you need to customize other fields after construction.
pub fn default_truenode (
  id     : ID,
  source : SourceName,
  title  : String,
) -> TrueNode {
  TrueNode {
    title,
    body           : None,
    id,
    source,
    parent_ignores : false,
    indefinitive   : false,
    graphStats     : GraphNodeStats::default(),
    viewStats      : ViewNodeStats::default(),
    edit_request   : None,
    view_requests  : HashSet::new(),
    diff           : None,
  }}

pub fn mk_definitive_orgnode (
  id     : ID,
  source : SourceName,
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
  source         : SourceName,
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
/// The exception is that the 'GraphNodeStats' and 'ViewNodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the OrgNode tree.
pub fn mk_orgnode (
  id             : ID,
  source         : SourceName,
  title          : String,
  body           : Option < String >,
  parent_ignores : bool,
  indefinitive   : bool,
  edit_request   : Option < EditRequest >,
  view_requests  : HashSet < ViewRequest >,
) -> OrgNode {
  OrgNode { focused : false,
            folded  : false,
            kind    : OrgNodeKind::True (
              TrueNode { body,
                         parent_ignores,
                         indefinitive,
                         edit_request,
                         view_requests,
                         .. default_truenode (
                           id, source, title ) } ) }}

/// Create a Scaffold OrgNode from a Scaffold.
pub fn orgnode_from_scaffold ( scaffold : Scaffold ) -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::Scaff ( scaffold ),
  }}

/// Helper to create a BufferRoot OrgNode.
pub fn forest_root_orgnode () -> OrgNode {
  OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::Scaff ( Scaffold::BufferRoot ),
  }}
