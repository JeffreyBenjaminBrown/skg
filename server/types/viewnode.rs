/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'SkgNode' type.
/// Nodes of the tree are represented via the 'ViewNode' type.
///   (That name might change once there are more clients. The only client so far is written in Emacs org-mode; hence the name.)
/// Some 'ViewNode's correspond to SkgNodes; these are 'TrueNode's.
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
pub struct ViewNode {
  pub focused : bool,
  pub folded  : bool,
  pub kind    : ViewNodeKind,
}

#[derive( Debug, Clone, PartialEq )]
pub enum ViewNodeKind {
  True         (TrueNode),
  Scaff        (Scaffold),
  Deleted      (DeletedNode),
  DeletedScaff (ScaffoldKind),
}

/// A node whose .skg file was deleted by a save in another buffer.
/// Inert: generates no save instructions, excluded from parent's
/// contains list, never "completed".
/// Retains its children so the user's subtree is preserved.
#[derive( Debug, Clone, PartialEq )]
pub struct DeletedNode {
  pub id     : ID,
  pub source : SourceName,
  pub title  : String,
  pub body   : Option < String >,
}

pub type TrueNode          = TrueNode_Generic < ID, SourceName >;
pub type UncheckedTrueNode = TrueNode_Generic < Option < ID >,
                                                Option < SourceName >>;

/// A ViewNode that corresponds to a SkgNode.
#[derive( Debug, Clone, PartialEq )]
pub struct TrueNode_Generic < Id, Src > {
  pub title          : String,
  pub id             : Id,
  pub source         : Src,
  pub parent_ignores : bool, // When true, if the buffer is saved, this node has no effect on its parent. It is effectively a new tree root, but it does not have to be located at the top of the buffer tree with the other roots.
  // PITFALL : Don't move parent_ignores to ViewNodeStats. Doing so might seem tidy, because parent_ignores describes another relationship between the node and its view-ancestry. But parent_ignores is different because the user can in some cases reasonably change its value. That is, parent_ignores is not dictated solely by the view, but instead by some combination of the view and the user's intentions.

  // The next two *Stats fields only influence how the node is shown. Editing them and saving the buffer leaves the graph unchanged, and those edits will be immediately lost, as this data is regenerated each time the view is rebuilt.
  pub graphStats    : GraphNodeStats,
  pub viewStats     : ViewNodeStats,

  pub view_requests : HashSet < ViewRequest >,

  pub diff          : Option < NodeDiffStatus >,

  pub indef_or_def  : IndefOrDef,
}

/// Each TrueNode has one of these.
/// - A Definitive represents an editable view.
///   The user's changes to title, body and children
///   will be written to disk and the dbs when they save.
/// - An Indefinitive represents a read-only view,
///   in which case the body is not presented.
///   (TODO ? Maybe it should be.)
#[derive( Debug, Clone, PartialEq )]
pub enum IndefOrDef {
  Definitive {
    body         : Option < String >,
    edit_request : Option < EditRequest >, },
  Indefinitive, }

/// Containerward path statistics: how a node relates to the
/// container hierarchy (path length, fork count, cycle detection).
#[derive(Debug, Clone, PartialEq)]
pub struct ContainerwardPathStats {
  pub length : usize,
  pub forks  : usize,
  pub cycles : bool,
}

/// Graph-level statistics about a node.
/// These are derived from the graph database and are the same
/// regardless of where/how the node appears in a view.
#[derive(Debug, Clone, PartialEq)]
pub struct GraphNodeStats {
  pub aliasing          : bool, // whether it has aliases
  pub extraIDs          : bool, // whether it has extra IDs (from merging)
  pub overriding        : bool, // overrides *or* overridden, anywhere
  pub subscribing       : bool, // subscriber *or* subscribee, anywhere
  pub numContainers     : Option<usize>,
  pub numContents       : Option<usize>,
  pub linksInHerald     : Option<String>,
  pub containerwardPath : Option<ContainerwardPathStats>,
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
/// but encode information about the ViewNodes around them.
#[derive( Debug, Clone, PartialEq )]
pub enum Scaffold {
  Alias { text: String, // an alias for the node's grandparent
          diff: Option<FieldDiffStatus> },
  AliasCol, // The node collects (as children) aliases for its parent.
  BufferRoot, // Not rendered. Makes forests easier to process. Its children are the level-1 headlines of the org buffer.
  HiddenInSubscribeeCol, // Child of a Subscribee. Collects nodes that the subscriber hides from its subscriptions, and that are top-level content of this subscribee.
  HiddenOutsideOfSubscribeeCol, // Child of SubscribeeCol. Collects nodes that the subscriber hides from its subscriptions, but that are not top-level content of any of its subscribees.
  ID { id: ID, // an ID of the node's grandparent.
       diff: Option<FieldDiffStatus> },
  IDCol, // Collects (as children) Scaffold::IDs for its parent.
  SubscribeeCol, // Collects subscribees for its parent.
  TextChanged, // Indicates title/body changed between disk and HEAD. Visible in 'git diff mode'.
}

/// A discriminant (i.e. some labels) for the Scaffold variants.
/// (We can't simply use the Scaffold variants themselves,
/// because of the Alias/ID payloads.)
/// Used for the bijective Emacs string mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScaffoldKind { Alias,
                        AliasCol,
                        BufferRoot,
                        HiddenInSubscribeeCol,
                        HiddenOutsideOfSubscribeeCol,
                        IDCol,
                        ID,
                        SubscribeeCol,
                        TextChanged, }

/// Requests for editing operations on a node.
/// Only one edit request is allowed per node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditRequest {
  Merge (ID), // The node with this request is the acquirer. The node with the ID that this request specifies is the acquiree.
  Delete, // request to delete this node
}

/// Requests for additional views related to a node.
/// Multiple view requests can be active simultaneously.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ViewRequest {
  Aliases,
  Containerward,
  ContainerwardStats,
  Sourceward,
  Definitive,
}

//
// Implementations
//

impl < Id, Src > TrueNode_Generic < Id, Src > {
  /// A phantom is a display-only placeholder for a removed node
  /// in git diff view. Identified by Removed or RemovedHere status.
  pub fn is_phantom (
    &self,
  ) -> bool {
    matches!( self . diff,
              Some (NodeDiffStatus::Removed)
            | Some (NodeDiffStatus::RemovedHere) ) }

  pub fn is_indefinitive (&self) -> bool {
    matches! ( self . indef_or_def,
               IndefOrDef::Indefinitive ) }

  pub fn body (&self) -> Option < &String > {
    match &self . indef_or_def {
      IndefOrDef::Definitive { body, .. } =>
        body . as_ref(),
      IndefOrDef::Indefinitive => None, }}

  pub fn edit_request (&self) -> Option < &EditRequest > {
    match &self . indef_or_def {
      IndefOrDef::Definitive { edit_request, .. } =>
        edit_request . as_ref(),
      IndefOrDef::Indefinitive => None, }}
}

impl ContainerwardPathStats {
  pub fn to_display_atom (&self) -> String {
    let sep : &str =
      if self . cycles { "↻" } else { "⤊" };
    if self . forks <= 1
      { format! ( "{}{}", sep, self . length ) }
    else
      { format! ( "{}{}{}", self . forks, sep, self . length ) } }
  pub fn from_display_atom (s: &str) -> Option<Self> {
    // Parse "2⤊5", "⤊3", "2↻5", "↻3"
    let (before, after, cycles) : (&str, &str, bool) =
      if let Some (pos) = s . find ('⤊')
        { ( &s[..pos], &s[pos + '⤊' . len_utf8 ()..], false ) }
      else if let Some (pos) = s . find ('↻')
        { ( &s[..pos], &s[pos + '↻' . len_utf8 ()..], true ) }
      else { return None; };
    let length : usize = after . parse () . ok () ?;
    let forks : usize =
      if before . is_empty () { 1 }
      else { before . parse () . ok () ? };
    Some ( ContainerwardPathStats { length, forks, cycles } ) } }

impl ScaffoldKind {
  /// Single source of truth for ScaffoldKind <-> Emacs string bijection.
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

  /// String representation as used in Emacs metadata sexps.
  pub fn repr_in_client (&self) -> String {
    Self::REPRS_IN_CLIENT . iter()
      . find ( |(_, k)| k == self )
      . map ( |(s, _)| s . to_string() )
      . expect ("REPRS_IN_CLIENT should cover all ScaffoldKinds") }

  /// Parse a client string to a ScaffoldKind.
  pub fn from_client_string ( s: &str ) -> Option<ScaffoldKind> {
    Self::REPRS_IN_CLIENT . iter()
      . find ( |(cs, _)| *cs == s )
      . map ( |(_, k)| *k ) }

  /// The default title for a scaffold of this kind.
  pub fn default_title (&self) -> &'static str {
    match self {
      ScaffoldKind::AliasCol                     => "its aliases",
      ScaffoldKind::IDCol                        => "its IDs",
      ScaffoldKind::SubscribeeCol                => "it subscribes to these",
      ScaffoldKind::HiddenInSubscribeeCol        => "hidden from this subscription",
      ScaffoldKind::HiddenOutsideOfSubscribeeCol => "hidden from all subscriptions",
      _                                          => "",
    }} }

impl Scaffold {
  /// Get the kind (discriminant) of this Scaffold.
  pub fn kind (&self) -> ScaffoldKind {
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
    self . kind() == other . kind() }

  /// String representation as used in Emacs metadata sexps.
  pub fn repr_in_client (&self) -> String {
    self . kind () . repr_in_client () }

  pub fn title (&self) -> &str {
    match self {
      Scaffold::Alias           { text, .. }  => text,
      Scaffold::ID              { id, .. }    => id,
      _ => self . kind () . default_title (),
    }}

  /// A distinguishable label for use in error messages.
  /// For scaffolds with payload, includes the payload.
  /// For others, uses repr_in_client().
  pub fn error_label (&self) -> String {
    match self {
      Scaffold::Alias { text, .. } =>
        format!( "scaffold:alias({})", text ),
      Scaffold::ID { id, .. } =>
        format!( "scaffold:id({})", id ),
      _ =>
        format!( "scaffold:{}", self . repr_in_client() ),
    }} }

impl ViewRequest {
  /// Single source of truth for ViewRequest <-> client string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, ViewRequest)] = &[
    ("aliases",            ViewRequest::Aliases),
    ("containerwardView",  ViewRequest::Containerward),
    ("containerwardStats", ViewRequest::ContainerwardStats),
    ("sourcewardView",     ViewRequest::Sourceward),
    ("definitiveView",     ViewRequest::Definitive),
  ];

  /// String representation as used in client metadata.
  pub fn repr_in_client (&self) -> &'static str {
    Self::REPRS_IN_CLIENT . iter()
      . find ( |(_, vr)| vr == self )
      . map ( |(s, _)| *s )
      . expect ("REPRS_IN_CLIENT should cover all ViewRequest variants") }

  /// Parse a client string to a ViewRequest.
  pub fn from_client_string ( s: &str ) -> Option<ViewRequest> {
    Self::REPRS_IN_CLIENT . iter()
      . find ( |(cs, _)| *cs == s )
      . map ( |(_, vr)| *vr ) }
}

impl AsRef<ViewNode> for ViewNode {
  fn as_ref (&self) -> &ViewNode {
    self }}

impl AsMut<ViewNode> for ViewNode {
  fn as_mut (&mut self) -> &mut ViewNode {
    self }}

impl ViewNode {
  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn title (&self) -> &str {
    match &self . kind {
      ViewNodeKind::True    (t) => &t . title,
      ViewNodeKind::Scaff   (s) => s . title (),
      ViewNodeKind::Deleted (d) => &d . title,
      ViewNodeKind::DeletedScaff (kind) => kind . default_title (),
    }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body (&self) -> Option < &String > {
    match &self . kind {
      ViewNodeKind::True    (t) => t . body (),
      ViewNodeKind::Scaff   (_) => None,
      ViewNodeKind::Deleted (d) => d . body . as_ref (),
      ViewNodeKind::DeletedScaff (_) => None,
    }}
}

impl fmt::Display for EditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      EditRequest::Merge (id) => write!(f, "(merge {})", id . 0),
      EditRequest::Delete    => write!(f, "toDelete"),
    }} }

impl FromStr for EditRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "toDelete" => Ok (EditRequest::Delete),
      _ => {
        // Try to parse as "merge <id>"
        if let Some (id_str) = s . strip_prefix ("merge ") {
          Ok ( EditRequest::Merge ( ID::from (id_str) ) )
        } else {
          Err ( format! ( "Unknown EditRequest value: {}", s ))
        }} }} }

impl fmt::Display for ViewRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    write!(f, "{}", self . repr_in_client()) } }

impl FromStr for ViewRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    Self::from_client_string (s)
      . ok_or_else ( || format! ( "Unknown ViewRequest value: {}", s ) ) } }

//
// Defaults
//

impl Default for GraphNodeStats {
  fn default () -> Self {
    GraphNodeStats {
      aliasing          : false,
      extraIDs          : false,
      overriding        : false,
      subscribing       : false,
      numContainers     : Some (1),
      numContents       : Some (0),
      linksInHerald     : None,
      containerwardPath : None,
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
    id,
    source,
    parent_ignores : false,
    graphStats     : GraphNodeStats::default(),
    viewStats      : ViewNodeStats::default(),
    view_requests  : HashSet::new(),
    diff           : None,
    indef_or_def   : IndefOrDef::Definitive {
      body         : None,
      edit_request : None },
  }}

/// Create an indefinitive phantom ViewNode with a diff status.
pub fn mk_phantom_viewnode (
  id     : ID,
  source : SourceName,
  title  : String,
  diff   : NodeDiffStatus,
) -> ViewNode {
  let mut viewnode : ViewNode =
    mk_indefinitive_viewnode ( id, source, title, false );
  if let ViewNodeKind::True ( ref mut t ) = viewnode . kind
    { t . diff = Some (diff); }
  viewnode }

pub fn mk_definitive_viewnode (
  id     : ID,
  source : SourceName,
  title  : String,
  body   : Option < String >,
) -> ViewNode { mk_viewnode ( id,
                            source,
                            title,
                            false,              // parent_ignores
                            IndefOrDef::Definitive {
                              body,
                              edit_request : None },
                            HashSet::new () ) } // view_requests

/// Create an indefinitive ViewNode from disk data.
/// Body is always None since indefinitive nodes don't have editable content.
pub fn mk_indefinitive_viewnode (
  id             : ID,
  source         : SourceName,
  title          : String,
  parent_ignores : bool,
) -> ViewNode { mk_viewnode ( id,
                            source,
                            title,
                            parent_ignores,
                            IndefOrDef::Indefinitive,
                            HashSet::new ( )) } // view_requests

/// Convert a definitive ViewNode to indefinitive.
/// Discards body and edit_request.
/// Errors if the input is not a TrueNode.
pub fn mk_indefinitive_from_viewnode (
  viewnode       : ViewNode,
  parent_ignores : bool,
) -> Result < ViewNode, String > {
  let ViewNodeKind::True (t) = viewnode . kind
    else { return Err (
      "mk_indefinitive_from_viewnode: expected TrueNode"
        . to_string () ) };
  Ok ( mk_indefinitive_viewnode ( t . id,
                                  t . source,
                                  t . title,
                                  parent_ignores )) }

/// Create a ViewNode with *nearly* full metadata control.
/// The exception is that the 'GraphNodeStats' and 'ViewNodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the ViewNode tree.
pub fn mk_viewnode (
  id             : ID,
  source         : SourceName,
  title          : String,
  parent_ignores : bool,
  indef_or_def   : IndefOrDef,
  view_requests  : HashSet < ViewRequest >,
) -> ViewNode {
  ViewNode { focused : false,
             folded  : false,
             kind    : ViewNodeKind::True (
               TrueNode { parent_ignores,
                          view_requests,
                          indef_or_def,
                          .. default_truenode (
                            id, source, title ) } ) }}

/// Create a Scaffold ViewNode from a Scaffold.
pub fn viewnode_from_scaffold ( scaffold : Scaffold ) -> ViewNode {
  ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::Scaff (scaffold),
  }}

/// Helper to create a BufferRoot ViewNode.
pub fn forest_root_viewnode () -> ViewNode {
  ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::Scaff (Scaffold::BufferRoot),
  }}
