/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'NodeComplete' type.
/// Nodes of the tree are represented via the 'ViewNode' type.
///   (That name might change once there are more clients. The only client so far is written in Emacs org-mode; hence the name.)
/// Some 'ViewNode's correspond to NodeCompletes; these are 'TrueNode's.
/// Others do not so correspond, but rather encode information about neighboring tree nodes. These are 'Scaffold' nodes.

use super::git::{ExistenceAxes, MembershipAxes, Sign};
use super::misc::{ID, SourceName};
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

/// What this node's visible parent is relative to this node --
/// or, more precisely, what it *was* when this buffer was (re)drawn,
/// before the user may have moved the node elsewhere, invalidating this.
/// Most nodes are born as content of the parent node,
/// but there can be other reasons -- e.g. the user asked to see backlinks.
///
/// PITFALL: There might be multiple relationships between parent and child --
/// e.g. cyclic containment. This does not encode all of them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParentIs {
  Container,   // default: parent contains this child.
  Collector,   // parent is a generated relation collection scaffold.
  Content,     // containerward backpath: parent is contained by this child.
  LinkTarget,  // sourceward backpath: parent is linked to by this child.
  Independent, // visible parent exists, but no graph relationship to it is claimed.
  Absent,      // no visible parent; the only viewtree parent is BufferRoot.
}

//
// Type declarations
//

/// Corresponds to an Emacs headline-body pair.
#[derive( Debug, Clone, PartialEq )]
pub struct ViewNode {
  pub focused     : bool,
  pub folded      : bool, // Is this     hidden in its parent?
  pub body_folded : bool, // Is the body hidden in this?
  pub kind        : ViewNodeKind,
}

#[derive( Debug, Clone, PartialEq )]
pub enum ViewNodeKind {
  True         (TrueNode),
  Scaff        (Scaffold),
  Deleted      (DeletedNode),
  DeletedScaff (ScaffoldKind),
  Unknown      (UnknownNode),
  Inactive     (InactiveNode),
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

/// A node referenced by some other node's `contains` (or similar list) for which neither in-Rust graph nor disk has any record -- not as a primary pid, not as an extra_id, and not recoverable through any phantom/diff-view procedure either. Distinct from DeletedNode, which knows its source and last-seen text; an UnknownNode is a dangling pointer with no metadata of its own. Carrying it as its own ViewNodeKind variant (rather than failing the whole view) is what keeps a single bad reference deep in a subtree from killing the view at the root.
#[derive( Debug, Clone, PartialEq )]
pub struct UnknownNode {
  pub id : ID,
}

#[derive( Debug, Clone, PartialEq )]
pub struct InactiveNode {
  pub id         : ID,
  pub source     : SourceName,
  pub membership : MembershipAxes,
}

pub type TrueNode            = TrueNode_Generic < ID, SourceName >;
pub type MaybePlacedTruenode = TrueNode_Generic < Option < ID >,
                                                  Option < SourceName >>;

/// A ViewNode that corresponds to a NodeComplete.
#[derive( Debug, Clone, PartialEq )]
pub struct TrueNode_Generic < Id, Src > {
  pub title          : String,
  pub id             : Id,
  pub source         : Src,
  pub parentIs          : ParentIs, // When not Container, this node has no effect on its parent if the buffer is saved.
  // PITFALL: Don't move parentIs to ViewNodeStats. It describes the node-to-parent relationship, but unlike viewStats fields it can be meaningfully changed by the user. It is not dictated solely by the view, but by some combination of the view and the user's intentions.

  // The next two *Stats fields only influence how the node is shown. Editing them and saving the buffer leaves the graph unchanged, and those edits will be immediately lost, as this data is regenerated each time the view is rebuilt.
  pub graphStats    : GraphNodeStats,
  pub viewStats     : ViewNodeStats,

  pub view_requests : HashSet < ViewRequest >,
  /// Per-stage diff state for the node's '.skg' file existence.
  pub existence     : ExistenceAxes,
  /// Per-stage diff state for the node's membership at this position
  /// in the parent's contains list.
  pub membership    : MembershipAxes,
  /// True iff the node's source is not a git repo (or has no commits).
  /// A per-source fact, not an axis.
  pub not_in_git    : bool,
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

/// Two counts describing a node's containment relationships.
/// herald() computes a terse display string on demand.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeContainRels {
  pub containers : usize,
  pub contents   : usize,
}

/// Two counts describing inbound textlink sources.
/// herald() computes a terse display string on demand.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeLinksourceRels {
  pub sources_with_content    : usize,
  pub sources_without_content : usize,
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
  pub containRels       : Option<NodeContainRels>,
  pub linksourceRels    : Option<NodeLinksourceRels>,
}

/// View-specific statistics about a node.
/// These depend on the node's position in the current view tree.
/// `cycle` depends on ancestors; `parentIs*` depends on the specific parent.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewNodeStats {
  pub cycle             : bool,
  pub parentIsContainer : bool,
  pub parentIsContent   : bool,
  pub sourceAtBoundary  : bool, // True if a root or if source differs from source of nearest truenode ancestor.
}

/// Scaffold nodes are display-only structures
/// that don't correspond per se to nodes in the graph,
/// but encode information about the ViewNodes around them.
#[derive( Debug, Clone, PartialEq )]
pub enum Scaffold {
  Alias { text: String, // an alias for the node's grandparent
          membership: MembershipAxes },
  AliasCol, // The node collects (as children) aliases for its parent.
  BufferRoot, // Not rendered. Makes viewforests easier to process. Its children are the level-1 headlines of the org buffer.
  HiddenInSubscribeeCol, // Child of a Subscribee. Collects nodes that the subscriber hides from its subscriptions, and that are top-level content of this subscribee.
  HiddenOutsideOfSubscribeeCol, // Child of SubscribeeCol. Collects nodes that the subscriber hides from its subscriptions, but that are not top-level content of any of its subscribees.
  // DISPLAY NOTE: Shown after all Subscribees, under the same SubscribeeCol.
  HiddenCol, // Collects nodes hidden by its parent through hides_from_its_subscriptions.
  HiderCol, // Collects nodes that hide its parent through hides_from_its_subscriptions.
  ID { id: ID, // an ID of grandparent (the parent being an IDCol)
       membership: MembershipAxes },
  IDCol, // Collects (as children) Scaffold::IDs for its parent.
  OverriddenCol, // Collects nodes whose view this parent overrides.
  OverriderCol, // Collects nodes that override this parent's view.
  SubscriberCol, // Collects nodes that subscribe to its parent.
  SubscribeeCol, // Collects subscribees for its parent.
  TextChanged { staged: bool, unstaged: bool }, // Indicates title or body changed between stages. Visible in 'git diff mode'. Per-stage bools mark whether the change is staged (HEAD vs index) and/or unstaged (index vs worktree).
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
                        HiddenCol,
                        HiderCol,
                        IDCol,
                        ID,
                        OverriddenCol,
                        OverriderCol,
                        SubscriberCol,
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
  Sourceward,
  Definitive,
}

//
// Implementations
//

impl < Id, Src > TrueNode_Generic < Id, Src > {
  pub fn parent_ignores_it (&self) -> bool {
    self . parentIs != ParentIs::Container }

  /// A phantom is a display-only placeholder for a node that does not
  /// exist as a real, editable entity in the worktree at this position.
  /// Triggered by either:
  /// - any membership axis being '-' (removed in some stage), or
  /// - the worktree existence axis being '-' (file deleted), or
  /// - the "moved twice" pattern: stagedM = +, unstagedM = -.
  pub fn is_phantom (
    &self,
  ) -> bool {
    self . membership . staged   == Some (Sign::Minus)
    || self . membership . unstaged == Some (Sign::Minus)
    || self . existence  . unstaged == Some (Sign::Minus) }

  /// A "removed-here" phantom: a phantom whose '.skg' file is still
  /// present in the worktree (so TypeDB still knows the node and can
  /// answer queries about it). Distinguished from a phantom whose file
  /// is also gone, for which graph queries would fail.
  pub fn is_removedhere_phantom (
    &self,
  ) -> bool {
    self . is_phantom ()
    && self . existence . unstaged != Some (Sign::Minus) }

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

fn herald_from_pair (
  left          : usize,
  left_default  : usize,
  right         : usize,
  right_default : usize,
  separator     : &str,
) -> Option<String> {
  if left == left_default && right == right_default { None }
  else if right == right_default
    { Some ( format! ("{}{}", left, separator )) }
  else if left == left_default
    { Some ( format! ("{}{}", separator, right )) }
  else
    { Some ( format! ("{}{}{}", left, separator, right )) }}

impl NodeContainRels {
  pub fn herald_for_content (&self) -> Option<String> {
    herald_from_pair ( self . containers, 1,
                       self . contents,   0,
                       "{" ) }
  pub fn herald_for_non_content (&self) -> String {
    self . herald_for_content ()
      . unwrap_or_else ( || format! ("{}{{", self . containers) ) } }

impl NodeLinksourceRels {
  pub fn herald (&self) -> Option<String> {
    herald_from_pair ( self . sources_with_content,    0,
                       self . sources_without_content, 0,
                       "→" ) } }

impl ScaffoldKind {
  /// Single source of truth for ScaffoldKind <-> Emacs string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, ScaffoldKind)] = &[
    ("alias",                        ScaffoldKind::Alias),
    ("aliasCol",                     ScaffoldKind::AliasCol),
    ("forestRoot",                   ScaffoldKind::BufferRoot),
    ("hiddenInSubscribeeCol",        ScaffoldKind::HiddenInSubscribeeCol),
    ("hiddenOutsideOfSubscribeeCol", ScaffoldKind::HiddenOutsideOfSubscribeeCol),
    ("hiddenCol",                    ScaffoldKind::HiddenCol),
    ("hiderCol",                     ScaffoldKind::HiderCol),
    ("overriddenCol",                ScaffoldKind::OverriddenCol),
    ("overriderCol",                 ScaffoldKind::OverriderCol),
    ("subscriberCol",                ScaffoldKind::SubscriberCol),
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
      ScaffoldKind::SubscriberCol                => "these subscribe to it",
      ScaffoldKind::OverriddenCol                => "it overrides the view of these",
      ScaffoldKind::OverriderCol                 => "these override the view of it",
      ScaffoldKind::HiddenCol                    => "it hides these from its subscriptions",
      ScaffoldKind::HiderCol                     => "these hide it from their subscriptions",
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
      Scaffold::HiddenCol                    => ScaffoldKind::HiddenCol,
      Scaffold::HiderCol                     => ScaffoldKind::HiderCol,
      Scaffold::OverriddenCol                => ScaffoldKind::OverriddenCol,
      Scaffold::OverriderCol                 => ScaffoldKind::OverriderCol,
      Scaffold::SubscriberCol                => ScaffoldKind::SubscriberCol,
      Scaffold::SubscribeeCol                => ScaffoldKind::SubscribeeCol,
      Scaffold::TextChanged { .. }           => ScaffoldKind::TextChanged,
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
      Scaffold::SubscribeeCol
        | Scaffold::SubscriberCol
        | Scaffold::OverriddenCol
        | Scaffold::OverriderCol
        | Scaffold::HiderCol
        | Scaffold::HiddenCol
        | Scaffold::HiddenInSubscribeeCol
        | Scaffold::HiddenOutsideOfSubscribeeCol => "",
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
      // Empty: the id already appears in the metadata; surfacing it
      // again as the org headline title would duplicate it on the
      // line. The metadata alone is non-empty, so viewnode_to_text
      // is satisfied.
      ViewNodeKind::Unknown (_) => "",
      ViewNodeKind::Inactive (_) => "",
    }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body (&self) -> Option < &String > {
    match &self . kind {
      ViewNodeKind::True    (t) => t . body (),
      ViewNodeKind::Scaff   (_) => None,
      ViewNodeKind::Deleted (d) => d . body . as_ref (),
      ViewNodeKind::DeletedScaff (_) => None,
      ViewNodeKind::Unknown (_) => None,
      ViewNodeKind::Inactive (_) => None,
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
      containRels       : Some ( NodeContainRels {
        containers : 1, contents : 0 } ),
      linksourceRels    : Some ( NodeLinksourceRels {
        sources_with_content    : 0,
        sources_without_content : 0 } ),
    }} }

impl Default for ViewNodeStats {
  fn default () -> Self {
    ViewNodeStats {
      cycle             : false,
      parentIsContainer : true,
      parentIsContent   : false,
      sourceAtBoundary  : false,
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
    parentIs          : ParentIs::Container,
    graphStats     : GraphNodeStats::default(),
    viewStats      : ViewNodeStats::default(),
    view_requests  : HashSet::new(),
    existence      : ExistenceAxes::default(),
    membership     : MembershipAxes::default(),
    not_in_git     : false,
    indef_or_def   : IndefOrDef::Definitive {
      body         : None,
      edit_request : None },
  }}

/// Create an indefinitive phantom ViewNode with the given diff axes.
/// At least one membership axis or the unstaged-existence axis should be
/// negative for this to be a real phantom; callers must ensure that.
pub fn mk_phantom_viewnode (
  id         : ID,
  source     : SourceName,
  title      : String,
  existence  : ExistenceAxes,
  membership : MembershipAxes,
) -> ViewNode {
  let mut viewnode : ViewNode =
    mk_indefinitive_viewnode ( id, source, title, ParentIs::Container );
  if let ViewNodeKind::True ( ref mut t ) = viewnode . kind
    { t . existence  = existence;
      t . membership = membership; }
  viewnode }

pub fn mk_definitive_viewnode (
  id     : ID,
  source : SourceName,
  title  : String,
  body   : Option < String >,
) -> ViewNode { mk_viewnode ( id,
                            source,
                            title,
                            ParentIs::Container,
                            IndefOrDef::Definitive {
                              body,
                              edit_request : None },
                            HashSet::new () ) } // view_requests

/// Build an UnknownNode wrapper. Use when a referenced ID resolves
/// to nothing in in_rust_graph, on disk, or via any phantom/diff procedure
/// -- the placeholder lets the view render the line as a herald
/// rather than aborting the whole BFS expansion.
pub fn mk_unknown_viewnode (
  id : ID,
) -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::Unknown ( UnknownNode { id } ),
  }}

pub fn mk_inactive_viewnode (
  id         : ID,
  source     : SourceName,
  membership : MembershipAxes,
) -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::Inactive (
      InactiveNode { id, source, membership } ),
  }}

/// Create an indefinitive ViewNode from disk data.
/// Body is always None since indefinitive nodes don't have editable content.
pub fn mk_indefinitive_viewnode (
  id     : ID,
  source : SourceName,
  title  : String,
  parentIs  : ParentIs,
) -> ViewNode { mk_viewnode ( id,
                            source,
                            title,
                            parentIs,
                            IndefOrDef::Indefinitive,
                            HashSet::new ( )) } // view_requests

/// Convert a definitive ViewNode to indefinitive.
/// Discards body and edit_request.
/// Errors if the input is not a TrueNode.
pub fn mk_indefinitive_from_viewnode (
  viewnode : ViewNode,
  parentIs    : ParentIs,
) -> Result < ViewNode, String > {
  let ViewNodeKind::True (t) = viewnode . kind
    else { return Err (
      "mk_indefinitive_from_viewnode: expected TrueNode"
        . to_string () ) };
  Ok ( mk_indefinitive_viewnode ( t . id,
                                  t . source,
                                  t . title,
                                  parentIs )) }

/// Create a ViewNode with *nearly* full metadata control.
/// The exception is that the 'GraphNodeStats' and 'ViewNodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the ViewNode tree.
pub fn mk_viewnode (
  id            : ID,
  source        : SourceName,
  title         : String,
  parentIs         : ParentIs,
  indef_or_def  : IndefOrDef,
  view_requests : HashSet < ViewRequest >,
) -> ViewNode {
  ViewNode { focused     : false,
             folded      : false,
             body_folded : false,
             kind        : ViewNodeKind::True (
               TrueNode { parentIs,
                          view_requests,
                          indef_or_def,
                          .. default_truenode (
                            id, source, title ) } ) }}

/// Create a Scaffold ViewNode from a Scaffold.
pub fn viewnode_from_scaffold ( scaffold : Scaffold ) -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::Scaff (scaffold),
  }}

/// Helper to create a BufferRoot ViewNode.
pub fn viewforest_root_viewnode () -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::Scaff (Scaffold::BufferRoot),
  }}
