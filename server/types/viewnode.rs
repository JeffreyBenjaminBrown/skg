/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'NodeComplete' type.
/// Nodes of the tree are represented via the 'ViewNode' type.
///   (That name might change once there are more clients. The only client so far is written in Emacs org-mode; hence the name.)
/// Some 'ViewNode's correspond to whole graph nodes; these are 'Vognode's.
/// Others encode information about neighboring tree nodes, such as
/// aliases, IDs, and partner collections.

use super::git::{ExistenceAxes, MembershipAxes, Sign};
use super::misc::{ID, SourceName};
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

/// Whether this node participates in the collection represented by
/// its visible parent. For an ordinary Vognode parent, that collection
/// is the parent's content. For a PartnerCol parent, the RoleCol
/// decides the relation collection. For other kinds of parent's,
/// a node's ParentIs has no effect.
///
/// PITFALL: If a vognode is indefinitive, *none* of it children affect it,
/// just as edits to itself do not affect it,
/// regardless of what the children might claim with their ParentIs field.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParentIs {
  Affected,    // default: upon saving, this can affect its parent's collection
  Independent, // upon saving, this cannot affect its parent
  Absent,      // No (visible) parent. (BufferRoot is not visible.)
}

/// Why a generated node was originally displayed under its visible parent.
/// This is view history used for display and stale-relation validation,
/// not save extraction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Birth {
  Unremarkable,
  ContainsParent,
  LinksToParent,
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
  Vognode       (Vognode),
  QualCol       (QualCol),
  Qual          (Qual),
  PartnerCol    (RoleCol),
  BufferRoot,
  DeadScaffold,
}

#[derive( Debug, Clone, PartialEq )]
pub enum Vognode {
  Normal   (TrueNode),
  DiffPhantom (DiffPhantomNode), // Diff-only placeholder: absent from git worktree but present in git HEAD ("removed"), or still present in worktree but no longer a member of its parent ("removedHere"). Exists only in the diff view. TODO/DONE/local-view-update/plan_v2.org §11 payload reduction (2026-06-04): now carries a slim DiffPhantomNode, not a TrueNode -- a phantom is always indefinitive/bodyless and its parentIs is never read or rendered, so it needs none of TrueNode's parentIs/birth/viewStats/view_requests/indef_or_def. See DiffPhantomNode_Generic + TODO/DONE/local-view-update/plan_v2.org §18.
  Inactive (InactiveNode), // From a source that is inactive (see "source sets").
  Unknown  (UnknownNode), // If it *ever* existed in the graph, Skg didn't find it.
  Deleted  (DeletedNode), // No longer exists in the graph.
}

/// A placeholder ("phantom") Vognode for a node whose .skg file a save just
/// removed -- one of the three placeholder kinds, alongside DiffPhantomNode and
/// UnknownNode. All three stand in for something that is NOT a current graph
/// member (so all three are inert on save and excluded from the view's
/// collateral pids, `pids_from_viewforest`); they differ in *why* the node is
/// absent and thus in how much they can still say about it.
///
/// ARISES: during post-save / collateral re-render (NOT in git diff mode), when
/// a node already materialized in the view turns up in
/// `deleted_by_this_save_pids`. A Normal content child flips here via
/// `mutate_truenode_to_deletednode`; an Inactive node via
/// `convert_inactive_to_deleted_if_deleted`. So it marks a node that genuinely
/// no longer exists in the graph, killed by a completed save (this buffer's or a
/// shared one's) -- not a git artifact.
///
/// USED: inert -- it generates no save instructions and is excluded from its
/// parent's contains list. It is never "completed", yet it retains its children
/// (which `mark_orphans_under_dead_parents_independent` demotes to Independent)
/// so the user's subtree under a vanished node survives. A childless Deleted is
/// pruned by the postorder sweep (`is_self_deletable_when_empty`), and a Deleted
/// may stand as a view root (validate_tree) so a deleted root still shows.
///
/// DISTINCT INFO: unlike UnknownNode it knows its `source`, and unlike either
/// other phantom it keeps the `title`/`body` it last displayed -- because it was
/// a fully materialized node right up until the save deleted it, so that text is
/// still worth showing. (The title is empty for one promoted from an Inactive
/// node, which carried none.) It holds no diff axes: it is not about git stages.
#[derive( Debug, Clone, PartialEq )]
pub struct DeletedNode {
  pub id     : ID,
  pub source : SourceName,
  pub title  : String,
  pub body   : Option < String >,
}

/// A placeholder ("phantom") Vognode for a reference that resolves to nothing --
/// one of the three placeholder kinds, alongside DiffPhantomNode and DeletedNode
/// (see DeletedNode for the shared framing).
///
/// ARISES: when some node's `contains` (or similar list) names an ID that has no
/// record anywhere -- not a primary pid, not an extra_id, not in TypeDB, not on
/// disk, and not recoverable through any phantom/diff procedure. Built by
/// `mk_unknown_viewnode`, e.g. when `nodecomplete_and_viewnode_from_id` returns
/// None. A genuine dangling pointer.
///
/// USED: it lets the view degrade gracefully -- carrying the bad reference as
/// its own kind, rather than erroring, is what keeps a single bad reference deep
/// in a subtree from killing the whole view at its root. Inert on save (it emits
/// no instructions, though its descendants still might, so extraction recurses
/// through it). If it is still a member of its parent's contains it is retained
/// (a present-but-unresolvable reference); if it is no longer a member it
/// converts to a DeadScaffold (`convert_nonmember_unknown_children_to_dead`) and
/// is pruned.
///
/// DISTINCT INFO: it carries ONLY the `id`. Unlike DeletedNode it has no source
/// (it is the one Vognode kind for which `pid_and_source` returns None) and no
/// last-seen text; unlike DiffPhantomNode it has no diff axes. That emptiness IS
/// the information: a reference exists, but we have no record of its target.
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
pub type MpTruenode = TrueNode_Generic < Option < ID >,
                                         Option < SourceName >>;

/// A ViewNode that corresponds to a NodeComplete.
#[derive( Debug, Clone, PartialEq )]
pub struct TrueNode_Generic < Id, Src > {
  pub title         : String,
  pub id            : Id,
  pub source        : Src,
  pub parentIs      : ParentIs,
  pub birth         : Birth,

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

pub type DiffPhantomNode   = DiffPhantomNode_Generic < ID, SourceName >;
pub type MpDiffPhantomNode = DiffPhantomNode_Generic < Option < ID >,
                                                       Option < SourceName >>;

/// The slim payload of a `Vognode::DiffPhantom` -- one of the three placeholder
/// ("phantom") Vognode kinds, alongside DeletedNode and UnknownNode (see
/// DeletedNode for the shared framing).
///
/// ARISES: only in git diff mode, from the diff-completion code. Two shapes,
/// both detected by `diff_axes_require_phantom`: a "removed" member (present in
/// the parent's HEAD/index contains but gone from the worktree's, inserted by
/// `insert_phantoms_for_missing_contains` / `mk_phantom_viewnode`), or a Normal
/// node flipped in place by `normal_to_phantom` because its own membership/
/// existence axes went negative. "removed" vs "removedHere" (its .skg file is
/// still in the worktree, so TypeDB can still answer about it) is told apart by
/// `is_removedhere_diffPhantom`.
///
/// USED: as a read-only diff annotation. It depicts a removed member at its
/// correct HEAD position among surviving siblings, decorated with per-stage diff
/// atoms. Always indefinitive and bodyless (enforced by `normal_to_phantom` /
/// `mk_phantom_viewnode`); its parentIs is never read or rendered (implicit
/// Affected); a Normal child left under it is demoted to Independent.
///
/// DISTINCT INFO: it is the only phantom that carries the git-diff coordinates
/// -- per-stage `existence` and `membership` axes plus `not_in_git` -- because
/// it exists solely to show a change between git snapshots. It keeps a `title`
/// and `source` (resolved via `title_for_phantom`; the source may be the
/// SourceName NOT_FOUND sentinel) and `graphStats`, which IS rendered on
/// phantoms. It needs NONE of TrueNode's parentIs / birth / viewStats /
/// view_requests / indef_or_def (TODO/DONE/local-view-update/plan_v2.org §11 reduction; see §18): nothing
/// reads a phantom's parentIs, and every phantom is indefinitive.
#[derive( Debug, Clone, PartialEq )]
pub struct DiffPhantomNode_Generic < Id, Src > {
  pub title      : String,
  pub id         : Id,
  pub source     : Src,
  /// Per-stage diff state for the node's '.skg' file existence.
  pub existence  : ExistenceAxes,
  /// Per-stage diff state for the node's membership at this position
  /// in the parent's contains list.
  pub membership : MembershipAxes,
  /// True iff the node's source is not a git repo (or has no commits).
  pub not_in_git : bool,
  pub graphStats : GraphNodeStats,
}

impl < Id, Src > DiffPhantomNode_Generic < Id, Src > {
  /// Build a phantom payload from a TrueNode, keeping only the phantom-relevant
  /// fields and discarding parentIs / birth / viewStats / view_requests /
  /// indef_or_def. Used when flipping a Normal node to a phantom and by the
  /// placed<->maybe-placed conversions.
  pub fn from_truenode ( t : TrueNode_Generic < Id, Src > ) -> Self {
    DiffPhantomNode_Generic {
      title      : t . title,
      id         : t . id,
      source     : t . source,
      existence  : t . existence,
      membership : t . membership,
      not_in_git : t . not_in_git,
      graphStats : t . graphStats,
    }}

  /// A phantom is always indefinitive, hence never has a body.
  pub fn body (&self) -> Option < &String > { None }
  pub fn is_indefinitive (&self) -> bool { true }

  /// True iff this node's diff axes require phantom display; for a correctly
  /// constructed phantom this holds, but some shared code asks regardless.
  pub fn should_be_diffPhantom (&self) -> bool {
    diff_axes_require_phantom (&self . existence, &self . membership) }

  /// A "removed-here" phantom whose '.skg' file is still in the worktree.
  pub fn is_removedhere_diffPhantom (&self) -> bool {
    self . should_be_diffPhantom ()
    && self . existence . unstaged != Some (Sign::Minus) }
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

#[derive( Debug, Clone, Copy, PartialEq, Eq )]
pub enum QualCol {
  ID,
  Alias,
}

#[derive( Debug, Clone, PartialEq )]
pub enum Qual {
  Alias { text: String, // an alias for the node's grandparent
          membership: MembershipAxes },
  ID { id: ID, // an ID of grandparent (the parent being an IDCol)
       membership: MembershipAxes },
  TextChanged { staged: bool, unstaged: bool }, // Indicates title or body changed between stages. Visible in 'git diff mode'. Per-stage bools mark whether the change is staged (HEAD vs index) and/or unstaged (index vs worktree).
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoleCol {
  Subscribee, // Collects subscribees its parent subscribes to. Writeable.
  Subscriber, // Collects nodes that subscribe to its parent. Read-only (editable from the other side of the relationship).
  Overridden, // Collects nodes whose view its parent overrides. Writeable.
  Overrider, // Collects nodes that override its parent's view. Read-only (editable from the other side of the relationship).
  Hider, // Collects nodes that hide its parent. Read-only (editable from the other side of the relationship).
  Hidden, // Collects nodes its parent hides. Read-only (but these relationships are editable from this side of the relationhip, within the parent's SubscribeeCol).
  HiddenInSubscribee, // Child of a subscribee-as-such. Collects children of the subscribee that the subscriber hides. Read-only (but these relationships are editable by modifying the listed contents of the subscribee-as-such).
  HiddenOutsideOfSubscribee, // Child of a SubscribeeCol. Collects things unnecessarily hidden by the SubscribeeCol's parent, because they are not contained by anything it subscribes to. Read-only. Shown after all Subscribees, under the same SubscribeeCol.
  // TODO | PITFALL: HiddenOutsideOfSubscribee should be editable. Currently the client offers no easy way for a user to unhide things unnecessarily hidden. (It is technically possible, by creating a node you own, subscribing to it, and then modifying a subscribee-as-such representative of the new node. But that's baroque.)
}

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

/// True when a node's diff axes require displaying it with the
/// `Vognode::DiffPhantom` variant. Triggered by either:
/// - any membership axis being '-' (removed in some stage), or
/// - the worktree existence axis being '-' (file deleted), or
/// - the "moved twice" pattern: stagedM = +, unstagedM = -.
/// Shared by TrueNode_Generic and DiffPhantomNode_Generic, which both carry
/// these axes.
pub fn diff_axes_require_phantom (
  existence  : &ExistenceAxes,
  membership : &MembershipAxes,
) -> bool {
  membership . staged   == Some (Sign::Minus)
  || membership . unstaged == Some (Sign::Minus)
  || existence  . unstaged == Some (Sign::Minus) }

impl < Id, Src > TrueNode_Generic < Id, Src > {
  pub fn should_be_diffPhantom (
    &self,
  ) -> bool {
    diff_axes_require_phantom (&self . existence, &self . membership) }

  /// A "removed-here" phantom: a phantom whose '.skg' file is still
  /// present in the worktree (so TypeDB still knows the node and can
  /// answer queries about it). Distinguished from a phantom whose file
  /// is also gone, for which graph queries would fail.
  pub fn is_removedhere_diffPhantom (
    &self,
  ) -> bool {
    self . should_be_diffPhantom ()
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

impl RoleCol {
  pub fn repr_in_client (self) -> &'static str {
    match self {
      RoleCol::Subscribee                => "subscribeeCol",
      RoleCol::Subscriber                => "subscriberCol",
      RoleCol::Overridden                => "overriddenCol",
      RoleCol::Overrider                 => "overriderCol",
      RoleCol::Hider                     => "hiderCol",
      RoleCol::Hidden                    => "hiddenCol",
      RoleCol::HiddenInSubscribee        => "hiddenInSubscribeeCol",
      RoleCol::HiddenOutsideOfSubscribee => "hiddenOutsideOfSubscribeeCol",
    }}

  pub fn from_client_string (s : &str) -> Option<RoleCol> {
    match s {
      "subscribeeCol"                => Some (RoleCol::Subscribee),
      "subscriberCol"                => Some (RoleCol::Subscriber),
      "overriddenCol"                => Some (RoleCol::Overridden),
      "overriderCol"                 => Some (RoleCol::Overrider),
      "hiderCol"                     => Some (RoleCol::Hider),
      "hiddenCol"                    => Some (RoleCol::Hidden),
      "hiddenInSubscribeeCol"        => Some (RoleCol::HiddenInSubscribee),
      "hiddenOutsideOfSubscribeeCol" => Some (RoleCol::HiddenOutsideOfSubscribee),
      _                              => None,
    } }

  pub fn origin_depth (self) -> usize {
    match self {
      RoleCol::Subscribee
        | RoleCol::Subscriber
        | RoleCol::Overridden
        | RoleCol::Overrider
        | RoleCol::Hider
        | RoleCol::Hidden => 1,
      RoleCol::HiddenInSubscribee => 3,
      RoleCol::HiddenOutsideOfSubscribee => 2, } }
}

impl QualCol {
  pub fn repr_in_client (self) -> &'static str {
    match self {
      QualCol::Alias => "aliasCol",
      QualCol::ID    => "idCol",
    } }

  pub fn origin_depth (self) -> usize {
    1 }
}

impl Qual {
  pub fn repr_in_client (&self) -> &'static str {
    match self {
      Qual::Alias { .. }       => "alias",
      Qual::ID { .. }          => "id",
      Qual::TextChanged { .. } => "textChanged",
    } }

  pub fn title (&self) -> &str {
    match self {
      Qual::Alias { text, .. } => text,
      Qual::ID    { id, .. }   => id,
      Qual::TextChanged { .. } => "",
    } }
}

impl Vognode {
  /// The id of a Normal or DiffPhantom vognode (the two TrueNode-ish kinds),
  /// or None for Inactive/Unknown/Deleted. Since the TODO/DONE/local-view-update/plan_v2.org §11 reduction the Normal
  /// and DiffPhantom payloads are different types, so this returns just the
  /// shared identity rather than a `&TrueNode`.
  pub fn normal_or_phantom_id (&self) -> Option<&ID> {
    match self {
      Vognode::Normal     (t) => Some (&t . id),
      Vognode::DiffPhantom (p) => Some (&p . id),
      _ => None,
    } }

  pub fn id (&self) -> &ID {
    match self {
      Vognode::Normal   (t) => &t . id,
      Vognode::DiffPhantom  (p) => &p . id,
      Vognode::Inactive (i) => &i . id,
      Vognode::Unknown  (u) => &u . id,
      Vognode::Deleted  (d) => &d . id,
    } }

  pub fn pid_and_source (
    &self,
  ) -> Option<(&ID, &SourceName)> {
    match self {
      Vognode::Normal   (t) => Some ((&t . id, &t . source)),
      Vognode::DiffPhantom (p) => Some ((&p . id, &p . source)),
      Vognode::Inactive (i) => Some ((&i . id, &i . source)),
      Vognode::Deleted  (d) => Some ((&d . id, &d . source)),
      Vognode::Unknown  (_) => None,
    } }
}

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
  pub fn normal_to_phantom (
    &mut self,
  ) {
    if let ViewNodeKind::Vognode (Vognode::Normal (t))
      = &self . kind
      { if t . should_be_diffPhantom ()
        { // A phantom is ALWAYS indefinitive and renders no body (Jeff's
          // TODO/DONE/local-view-update/progress.org §9 TODO): the diff view presumes git literacy (magit
          // shows the real node), and a phantom must never be a node's
          // definitive instance -- if Removed it has nothing to define, and if
          // RemovedHere "edit it here, where it isn't" is dangerously
          // confusing. So drop body/edit_request when flipping a (possibly
          // Definitive) Normal node to a phantom. mk_phantom_viewnode already
          // builds from an Indefinitive base, so now every phantom is
          // indefinitive by construction.
          let phantom : DiffPhantomNode =
            DiffPhantomNode::from_truenode ( t . clone () );
          self . kind = ViewNodeKind::Vognode (
            Vognode::DiffPhantom (phantom)); }}}

  pub fn title (&self) -> &str {
    match &self . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) => &t . title,
      ViewNodeKind::Vognode (Vognode::DiffPhantom (p)) => &p . title,
      ViewNodeKind::Vognode (Vognode::Deleted (d)) =>
        &d . title,
      ViewNodeKind::Qual (q) =>
        q . title (),
      ViewNodeKind::QualCol (_)
        | ViewNodeKind::PartnerCol (_)
        | ViewNodeKind::BufferRoot
        | ViewNodeKind::DeadScaffold
        | ViewNodeKind::Vognode (Vognode::Unknown (_))
        | ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
        "",
    }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body (&self) -> Option < &String > {
    match &self . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) => t . body (),
      ViewNodeKind::Vognode (Vognode::DiffPhantom (p)) => p . body (),
      ViewNodeKind::Vognode (Vognode::Deleted (d)) => d . body . as_ref (),
      ViewNodeKind::Vognode (Vognode::Unknown (_))
        | ViewNodeKind::Vognode (Vognode::Inactive (_))
        | ViewNodeKind::QualCol (_)
        | ViewNodeKind::Qual (_)
        | ViewNodeKind::PartnerCol (_)
        | ViewNodeKind::BufferRoot
        | ViewNodeKind::DeadScaffold => None,
    }}

  pub fn is_truenode_and_parentIs_affected (&self) -> bool {
    match &self . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) =>
        t . parentIs == ParentIs::Affected,
      _ => false,
    }}

  pub fn id_if_vognode (&self) -> Option<&ID> {
    match &self . kind {
      ViewNodeKind::Vognode (v) => Some (v . id ()),
      _ => None,
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
    parentIs       : ParentIs::Affected,
    birth          : Birth::Unremarkable,
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
    mk_indefinitive_viewnode ( id, source, title, ParentIs::Affected );
  if let ViewNodeKind::Vognode (Vognode::Normal (mut t)) = viewnode . kind
    { t . existence  = existence;
      t . membership = membership;
      viewnode . kind = ViewNodeKind::Vognode (
        Vognode::DiffPhantom ( DiffPhantomNode::from_truenode (t) )); }
  else
    // mk_indefinitive_viewnode always yields a Normal vognode; if that ever
    // changes, fail loudly rather than silently return a non-phantom.
    { unreachable! (
        "mk_phantom_viewnode: mk_indefinitive_viewnode did not yield a Normal vognode" ); }
  viewnode }

pub fn mk_definitive_viewnode (
  id     : ID,
  source : SourceName,
  title  : String,
  body   : Option < String >,
) -> ViewNode { mk_viewnode ( id,
                            source,
                            title,
                            ParentIs::Affected,
                            Birth::Unremarkable,
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
    kind        : ViewNodeKind::Vognode (
      Vognode::Unknown ( UnknownNode { id } ) ),
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
    kind        : ViewNodeKind::Vognode (
      Vognode::Inactive ( InactiveNode { id, source, membership } ) ),
  }}

/// Create an indefinitive ViewNode from disk data.
/// Body is always None since indefinitive nodes don't have editable content.
pub fn mk_indefinitive_viewnode (
  id     : ID,
  source : SourceName,
  title  : String,
  parentIs  : ParentIs,
) -> ViewNode {
  mk_indefinitive_viewnode_with_birth (
    id, source, title, parentIs, Birth::Unremarkable ) }

pub fn mk_indefinitive_viewnode_with_birth (
  id       : ID,
  source   : SourceName,
  title    : String,
  parentIs : ParentIs,
  birth    : Birth,
) -> ViewNode { mk_viewnode ( id,
                            source,
                            title,
                            parentIs,
                            birth,
                            IndefOrDef::Indefinitive,
                            HashSet::new ( )) } // view_requests

/// Convert a definitive ViewNode to indefinitive.
/// Discards body and edit_request.
/// Errors if the input is not a TrueNode.
pub fn mk_indefinitive_from_viewnode (
  viewnode : ViewNode,
  parentIs    : ParentIs,
  birth       : Birth,
) -> Result < ViewNode, String > {
  let (id, source, title) : (ID, SourceName, String) =
    match viewnode . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) =>
        ( t . id, t . source, t . title ),
      ViewNodeKind::Vognode (Vognode::DiffPhantom (p)) =>
        ( p . id, p . source, p . title ),
      _ => return Err (
        "mk_indefinitive_from_viewnode: expected TrueNode"
          . to_string () ) };
  Ok ( mk_indefinitive_viewnode_with_birth (
    id, source, title, parentIs, birth )) }

/// Create a ViewNode with *nearly* full metadata control.
/// The exception is that the 'GraphNodeStats' and 'ViewNodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the ViewNode tree.
pub fn mk_viewnode (
  id            : ID,
  source        : SourceName,
  title         : String,
  parentIs      : ParentIs,
  birth         : Birth,
  indef_or_def  : IndefOrDef,
  view_requests : HashSet < ViewRequest >,
) -> ViewNode {
  ViewNode { focused     : false,
             folded      : false,
             body_folded : false,
             kind        : ViewNodeKind::Vognode (
               Vognode::Normal (
                 TrueNode { parentIs,
                            birth,
                            view_requests,
                            indef_or_def,
                            .. default_truenode (
                              id, source, title ) } ) ) }}

/// Helper to create a BufferRoot ViewNode.
pub fn viewforest_root_viewnode () -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::BufferRoot,
  }}
