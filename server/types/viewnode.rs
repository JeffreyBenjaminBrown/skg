/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'Graphnode' type.
/// Nodes of the tree are represented via the 'Viewnode' type.
///   (That name might change once there are more clients. The only client so far is written in Emacs org-mode; hence the name.)
/// Some 'Viewnode's represent graphnodes, which need not exist; these are
/// 'Vognode's (active, inactive, or phantom).
/// Others encode information about neighboring tree nodes, such as
/// aliases, IDs, and partner folders.

use super::git::{NodeAxes, RelationshipAxes, Sign};
use super::misc::{ID, RepoName};
use super::nodes::complete::Flag;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

/// Whether this node participates in the membership represented by
/// its visible parent. For an ordinary Vognode parent, that membership
/// is the parent's content. For a PartnerFolder parent, the PartnerFolder
/// decides the membership (per its relation and role). For other kinds of parent's,
/// a node's AffectsParent has no effect.
///
/// PITFALL: If a vognode is write-protected, *none* of it children affect it,
/// just as edits to itself do not affect it,
/// regardless of what the children might claim with their AffectsParent field.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AffectsParent {
  True,  // this node affects its parent
  False, // it doesn't
  NA,    // It has no parent
}

/// Why a generated node was originally displayed under its visible parent.
/// This is view history used for display and stale-relation validation,
/// not save extraction.
///
/// A 'Backpath(role)' node was grafted by the backpath engine as an
/// ancestry partner; the RelationRole names the role that partner plays
/// toward its org-parent (the origin) -- e.g. 'CONTAINER' for a
/// containerward ancestor, 'MENTIONER' for a node that links to the
/// origin. The role determines the wire ROLENAME and the herald glyph
/// (see PARTNER_ROLE_VOCAB).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Birth {
  Unremarkable,
  Backpath (RelationRole),
}

//
// Type declarations
//

/// Corresponds to an Emacs headline-body pair.
#[derive( Debug, Clone, PartialEq )]
pub struct Viewnode {
  pub focused     : bool,
  pub folded      : bool, // Is this     hidden in its parent?
  pub body_folded : bool, // Is the body hidden in this?
  pub kind        : ViewnodeKind,
}

#[derive( Debug, Clone, PartialEq )]
pub enum ViewnodeKind {
  Vognode       (Vognode),
  PropertyFolder       (PropertyFolder),
  Property          (Property),
  PartnerFolder    (PartnerFolder),
  BufferRoot,
  DeadViewnode,
}

/// A viewnode that represents a graphnode, which need not exist: Active
/// and Inactive vognodes represent current graph members; a Phantom
/// represents a missing or historical one.
#[derive( Debug, Clone, PartialEq )]
pub enum Vognode {
  Active   (ActiveVognode),
  Inactive (InactiveVognode), // From a repo that is inactive (see "repo sets").
  Phantom  (Phantom),
}

/// The three display-only placeholder kinds. None of them is a current graph
/// member: all are inert on save and excluded from the view's collateral pids
/// (`pids_from_viewforest`). They differ in *why* the node is absent and thus
/// in how much they can still say about it.
#[derive( Debug, Clone, PartialEq )]
pub enum Phantom {
  Diff    (PhantomDiff), // Diff-only placeholder: absent from git worktree but present in git HEAD ("removed"), or still present in worktree but no longer a member of its parent ("removedHere"). Exists only in the diff view. TODO/DONE/local-view-update/plan_v2.org §11 payload reduction (2026-06-04): now carries a slim PhantomDiff, not an ActiveVognode -- a phantom is always write-protected/bodyless and its affectsParent is never read or rendered, so it needs none of ActiveVognode's affectsParent/birth/viewStats/view_requests/editability. See PhantomDiff_Generic + TODO/DONE/local-view-update/plan_v2.org §18.
  Deleted (PhantomDeleted), // Epistemically: No longer exists in the graph. Procedurally: Skg just watched the user delete this node (maybe from a different view), but for some reason (e.g. its view-descendents are interesting, or it is a root) had to retain an image of it here.
  // PITFALL: There is an exception. If Skg watches a user delete a node, while that user has a view of a foreign node that refers to the deleted node, that "foreigner's view" will show it as Unknown rather than Deleted. This is to maintain consistency with how that relationship to a nonexistent node will appear when viewed in later sessions.
  Unknown (PhantomUnknown), // Skg can't find it (and, unlike Deleted, does not know why). Can result from bad data, or from a reference to another user's node that has since been deleted.
}

/// A placeholder ("phantom") for a node whose .skg file a save just
/// removed -- one of the three placeholder kinds, alongside PhantomDiff and
/// PhantomUnknown. All three stand in for something that is NOT a current graph
/// member (so all three are inert on save and excluded from the view's
/// collateral pids, `pids_from_viewforest`); they differ in *why* the node is
/// absent and thus in how much they can still say about it.
///
/// ARISES: during post-save / collateral re-render (NOT in git diff mode), when
/// a node already materialized in the view turns up in
/// `deleted_by_this_save_pids`. An Active content child flips here via
/// `mutate_activeVognode_to_deletednode`. (An Inactive node is NOT flipped: it is an
/// anonymous placeholder, and turning it into a DELETED marker would leak that a
/// hidden node vanished, so it just lingers until the next full rerender drops
/// it.) So it marks a node that genuinely no longer exists in the graph, killed
/// by a completed save (this buffer's or a shared one's) -- not a git artifact.
///
/// USED: inert -- it generates no save instructions and is excluded from its
/// parent's contains list. It is never "completed", yet it retains its children
/// (which `mark_orphans_under_dead_parents_false` demotes to Independent)
/// so the user's subtree under a vanished node survives. A childless Deleted is
/// pruned by the postorder sweep (`is_self_deletable_when_empty`), and a Deleted
/// may stand as a view root (validate_tree) so a deleted root still shows.
///
/// DISTINCT INFO: unlike PhantomUnknown it knows its `repo`, and unlike either
/// other phantom it keeps the `title`/`body` it last displayed -- because it was
/// a fully materialized node right up until the save deleted it, so that text is
/// still worth showing. (The title is empty for one promoted from an Inactive
/// node, which carried none.) It holds no diff axes: it is not about git stages.
#[derive( Debug, Clone, PartialEq )]
pub struct PhantomDeleted {
  pub id     : ID,
  pub home_repo : RepoName,
  pub title  : String,
  pub body   : Option < String >,
}

/// A placeholder ("phantom") for a reference that resolves to nothing --
/// one of the three placeholder kinds, alongside PhantomDiff and PhantomDeleted
/// (see PhantomDeleted for the shared framing).
///
/// ARISES: when some node's `contains` (or similar list) names an ID that has no
/// record anywhere -- not a primary pid or extra_id in the graph, not on disk,
/// and not recoverable through any phantom/diff procedure. Built by
/// `mk_unknown_viewnode`, e.g. when `graphnode_and_viewnode_from_id` returns
/// None. A genuine dangling pointer.
///
/// USED: it lets the view degrade gracefully -- carrying the bad reference as
/// its own kind, rather than erroring, is what keeps a single bad reference deep
/// in a subtree from killing the whole view at its root. Inert on save (it emits
/// no instructions, though its descendants still might, so extraction recurses
/// through it). If it is still a member of its parent's contains it is retained
/// (a present-but-unresolvable reference); if it is no longer a member it
/// converts to a DeadViewnode (`convert_nonmember_unknown_children_to_dead`) and
/// is pruned.
///
/// DISTINCT INFO: it carries ONLY the `id`. Unlike PhantomDeleted it has no repo
/// (it is the one Phantom kind for which `pid_and_repo` returns None) and no
/// last-seen text; unlike PhantomDiff it has no diff axes. That emptiness IS
/// the information: a reference exists, but we have no record of its target.
#[derive( Debug, Clone, PartialEq )]
pub struct PhantomUnknown {
  pub id                 : ID,
  /// Display-only fact about this occurrence's binding relationship.
  pub relRepo         : Option<RepoName>,
  /// A pending relRepo change, consumed only by save.
  pub relRepo_request : Option<RepoName>,
}

/// An anonymous "something from an inactive repo is/was here"
/// placeholder. It carries NO data on purpose: an inactive node's id,
/// repo, title, etc. describe content the user hid by restricting
/// the active repo-set, so rendering any of it would leak.
///
/// SCOPE: an InactiveVognode arises ONLY from a repo-set REDUCTION of an
/// already-drawn buffer ('update_buffer/repo_switch.rs' converts
/// now-inactive Active nodes in place), where it is kept to host
/// already-drawn active descendants. A de-novo render under a
/// restricted set NEVER creates one: inactive content members are
/// omitted from goal lists ('omit_inactive_members') and their content
/// is never expanded (completion does nothing at an inactive node's
/// visit). So a public node reachable only through a private parent
/// simply does not appear on a fresh restricted view.
///
/// Being dataless, it is inert everywhere: it emits no save intention
/// (its membership is owned by the disk weave), is not a collateral
/// pid, is irrelevant to every reconciler (preserved as-is, never
/// goal-matched), and renders as the bare atom 'inactiveNode'.
#[derive( Debug, Clone, PartialEq )]
pub struct InactiveVognode;

pub type ActiveVognode   = ActiveVognode_Generic < ID, RepoName >;
pub type MpActiveVognode = ActiveVognode_Generic < Option < ID >,
                                             Option < RepoName >>;

/// A Viewnode that corresponds to a Graphnode.
#[derive( Debug, Clone, PartialEq )]
pub struct ActiveVognode_Generic < Id, Src > {
  pub title         : String,
  pub id            : Id,
  pub home_repo        : Src,
  pub affectsParent      : AffectsParent,
  pub birth         : Birth,

  // The next two *Stats fields only influence how the node is shown. Editing them and saving the buffer leaves the graph unchanged, and those edits will be immediately lost, as this data is regenerated each time the view is rebuilt.
  pub graphStats    : GraphnodeStats,
  pub viewStats     : ViewnodeStats,
  /// A requested repo for this occurrence's binding relationship. Unlike
  /// `viewStats.relRepo`, this is save intent.
  pub relRepo_request : Option<RepoName>,

  pub view_requests : HashSet < ViewRequest >,
  /// Per-stage diff state for the node's '.skg' file (the node axis).
  pub node_axes     : NodeAxes,
  /// Per-stage diff state for the node's relationship at this position
  /// in the parent's contains list (the relationship axis).
  pub relationship_axes    : RelationshipAxes,
  /// True iff the node's repo is not a git repo (or has no commits).
  /// A per-repo fact, not an axis.
  pub not_in_git    : bool,
  pub editability  : Editability,
}

pub type PhantomDiff   = PhantomDiff_Generic < ID, RepoName >;
pub type MpPhantomDiff = PhantomDiff_Generic < Option < ID >,
                                               Option < RepoName >>;

/// The slim payload of a `Phantom::Diff` -- one of the three placeholder
/// ("phantom") kinds, alongside PhantomDeleted and PhantomUnknown (see
/// PhantomDeleted for the shared framing).
///
/// ARISES: only in git diff mode, from the diff-completion code. Two shapes,
/// both detected by `diff_axes_require_phantom`: a "removed" member (present in
/// the parent's HEAD/index contains but gone from the worktree's, inserted by
/// `insert_phantoms_for_missing_contains` / `mk_phantom_viewnode`), or a Normal
/// node flipped in place by `normal_to_phantom` because its own relationship/
/// node axes went negative. "removed" vs "removedHere" (its .skg file is
/// still in the worktree, so the graph can still answer about it) is told apart by
/// `is_removedhere_diffPhantom`.
///
/// USED: as a write-protected diff annotation. It depicts a removed member at its
/// correct HEAD position among surviving siblings, decorated with per-stage diff
/// atoms. Always write-protected and bodyless (enforced by `normal_to_phantom` /
/// `mk_phantom_viewnode`); its affectsParent is never read or rendered (implicit
/// Affected); a Normal child left under it is demoted to Independent.
///
/// DISTINCT INFO: it is the only phantom that carries the git-diff coordinates
/// -- per-stage `node_axes` and `relationship_axes` plus `not_in_git` -- because
/// it exists solely to show a change between git snapshots. It keeps a `title`
/// and `repo` (resolved via `title_for_phantom`; the repo may be the
/// RepoName NOT_FOUND sentinel) and `graphStats`, which IS rendered on
/// phantoms. It needs NONE of ActiveVognode's affectsParent / birth / viewStats /
/// view_requests / editability (TODO/DONE/local-view-update/plan_v2.org §11 reduction; see §18): nothing
/// reads a phantom's affectsParent, and every phantom is write-protected.
#[derive( Debug, Clone, PartialEq )]
pub struct PhantomDiff_Generic < Id, Src > {
  pub title      : String,
  pub id         : Id,
  pub home_repo     : Src,
  /// Per-stage diff state for the node's '.skg' file (the node axis).
  pub node_axes  : NodeAxes,
  /// Per-stage diff state for the node's relationship at this position
  /// in the parent's contains list (the relationship axis).
  pub relationship_axes : RelationshipAxes,
  /// True iff the node's repo is not a git repo (or has no commits).
  pub not_in_git : bool,
  pub graphStats : GraphnodeStats,
}

impl < Id, Src > PhantomDiff_Generic < Id, Src > {
  /// Build a phantom payload from an ActiveVognode, keeping only the phantom-relevant
  /// fields and discarding affectsParent / birth / viewStats / view_requests /
  /// editability. Used when flipping an Active node to a phantom and by the
  /// placed<->maybe-placed conversions.
  pub fn from_activeVognode ( t : ActiveVognode_Generic < Id, Src > ) -> Self {
    PhantomDiff_Generic {
      title      : t . title,
      id         : t . id,
      home_repo     : t . home_repo,
      node_axes  : t . node_axes,
      relationship_axes : t . relationship_axes,
      not_in_git : t . not_in_git,
      graphStats : t . graphStats,
    }}

  /// A phantom is always write-protected, hence never has a body.
  pub fn body (&self) -> Option < &String > { None }
  pub fn is_writeProtected (&self) -> bool { true }

  /// True iff this node's diff axes require phantom display; for a correctly
  /// constructed phantom this holds, but some shared code asks regardless.
  pub fn should_be_diffPhantom (&self) -> bool {
    diff_axes_require_phantom (&self . node_axes, &self . relationship_axes) }

  /// A "removed-here" phantom whose '.skg' file is still in the worktree.
  pub fn is_removedhere_diffPhantom (&self) -> bool {
    self . should_be_diffPhantom ()
    && self . node_axes . unstaged != Some (Sign::Minus) }
}

/// Each ActiveVognode has one of these.
/// - A Definitive represents an editable view.
///   The user's changes to title, body and children
///   will be written to disk and the dbs when they save.
/// - `WriteProtected` represents a write-protected view,
///   in which case the body is not presented.
///   (TODO ? Maybe it should be.)
#[derive( Debug, Clone, PartialEq )]
pub enum Editability {
  Definitive {
    body         : Option < String >,
    edit_request : Option < NodeEditRequest >, },
  WriteProtected, }

/// Containerward path statistics: how a node relates to the
/// container hierarchy (path length, fork count, cycle detection).
#[derive(Debug, Clone, PartialEq)]
pub struct ContainerwardPathStats {
  pub length : usize,
  pub forks  : usize,
  pub cycles : bool,
}

/// Directional member counts for the five graph relations, plus the
/// interesting inbound links and distinct outbound targets, feeding the uniform-herald token grammar
/// (server/herald_tokens.rs). All are graph-level (position-independent)
/// counts of a node's members on each side of each relation.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct RelationCounts {
  pub containers    : usize, // C inbound: nodes that contain it
  pub contents      : usize, // C outbound: nodes it contains
  pub hiders        : usize, // H inbound: nodes that hide it
  pub hides         : usize, // H outbound: nodes it hides
  pub subscribers   : usize, // S inbound: nodes that subscribe to it
  pub subscribees   : usize, // S outbound: its subscribees
  pub overriders    : usize, // O inbound: nodes that override it
  pub overrides_out : usize, // O outbound: nodes it overrides
  pub link_total       : usize, // L inbound: distinct visible repos
  pub link_substantive : usize, // inbound repos with body, content, or multiple targets
  pub link_targets     : usize, // L outbound: distinct visible resolved targets
}

/// Graph-level statistics about a node.
/// These are derived from the graph database and are the same
/// regardless of where/how the node appears in a view.
#[derive(Debug, Clone, PartialEq)]
pub struct GraphnodeStats {
  pub aliases   : usize, // number of aliases (-> Ak)
  pub extra_ids : usize, // number of extra IDs from merging (-> Ik)
  pub flags : usize, // number of logically true flags (-> Fk)
  /// The directional member counts, or None for a node without stats
  /// (e.g. a repoless reference). Feeds the token grammar.
  pub rels      : Option<RelationCounts>,
}

/// View-specific statistics about a node.
/// These depend on the node's position in the current view tree.
/// `cycle` depends on ancestors; `affectsParent*` depends on the specific parent.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeStats {
  pub cycle             : bool,
  pub homeRepoAtBoundary  : bool, // True if a root or if repo differs from repo of nearest activeVognode ancestor.
  /// The relationship heralds as the SEMANTIC `(rels ...)` sexp string
  /// (server/herald_tokens.rs `relationship_heralds_sexp`): per-relation
  /// member counts and which tracked ancestors are members on each side,
  /// plus the birth. None when there is nothing to say. The
  /// client parses this and decides all presentation (letters, colors,
  /// order). See TODO/heralds-semantic-wire.org.
  pub rel_heralds           : Option<String>,
  /// Some(N) means this viewnode was drawn here in place of N,
  /// which it (transitively) overrides. Herald pink "ĥ" after "O".
  /// LOAD-BEARING, unlike the other view stats: save extraction
  /// collects N (not this node's own ID) into the parent's lists
  /// wherever this marker appears, so a substituted child
  /// round-trips to the original ID instead of rewriting the
  /// parent's contains. Serialized as the keyed form
  /// '(overridesHere N)'; tamper-checked at buffer validation
  /// (the carrier's ID must equal the visibility-ungated
  /// 'resolve_override (N).effective').
  pub overridesHere         : Option<ID>,
  /// True iff the node is drawn write-protected here while its graph
  /// node has a body -- a body the rendering hides. Herald "B",
  /// hugging the ☮ (TODO/more.org). Display-only, like the assembled
  /// herald strings; the parser accepts and discards it.
  pub hidden_body           : bool,
  /// Some(NAME) when the relationship instance this position's
  /// binding edge to its org-parent represents (=contains= for an
  /// ordinary content child; the folder's relation for a simple
  /// PartnerFolder member -- see 'PartnerFolder::relation_member_role')
  /// is recorded in a repo that DIFFERS from that edge's DEFAULT
  /// (see the relationship-default policy in 'SkgConfig'). None when
  /// equal to the default:
  /// the suppression is deliberate (render-and-gating,
  /// TODO/user-owned_autofork_chain/5_plan.org) -- the herald marks
  /// exactly the deliberately privatized edges. Also None: without a
  /// graph handle; for a node that is not genuinely a member here
  /// (affectsParent != Affected, or a backpath graft); and for the two
  /// compound filter folders (HiddenInSubscribee /
  /// HiddenOutsideOfSubscribee), which have no single
  /// 'relation_member_role' to read a repo from.
  /// This is a display fact, unlike a requested replacement stored in
  /// 'ActiveVognode_Generic::relRepo_request'.  Save extraction never
  /// treats this value as an instruction.
  /// Herald: red "~NAME" immediately before the ⌂ homeRepoHerald
  /// (server/heralds.rs).
  pub relRepo            : Option<RepoName>,
}

#[derive( Debug, Clone, PartialEq, Eq, Hash )]
pub enum PropertyFolder {
  ID,
  Alias,
  Flags { title : String, body : Option<String> },
}

#[derive( Debug, Clone, PartialEq )]
pub enum Property {
  Alias { text: String, // an alias for the node's grandparent
          relRepo: Option<RepoName>,
          relRepo_request: Option<RepoName>,
          relationship_axes: RelationshipAxes },
  ID { id: ID, // an ID of grandparent (the parent being an IDFolder)
       relationship_axes: RelationshipAxes },
  /// A true file-level boolean flag of the node's grandparent.
  Flag {
    flag : Flag,
    title    : String,
    body     : Option<String>,
  },
  TextChanged { staged: bool, unstaged: bool }, // Indicates title or body changed between stages. Visible in 'git diff mode'. Per-stage bools mark whether the change is staged (HEAD vs index) and/or unstaged (index vs worktree).
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PartnerFolder {
  Subscribee, // Collects subscribees its parent subscribes to. Writeable.
  Subscriber, // Collects nodes that subscribe to its parent. Write-protected (editable from the other side of the relationship).
  Overridden, // Collects nodes whose view its parent overrides. Writeable.
  Overrider, // Collects nodes that override its parent's view. Write-protected (editable from the other side of the relationship).
  Hider, // Collects nodes that hide its parent. Write-protected (editable from the other side of the relationship).
  // This folder is not itself hidden. Its children represent the nodes hidden
  // by the node represented by its parent. The relationships are write-protected
  // here, but editable within the parent's SubscribeeFolder.
  Hidden,
  HiddenInSubscribee, // Child of a subscribee-as-such. Collects children of the subscribee that the subscriber hides. Write-protected (but these relationships are editable by modifying the listed contents of the subscribee-as-such).
  HiddenOutsideOfSubscribee, // Child of a SubscribeeFolder. Collects things hidden by the SubscribeeFolder's parent but absent from every subscribee's content. This derived filter is editable as an exclusive visible-outside subset; its hide repos remain derived. Shown after all Subscribees, under the same SubscribeeFolder.
}

/// How a PartnerFolder's membership relates to user edits.
/// Every layer that treats some PartnerFolders differently from others
/// (save extraction, reconciliation, herald metadata) should consult
/// 'PartnerFolder::policy' rather than matching on folder variants, so the
/// policies cannot drift apart per file.
///
/// STALE-MEMBER RULE (uniform; decided 2026-06-10, superseding the
/// discard policy once planned for the filter folders in
/// TODO/full-schema/7_saving-readonly-cols.org): during
/// reconciliation, a stale member that is a leaf is deleted, and one
/// with children is demoted to affectsParent=false, whatever the
/// policy. The policies differ in:
/// - whether buffer membership is read at save extraction
///   ('WritableSet' and 'EditableFilter'),
/// - where the goal list comes from ('WritableSet' and 'WriteProtectedSet'
///   from 'relation_member_role'; both filter policies from hide state),
/// - goal-list order ('WritableSet': graph/disk order, which the
///   user's own save defines; 'WriteProtectedSet': the view's current
///   member order, then missing members appended; filter folders are
///   derived),
/// - whether repairs warn (the write-protected policies, in the saved view).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FolderPolicy {
  WritableSet,    // Membership edits are graph edits. An absent folder means no opinion; a present-but-empty folder means an explicit empty set (see 'MSV').
  EditableFilter, // A visible derived subset is an explicit edit of that subset; repo requests remain unsupported.
  WriteProtectedSet,    // Membership is generated from the graph. User order is respected view-locally; membership edits are repaired, with a warning.
  WriteProtectedFilter, // Membership is derived from hide state rather than from a relation role. Repaired, with a warning.
}

/// Requests for editing operations on a node.
/// Only one edit request is allowed per node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeEditRequest {
  NodeMerge (ID), // The node with this request is the acquirer. The node with the ID that this request specifies is the acquiree.
  Delete, // request to delete this node
  SetFlag { flag : Flag, value : bool },
}

/// Which relation's folders a 'Folder' view-request builds. A Folder
/// builds BOTH folders of its relation, so it is named by the RELATION
/// (relname), unlike a Path, which is named by a single partner ROLE.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FolderRelation {
  Aliases,
  OverridesViewOf,
  HidesFromItsSubscriptions,
  SubscribesTo,
}

impl FolderRelation {
  pub const ALL : [FolderRelation; 4] = [
    FolderRelation::Aliases,
    FolderRelation::OverridesViewOf,
    FolderRelation::HidesFromItsSubscriptions,
    FolderRelation::SubscribesTo ];

  pub fn relname (
    self,
  ) -> &'static str {
    match self {
      FolderRelation::Aliases                   => "aliases",
      FolderRelation::OverridesViewOf           => "overrides_view_of",
      FolderRelation::HidesFromItsSubscriptions => "hides_from_its_subscriptions",
      FolderRelation::SubscribesTo              => "subscribes_to", } }

  pub fn from_relname (
    s : &str,
  ) -> Option<FolderRelation> {
    FolderRelation::ALL . iter ()
      . find ( |relation| relation . relname () == s )
      . copied () }
}

/// Requests for additional views related to a node.
/// Multiple view requests can be active simultaneously.
/// - 'Folder(rel)' builds BOTH folders of the relation, populated from the graph.
/// - 'Path(role)' builds the backpath for that one partner role.
/// - 'Definitive' makes the (write-protected) node editable.
/// - 'Fork' is the explicit 'skg-fork-node' gesture: clone this (owned)
///   node into a private fork that overrides it. Consumed on the save
///   path (fork detection), not during view completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ViewRequest {
  Folder (FolderRelation),
  Path (RelationRole),
  Flags,
  Definitive,
  Fork,
}

//
// Implementations
//

/// True when a node's diff axes require displaying it with the
/// `Phantom::Diff` variant. Triggered by either:
/// - any relationship axis being '-' (removed in some stage), or
/// - the worktree node axis being '-' (file deleted), or
/// - the "moved twice" pattern: stagedM = +, unstagedM = -.
/// Shared by ActiveVognode_Generic and PhantomDiff_Generic, which both carry
/// these axes.
pub fn diff_axes_require_phantom (
  node_axes  : &NodeAxes,
  relationship_axes : &RelationshipAxes,
) -> bool {
  relationship_axes . staged   == Some (Sign::Minus)
  || relationship_axes . unstaged == Some (Sign::Minus)
  || node_axes  . unstaged == Some (Sign::Minus) }

impl < Id, Src > ActiveVognode_Generic < Id, Src > {
  pub fn should_be_diffPhantom (
    &self,
  ) -> bool {
    diff_axes_require_phantom (&self . node_axes, &self . relationship_axes) }

  /// A "removed-here" phantom: a phantom whose '.skg' file is still
  /// present in the worktree (so the graph still knows the node and can
  /// answer queries about it). Distinguished from a phantom whose file
  /// is also gone, for which graph queries would fail.
  pub fn is_removedhere_diffPhantom (
    &self,
  ) -> bool {
    self . should_be_diffPhantom ()
    && self . node_axes . unstaged != Some (Sign::Minus) }

  pub fn is_writeProtected (&self) -> bool {
    matches! ( self . editability,
               Editability::WriteProtected ) }

  pub fn body (&self) -> Option < &String > {
    match &self . editability {
      Editability::Definitive { body, .. } =>
        body . as_ref(),
      Editability::WriteProtected => None, }}

  pub fn edit_request (&self) -> Option < &NodeEditRequest > {
    match &self . editability {
      Editability::Definitive { edit_request, .. } =>
        edit_request . as_ref(),
      Editability::WriteProtected => None, }}
}

impl ActiveVognode {
  /// The COLLECTED ID: what this viewnode contributes to its
  /// parent's collected lists -- the 'overridesHere' original when
  /// the node was drawn in place of one, else its own ID. Save
  /// extraction, the distinctness validators and the content
  /// reconciler's orderkey all read this, never the raw 'id', so a
  /// drawn overrider round-trips to the original member instead of
  /// rewriting its parent's contains.
  pub fn collected_id (&self) -> ID {
    self . viewStats . overridesHere . clone ()
    . unwrap_or_else ( || self . id . clone () ) }
}

impl MpActiveVognode {
  /// As 'ActiveVognode::collected_id', for the maybe-placed stage (the
  /// node might not have an ID yet; a marker, if present, wins).
  pub fn collected_id (&self) -> Option<ID> {
    self . viewStats . overridesHere . clone ()
    . or_else ( || self . id . clone () ) }
}

impl PartnerFolder {
  pub fn policy (self) -> FolderPolicy {
    match self {
      PartnerFolder::Subscribee
        | PartnerFolder::Overridden
        => FolderPolicy::WritableSet,
      PartnerFolder::Subscriber
        | PartnerFolder::Overrider
        | PartnerFolder::Hider
        | PartnerFolder::Hidden
        => FolderPolicy::WriteProtectedSet,
      PartnerFolder::HiddenInSubscribee
        => FolderPolicy::WriteProtectedFilter,
      PartnerFolder::HiddenOutsideOfSubscribee
        => FolderPolicy::EditableFilter,
    } }

  pub fn repr_in_client (self) -> &'static str {
    match self {
      PartnerFolder::Subscribee                => "subscribeeFolder",
      PartnerFolder::Subscriber                => "subscriberFolder",
      PartnerFolder::Overridden                => "overriddenFolder",
      PartnerFolder::Overrider                 => "overriderFolder",
      PartnerFolder::Hider                     => "hiderFolder",
      PartnerFolder::Hidden                    => "hiddenFolder",
      PartnerFolder::HiddenInSubscribee        => "hiddenInSubscribeeFolder",
      PartnerFolder::HiddenOutsideOfSubscribee => "hiddenOutsideOfSubscribeeFolder",
    }}

  pub fn from_client_string (s : &str) -> Option<PartnerFolder> {
    match s {
      "subscribeeFolder"                => Some (PartnerFolder::Subscribee),
      "subscriberFolder"                => Some (PartnerFolder::Subscriber),
      "overriddenFolder"                => Some (PartnerFolder::Overridden),
      "overriderFolder"                 => Some (PartnerFolder::Overrider),
      "hiderFolder"                     => Some (PartnerFolder::Hider),
      "hiddenFolder"                    => Some (PartnerFolder::Hidden),
      "hiddenInSubscribeeFolder"        => Some (PartnerFolder::HiddenInSubscribee),
      "hiddenOutsideOfSubscribeeFolder" => Some (PartnerFolder::HiddenOutsideOfSubscribee),
      _                              => None,
    } }

}

impl PropertyFolder {
  pub fn flags () -> PropertyFolder {
    PropertyFolder::Flags { title : String::new (), body : None } }

  pub fn is_flags (&self) -> bool {
    matches! (self, PropertyFolder::Flags { .. }) }

  pub fn repr_in_client (&self) -> &'static str {
    match self {
      PropertyFolder::Alias => "aliasFolder",
      PropertyFolder::ID    => "idFolder",
      PropertyFolder::Flags { .. } => "flagsFolder",
    } }

  pub fn title (&self) -> &str {
    match self {
      PropertyFolder::Flags { title, .. } => title,
      PropertyFolder::Alias | PropertyFolder::ID => "", } }

  pub fn body (&self) -> Option<&String> {
    match self {
      PropertyFolder::Flags { body, .. } => body . as_ref (),
      PropertyFolder::Alias | PropertyFolder::ID => None, } }

}

impl Property {
  pub fn repr_in_client (&self) -> &'static str {
    match self {
      Property::Alias { .. }       => "alias",
      Property::ID { .. }          => "id",
      Property::Flag { .. }    => "flag",
      Property::TextChanged { .. } => "textChanged",
    } }

  pub fn title (&self) -> &str {
    match self {
      Property::Alias { text, .. } => text,
      Property::ID    { id, .. }   => id,
      Property::Flag { title, .. } => title,
      Property::TextChanged { .. } => "",
    } }

  pub fn body (&self) -> Option<&String> {
    match self {
      Property::Flag { body, .. } => body . as_ref (),
      _ => None, } }
}

impl Vognode {
  /// None for an Inactive vognode: it is an anonymous placeholder with
  /// no id (see InactiveVognode). A phantom returns the id it stands for.
  pub fn id (&self) -> Option<&ID> {
    match self {
      Vognode::Active   (t) => Some (&t . id),
      Vognode::Inactive (_) => None,
      Vognode::Phantom  (p) => Some (p . id ()),
    } }

  pub fn pid_and_repo (
    &self,
  ) -> Option<(&ID, &RepoName)> {
    match self {
      Vognode::Active   (t) => Some ((&t . id, &t . home_repo)),
      Vognode::Inactive (_) => None,
      Vognode::Phantom  (p) => p . pid_and_repo (),
    } }

  /// Whether this vognode represents a current graph member
  /// (Active or Inactive), as opposed to a phantom.
  pub fn is_graph_member (&self) -> bool {
    ! matches! (self, Vognode::Phantom (_)) }
}

impl Phantom {
  pub fn id (&self) -> &ID {
    match self {
      Phantom::Diff    (p) => &p . id,
      Phantom::Deleted (d) => &d . id,
      Phantom::Unknown (u) => &u . id,
    } }

  pub fn pid_and_repo (
    &self,
  ) -> Option<(&ID, &RepoName)> {
    match self {
      Phantom::Diff    (p) => Some ((&p . id, &p . home_repo)),
      Phantom::Deleted (d) => Some ((&d . id, &d . home_repo)),
      Phantom::Unknown (_) => None, // preserves Unknown's lone-None invariant
    } }
}

impl ViewRequest {
  /// The MATCH-position atoms the server can emit inside
  /// '(viewRequests ...)'. The RELNAME / ROLENAME arguments of the
  /// '(folder ...)' / '(path ...)' forms are VALUE-position (echoed by the
  /// herald's ANY/IT), so they are deliberately absent -- like IDs and
  /// counts elsewhere. Enumerated for the herald conformance test
  /// (server/heralds.rs).
  pub const EMITTABLE_MATCH_ATOMS : [&'static str; 4] =
    [ "folder", "path", "flags", "definitiveView" ];
}

impl AsRef<Viewnode> for Viewnode {
  fn as_ref (&self) -> &Viewnode {
    self }}

impl AsMut<Viewnode> for Viewnode {
  fn as_mut (&mut self) -> &mut Viewnode {
    self }}

impl Viewnode {
  /// Consume every save-only `(editRequest ...)` carried by this occurrence.
  /// Call only after the save has committed: failed saves and confirmation
  /// round-trips must leave requests in the user's buffer. `view_requests` are
  /// deliberately separate -- completion fulfills those while rendering.
  pub fn consume_edit_request_after_save (
    &mut self,
  ) {
    match &mut self . kind {
      ViewnodeKind::Vognode (Vognode::Active (active)) => {
        active . relRepo_request = None;
        if let Editability::Definitive { edit_request, .. } =
          &mut active . editability
        { *edit_request = None; }},
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
        unknown . relRepo_request = None,
      ViewnodeKind::Property (Property::Alias { relRepo_request, .. }) =>
        *relRepo_request = None,
      _ => {}, }}

  pub fn normal_to_phantom (
    &mut self,
  ) {
    if let ViewnodeKind::Vognode (Vognode::Active (t))
      = &self . kind
      { if t . should_be_diffPhantom ()
        { // A phantom is ALWAYS write-protected and renders no body (Jeff's
          // TODO/DONE/local-view-update/progress.org §9 TODO): the diff view presumes git literacy (magit
          // shows the real node), and a phantom must never be a node's
          // definitive instance -- if Removed it has nothing to define, and if
          // RemovedHere "edit it here, where it isn't" is dangerously
          // confusing. So drop body/edit_request when flipping a (possibly
          // Definitive) Active node to a phantom. mk_phantom_viewnode already
          // builds from a `WriteProtected` base, so now every phantom is
          // write-protected by construction.
          let phantom : PhantomDiff =
            PhantomDiff::from_activeVognode ( t . clone () );
          self . kind = ViewnodeKind::Vognode (Vognode::Phantom (
            Phantom::Diff (phantom))); }}}

  pub fn title (&self) -> &str {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Active (t)) => &t . title,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p))) => &p . title,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) =>
        &d . title,
      ViewnodeKind::Property (q) =>
        q . title (),
      ViewnodeKind::PropertyFolder (folder) => folder . title (),
      ViewnodeKind::PartnerFolder (_)
        | ViewnodeKind::BufferRoot
        | ViewnodeKind::DeadViewnode
        | ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (_)))
        | ViewnodeKind::Vognode (Vognode::Inactive (_)) =>
        "",
    }}

  /// Reasonable for both ActiveVognodes and Non-vognodes.
  pub fn body (&self) -> Option < &String > {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Active (t)) => t . body (),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p))) => p . body (),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) => d . body . as_ref (),
      ViewnodeKind::PropertyFolder (folder) => folder . body (),
      ViewnodeKind::Property (property) => property . body (),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (_)))
        | ViewnodeKind::Vognode (Vognode::Inactive (_))
        | ViewnodeKind::PartnerFolder (_)
        | ViewnodeKind::BufferRoot
        | ViewnodeKind::DeadViewnode => None,
    }}

  pub fn is_activeVognode_and_affectsParent_true (&self) -> bool {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Active (t)) =>
        t . affectsParent == AffectsParent::True,
      _ => false,
    }}

  /// The id of a vognode that represents a current graph member: None
  /// for phantoms, inactive vognodes and non-vognodes.
  pub fn id_if_graph_member (&self) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (v) if v . is_graph_member () => v . id (),
      _ => None,
    }}

  /// The id of an Active vognode or a Diff phantom -- the two ActiveVognode-ish
  /// kinds. None for everything else
  /// (Inactive, Deleted/Unknown phantoms, folders, non-vognodes, BufferRoot).
  pub fn active_or_diff_phantom_id (&self) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Active (t)) => Some (&t . id),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p)))   => Some (&p . id),
      _ => None,
    }}
}

impl fmt::Display for NodeEditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      NodeEditRequest::NodeMerge (id) => write!(f, "(merge {})", id . 0),
      NodeEditRequest::Delete    => write!(f, "toDelete"),
      NodeEditRequest::SetFlag { flag, value } => write! (
        f, "(flag {} {})", flag . wire_name (), value),
    }} }

impl FromStr for NodeEditRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "toDelete" => Ok (NodeEditRequest::Delete),
      _ => {
        // Try to parse as "merge <id>"
        if let Some (id_str) = s . strip_prefix ("merge ") {
          Ok ( NodeEditRequest::NodeMerge ( ID::from (id_str) ) )
        } else {
          Err ( format! ( "Unknown NodeEditRequest value: {}", s ))
        }} }} }

impl fmt::Display for ViewRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      ViewRequest::Folder  (rel)  => write! (f, "(folder {})",  rel  . relname  ()),
      ViewRequest::Path (role) => write! (f, "(path {})", role . rolename ()),
      ViewRequest::Flags   => write! (f, "flags"),
      ViewRequest::Definitive  => write! (f, "definitiveView"),
      ViewRequest::Fork        => write! (f, "fork"), } } }

//
// Defaults
//

impl Default for GraphnodeStats {
  fn default () -> Self {
    GraphnodeStats {
      aliases   : 0,
      extra_ids : 0,
      flags : 0,
      rels      : None,
    }} }

impl Default for ViewnodeStats {
  fn default () -> Self {
    ViewnodeStats {
      cycle             : false,
      homeRepoAtBoundary  : false,
      rel_heralds       : None,
      overridesHere     : None,
      hidden_body       : false,
      relRepo        : None,
    }} }

//
// Constructor functions
//

/// Create an ActiveVognode with default values for all fields except id, repo, and title.
/// Useful when you need to customize other fields after construction.
pub fn default_activeVognode (
  id     : ID,
  repo : RepoName,
  title  : String,
) -> ActiveVognode {
  ActiveVognode {
    title,
    id,
    home_repo: repo,
    affectsParent       : AffectsParent::True,
    birth          : Birth::Unremarkable,
    graphStats     : GraphnodeStats::default(),
    viewStats      : ViewnodeStats::default(),
    relRepo_request : None,
    view_requests  : HashSet::new(),
    node_axes      : NodeAxes::default(),
    relationship_axes     : RelationshipAxes::default(),
    not_in_git     : false,
    editability   : Editability::Definitive {
      body         : None,
      edit_request : None },
  }}

/// Create a write-protected phantom Viewnode with the given diff axes.
/// At least one relationship axis or the unstaged-node axis should be
/// negative for this to be a real phantom; callers must ensure that.
pub fn mk_phantom_viewnode (
  id         : ID,
  repo     : RepoName,
  title      : String,
  node_axes  : NodeAxes,
  relationship_axes : RelationshipAxes,
) -> Viewnode {
  let mut viewnode : Viewnode =
    mk_writeProtected_viewnode ( id, repo, title, AffectsParent::True );
  if let ViewnodeKind::Vognode (Vognode::Active (mut t)) = viewnode . kind
    { t . node_axes  = node_axes;
      t . relationship_axes = relationship_axes;
      viewnode . kind = ViewnodeKind::Vognode (Vognode::Phantom (
        Phantom::Diff ( PhantomDiff::from_activeVognode (t) ))); }
  else
    // mk_writeProtected_viewnode always yields an Active vognode; if that ever
    // changes, fail loudly rather than silently return a non-phantom.
    { unreachable! (
        "mk_phantom_viewnode: mk_writeProtected_viewnode did not yield an Active vognode" ); }
  viewnode }

pub fn mk_definitive_viewnode (
  id     : ID,
  repo : RepoName,
  title  : String,
  body   : Option < String >,
) -> Viewnode { mk_viewnode ( id,
                            repo,
                            title,
                            AffectsParent::True,
                            Birth::Unremarkable,
                            Editability::Definitive {
                              body,
                              edit_request : None },
                            HashSet::new () ) } // view_requests

/// Build an PhantomUnknown wrapper. Use when a referenced ID resolves
/// to nothing in in_rust_graph, on disk, or via any phantom/diff procedure
/// -- the placeholder lets the view render the line as a herald
/// rather than aborting the whole BFS expansion.
pub fn mk_unknown_viewnode (
  id : ID,
) -> Viewnode {
  Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::Vognode (Vognode::Phantom (
      Phantom::Unknown ( PhantomUnknown {
        id,
        relRepo         : None,
        relRepo_request : None,
      } ) )),
  }}

pub fn mk_inactive_viewnode (
) -> Viewnode {
  Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::Vognode (
      Vognode::Inactive ( InactiveVognode ) ),
  }}

/// Create a write-protected Viewnode from disk data.
/// Body is always None since write-protected nodes don't have editable content.
pub fn mk_writeProtected_viewnode (
  id     : ID,
  repo : RepoName,
  title  : String,
  affectsParent  : AffectsParent,
) -> Viewnode {
  mk_writeProtected_viewnode_with_birth (
    id, repo, title, affectsParent, Birth::Unremarkable ) }

pub fn mk_writeProtected_viewnode_with_birth (
  id       : ID,
  repo   : RepoName,
  title    : String,
  affectsParent : AffectsParent,
  birth    : Birth,
) -> Viewnode { mk_viewnode ( id,
                            repo,
                            title,
                            affectsParent,
                            birth,
                            Editability::WriteProtected,
                            HashSet::new ( )) } // view_requests

/// Convert a definitive Viewnode to write-protected.
/// Discards body and edit_request.
/// Errors if the input is not an ActiveVognode.
pub fn mk_writeProtected_from_viewnode (
  mut viewnode : Viewnode,
  affectsParent    : AffectsParent,
  birth       : Birth,
) -> Result < Viewnode, String > {
  match &mut viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Active (t)) => {
      // Mutate in place, so that every field not named here
      // (view_requests, diff axes, graphStats, viewStats, and any
      // field added later) is preserved rather than silently reset.
      t . affectsParent = affectsParent;
      t . birth = birth;
      t . editability = // discards body and edit_request
        Editability::WriteProtected;
      Ok (viewnode) },
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p))) =>
      // A phantom carries none of the fields preserved above,
      // so it is rebuilt rather than mutated.
      Ok ( mk_writeProtected_viewnode_with_birth (
        p . id . clone (), p . home_repo . clone (), p . title . clone (),
        affectsParent, birth )),
    _ => Err (
      "mk_writeProtected_from_viewnode: expected ActiveVognode"
        . to_string () ) }}

/// Create a Viewnode with *nearly* full metadata control.
/// The exception is that the 'GraphnodeStats' and 'ViewnodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the Viewnode tree.
pub fn mk_viewnode (
  id            : ID,
  repo        : RepoName,
  title         : String,
  affectsParent      : AffectsParent,
  birth         : Birth,
  editability  : Editability,
  view_requests : HashSet < ViewRequest >,
) -> Viewnode {
  Viewnode { focused     : false,
             folded      : false,
             body_folded : false,
             kind        : ViewnodeKind::Vognode (
               Vognode::Active (
                 ActiveVognode { affectsParent,
                            birth,
                            view_requests,
                            editability,
                            .. default_activeVognode (
                              id, repo, title ) } ) ) }}

/// Helper to create a BufferRoot Viewnode.
pub fn viewforest_root_viewnode () -> Viewnode {
  Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::BufferRoot,
  }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/viewnode.rs"]
mod tests;
