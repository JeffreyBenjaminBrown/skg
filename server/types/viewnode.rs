/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'Graphnode' type.
/// Nodes of the tree are represented via the 'Viewnode' type.
/// Some 'Viewnode's represent graphnodes, which need not exist; these are
/// 'Vognode's (unrestricted, restricted, or phantom).
/// Others encode information about neighboring tree nodes, such as
/// aliases, IDs, and partner folders.

use super::errors::HeadlineName;
use super::misc::{ID, SkgrepoChange, SkgrepoName};
use super::nodes::complete::Flag;
use std::collections::{BTreeSet, HashSet};
use std::fmt;
use std::str::FromStr;

/// Whether this node participates in the membership represented by
/// its viewparent. For an ordinary Vognode parent, that membership
/// is the parent's content. For a PartnerFolder parent, the PartnerFolder
/// decides the membership (per its relation and role). For other kinds of parent's,
/// a node's Membership has no effect; a view root is a 'Member'.
/// On the wire a non-member carries the bare atom 'nonMember' and a
/// member carries nothing.
///
/// PITFALL: If a vognode is write-protected, *none* of it children affect it,
/// just as edits to itself do not affect it,
/// regardless of what the children might claim with their Membership field.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Membership {
  Member,    // this node takes part in its viewparent's membership
  NonMember, // it doesn't
}

/// The diff view's facts about one viewnode, the parts of its
/// '(diff ...)' form: the three kinds of change ('(node ...)',
/// '(rel ...)', '(properties changed)'), which leaves a request on it
/// may name ('(leaves ...)'), and which of its change-forest leaves
/// are marked commit-ready ('(ready ...)'). Display facts: the view
/// derives them, and a save reads nothing from them.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ChangeFacts {
  pub change   : DiffChange,
  /// None when nothing is ready. 'Some' of an empty set is the bare
  /// '(ready)' of a leaf with a single object.
  pub ready    : Option<BTreeSet<DiffObject>>,
  /// None when the viewnode names no change-forest leaf.
  pub leaves   : Option<LeavesFact>,
}

/// '(leaves N OBJECT...)': the change-forest leaves a readiness
/// request on a viewnode may name ('docs/diff-view.org', "Facts on
/// the wire"). The OBJECTs are 'node' and/or 'rel' on a vognode or
/// historical phantom, each worth one leaf; 'all' on a modified node
/// (beside 'rel' when that names a leaf) and on a commit readiness
/// aggregate; none where a bare request names them all.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LeavesFact {
  /// How many leaves a request naming every one of 'objects' touches.
  pub count   : usize,
  pub objects : BTreeSet<DiffObject>,
}

/// The three kinds of change a viewnode shows, the first three parts
/// of its '(diff ...)' form: its node's existence ('(node added)',
/// '(node removed)'), the existence of its relationship to its
/// viewparent ('(rel added)', '(rel removed)'), and whether its node's
/// other properties changed ('(properties changed)'). None on an
/// existence means the thing is in both graphs of the straddle, or
/// that it does not apply. A node in only one graph has no other
/// properties to compare, so 'properties' is true only when 'node' is
/// None. On a property leaf the one existence it shows is 'rel': that
/// of its entry in its folder.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DiffChange {
  pub node       : Option<Existence>,
  pub rel        : Option<Existence>,
  pub properties : bool,
}

/// One step of change between the two graphs of a straddle: 'Added'
/// when only the after-graph holds the thing (a node, a relationship,
/// a folder entry), 'Removed' when only the before-graph does.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Existence { Added, Removed }

/// The diff objects of a viewnode's change-forest leaves: 'node' (the node's
/// own existence), 'rel' (its relationship to its viewparent), and
/// 'all' (every leaf the node records, or every leaf of a commit
/// readiness aggregate, one level down).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DiffObject {
  Node,
  Rel,
  All,
}

/// '(readyRequest OBJECTS)' or '(unreadyRequest OBJECTS)': save intent that
/// sets or clears the commit-readiness marks of the viewnode's
/// leaves on those objects. No objects means the viewnode's only
/// change-forest leaf.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadinessRequest {
  pub ready   : bool, // true for readyRequest, false for unreadyRequest
  pub objects : BTreeSet<DiffObject>,
}

//
// Type declarations
//

/// Corresponds to an Emacs headline-body pair.
#[derive( Debug, Clone, PartialEq )]
pub struct Viewnode {
  pub focused         : bool,
  pub org_folded      : bool, // Is this     hidden in its parent?
  pub org_body_folded : bool, // Is the body hidden in this?
  pub kind            : ViewnodeKind,
  /// The diff view's display facts. Empty on a kind that never
  /// carries a change (a folder, a restricted vognode, the buffer
  /// root, a tombstone, 'BodyDiff'), and outside the diff view.
  pub change_facts      : ChangeFacts,
  /// Save intent, like 'relRepo_request', but allowed on a
  /// write-protected occurrence, and beside an edit request.
  /// None on a kind that never carries a change.
  pub readiness_request : Option<ReadinessRequest>,
  /// The viewnode ID, printed '(vnid X)'. None until the view assigns
  /// one, at the end of a completion ('ViewnodeIdRecord' in
  /// 'server/types/views_state.rs'); a parsed viewnode keeps the one
  /// its headline carried.
  pub viewnode_id       : Option<ViewnodeId>,
}

/// A viewnode ID: the identifier of one viewnode within one buffer,
/// unique for the life of the buffer ('docs/api-and-formats.org',
/// "The metadata headers"). One or more lowercase letters and digits.
/// The view assigns them from a counter in base 36, at least two
/// characters long; a viewnode ID once used is never reused in the buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ViewnodeId (pub String);

#[derive( Debug, Clone, PartialEq )]
pub enum ViewnodeKind {
  Vognode       (Vognode),
  PropertyFolder       (PropertyFolder),
  Property          (Property),
  PartnerFolder    (PartnerFolder),
  BufferRoot,
  Tombstone,
}

/// A viewnode that represents a graphnode, which need not exist: Unrestricted
/// and Restricted vognodes represent current graphnodes; a Phantom
/// represents a missing or historical one.
#[derive( Debug, Clone, PartialEq )]
pub enum Vognode {
  Unrestricted   (UnrestrictedVognode),
  Restricted (RestrictedVognode), // From a skgrepo that is restricted (see 'docs/api-and-formats.org', "skgrepo-sets").
  Phantom  (Phantom),
}

/// The three display-only phantom kinds. None of them is a current
/// graphnode: all are save-inert on save and excluded from the view's collateral pids
/// (`pids_from_viewforest`). They differ in *why* the node is absent and thus
/// in how much they can still say about it.
#[derive( Debug, Clone, PartialEq )]
pub enum Phantom {
  Deleted (PhantomDeleted), // Epistemically: No longer exists in the graph. Procedurally: Skg just watched the user delete this node (maybe from a different view), but for some reason (e.g. its view-descendents are interesting, or it is a root) had to retain an image of it here.
  // PITFALL: There is an exception. If Skg watches a user delete a node, while that user has a view of a foreign node that refers to the deleted node, that "foreigner's view" will show it as Unknown rather than Deleted. This is to maintain consistency with how that relationship to a nonexistent node will appear when viewed in later sessions.
  Unknown (PhantomUnknown), // Skg can't find it (and, unlike Deleted, does not know why). Can result from bad data, or from a reference to another user's node that has since been deleted.
  Historical (PhantomHistorical), // The diff view's image of a node that HEAD has and the worktree lacks.
}

/// A phantom for a node whose .skg file a save just
/// removed -- one of the three phantom kinds, alongside PhantomHistorical and
/// PhantomUnknown. All three stand in for something that is not a current
/// graphnode (so all three are save-inert on save and excluded from the view's
/// collateral pids, `pids_from_viewforest`); they differ in *why* the node is
/// absent and thus in how much they can still say about it.
///
/// ARISES: during post-save / collateral re-render, when
/// a node already materialized in the view turns up in
/// `deleted_by_this_save_pids`. An Unrestricted content child flips here via
/// `mutate_unrestrictedVognode_to_deletednode`. (A Restricted node is not flipped: it is an
/// anonymous restricted vognode, and turning it into a DELETED placeholder would leak that a
/// hidden node vanished, so it just lingers until the next full rerender drops
/// it.) So it marks a node that genuinely no longer exists in the graph, killed
/// by a completed save (this buffer's or a shared one's) -- not a git artifact.
///
/// USED: save-inert -- it generates no fieldIntents and is excluded from its
/// parent's contains list. It is never "completed", yet it retains its children
/// (which `mark_orphans_under_dead_parents_false` makes non-members)
/// so the user's subtree under a vanished node survives. A childless Deleted is
/// pruned by the postorder sweep (`is_self_deletable_when_empty`), and a Deleted
/// may stand as a view root (validate_tree) so a deleted root still shows.
///
/// DISTINCT INFO: unlike PhantomUnknown it knows its `repo`, and unlike either
/// other phantom it keeps the `title`/`body` it last displayed -- because it was
/// a fully materialized node right up until the save deleted it, so that text is
/// still worth showing.
#[derive( Debug, Clone, PartialEq )]
pub struct PhantomDeleted {
  pub skgid        : ID,
  pub home_skgrepo : SkgrepoName,
  pub title        : String,
  pub body         : Option < String >,
}

/// A phantom for a reference that resolves to nothing --
/// one of the three phantom kinds, alongside PhantomHistorical and PhantomDeleted
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
/// no fieldIntents, though its descendants still might, so extraction recurses
/// through it). If it is still a member of its parent's contains it is retained
/// (a present-but-unresolvable reference); if it is no longer a member it
/// converts to a tombstone (`convert_nonmember_unknown_children_to_tombstones`) and
/// is pruned.
///
/// DISTINCT INFO: it carries only the `id`. Unlike PhantomDeleted it has no skgrepo
/// (it is the one Phantom kind for which `pid_and_repo` returns None) and no
/// last-seen text; unlike PhantomHistorical it has no old text. That emptiness is
/// the information: a reference exists, but we have no record of its target.
#[derive( Debug, Clone, PartialEq )]
pub struct PhantomUnknown {
  pub skgid                 : ID,
  /// Display-only fact about this occurrence's binding relationship.
  pub relRepo         : Option<SkgrepoName>,
  /// A pending relRepo change, consumed only by save.
  pub relRepo_request : Option<SkgrepoName>,
}

/// A phantom for a node that the before-graph of the diff
/// view's graph straddle has and the after-graph lacks: a node deleted
/// (or acquired by a nodeMerge) since HEAD.
///
/// ARISES: only in a diff view, built from the graph straddle
/// ('server/diff_view/change_viewnodes.rs'): under each former
/// container as a former member, or as a view root. Its own old
/// content hangs beneath it, recursing into deleted members.
///
/// USED: write-protected and save-inert, like every phantom: it emits
/// no fieldIntents, is excluded from its parent's contains, and the
/// vognodes under it are not its members. The diff view's
/// reconciler draws it, and knows it by its kind.
///
/// DISTINCT INFO: the before-graph's title, body and home skgrepo.
/// Its viewnode's change facts are those of its existence ('(node
/// removed)') and of its relationship to its viewparent ('(rel
/// removed)'), each a change-forest leaf.
#[derive( Debug, Clone, PartialEq )]
pub struct PhantomHistorical {
  pub skgid             : ID,
  pub home_skgrepo      : SkgrepoName,
  pub title             : String,
  pub body              : Option<String>,
  /// True when the user asked, by '(editRequest restoreNode)', that
  /// the save write the node back as HEAD recorded it. The only edit
  /// request a phantom carries; consumed after the save.
  pub restoreNode_request : bool,
}

/// An anonymous "something from a restricted repo is/was here"
/// vognode. It carries no data on purpose: a restricted node's id,
/// skgrepo, title, etc. describe content the user hid by restricting
/// the skgrepo restriction, so rendering any of it would leak.
///
/// SCOPE: a RestrictedVognode arises only from a skgrepo-set reduction of an
/// already-rendered buffer ('update_buffer/repo_switch.rs' converts
/// now-restricted Unrestricted nodes in place), where it is kept to host
/// already-extended unrestricted descendants. A de-novo render under a
/// restricted set never creates one: restricted content members are
/// omitted from goal lists ('omit_restricted_members') and their content
/// is never extended (completion does nothing at a restricted node's
/// visit). So a public node reachable only through a private parent
/// simply does not appear on a fresh restricted view.
///
/// Being dataless, it is save-inert everywhere: it emits no save intention
/// (its membership is owned by disk supplementation), is not a collateral
/// pid, is unmanaged by every reconciler (preserved as-is, never
/// goal-matched), and renders as the bare atom 'restrictedNode'.
#[derive( Debug, Clone, PartialEq )]
pub struct RestrictedVognode;

pub type UnrestrictedVognode   = UnrestrictedVognode_Generic < ID, SkgrepoName >;
pub type MpUnrestrictedVognode = UnrestrictedVognode_Generic < Option < ID >,
                                             Option < SkgrepoName >>;

/// A Viewnode that corresponds to a Graphnode.
#[derive( Debug, Clone, PartialEq )]
pub struct UnrestrictedVognode_Generic < Id, Repo > {
  pub title         : String,
  pub skgid         : Id,
  pub home_skgrepo  : Repo,
  pub membership      : Membership,

  // The next two *Stats fields only influence how the node is shown. Editing them and saving the buffer leaves the graph unchanged, and those edits will be immediately lost, as this data is regenerated each time the view is rebuilt.
  pub graphStats    : GraphnodeStats,
  pub viewStats     : ViewnodeStats,
  /// A requested skgrepo for this occurrence's binding relationship. Unlike
  /// `viewStats.relRepo`, this is save intent.
  pub relRepo_request : Option<SkgrepoName>,
  /// True when the user asked, by '(editRequest restoreRel)', that the
  /// save re-add the relationship to the viewparent that this removed
  /// member stands for, as HEAD recorded it. Like 'relRepo_request', it
  /// concerns that relationship rather than the node, so it lives
  /// outside 'Editability': a write-protected occurrence carries it too.
  pub restoreRel_request : bool,

  pub view_requests : HashSet < ViewRequest >,
  pub editability  : Editability,
}

/// Each UnrestrictedVognode has one of these.
/// - A Editable represents an editable view.
///   The user's changes to title, body and children
///   will be written to disk and the dbs when they save.
/// - `WriteProtected` represents a write-protected view,
///   in which case the body is not presented.
///   (TODO ? Maybe it should be.)
#[derive( Debug, Clone, PartialEq )]
pub enum Editability {
  Editable {
    body         : Option < String >,
    edit_request : Option < NodeEditRequest >, },
  WriteProtected, }

/// Directional member counts for the five graph relations, plus the
/// substantive mentioners and distinct outbound targets, feeding the uniform-herald token grammar
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
  pub link_total       : usize, // L inbound: distinct visible skgrepos
  pub link_substantive : usize, // inbound skgrepos with body, content, or multiple targets
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
/// These depend on the node's position in the current viewforest.
/// `cycle` depends on ancestors; `membership` depends on the specific parent.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeStats {
  pub cycle                 : bool,
  pub homeSkgrepoAtBoundary : bool, // True if a root or if skgrepo differs from skgrepo of nearest unrestrictedVognode ancestor.
  /// The relationship heralds as the semantic `(rels ...)` sexp string
  /// (server/herald_tokens.rs `relationship_heralds_sexp`): per-relation
  /// member counts and which tracked ancestors are members on each side,
  /// plus the birth facts. None when there is nothing to say. The
  /// client parses this and decides all presentation (letters, colors,
  /// order). See 'docs/heralds.org'.
  pub rel_heralds           : Option<String>,
  /// Some(N) means this viewnode is shown here in place of N,
  /// which it (transitively) overrides. Herald pink "ĥ" after "O".
  /// Load-bearing, unlike the other view stats: save extraction
  /// collects N (not this node's own ID) into the parent's lists
  /// wherever this fact appears, so a substituted child
  /// round-trips to the original ID instead of rewriting the
  /// parent's contains. Serialized as the keyed form
  /// '(overridesHere N)'; tamper-checked at buffer validation
  /// (the carrier's ID must equal the visibility-ungated
  /// 'resolve_override (N).effective').
  pub overridesHere         : Option<ID>,
  /// True iff the node is shown write-protected here while its graph
  /// node has a body -- a body the rendering hides. Herald "B",
  /// hugging the ☮. Display-only, like the assembled
  /// herald strings; the parser accepts and discards it.
  pub omitted_body           : bool,
  /// Some(NAME) when the relationship instance this position's
  /// binding relationship to its viewparent represents (=contains= for an
  /// ordinary content child; the folder's relation for a simple
  /// PartnerFolder member -- see 'PartnerFolder::relation_member_role')
  /// is recorded in a skgrepo that differs from that relationship's default
  /// (see the relationship-default policy in 'SkgConfig'). None when
  /// equal to the default:
  /// the suppression is deliberate (see 'docs/telescopes.org',
  /// "What leaks, and what repair fixes") -- the herald marks
  /// exactly the deliberately privatized relationships. Also None: without a
  /// graph handle; for a node that is not genuinely a member here
  /// (a non-member); and for the two
  /// compound filter folders (HiddenInSubscribee /
  /// HiddenOutsideOfSubscribee), which have no single
  /// 'relation_member_role' to read a skgrepo from.
  /// This is a display fact, unlike a requested replacement stored in
  /// 'UnrestrictedVognode_Generic::relRepo_request'.  Save extraction never
  /// treats this value as a fieldIntent.
  /// Herald: red "~NAME" immediately before the ⌂ homeRepoHerald
  /// (server/heralds.rs).
  pub relRepo            : Option<SkgrepoName>,
  /// Some(OLD to NEW) in the skgrepo-changes buffer of the diff
  /// view, for a node whose home skgrepo moved from OLD to NEW since
  /// HEAD. Display-only; the buffer's pass sets it after completion
  /// ('set_homeSkgrepoMove_facts_in_viewforest'), and the parser discards
  /// it. Herald "-OLD +NEW".
  pub home_skgrepo_move  : Option<SkgrepoChange>,
  /// Some(LETTER) beside 'home_skgrepo_move' when the nearest vognode
  /// ancestor whose home moved moved the same way. LETTER names that
  /// ancestor as the relationship heralds name ancestors: a for the
  /// viewparent, b for the grandparent, and so on. Display-only.
  /// Herald "Δ⌂=LETTER".
  pub moved_like_ancestor : Option<String>,
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
          relRepo: Option<SkgrepoName>,
          relRepo_request: Option<SkgrepoName> },
  ID { skgid: ID }, // an ID of grandparent (the parent being an IDFolder)
  /// A true file-level boolean flag of the node's grandparent.
  Flag {
    flag : Flag,
    title    : String,
    body     : Option<String>, },
  // The diff view's properties, below, are display-only and
  // save-inert: the validator ignores them, and the diff view's
  // reconciler draws them and knows them by their kind. Each but BodyDiff is a change-forest leaf or
  // commit readiness aggregate, so its viewnode carries '(ready ...)' and a
  // readiness request.
  /// In an alias folder: an alias the worktree lost since HEAD. The
  /// alias reader never reads it. Its change-forest leaf is that alias.
  RemovedAlias { text : String },
  /// In an ID folder: an extra ID the worktree lost since HEAD. The
  /// ID check never counts it. Its change-forest leaf is that ID.
  RemovedID { skgid : ID },
  /// The node's text at HEAD, under a node whose text changed.
  /// 'title' is Some iff the title changed, 'body' Some iff the body
  /// changed (the empty string for a body HEAD lacked). Its
  /// change-forest leaf is the node's text change.
  OldText {
    title : Option<String>,
    body  : Option<String>, },
  /// The line diff of the node's body, under its OldText.
  BodyDiff { diff : String },
  /// The node's home skgrepo changed. Its change-forest leaf is the
  /// node's home skgrepo.
  HomeSkgrepoMove { skgrepo_change : SkgrepoChange },
  /// Under a member: the relationship to its viewparent moved from
  /// one skgrepo to another. Its change-forest leaf is that
  /// relationship's relRepo.
  RelRepoMove { skgrepo_change : SkgrepoChange },
  /// A childless stand-in for a set of changes recorded in its
  /// parent's files, carrying their derived readiness.
  ChangesetMarker { kind : ChangesetMarkerKind },
}

/// Which changes a 'Property::ChangesetMarker' stands for.
#[derive( Debug, Clone, Copy, PartialEq, Eq, Hash )]
pub enum ChangesetMarkerKind {
  Content,   // "its content changed": the ~ change-forest leaves of its content
  RelRepo,   // "some content relationships changed skgrepo"
  OrderOnly, // "order-only change", always ready
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PartnerFolder {
  Subscribee, // Collects subscribees its parent subscribes to. Writable.
  Subscriber, // Collects nodes that subscribe to its parent. Write-protected (editable from the other side of the relationship).
  Overridden, // Collects nodes whose view its parent overrides. Writable.
  Overrider, // Collects nodes that override its parent's view. Write-protected (editable from the other side of the relationship).
  Hider, // Collects nodes that hide its parent. Write-protected (editable from the other side of the relationship).
  // This folder is not itself hidden. Its children represent the nodes hidden
  // by the node represented by its parent. The relationships are write-protected
  // here, but editable within the parent's SubscribeeFolder.
  Hidden,
  HiddenInSubscribee, // Child of a subscribee-as-such. Collects children of the subscribee that the subscriber hides. Write-protected (but these relationships are editable by modifying the listed contents of the subscribee-as-such).
  HiddenOutsideOfSubscribee, // Child of a SubscribeeFolder. Collects things hidden by the SubscribeeFolder's parent but absent from every subscribee's content. This derived filter is editable as an exclusive visible-outside subset; its hide skgrepos remain derived. Shown after all Subscribees, under the same SubscribeeFolder.
  Container, // Collects nodes that contain its parent. Write-protected (editable from the other side of the relationship).
  Mentioner, // Collects nodes that link to its parent. Write-protected (a link lives in the linking node's text).
  // The diff view's two derived folders. Their membership comes from the extra IDs of the graph straddle's after-graph; both are save-inert.
  AcquiredBy, // Child of a historical phantom: the node that acquired it by a nodeMerge.
  Acquired, // Child of an acquirer: the historical phantoms of the nodes it acquired.
}

/// How a PartnerFolder's membership relates to user edits.
/// Every layer that treats some PartnerFolders differently from others
/// (save extraction, reconciliation, herald metadata) should consult
/// 'PartnerFolder::policy' rather than matching on folder variants, so the
/// policies cannot drift apart per file.
///
/// Every policy obeys the one rule (the glossary's "the one rule:
/// convert, then prune"): during reconciliation a stale member is
/// converted, a leaf to a tombstone the prune removes, one with
/// children retained as non-member (nonMember), keeping its
/// editability.
/// The policies differ in:
/// - whether buffer membership is read at save extraction
///   ('EditableSet' and 'EditableFilter'),
/// - where the goal list comes from ('EditableSet' and 'WriteProtectedSet'
///   from 'relation_member_role'; both filter policies from hide state),
/// - goal-list order ('EditableSet': graph/disk order, which the
///   user's own save defines; 'WriteProtectedSet': the view's current
///   member order, then missing members appended; filter folders are
///   derived),
/// - whether repairs warn (the write-protected policies, in the saved view).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FolderPolicy {
  EditableSet,    // Membership edits are graph edits. An absent folder means no opinion; a present-but-empty folder means an explicit empty set (see 'MSV').
  EditableFilter, // A visible derived subset is an explicit edit of that subset; skgrepo requests remain unsupported.
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
  // The diff view's two restore requests ('docs/diff-view.org',
  // "Restoring what a change removed"). Neither lives in
  // 'Editability': a parsed RestoreNode becomes
  // 'PhantomHistorical::restoreNode_request', a parsed RestoreRel
  // 'UnrestrictedVognode_Generic::restoreRel_request'.
  RestoreNode, // write back a node deleted since HEAD
  RestoreRel,  // re-add a relationship removed since HEAD
}

/// Which relation's folders a 'Folder' view-request builds. A Folder
/// builds both folders of its relation, so it is named by the relation
/// (relname), unlike a Path, which is named by a single partner role.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FolderRelation {
  Aliases,
  Overrides,
  HidesFromSubs,
  SubscribesTo,
  // These two build one write-protected folder each: their other
  // side is the node's content, or its links, shown without a folder.
  Containers,
  Mentioners,
}

impl FolderRelation {
  pub const ALL : [FolderRelation; 6] = [
    FolderRelation::Aliases,
    FolderRelation::Overrides,
    FolderRelation::HidesFromSubs,
    FolderRelation::SubscribesTo,
    FolderRelation::Containers,
    FolderRelation::Mentioners ];

  pub fn relname (
    self,
  ) -> &'static str {
    match self {
      FolderRelation::Aliases                   => "aliases",
      FolderRelation::Overrides                 => "overrides",
      FolderRelation::HidesFromSubs             => "hidesFromSubs",
      FolderRelation::SubscribesTo              => "subscribesTo",
      FolderRelation::Containers                => "containers",
      FolderRelation::Mentioners                => "mentioners", } }

  pub fn from_relname (
    s : &str,
  ) -> Option<FolderRelation> {
    FolderRelation::ALL . iter ()
      . find ( |relation| relation . relname () == s )
      . copied () }
}

/// Requests for viewbranches related to a node.
/// Multiple view requests can be unrestricted simultaneously.
/// - 'Folder(rel)' builds both folders of the relation, populated from the graph.
/// - 'Editable' makes the (write-protected) node editable.
/// - 'WriteProtected' makes the (editable) node write-protected,
///   and its direct unrestricted children non-members.
/// - 'WriteProtectedSubtree' does that to the node and its whole
///   content subtree.
/// - 'Fork' is the explicit 'skg-fork-node' gesture: clone this (owned)
///   node into a private fork that overrides it. Consumed on the save
///   path (fork detection), not during view completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ViewRequest {
  Folder (FolderRelation),
  Flags,
  Editable,
  WriteProtected,
  WriteProtectedSubtree,
  Fork,
}

//
// Implementations
//

impl < Id, Repo > UnrestrictedVognode_Generic < Id, Repo > {
  pub fn is_writeProtected (&self) -> bool {
    matches! ( self . editability,
               Editability::WriteProtected ) }

  /// Make this occurrence write-protected: discard its body and edit
  /// request, and drop its editable-view request, which would
  /// otherwise make it editable again at its next visit.
  pub fn write_protect (&mut self) {
    self . view_requests . remove (&ViewRequest::Editable);
    self . editability = Editability::WriteProtected; }

  pub fn body (&self) -> Option < &String > {
    match &self . editability {
      Editability::Editable { body, .. } =>
        body . as_ref(),
      Editability::WriteProtected => None, }}

  pub fn edit_request (&self) -> Option < &NodeEditRequest > {
    match &self . editability {
      Editability::Editable { edit_request, .. } =>
        edit_request . as_ref(),
      Editability::WriteProtected => None, }}
}

impl UnrestrictedVognode {
  /// The collected ID: what this viewnode contributes to its
  /// parent's collected lists -- the 'overridesHere' original when
  /// the node is shown in place of one, else its own ID. Save
  /// extraction, the distinctness validators and the content
  /// reconciler's orderkey all read this, never the raw 'id', so a
  /// shown overrider round-trips to the original member instead of
  /// rewriting its parent's contains.
  pub fn collected_skgid (&self) -> ID {
    self . viewStats . overridesHere . clone ()
    . unwrap_or_else ( || self . skgid . clone () ) }
}

impl MpUnrestrictedVognode {
  /// As 'UnrestrictedVognode::collected_id', for the maybe-placed stage (the
  /// node might not have an ID yet; a marker, if present, wins).
  pub fn collected_skgid (&self) -> Option<ID> {
    self . viewStats . overridesHere . clone ()
    . or_else ( || self . skgid . clone () ) }
}

impl PartnerFolder {
  pub fn policy (self) -> FolderPolicy {
    match self {
      PartnerFolder::Subscribee
        | PartnerFolder::Overridden
        => FolderPolicy::EditableSet,
      PartnerFolder::Subscriber
        | PartnerFolder::Overrider
        | PartnerFolder::Hider
        | PartnerFolder::Hidden
        | PartnerFolder::Container
        | PartnerFolder::Mentioner
        | PartnerFolder::AcquiredBy
        | PartnerFolder::Acquired
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
      PartnerFolder::Container                 => "containerFolder",
      PartnerFolder::Mentioner                 => "mentionerFolder",
      PartnerFolder::AcquiredBy                => "acquiredByFolder",
      PartnerFolder::Acquired                  => "acquiredFolder",
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
      "containerFolder"                 => Some (PartnerFolder::Container),
      "mentionerFolder"                 => Some (PartnerFolder::Mentioner),
      "acquiredByFolder"                => Some (PartnerFolder::AcquiredBy),
      "acquiredFolder"                  => Some (PartnerFolder::Acquired),
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
      Property::RemovedAlias { .. }    => "removedAlias",
      Property::RemovedID { .. }       => "removedID",
      Property::OldText { .. }         => "oldText",
      Property::BodyDiff { .. }        => "bodyDiff",
      Property::HomeSkgrepoMove { .. } => "homeSkgrepoMove",
      Property::RelRepoMove { .. }     => "relRepoMove",
      Property::ChangesetMarker { .. } => "changesetMarker",
    } }

  pub fn title (&self) -> &str {
    match self {
      Property::Alias { text, .. } => text,
      Property::ID    { skgid, .. }   => skgid,
      Property::Flag { title, .. } => title,
      Property::RemovedAlias { text, .. } => text,
      Property::RemovedID { skgid, .. }   => skgid,
      Property::OldText { title, .. } =>
        title . as_deref () . unwrap_or (""),
      Property::BodyDiff { .. }
        | Property::HomeSkgrepoMove { .. }
        | Property::RelRepoMove { .. }
        | Property::ChangesetMarker { .. } => "",
    } }

  pub fn body (&self) -> Option<&String> {
    match self {
      Property::Flag { body, .. } => body . as_ref (),
      Property::OldText { body, .. } => body . as_ref (),
      Property::BodyDiff { diff } => Some (diff),
      _ => None, } }
}

impl ChangesetMarkerKind {
  pub const ALL : [ChangesetMarkerKind; 3] = [
    ChangesetMarkerKind::Content,
    ChangesetMarkerKind::RelRepo,
    ChangesetMarkerKind::OrderOnly ];

  pub fn wire_name (
    self,
  ) -> &'static str {
    match self {
      ChangesetMarkerKind::Content   => "content",
      ChangesetMarkerKind::RelRepo   => "relRepo",
      ChangesetMarkerKind::OrderOnly => "orderOnly", } }

  pub fn from_wire_name (
    s : &str,
  ) -> Option<ChangesetMarkerKind> {
    ChangesetMarkerKind::ALL . into_iter ()
      . find ( |kind| kind . wire_name () == s ) }
}

impl Vognode {
  /// None for a Restricted vognode: it is an anonymous restricted vognode with
  /// no id (see RestrictedVognode). A phantom returns the id it stands for.
  pub fn skgid (&self) -> Option<&ID> {
    match self {
      Vognode::Unrestricted   (t) => Some (&t . skgid),
      Vognode::Restricted (_) => None,
      Vognode::Phantom  (p) => Some (p . skgid ()),
    } }

  pub fn pid_and_skgrepo (
    &self,
  ) -> Option<(&ID, &SkgrepoName)> {
    match self {
      Vognode::Unrestricted   (t) => Some ((&t . skgid, &t . home_skgrepo)),
      Vognode::Restricted (_) => None,
      Vognode::Phantom  (p) => p . pid_and_skgrepo (),
    } }

  /// Whether this vognode represents a current graphnode
  /// (Unrestricted or Restricted), as opposed to a phantom.
  pub fn is_current_graphnode (&self) -> bool {
    ! matches! (self, Vognode::Phantom (_)) }
}

impl Phantom {
  pub fn skgid (&self) -> &ID {
    match self {
      Phantom::Deleted (d) => &d . skgid,
      Phantom::Unknown (u) => &u . skgid,
      Phantom::Historical (h) => &h . skgid,
    } }

  pub fn pid_and_skgrepo (
    &self,
  ) -> Option<(&ID, &SkgrepoName)> {
    match self {
      Phantom::Deleted (d) => Some ((&d . skgid, &d . home_skgrepo)),
      Phantom::Unknown (_) => None, // preserves Unknown's lone-None invariant
      Phantom::Historical (h) => Some ((&h . skgid, &h . home_skgrepo)),
    } }
}

impl DiffChange {
  /// The forms of the change, in their order on the wire: '(node
  /// added|removed)', '(rel added|removed)', '(properties changed)',
  /// each only when true.
  pub fn forms (
    &self,
  ) -> Vec<String> {
    let existence = |head : &str, existence : Option<Existence>| -> Option<String> {
      existence . map ( |existence|
        format! ( "({} {})", head, existence . wire_name () )) };
    existence ("node", self . node) . into_iter ()
      . chain ( existence ("rel", self . rel) )
      . chain ( self . properties . then ( ||
        "(properties changed)" . to_string () ))
      . collect () }

  /// The atoms the serializer can emit, for the herald conformance test.
  pub const EMITTABLE_ATOMS : [&'static str; 6] =
    [ "node", "rel", "added", "removed", "properties", "changed" ];
}

impl Existence {
  pub fn wire_name (
    self,
  ) -> &'static str {
    match self {
      Existence::Added   => "added",
      Existence::Removed => "removed", } }

  pub fn from_wire_name (
    name : &str,
  ) -> Option<Existence> {
    match name {
      "added"   => Some (Existence::Added),
      "removed" => Some (Existence::Removed),
      _         => None, } }
}

impl DiffObject {
  pub const ALL : [DiffObject; 3] =
    [ DiffObject::Node, DiffObject::Rel, DiffObject::All ];

  pub fn wire_name (
    self,
  ) -> &'static str {
    match self {
      DiffObject::Node => "node",
      DiffObject::Rel  => "rel",
      DiffObject::All  => "all", } }

  pub fn from_wire_name (
    s : &str,
  ) -> Option<DiffObject> {
    DiffObject::ALL . into_iter ()
      . find ( |object| object . wire_name () == s ) }
}

impl ReadinessRequest {
  pub fn form_head (
    &self,
  ) -> &'static str {
    if self . ready { "readyRequest" } else { "unreadyRequest" } }
}

impl ViewRequest {
  /// The match-position atoms the server can emit inside
  /// '(viewRequests ...)'. The RELNAME argument of the
  /// '(folder ...)' form is value-position (echoed by the
  /// herald's ANY/IT), so they are deliberately absent -- like IDs and
  /// counts elsewhere. Enumerated for the herald conformance test
  /// (server/heralds.rs).
  pub const EMITTABLE_MATCH_ATOMS : [&'static str; 5] =
    [ "folder", "flags", "editableView",
      "writeProtectedView", "writeProtectedSubtreeView" ];
}

impl ViewnodeId {
  /// The viewnode ID the counter 'counter' names: base 36, padded to at
  /// least two characters.
  pub fn from_counter (
    counter : u64,
  ) -> ViewnodeId {
    const DIGITS : &[u8; 36] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let mut reversed_digits : Vec<u8> = Vec::new ();
    let mut rest : u64 = counter;
    loop {
      reversed_digits . push ( DIGITS [ (rest % 36) as usize ] );
      rest /= 36;
      if rest == 0 { break; }}
    while reversed_digits . len () < 2 {
      reversed_digits . push (b'0'); }
    reversed_digits . reverse ();
    ViewnodeId ( String::from_utf8 (reversed_digits)
          . expect ("base-36 digits are ASCII") ) }

  /// The viewnode ID named by 'text', if it is one or more lowercase letters
  /// and digits.
  pub fn parse (
    text : &str,
  ) -> Result<ViewnodeId, String> {
    if ! text . is_empty ()
       && text . chars () . all ( |c| c . is_ascii_lowercase ()
                                     || c . is_ascii_digit () )
    { Ok ( ViewnodeId ( text . to_string () )) }
    else { Err ( format! (
      "A viewnode ID is one or more lowercase letters and digits, not {:?}",
      text )) }}

  /// The counter value this viewnode ID reads as in base 36, if it fits.
  pub fn counter_value (
    &self,
  ) -> Option<u64> {
    self . 0 . chars () . try_fold ( 0u64, |value, c| {
      let digit : u64 = c . to_digit (36) ? as u64;
      value . checked_mul (36) ? . checked_add (digit) } ) }
}

impl fmt::Display for ViewnodeId {
  fn fmt (&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
    write! (f, "{}", self . 0) }}

impl AsRef<Viewnode> for Viewnode {
  fn as_ref (&self) -> &Viewnode {
    self }}

impl AsMut<Viewnode> for Viewnode {
  fn as_mut (&mut self) -> &mut Viewnode {
    self }}

impl Viewnode {
  /// Make this viewnode one of kind 'kind', keeping its focus, fold
  /// state and viewnode ID. The diff view's facts and the readiness request
  /// describe the kind replaced, so they go with it.
  pub fn replace_kind (
    &mut self,
    kind : ViewnodeKind,
  ) {
    self . kind              = kind;
    self . change_facts      = ChangeFacts::default ();
    self . readiness_request = None; }

  /// The node's ID, when this viewnode is an editable occurrence of
  /// it: an unrestricted vognode that is not write-protected.
  pub fn editable_skgid (
    &self,
  ) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if ! t . is_writeProtected () => Some (&t . skgid),
      _ => None, }}

  /// Consume every save-only `(editRequest ...)`, and every readiness
  /// request, carried by this occurrence.
  /// Call only after the save's skgsave-commit: failed saves and confirmation
  /// round-trips must leave requests in the user's buffer. `view_requests` are
  /// deliberately separate -- completion fulfills those while rendering.
  pub fn consume_edit_request_after_save (
    &mut self,
  ) {
    self . readiness_request = None;
    match &mut self . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) => {
        restriction . relRepo_request = None;
        restriction . restoreRel_request = false;
        if let Editability::Editable { edit_request, .. } =
          &mut restriction . editability
        { *edit_request = None; }},
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
        unknown . relRepo_request = None,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (historical))) =>
        historical . restoreNode_request = false,
      ViewnodeKind::Property (Property::Alias { relRepo_request, .. }) =>
        *relRepo_request = None,
      _ => {}, }}

  pub fn title (&self) -> &str {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => &t . title,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) =>
        &d . title,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
        &h . title,
      ViewnodeKind::Property (q) =>
        q . title (),
      ViewnodeKind::PropertyFolder (folder) => folder . title (),
      ViewnodeKind::PartnerFolder (_)
        | ViewnodeKind::BufferRoot
        | ViewnodeKind::Tombstone
        | ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (_)))
        | ViewnodeKind::Vognode (Vognode::Restricted (_)) =>
        "",
    }}

  /// What a message calls this headline: its viewnode ID, else its title.
  pub fn headline_name (&self) -> HeadlineName {
    HeadlineName::of ( self . viewnode_id . as_ref (), self . title () ) }

  /// Reasonable for both UnrestrictedVognodes and Non-vognodes.
  pub fn body (&self) -> Option < &String > {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => t . body (),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) => d . body . as_ref (),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) => h . body . as_ref (),
      ViewnodeKind::PropertyFolder (folder) => folder . body (),
      ViewnodeKind::Property (property) => property . body (),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (_)))
        | ViewnodeKind::Vognode (Vognode::Restricted (_))
        | ViewnodeKind::PartnerFolder (_)
        | ViewnodeKind::BufferRoot
        | ViewnodeKind::Tombstone => None,
    }}

  pub fn is_unrestrictedVognode_and_member (&self) -> bool {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        t . membership == Membership::Member,
      _ => false,
    }}

  /// The id of a vognode that represents a current graphnode: None
  /// for phantoms, restricted vognodes and non-vognodes.
  pub fn skgid_if_current_graphnode (&self) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (v) if v . is_current_graphnode () => v . skgid (),
      _ => None,
    }}

  /// The id of an Unrestricted vognode. None for everything else
  /// (Restricted vognodes, phantoms, folders, non-vognodes, BufferRoot).
  pub fn unrestricted_skgid (&self) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => Some (&t . skgid),
      _ => None,
    }}
}

impl fmt::Display for NodeEditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      NodeEditRequest::NodeMerge (skgid) => write!(f, "(merge {})", skgid . 0),
      NodeEditRequest::Delete    => write!(f, "toDelete"),
      NodeEditRequest::SetFlag { flag, value } => write! (
        f, "(flag {} {})", flag . wire_name (), value),
      NodeEditRequest::RestoreNode => write!(f, "restoreNode"),
      NodeEditRequest::RestoreRel  => write!(f, "restoreRel"),
    }} }

impl FromStr for NodeEditRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "toDelete"    => Ok (NodeEditRequest::Delete),
      "restoreNode" => Ok (NodeEditRequest::RestoreNode),
      "restoreRel"  => Ok (NodeEditRequest::RestoreRel),
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
      ViewRequest::Flags   => write! (f, "flags"),
      ViewRequest::Editable    => write! (f, "editableView"),
      ViewRequest::WriteProtected        => write! (f, "writeProtectedView"),
      ViewRequest::WriteProtectedSubtree => write! (f, "writeProtectedSubtreeView"),
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
      homeSkgrepoAtBoundary  : false,
      rel_heralds       : None,
      overridesHere     : None,
      omitted_body      : false,
      relRepo        : None,
      home_skgrepo_move : None,
      moved_like_ancestor : None,
    }} }

//
// Constructor functions
//

/// Create an UnrestrictedVognode with default values for all fields except id, skgrepo, and title.
/// Useful when you need to customize other fields after construction.
pub fn default_unrestrictedVognode (
  skgid   : ID,
  skgrepo : SkgrepoName,
  title   : String,
) -> UnrestrictedVognode {
  UnrestrictedVognode {
    title,
    skgid,
    home_skgrepo: skgrepo,
    membership       : Membership::Member,
    graphStats     : GraphnodeStats::default(),
    viewStats      : ViewnodeStats::default(),
    relRepo_request : None,
    restoreRel_request : false,
    view_requests  : HashSet::new(),
    editability    : Editability::Editable {
      body         : None,
      edit_request : None },
  }}

pub fn mk_editable_viewnode (
  skgid   : ID,
  skgrepo : SkgrepoName,
  title   : String,
  body    : Option < String >,
) -> Viewnode { mk_viewnode ( skgid,
                            skgrepo,
                            title,
                            Membership::Member,
                            Editability::Editable {
                              body,
                              edit_request : None },
                            HashSet::new () ) } // view_requests

/// Build a PhantomUnknown wrapper. Use when a referenced ID resolves
/// to nothing in in_rust_graph, on disk, or via any phantom/diff procedure
/// -- the phantom lets the view render the line as a herald
/// rather than aborting the whole BFS extension.
pub fn mk_unknown_viewnode (
  skgid : ID,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::Vognode (Vognode::Phantom (
    Phantom::Unknown ( PhantomUnknown {
      skgid,
      relRepo         : None,
      relRepo_request : None, } ) )) ) }

pub fn mk_restricted_viewnode (
) -> Viewnode {
  viewnode_of ( ViewnodeKind::Vognode (
    Vognode::Restricted ( RestrictedVognode ) )) }

/// Create a write-protected Viewnode from disk data.
/// Body is always None since write-protected nodes don't have editable content.
pub fn mk_writeProtected_viewnode (
  skgid     : ID,
  skgrepo : SkgrepoName,
  title  : String,
  membership  : Membership,
) -> Viewnode { mk_viewnode ( skgid,
                            skgrepo,
                            title,
                            membership,
                            Editability::WriteProtected,
                            HashSet::new ( )) } // view_requests

/// Convert an editable Viewnode to write-protected.
/// Discards body, edit_request and the editable-view request.
/// Errors if the input is not an UnrestrictedVognode.
pub fn mk_writeProtected_from_viewnode (
  mut viewnode : Viewnode,
  membership    : Membership,
) -> Result < Viewnode, String > {
  match &mut viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => {
      // Mutate in place, so that every field not named here
      // (the other view_requests, graphStats,
      // viewStats, and any field added later) is preserved rather
      // than silently reset.
      t . membership = membership;
      t . write_protect ();
      Ok (viewnode) },
    _ => Err (
      "mk_writeProtected_from_viewnode: expected UnrestrictedVognode"
        . to_string () ) }}

/// Create a Viewnode with *nearly* full metadata control.
/// The exception is that the 'GraphnodeStats' and 'ViewnodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the Viewnode tree.
pub fn mk_viewnode (
  skgid         : ID,
  skgrepo       : SkgrepoName,
  title         : String,
  membership      : Membership,
  editability  : Editability,
  view_requests : HashSet < ViewRequest >,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::Vognode (
    Vognode::Unrestricted (
      UnrestrictedVognode { membership,
                            view_requests,
                            editability,
                            .. default_unrestrictedVognode (
                              skgid, skgrepo, title ) } ) )) }

/// Helper to create a BufferRoot Viewnode.
pub fn viewforest_root_viewnode () -> Viewnode {
  viewnode_of (ViewnodeKind::BufferRoot) }

/// A viewnode of the given kind, unfocused and not org-folded,
/// carrying no change facts, no readiness request and no viewnode ID.
/// A literal that
/// sets other fields can take the rest from it:
/// 'Viewnode { org_folded : true, .. viewnode_of (kind) }'.
pub fn viewnode_of (
  kind : ViewnodeKind,
) -> Viewnode {
  Viewnode { focused           : false,
             org_folded        : false,
             org_body_folded   : false,
             kind,
             change_facts      : ChangeFacts::default (),
             readiness_request : None,
             viewnode_id       : None } }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/viewnode.rs"]
mod tests;
