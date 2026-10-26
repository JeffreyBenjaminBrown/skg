/// Skg lets users control a graph, viewing it through a tree view in a text editor.
/// Nodes of the graph are represented via the 'Graphnode' type.
/// Nodes of the tree are represented via the 'Viewnode' type.
/// Some 'Viewnode's represent graphnodes, which need not exist; these are
/// 'Vognode's (an occurrence, a ghost or an unknown).
/// Others encode information about neighboring tree nodes, such as
/// aliases, IDs, and partner folders.

use super::errors::HeadlineName;
use super::misc::{ID, SkgrepoChange, SkgrepoName};
use super::nodes::complete::Flag;
use crate::herald_tokens::RelationshipHeralds;
use std::collections::{BTreeSet, HashSet};
use std::fmt;
use std::str::FromStr;

/// The diff view's change facts about one viewnode, the parts of its
/// '(diff ...)' form: the three kinds of change ('(node ...)',
/// '(rel ...)', '(properties changed)'), which leaves a request on it
/// may name ('(leaves ...)'), and which of its change-forest leaves
/// are marked commit-ready ('(ready ...)'). Derived facts: the refresh
/// derives them, and the parser skips them.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ChangeFacts {
  pub change   : DiffChange,
  /// None when nothing is ready. 'Some' of an empty set is the bare
  /// '(ready)' of a leaf with a single object.
  pub ready    : Option<BTreeSet<DiffObject>>,
  /// None when the viewnode names no change-forest leaf.
  pub leaves   : Option<LeavesFact>,
  /// Some(OLD to NEW) in the skgrepo-changes buffer of the diff
  /// view, on an occurrence whose node's home skgrepo moved from OLD
  /// to NEW since HEAD. The refresh's tail sets it
  /// ('set_homeSkgrepoMove_facts_in_viewforest'); the printer writes
  /// it as '(homeSkgrepoMove (old OLD) (new NEW))' in '(diff ...)',
  /// which the parser skips. Herald "-OLD +NEW".
  pub home_skgrepo_move   : Option<SkgrepoChange>,
  /// Some(LETTER) beside 'home_skgrepo_move' when the nearest vognode
  /// ancestor whose home moved moved the same way. LETTER names that
  /// ancestor as the relationship heralds name ancestors: a for the
  /// viewparent, b for the grandparent, and so on. Written, and
  /// skipped, like 'home_skgrepo_move'. Herald "Δ⌂=LETTER".
  pub moved_like_ancestor : Option<String>,
}

/// '(leaves N OBJECT...)': the change-forest leaves a readiness
/// request on a viewnode may name ('docs/diff-view.org', "Change
/// facts on the wire"). The OBJECTs are 'node' and/or 'rel' on a vognode or
/// ghost, each worth one leaf; 'all' on a modified node
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
/// None. On a property member the one existence it shows is 'rel': that
/// of its membership in its folder.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DiffChange {
  pub node       : Option<Existence>,
  pub rel        : Option<Existence>,
  pub properties : bool,
}

/// One step of change between the two graphs of a straddle: 'Added'
/// when only the after-graph holds the thing (a node, a relationship,
/// a property member), 'Removed' when only the before-graph does.
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

/// '(editRequest (ready OBJECTS))' or '(editRequest (unready OBJECTS))':
/// save intent that
/// sets or clears the commit-readiness marks of the viewnode's
/// leaves on those objects. No objects means the viewnode's only
/// change-forest leaf.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadinessRequest {
  pub ready   : bool, // true for (ready ...), false for (unready ...)
  pub objects : BTreeSet<DiffObject>,
}

//
// Type declarations
//

/// Corresponds to an Emacs headline-body pair.
#[derive( Debug, Clone, PartialEq )]
pub struct Viewnode {
  pub foldAndFocus    : FoldAndFocus,
  pub kind            : ViewnodeKind,
  /// The diff view's change facts. Empty on a kind that never
  /// carries a change (a folder, the buffer root, a tombstone,
  /// 'BodyDiff'), and outside the diff view.
  pub change_facts      : ChangeFacts,
  /// Save intent, like 'relationshipEditRequest', but allowed on a
  /// write-protected occurrence, and beside an edit request.
  /// None on a kind that never carries a change.
  pub readiness_request : Option<ReadinessRequest>,
  /// The viewnode ID, printed '(vnid X)'. None until the view assigns
  /// one, at the refresh's tail ('ViewnodeIdRecord' in
  /// 'server/types/views_state.rs'); a parsed viewnode keeps the one
  /// its headline carried.
  pub viewnode_id       : Option<ViewnodeId>,
}

/// The three position atoms of a headline in the client's buffer:
/// 'focused', 'folded' and 'bodyFolded'. The client sets them
/// and the view keeps them; they say nothing about the graph.
#[derive( Debug, Clone, Copy, Default, PartialEq, Eq )]
pub struct FoldAndFocus {
  pub focused    : bool,
  pub folded     : bool, // Is this     hidden in its parent?
  pub bodyFolded : bool, // Is the body hidden in this?
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
  Folder        (Folder),
  PropertyMember    (PropertyMember),
  ChangeViewnode    (ChangeViewnode),
  BufferRoot,
  Tombstone,
}

impl ViewnodeKind {
  /// Whether this is a property folder: one whose members are the
  /// viewparent's aliases, IDs or true flags.
  pub fn is_property_folder (&self) -> bool {
    matches! ( self, ViewnodeKind::Folder (folder)
                     if folder . members_are_properties () ) }

  /// Whether this is a partner folder: one whose members are nodes.
  pub fn is_partner_folder (&self) -> bool {
    matches! ( self, ViewnodeKind::Folder (folder)
                     if ! folder . members_are_properties () ) }
}

/// A viewnode that stands for a gnid: an occurrence, for a node the
/// gated graph has; a ghost, only in the diff view, for a node HEAD
/// has and the worktree lacks; an unknown, for a gnid its parent's
/// list names and no node answers to. A restricted node has no
/// vognode: the refresh entombstones its occurrence (the ID rule,
/// 'server/refresh/id_rule.rs'), as it does the occurrence of a node
/// that vanished, unless the diff view can show it as a ghost.
#[derive( Debug, Clone, PartialEq )]
pub enum Vognode {
  Occurrence   (Occurrence),
  Ghost        (Ghost),
  Unknown      (Unknown),
}

/// The kind of a vognode, without its payload.
#[derive( Debug, Clone, Copy, PartialEq, Eq )]
pub enum VognodeKind {
  Occurrence,
  Ghost,
  Unknown,
}

impl VognodeKind {
  /// The kind as a message names it, with its article: "an
  /// occurrence", "a ghost", "an unknown".
  pub fn with_article (
    &self,
  ) -> &'static str {
    match self {
      VognodeKind::Occurrence => "an occurrence",
      VognodeKind::Ghost      => "a ghost",
      VognodeKind::Unknown    => "an unknown", } }
}

/// A reference that resolves to nothing: a gnid that its parent's
/// list names and no node answers to.
///
/// ARISES: when some node's `contains` (or similar list) names an ID that has no
/// record anywhere -- not a primary pid or extra_id in the graph, not on disk,
/// and not recoverable through any diff procedure. Built by
/// `mk_unknown_viewnode`, e.g. when `graphnode_and_viewnode_from_id` returns
/// None. A genuine dangling pointer. Can result from bad data, or from a
/// reference to another user's node that has since been deleted.
///
/// USED: it lets the view degrade gracefully -- carrying the bad reference as
/// its own kind, rather than erroring, is what keeps a single bad reference deep
/// in a subtree from killing the whole view at its root. Inert on save (it emits
/// no fieldIntents, though its descendants still might, so extraction recurses
/// through it). If it is still a member of its parent's contains it is retained
/// (a present-but-unresolvable reference); if the list no longer names it,
/// the refresh entombstones it and the prune removes it.
///
/// DISTINCT INFO: it carries only the `id`: no skgrepo (it is the one
/// vognode for which `pid_and_skgrepo` returns None) and no text.
/// That emptiness is the information: a reference exists, but we have
/// no record of its target.
#[derive( Debug, Clone, PartialEq )]
pub struct Unknown {
  pub gnid                 : ID,
  /// Display-only fact about this unknown's binding relationship.
  pub relRepo         : Option<SkgrepoName>,
  /// A pending request about this reference's relationship to its
  /// viewparent (only a relRepo change: the parser refuses
  /// restoreRel here), consumed only by save.
  pub relationshipEditRequest : Option<RelationshipEditRequest>,
  /// Whether the save reads its raw ID into its viewparent's list.
  /// The refresh marks it false, written 'noClaim', on one whose
  /// relationship HEAD alone warrants: a dangling partner the worktree
  /// lost.
  pub claimsMembership : bool,
  /// Whether the truth relates it to its viewparent as its position
  /// implies ('Warrant'), as the refresh set it. Never 'No': the ID
  /// rule makes a headline an unknown only where its viewparent's goal
  /// names the gnid. The printer writes none: the metadata grammar
  /// gives an unknown no '(view ...)'.
  pub warrant : Warrant,
}

/// A ghost: a node that the before-graph of the diff view's graph
/// straddle has and the after-graph lacks -- a node deleted (or
/// acquired by a nodeMerge) since HEAD. Drawn and retained only in
/// the diff view.
///
/// ARISES: only in a diff view, built from the graph straddle
/// ('server/diff_view/change_viewnodes.rs'): under each former
/// container as a former member, or as a view root; or, in place, from
/// an occurrence whose node a save deleted. Its own old content hangs
/// beneath it, recursing into deleted members. It is a pending change:
/// it stays until it is restored or the deletion is committed, and
/// becomes an occurrence again, in place, when its node is back.
///
/// USED: write-protected: it emits no fieldIntents of its own, and
/// the vognodes under it are not its members. When it claims
/// membership, the save keeps its gnid in its viewparent's list, as it
/// keeps an unknown's; otherwise it contributes nothing to the save.
/// The refresh draws it in the diff view, and knows it by its kind.
///
/// DISTINCT INFO: the before-graph's title, body and home skgrepo.
/// Its viewnode's change facts are those of its existence ('(node
/// removed)') and of its relationship to its viewparent ('(rel
/// removed)'), each a change-forest leaf.
#[derive( Debug, Clone, PartialEq )]
pub struct Ghost {
  pub gnid             : ID,
  pub home_skgrepo      : SkgrepoName,
  pub title             : String,
  pub body              : Option<String>,
  /// True when the user asked, by '(editRequest restoreNode)', that
  /// the save write the node back as HEAD recorded it. The only edit
  /// request a ghost carries; consumed after the save.
  pub restoreNode_request : bool,
  /// Whether the save reads the ghost's gnid as an entry of its
  /// viewparent's list. The refresh lets a ghost claim only where the
  /// worktree's list still names its gnid (a dangling reference to a
  /// node HEAD has), so that the save keeps that list intact; on the
  /// wire, as for an occurrence, 'noClaim' says no.
  pub claimsMembership : bool,
  /// Whether the truth relates it to its viewparent as its position
  /// implies ('Warrant'), as the refresh set it. Only 'No' is written
  /// ('noWarrant'): the ghost kind is its own ghost-warrant herald.
  pub warrant : Warrant,
}

/// A Viewnode that corresponds to a Graphnode.
#[derive( Debug, Clone, PartialEq )]
pub struct Occurrence {
  pub title         : String,
  pub gnid         : ID,
  pub home_skgrepo  : SkgrepoName,
  /// Whether the save reads this occurrence as an entry of its
  /// viewparent's list. For an occurrence parent, that list is the
  /// parent's content; for a partner folder, the folder's relation
  /// (per its role) decides it. Under any other parent the claim has
  /// no effect; a view root claims. On the wire a child that does not
  /// claim carries the bare atom 'noClaim', and one that claims
  /// carries nothing.
  ///
  /// PITFALL: If a vognode is write-protected, *none* of its children
  /// affect it, just as edits to itself do not affect it, regardless
  /// of what the children claim.
  pub claimsMembership : bool,
  /// Some(N) means this viewnode is shown here in place of N,
  /// which it (transitively) overrides. Herald pink "ĥ" after "O".
  /// Load-bearing, unlike the view stats: save extraction
  /// collects N (not this node's own ID) into the parent's lists
  /// wherever this fact appears, so a substituted child
  /// round-trips to the original ID instead of rewriting the
  /// parent's contains. Serialized as the keyed form
  /// '(overridesHere N)' at the top level; tamper-checked at
  /// buffer validation (the carrier's ID must equal the
  /// visibility-ungated 'resolve_override (N).effective').
  pub overridesHere    : Option<ID>,

  // The next two *Stats fields only influence how the node is shown. Editing them and saving the buffer leaves the graph unchanged, and those edits will be immediately lost, as this data is regenerated each time the view is rebuilt.
  pub graphStats    : GraphnodeStats,
  pub viewStats     : ViewnodeStats,
  /// A request about this occurrence's binding relationship to its
  /// viewparent: a skgrepo for it (unlike 'viewStats.relRepo', this
  /// is save intent), or, on a removed member, that the save re-add
  /// it as HEAD recorded it. It concerns that relationship rather
  /// than the node, so it lives outside 'Editability': a
  /// write-protected occurrence carries it too.
  pub relationshipEditRequest : Option<RelationshipEditRequest>,

  pub view_requests : HashSet < ViewRequest >,
  pub editability  : Editability,
}

/// Each Occurrence has one of these.
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

/// Whether the graph relates a viewnode to its viewparent as its
/// position implies: content under an occurrence, a partner of the
/// folder's relation in a partner folder, a container of the node
/// above it in a containerward chain, a property of the node in a
/// property folder. A derived fact, recomputed at every rendering
/// and independent of whether the viewnode claims membership.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Warrant {
  /// The truth relates it so; also every root, and every viewnode
  /// whose position implies no relationship.
  ByGraph,
  /// In the diff view, HEAD relates it so and the graph does not (a
  /// ghost warrant). On the wire: the atom 'ghostWarrant'. The
  /// refresh sets it ('set_warrants' in 'server/refresh/').
  ByHead,
  /// Neither does. On the wire: the atom 'noWarrant'.
  No,
}

/// View-specific statistics about a node.
/// These depend on the node's position in the current viewforest.
/// `cycle` depends on ancestors; `warrant` depends on the specific parent.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeStats {
  pub cycle                 : bool,
  /// True if a root or if skgrepo differs from skgrepo of nearest
  /// occurrence ancestor. Written '(view repoBoundary)'.
  pub repoBoundary          : bool,
  /// Set by the refresh ('set_warrants' in 'server/refresh/'); written
  /// as '(view noWarrant)' when 'Warrant::No'. A derived fact: the
  /// parser skips '(view ...)'.
  pub warrant           : Warrant,
  /// The relationship heralds as the semantic sexp strings
  /// '(rels ...)', '(props ...)' and '(birth ...)'
  /// (server/herald_tokens.rs 'relationship_heralds_sexp'), written
  /// inside '(view ...)'. The client parses them and decides all
  /// presentation (letters, colors, order). See 'docs/heralds.org'.
  pub rel_heralds           : RelationshipHeralds,
  /// True iff the node is shown write-protected here while its graph
  /// node has a body -- a body the rendering hides. Herald "B",
  /// hugging the ☮. Written '(view omittedBody)', a derived fact.
  pub omitted_body           : bool,
  /// Some(NAME) when the relationship instance this position's
  /// binding relationship to its viewparent represents (=contains= for an
  /// ordinary content child; the folder's relation for a simple
  /// partner folder's member -- see 'Folder::relation_member_role')
  /// is recorded in a skgrepo that differs from that relationship's default
  /// (see the relationship-default policy in 'SkgConfig'). None when
  /// equal to the default:
  /// the suppression is deliberate (see 'docs/telescopes.org',
  /// "What leaks, and what repair fixes") -- the herald marks
  /// exactly the deliberately privatized relationships. Also None: without a
  /// graph handle; for a node that is not genuinely a member here
  /// (one claiming nothing); and for the two
  /// compound partner folders (HiddenInSubscribee /
  /// HiddenOutsideOfSubscribee), which have no single
  /// 'relation_member_role' to read a skgrepo from.
  /// This is a derived fact, unlike a requested replacement stored in
  /// 'Occurrence::relationshipEditRequest'.  Save extraction never
  /// treats this value as a fieldIntent.
  /// Herald: red "~NAME" immediately before the ⌂ of repoBoundary
  /// (server/heralds.rs).
  pub relRepo            : Option<SkgrepoName>,
}

/// A member of a property folder: one alias, extra ID or true flag
/// of the folder's viewparent.
///
/// 'warrant' is set by the refresh ('set_warrants') and written
/// '(view noWarrant)' when 'Warrant::No'.
///
/// In the diff view an alias, ID or flag the worktree lost since HEAD
/// is a member with a ghost warrant ('Warrant::ByHead'): HEAD alone
/// warrants it, written '(view ghostWarrant)'. An alias or ID with a
/// ghost warrant claims no membership ('claimsMembership', which the
/// refresh marks, written 'noClaim'): the alias reader and the ID
/// check skip it, and its change-forest leaf is that alias or ID.
#[derive( Debug, Clone, PartialEq )]
pub enum PropertyMember {
  Alias { text: String, // an alias for the node's grandparent
          relRepo: Option<SkgrepoName>,
          relationshipEditRequest: Option<RelationshipEditRequest>,
          claimsMembership: bool,
          warrant: Warrant },
  ID { gnid: ID, // an ID of grandparent (the parent being an idFolder)
       claimsMembership: bool,
       warrant: Warrant },
  /// A true file-level boolean flag of the node's grandparent.
  Flag {
    flag : Flag,
    title    : String,
    body     : Option<String>,
    warrant : Warrant, },
}

/// A headline the diff view adds to describe a change. Display-only
/// and save-inert: the validator ignores them, and the refresh draws
/// them in the diff view and knows them by their kind. Each but
/// BodyDiff is a change-forest leaf or commit readiness aggregate, so
/// its viewnode carries '(ready ...)' and a readiness request.
#[derive( Debug, Clone, PartialEq )]
pub enum ChangeViewnode {
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

/// Which changes a 'ChangeViewnode::ChangesetMarker' stands for.
#[derive( Debug, Clone, Copy, PartialEq, Eq, Hash )]
pub enum ChangesetMarkerKind {
  Content,   // "its content changed": the ~ change-forest leaves of its content
  RelRepo,   // "some content relationships changed skgrepo"
  OrderOnly, // "order-only change", always ready
}

/// A viewnode grouping the children of one kind of relationship of
/// its viewparent: the twelve partner folders, whose members are
/// nodes (ten relation roles, the two compound partner folders, and
/// the diff view's two acquisition folders), and the three property
/// folders, whose members are the viewparent's aliases, IDs and true
/// flags.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Folder {
  Subscribee, // Collects subscribees its parent subscribes to. Writable.
  Subscriber, // Collects nodes that subscribe to its parent. Write-protected (editable from the other side of the relationship).
  Overridden, // Collects nodes whose view its parent overrides. Writable.
  Overrider, // Collects nodes that override its parent's view. Write-protected (editable from the other side of the relationship).
  Hider, // Collects nodes that hide its parent. Write-protected (editable from the other side of the relationship).
  // This folder is not itself hidden. Its children represent the nodes hidden
  // by the node represented by its parent. The relationships are write-protected
  // here, but editable within the parent's subscribeeFolder.
  Hidden,
  HiddenInSubscribee, // Child of a subscribee-as-such. Collects children of the subscribee that the subscriber hides. Write-protected (but these relationships are editable by modifying the listed contents of the subscribee-as-such).
  HiddenOutsideOfSubscribee, // Child of a subscribeeFolder. Collects things hidden by the subscribeeFolder's parent but absent from every subscribee's content. This compound partner folder is editable as an exclusive visible-outside subset; its hide skgrepos remain derived. Shown after all Subscribees, under the same subscribeeFolder.
  Container, // Collects nodes that contain its parent. Write-protected (editable from the other side of the relationship).
  Mentioner, // Collects nodes that link to its parent. Write-protected (a link lives in the linking node's text).
  // The diff view's two derived folders. Their membership comes from the extra IDs of the graph straddle's after-graph; both are save-inert.
  AcquiredBy, // Child of a ghost: the node that acquired it by a nodeMerge.
  Acquired, // Child of an acquirer: the ghosts of the nodes it acquired.
  // The three property folders.
  Alias,
  ID,
  /// Its title and body are what the user typed there; a fresh
  /// rendering has neither.
  Flags { title : String, body : Option<String> },
}

/// Requests for editing operations on a node.
/// Only one edit request is allowed per node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeEditRequest {
  NodeMerge (ID), // The node with this request is the acquirer. The node with the ID that this request specifies is the acquiree.
  Delete, // request to delete this node
  SetFlag { flag : Flag, value : bool },
}

/// A request about the relationship that binds a viewnode to its
/// viewparent, rather than about the viewnode's node, carried by an
/// occurrence, an unknown or an alias. At most one per headline:
/// the parser takes one '(editRequest ...)'.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelationshipEditRequest {
  /// '(editRequest (relRepo R))': record the relationship in R.
  RelRepo (SkgrepoName),
  /// '(editRequest restoreRel)': re-add the relationship to the
  /// viewparent that a removed member stands for, as HEAD recorded it.
  RestoreRel,
}

impl RelationshipEditRequest {
  /// The skgrepo that 'request' asks for, if it is a relRepo request.
  pub fn relRepo_of (
    request : &Option<RelationshipEditRequest>,
  ) -> Option<&SkgrepoName> {
    match request {
      Some (RelationshipEditRequest::RelRepo (skgrepo)) => Some (skgrepo),
      Some (RelationshipEditRequest::RestoreRel) | None => None, } }
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
///   so that its children claim nothing.
/// - 'WriteProtectedSubtree' does that to the node and its whole
///   content subtree.
/// - 'Fork' is the explicit 'skg-fork-node' gesture: clone this (owned)
///   node into a private fork that overrides it. Consumed on the save
///   path (fork detection), not by the refresh.
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

impl Occurrence {
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

  /// The collected ID: what this viewnode contributes to its
  /// parent's collected lists -- the 'overridesHere' original when
  /// the node is shown in place of one, else its own ID. Save
  /// extraction, the distinctness validators and the refresh's goal
  /// member ('member_of_child') all read this, never the raw 'id', so a
  /// shown overrider round-trips to the original member instead of
  /// rewriting its parent's contains.
  pub fn collected_gnid (&self) -> ID {
    self . overridesHere . clone ()
    . unwrap_or_else ( || self . gnid . clone () ) }
}

impl Folder {
  pub fn flags () -> Folder {
    Folder::Flags { title : String::new (), body : None } }

  pub fn is_flags (&self) -> bool {
    matches! (self, Folder::Flags { .. }) }

  /// Whether the members are properties of the viewparent (its
  /// aliases, IDs or true flags) rather than nodes.
  pub fn members_are_properties (&self) -> bool {
    matches! ( self, Folder::Alias | Folder::ID | Folder::Flags { .. } ) }

  /// Whether the folder's goal has an order the view follows: the
  /// viewparent's own file records the members in order (the
  /// subscribee, overridden, hidden and alias folders), or the folder
  /// is the compound partner folder of what the subscriber hides
  /// outside its subscribees, in the order of the subscriber's hides
  /// list. False when other nodes' files record the members, or they
  /// are derived or unordered: the view then keeps the order its
  /// members already have.
  pub fn goal_is_ordered (&self) -> bool {
    match self {
      Folder::Subscribee | Folder::Overridden | Folder::Hidden
        | Folder::HiddenOutsideOfSubscribee | Folder::Alias => true,
      Folder::Subscriber | Folder::Overrider | Folder::Hider
        | Folder::HiddenInSubscribee
        | Folder::Container | Folder::Mentioner
        | Folder::AcquiredBy | Folder::Acquired
        | Folder::ID | Folder::Flags { .. } => false, } }

  /// Whether the save reads the folder's membership, so that a member
  /// typed or removed there edits the graph: the subscribee,
  /// overridden and alias folders, and the compound partner folder of
  /// what the subscriber hides outside its subscribees. Every other
  /// folder is write-protected: its members are edited from the other
  /// side of the relationship, or derived.
  pub fn is_editable (&self) -> bool {
    match self {
      Folder::Subscribee | Folder::Overridden
        | Folder::HiddenOutsideOfSubscribee | Folder::Alias => true,
      Folder::Subscriber | Folder::Overrider | Folder::Hider
        | Folder::Hidden | Folder::HiddenInSubscribee
        | Folder::Container | Folder::Mentioner
        | Folder::AcquiredBy | Folder::Acquired
        | Folder::ID | Folder::Flags { .. } => false, } }

  /// Whether each member of a folder of this kind carries its
  /// containerward chain: every kind whose members are live nodes,
  /// partners in a role or members of a compound partner folder. The
  /// diff view's two acquisition folders, which record a nodeMerge, do
  /// not, nor do the property folders.
  pub fn members_carry_containerward_chain (&self) -> bool {
    ! ( self . members_are_properties ()
        || matches! ( self, Folder::AcquiredBy | Folder::Acquired )) }

  pub fn repr_in_client (&self) -> &'static str {
    match self {
      Folder::Subscribee                => "subscribeeFolder",
      Folder::Subscriber                => "subscriberFolder",
      Folder::Overridden                => "overriddenFolder",
      Folder::Overrider                 => "overriderFolder",
      Folder::Hider                     => "hiderFolder",
      Folder::Hidden                    => "hiddenFolder",
      Folder::HiddenInSubscribee        => "hiddenInSubscribeeFolder",
      Folder::HiddenOutsideOfSubscribee => "hiddenOutsideOfSubscribeeFolder",
      Folder::Container                 => "containerFolder",
      Folder::Mentioner                 => "mentionerFolder",
      Folder::AcquiredBy                => "acquiredByFolder",
      Folder::Acquired                  => "acquiredFolder",
      Folder::Alias                     => "aliasFolder",
      Folder::ID                        => "idFolder",
      Folder::Flags { .. }              => "flagsFolder",
    }}

  /// The folder a folder atom names; a flags folder comes with no
  /// text.
  pub fn from_client_string (s : &str) -> Option<Folder> {
    match s {
      "subscribeeFolder"                => Some (Folder::Subscribee),
      "subscriberFolder"                => Some (Folder::Subscriber),
      "overriddenFolder"                => Some (Folder::Overridden),
      "overriderFolder"                 => Some (Folder::Overrider),
      "hiderFolder"                     => Some (Folder::Hider),
      "hiddenFolder"                    => Some (Folder::Hidden),
      "hiddenInSubscribeeFolder"        => Some (Folder::HiddenInSubscribee),
      "hiddenOutsideOfSubscribeeFolder" => Some (Folder::HiddenOutsideOfSubscribee),
      "containerFolder"                 => Some (Folder::Container),
      "mentionerFolder"                 => Some (Folder::Mentioner),
      "acquiredByFolder"                => Some (Folder::AcquiredBy),
      "acquiredFolder"                  => Some (Folder::Acquired),
      "aliasFolder"                     => Some (Folder::Alias),
      "idFolder"                        => Some (Folder::ID),
      "flagsFolder"                     => Some (Folder::flags ()),
      _                                 => None,
    } }

  pub fn title (&self) -> &str {
    match self {
      Folder::Flags { title, .. } => title,
      _ => "", } }

  pub fn body (&self) -> Option<&String> {
    match self {
      Folder::Flags { body, .. } => body . as_ref (),
      _ => None, } }
}

impl PropertyMember {
  pub fn repr_in_client (&self) -> &'static str {
    match self {
      PropertyMember::Alias { .. } => "alias",
      PropertyMember::ID { .. }    => "id",
      PropertyMember::Flag { .. }  => "flag",
    } }

  pub fn title (&self) -> &str {
    match self {
      PropertyMember::Alias { text, .. }  => text,
      PropertyMember::ID    { gnid, .. } => gnid,
      PropertyMember::Flag { title, .. }  => title,
    } }

  pub fn body (&self) -> Option<&String> {
    match self {
      PropertyMember::Flag { body, .. } => body . as_ref (),
      PropertyMember::Alias { .. } | PropertyMember::ID { .. } => None, } }

  /// An alias member that claims membership and is warranted, until
  /// the stats pass says otherwise.
  pub fn alias (
    text                    : String,
    relRepo                 : Option<SkgrepoName>,
    relationshipEditRequest : Option<RelationshipEditRequest>,
  ) -> PropertyMember {
    PropertyMember::Alias { text, relRepo, relationshipEditRequest,
                            claimsMembership : true,
                            warrant      : Warrant::ByGraph } }

  /// An ID member, as 'alias' makes an alias member.
  pub fn id (
    gnid : ID,
  ) -> PropertyMember {
    PropertyMember::ID { gnid,
                         claimsMembership : true,
                         warrant      : Warrant::ByGraph } }

  /// A flag member, warranted until the stats pass says otherwise.
  pub fn flag (
    flag  : Flag,
    title : String,
    body  : Option<String>,
  ) -> PropertyMember {
    PropertyMember::Flag { flag, title, body,
                           warrant : Warrant::ByGraph } }
}

impl ChangeViewnode {
  pub fn repr_in_client (&self) -> &'static str {
    match self {
      ChangeViewnode::OldText { .. }         => "oldText",
      ChangeViewnode::BodyDiff { .. }        => "bodyDiff",
      ChangeViewnode::HomeSkgrepoMove { .. } => "homeSkgrepoMove",
      ChangeViewnode::RelRepoMove { .. }     => "relRepoMove",
      ChangeViewnode::ChangesetMarker { .. } => "changesetMarker",
    } }

  pub fn title (&self) -> &str {
    match self {
      ChangeViewnode::OldText { title, .. } =>
        title . as_deref () . unwrap_or (""),
      ChangeViewnode::BodyDiff { .. }
        | ChangeViewnode::HomeSkgrepoMove { .. }
        | ChangeViewnode::RelRepoMove { .. }
        | ChangeViewnode::ChangesetMarker { .. } => "",
    } }

  pub fn body (&self) -> Option<&String> {
    match self {
      ChangeViewnode::OldText { body, .. } => body . as_ref (),
      ChangeViewnode::BodyDiff { diff } => Some (diff),
      ChangeViewnode::HomeSkgrepoMove { .. }
        | ChangeViewnode::RelRepoMove { .. }
        | ChangeViewnode::ChangesetMarker { .. } => None, } }
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
  /// The id the vognode stands for.
  pub fn gnid (&self) -> &ID {
    match self {
      Vognode::Occurrence (t) => &t . gnid,
      Vognode::Ghost        (g) => &g . gnid,
      Vognode::Unknown      (u) => &u . gnid,
    } }

  /// The id and home skgrepo of the node it stands for; None for an
  /// unknown, which knows no skgrepo.
  pub fn pid_and_skgrepo (
    &self,
  ) -> Option<(&ID, &SkgrepoName)> {
    match self {
      Vognode::Occurrence (t) => Some ((&t . gnid, &t . home_skgrepo)),
      Vognode::Ghost        (g) => Some ((&g . gnid, &g . home_skgrepo)),
      Vognode::Unknown      (_) => None,
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
    if self . ready { "ready" } else { "unready" } }
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
  /// it: an occurrence that is not write-protected.
  pub fn editable_gnid (
    &self,
  ) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        if ! t . is_writeProtected () => Some (&t . gnid),
      _ => None, }}

  /// Consume every save-only `(editRequest ...)`, and every readiness
  /// request, carried by this occurrence.
  /// Call only after the save's skgsave-commit: failed saves and confirmation
  /// round-trips must leave requests in the user's buffer. `view_requests` are
  /// deliberately separate -- the refresh fulfills those.
  /// A 'restoreRel' the save fulfilled leaves its headline claiming the
  /// relationship it restored, so that the next save keeps it (the
  /// refresh marks 'noClaim' again where the save did not restore it).
  pub fn consume_edit_request_after_save (
    &mut self,
  ) {
    self . readiness_request = None;
    let restored = |request : &Option<RelationshipEditRequest>| -> bool {
      matches! ( request, Some (RelationshipEditRequest::RestoreRel) ) };
    match &mut self . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
        if restored (&occurrence . relationshipEditRequest) {
          occurrence . claimsMembership = true; }
        occurrence . relationshipEditRequest = None;
        if let Editability::Editable { edit_request, .. } =
          &mut occurrence . editability
        { *edit_request = None; }},
      ViewnodeKind::Vognode (Vognode::Unknown (unknown)) => {
        if restored (&unknown . relationshipEditRequest) {
          unknown . claimsMembership = true; }
        unknown . relationshipEditRequest = None },
      ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
        ghost . restoreNode_request = false,
      ViewnodeKind::PropertyMember (PropertyMember::Alias {
        relationshipEditRequest, claimsMembership, .. }) => {
        if restored (relationshipEditRequest) {
          *claimsMembership = true; }
        *relationshipEditRequest = None },
      _ => {}, }}

  pub fn title (&self) -> &str {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => &t . title,
      ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
        &ghost . title,
      ViewnodeKind::PropertyMember (member) => member . title (),
      ViewnodeKind::ChangeViewnode (change) => change . title (),
      ViewnodeKind::Folder (folder) => folder . title (),
      ViewnodeKind::BufferRoot
        | ViewnodeKind::Tombstone
        | ViewnodeKind::Vognode (Vognode::Unknown (_)) =>
        "",
    }}

  /// What a message says of this headline.
  pub fn headline_name (&self) -> HeadlineName {
    let graphnode_id : Option<&ID> = match &self . kind {
      ViewnodeKind::Vognode (vognode) => Some (vognode . gnid ()),
      _ => None };
    HeadlineName::of ( self . viewnode_id . as_ref (), graphnode_id,
                       self . title () ) }

  /// A distinguishable label for error messages.
  pub fn error_label (&self) -> String {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        => t . title . clone(),
      ViewnodeKind::PropertyMember (PropertyMember::Alias { text, .. }) =>
        format!("property:alias({})", text),
      ViewnodeKind::PropertyMember (PropertyMember::ID { gnid, .. }) =>
        format!("property:id({})", gnid),
      ViewnodeKind::PropertyMember (PropertyMember::Flag { flag, .. }) =>
        format!("property:flag({})", flag . wire_name ()),
      ViewnodeKind::ChangeViewnode (change) =>
        format! ("changeViewnode:{}", change . repr_in_client ()),
      ViewnodeKind::Folder (folder) if folder . members_are_properties () =>
        format!("propertyFolder:{}", folder . repr_in_client ()),
      ViewnodeKind::Folder (folder) =>
        format!("partnerFolder:{}", folder . repr_in_client ()),
      ViewnodeKind::BufferRoot =>
        "forestRoot" . to_string (),
      ViewnodeKind::Vognode (Vognode::Ghost (h)) =>
        format!("ghost:{}", h . gnid . 0),
      ViewnodeKind::Tombstone =>
        "tombstone" . to_string (),
      ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
        format!("unknown:{}", u . gnid . 0), }}

  /// Reasonable for both Occurrences and Non-vognodes.
  pub fn body (&self) -> Option < &String > {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => t . body (),
      ViewnodeKind::Vognode (Vognode::Ghost (ghost)) => ghost . body . as_ref (),
      ViewnodeKind::Folder (folder) => folder . body (),
      ViewnodeKind::PropertyMember (member) => member . body (),
      ViewnodeKind::ChangeViewnode (change) => change . body (),
      ViewnodeKind::Vognode (Vognode::Unknown (_))
        | ViewnodeKind::BufferRoot
        | ViewnodeKind::Tombstone => None,
    }}

  /// Whether this viewnode is an occurrence.
  pub fn is_occurrence (&self) -> bool {
    matches! ( &self . kind,
               ViewnodeKind::Vognode (Vognode::Occurrence (_)) ) }

  /// The id of an occurrence. None for everything else
  /// (ghosts, unknowns, folders, non-vognodes, BufferRoot).
  pub fn occurrence_gnid (&self) -> Option<&ID> {
    match &self . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => Some (&t . gnid),
      _ => None,
    }}
}

impl fmt::Display for NodeEditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      NodeEditRequest::NodeMerge (gnid) => write!(f, "(merge {})", gnid . 0),
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
      "toDelete"    => Ok (NodeEditRequest::Delete),
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
      repoBoundary      : false,
      warrant       : Warrant::ByGraph,
      rel_heralds       : RelationshipHeralds::default (),
      omitted_body      : false,
      relRepo        : None,
    }} }

//
// Constructor functions
//

/// Create an Occurrence with default values for all fields except id, skgrepo, and title.
/// Useful when you need to customize other fields after construction.
pub fn default_occurrence (
  gnid   : ID,
  skgrepo : SkgrepoName,
  title   : String,
) -> Occurrence {
  Occurrence {
    title,
    gnid,
    home_skgrepo: skgrepo,
    claimsMembership : true,
    overridesHere    : None,
    graphStats     : GraphnodeStats::default(),
    viewStats      : ViewnodeStats::default(),
    relationshipEditRequest : None,
    view_requests  : HashSet::new(),
    editability    : Editability::Editable {
      body         : None,
      edit_request : None },
  }}

pub fn mk_editable_viewnode (
  gnid   : ID,
  skgrepo : SkgrepoName,
  title   : String,
  body    : Option < String >,
) -> Viewnode { mk_viewnode ( gnid,
                            skgrepo,
                            title,
                            true,
                            Editability::Editable {
                              body,
                              edit_request : None },
                            HashSet::new () ) } // view_requests

/// Build an unknown. Use when a referenced ID resolves
/// to nothing in in_rust_graph, on disk, or via any diff procedure
/// -- the unknown lets the view render the line as a herald
/// rather than aborting the whole BFS extension.
pub fn mk_unknown_viewnode (
  gnid : ID,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::Vognode (Vognode::Unknown ( Unknown {
      gnid,
      relRepo         : None,
      relationshipEditRequest : None,
      claimsMembership : true,
      warrant : Warrant::ByGraph, } )) ) }

/// Create a write-protected Viewnode from disk data.
/// Body is always None since write-protected nodes don't have editable content.
pub fn mk_writeProtected_viewnode (
  gnid     : ID,
  skgrepo : SkgrepoName,
  title  : String,
  claimsMembership : bool,
) -> Viewnode { mk_viewnode ( gnid,
                            skgrepo,
                            title,
                            claimsMembership,
                            Editability::WriteProtected,
                            HashSet::new ( )) } // view_requests

/// Convert an editable Viewnode to write-protected.
/// Discards body, edit_request and the editable-view request.
/// Errors if the input is not an Occurrence.
pub fn mk_writeProtected_from_viewnode (
  mut viewnode : Viewnode,
  claimsMembership : bool,
) -> Result < Viewnode, String > {
  match &mut viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (t)) => {
      // Mutate in place, so that every field not named here
      // (the other view_requests, graphStats,
      // viewStats, and any field added later) is preserved rather
      // than silently reset.
      t . claimsMembership = claimsMembership;
      t . write_protect ();
      Ok (viewnode) },
    _ => Err (
      "mk_writeProtected_from_viewnode: expected Occurrence"
        . to_string () ) }}

/// Create a Viewnode with *nearly* full metadata control.
/// The exception is that the 'GraphnodeStats' and 'ViewnodeStats' are intentionally omitted,
/// because it would be difficult and dangerous to set that in isolation,
/// without considering the rest of the Viewnode tree.
pub fn mk_viewnode (
  gnid         : ID,
  skgrepo       : SkgrepoName,
  title         : String,
  claimsMembership : bool,
  editability  : Editability,
  view_requests : HashSet < ViewRequest >,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::Vognode (
    Vognode::Occurrence (
      Occurrence { claimsMembership,
                            view_requests,
                            editability,
                            .. default_occurrence (
                              gnid, skgrepo, title ) } ) )) }

/// Helper to create a BufferRoot Viewnode.
pub fn viewforest_root_viewnode () -> Viewnode {
  viewnode_of (ViewnodeKind::BufferRoot) }

/// A viewnode of the given kind, unfocused and not org-folded,
/// carrying no change facts, no readiness request and no viewnode ID.
/// A literal that
/// sets other fields can take the rest from it:
/// 'Viewnode { foldAndFocus : FoldAndFocus { folded : true, .. FoldAndFocus::default () },
///            .. viewnode_of (kind) }'.
pub fn viewnode_of (
  kind : ViewnodeKind,
) -> Viewnode {
  Viewnode { foldAndFocus      : FoldAndFocus::default (),
             kind,
             change_facts      : ChangeFacts::default (),
             readiness_request : None,
             viewnode_id       : None } }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/viewnode.rs"]
mod tests;
