//! The refresh: the one operation that takes a buffer, parsed and
//! validated, and the truth, and returns the corrected buffer
//! ('docs/data-model_technical.org', "The refresh: how a view follows
//! the graph"). Everything is decided from the buffer and the truth
//! as they are now.
//!
//! The walk is level order from the roots. A parent's own kind is
//! settled before its children are visited: at each parent, the
//! refresh's rungs ('visit.rs') re-kind its children by the ID rule
//! ('id_rule.rs'), merge duplicate viewnode siblings, set each
//! child's warrant against the parent's goal ('goal.rs'), mark the
//! claims, reinstate what the goal names and no child stands for
//! (as far as the extent allows, 'extent.rs'), reorder, substitute
//! overriders, and fulfill the children's view requests. In the diff
//! view the goal also holds what HEAD alone warrants, and change
//! viewnodes are recomputed in place ('diff_view.rs'). After the
//! walk, the tail ('tail.rs') computes the derived facts, prunes the
//! childless tombstones and gives every viewnode a vnid. The refresh
//! returns what it changed ('buffer_corrections.rs'); it renders
//! nothing, and its caller renders the forest once it is done with it.

pub mod ancestry;
pub mod buffer_corrections;
pub mod chain;
mod diff_view;
mod extent;
pub(crate) mod goal;
pub mod graphnodestats;
mod id_rule;
pub mod mode;
mod tail;
pub mod viewnodestats;
mod visit;

pub(crate) use tail::prune_childless_tombstones;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::GnidPresence;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Editability, ViewRequest, Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::ViewnodeIdRecord;
use crate::refresh::mode::RefreshMode;
use crate::refresh::ancestry::described_node;
use buffer_corrections::{BufferCorrectionPolicy, BufferCorrection, BufferCorrectionKind, BufferCorrectionLog};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;

/// The truth: the gated in-Rust graph, the worktree's graph as the
/// skgrepo restriction lets it be seen. In the diff view the mode
/// adds HEAD, through the graph straddle.
pub struct Truth<'a> {
  pub graph       : &'a InRustGraph,
  pub config      : &'a SkgConfig,
  pub restriction : Option<&'a SkgrepoRestriction>,
}

impl<'a> Truth<'a> {
  /// The truth of a runtime generation, under 'restriction'.
  pub fn of_runtime (
    runtime     : &'a RuntimeGeneration,
    restriction : Option<&'a SkgrepoRestriction>,
  ) -> Truth<'a> {
    Truth { graph : &runtime . graph, config : &runtime . config, restriction } }
}

/// Refresh the content view 'viewforest' against 'truth', drawing at
/// most the config's 'initial_node_limit' extensions of new editable
/// content: what every content view's refresh shares.
pub fn refresh_content_viewforest (
  viewforest               : &mut ViewForest,
  viewnode_id_record       : &mut ViewnodeIdRecord,
  truth                    : &Truth,
  buffer_correction_policy : &BufferCorrectionPolicy,
) -> Result<Vec<BufferCorrection>, Box<dyn Error>> {
  refresh_viewforest (
    viewforest, viewnode_id_record, truth, RefreshMode::ContentView,
    truth . config . initial_node_limit, buffer_correction_policy ) }

/// Refresh 'viewforest' against 'truth', in 'mode', drawing at most
/// 'node_budget' extensions of new editable content
/// ('extent.rs'). 'viewnode_id_record' assigns the vnids of the
/// viewnodes the refresh draws. 'buffer_correction_policy' says which
/// changes are no buffer corrections. Edit requests pass through,
/// but for one a headline's new kind or new write-protection has no
/// slot for, which is dropped and reported; view requests are
/// fulfilled and dropped. Returns what the refresh changed, in walk
/// order.
pub fn refresh_viewforest (
  viewforest               : &mut ViewForest,
  viewnode_id_record       : &mut ViewnodeIdRecord,
  truth                    : &Truth,
  mode                     : RefreshMode,
  node_budget              : usize,
  buffer_correction_policy : &BufferCorrectionPolicy,
) -> Result<Vec<BufferCorrection>, Box<dyn Error>> {
  let mut walk : Walk = Walk::new (truth, mode, node_budget, buffer_correction_policy);
  { let tree : &mut Tree<Viewnode> = viewforest . as_internal_tree_mut ();
    debug_assert! ( at_most_one_editable_occurrence_per_gnid (tree),
                    "the input holds two editable occurrences of one gnid" );
    fulfill_editableView_requests (tree, &mut walk);
    keep_the_first_editable_occurrence_of_each_node (tree, &mut walk);
    walk . editable_occurrence_of = editable_occurrences_in (tree);
    walk_in_level_order (tree, &mut walk) ?; }
  tail::finish (viewforest, viewnode_id_record, walk) }

/// What the walk carries from one parent to the next.
pub(crate) struct Walk<'a> {
  pub truth                    : &'a Truth<'a>,
  pub mode                     : RefreshMode<'a>,
  /// How many more editable occurrences may have their content drawn
  /// ('extent.rs').
  pub node_budget              : usize,
  /// Where each gnid's one editable occurrence stands. Which
  /// occurrences this refresh extends first the tree itself says: each
  /// carries an editable-view request through its own visit
  /// ('consume_editableView_request').
  pub editable_occurrence_of   : HashMap<ID, NodeId>,
  /// The compound folders this refresh added. Their members are drawn
  /// folded: in the clients' encoding a folder's collapse lives on its
  /// members, since 'folded' on a headline hides it in its parent.
  pub added_compound_folders   : HashSet<NodeId>,
  /// The folders this refresh added, on request or by default, or
  /// drew as change viewnodes. Their members are extent
  /// ('Walk::record_drawn').
  pub added_folders            : HashSet<NodeId>,
  pub buffer_corrections       : BufferCorrectionLog,
  pub buffer_correction_policy : &'a BufferCorrectionPolicy,
  /// The headlines entombstoned without a buffer correction
  /// ('BufferCorrectionPolicy'). What their entombstoning makes
  /// unsayable beneath them goes unreported too.
  pub quiet_tombstones         : HashSet<NodeId>,
}

impl<'a> Walk<'a> {
  fn new (
    truth                    : &'a Truth<'a>,
    mode                     : RefreshMode<'a>,
    node_budget              : usize,
    buffer_correction_policy : &'a BufferCorrectionPolicy,
  ) -> Walk<'a> {
    Walk { truth, mode, node_budget, buffer_correction_policy,
           editable_occurrence_of   : HashMap::new (),
           added_compound_folders   : HashSet::new (),
           added_folders            : HashSet::new (),
           buffer_corrections       : BufferCorrectionLog::default (),
           quiet_tombstones         : HashSet::new (), } }

  pub fn is_diff_view (
    &self,
  ) -> bool {
    matches! ( self . mode, RefreshMode::DiffView { .. } ) }

  /// Let the occurrence of 'gnid' at 'treeid' hold the gnid's one
  /// editability, unless another occurrence already does. Whether it
  /// may.
  pub fn hold_editability (
    &mut self,
    gnid   : &ID,
    treeid : NodeId,
  ) -> bool {
    *self . editable_occurrence_of . entry ( gnid . clone () )
      . or_insert (treeid) == treeid }

  /// The occurrence of 'gnid' at 'treeid' holds the gnid's
  /// editability no more, if it held it.
  pub fn release_editability (
    &mut self,
    gnid   : &ID,
    treeid : NodeId,
  ) {
    if self . editable_occurrence_of . get (gnid) == Some (&treeid) {
      self . editable_occurrence_of . remove (gnid); }}

  /// Whether some occurrence of 'gnid' holds its editability.
  pub fn editability_is_held (
    &self,
    gnid : &ID,
  ) -> bool {
    self . editable_occurrence_of . contains_key (gnid) }

  /// Whether this refresh draws what stands under 'parent' for the
  /// first time: the content of an occurrence it extends first (one
  /// carrying an editable-view request through its visit: one it drew,
  /// one to which a request passed, the roots of a fresh view's stub
  /// among them) and the members of a folder it added. Drawing those
  /// is extent, not buffer correction.
  pub fn draws_first_under (
    &self,
    tree   : &Tree<Viewnode>,
    parent : NodeId,
  ) -> bool {
    carries_editableView_request (tree, parent)
    || self . added_folders . contains (&parent) }

  /// Report the headline drawn at 'drawn', under 'parent', as
  /// reinstated, unless drawing it is extent ('draws_first_under'):
  /// a reinstatement is a buffer correction only where a list already drawn
  /// was missing a member, and only when the policy says so of the
  /// node whose list it is ('BufferCorrectionPolicy'): the restriction
  /// did not just widen, nor a nodeMerge the save requested give that
  /// node the member.
  pub(crate) fn record_drawn (
    &mut self,
    tree   : &Tree<Viewnode>,
    parent : NodeId,
    drawn  : NodeId,
  ) {
    if ! self . draws_first_under (tree, parent)
       && self . buffer_correction_policy . reports_reinstatement (
            gnid_whose_list_is_drawn_under (tree, parent) ) {
      self . buffer_corrections . record (tree, drawn, BufferCorrectionKind::Reinstated); }}
}

/// The gnid of the node whose list the children of 'parent' are
/// drawn from: an occurrence's own, or, under a folder, that of the
/// node the folder describes ('described_node'). None under any other
/// parent.
fn gnid_whose_list_is_drawn_under (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> Option<&ID> {
  let holder : NodeId = match &tree . get (parent) ? . value () . kind {
    ViewnodeKind::Folder (_) => described_node (tree, parent) ?,
    _ => parent, };
  tree . get (holder) ? . value () . occurrence_gnid () }

/// Visit every parent in level order, starting at the buffer root:
/// each visit runs the refresh's rungs over its children, which are then
/// visited after every parent already queued.
fn walk_in_level_order (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let mut queue : VecDeque<NodeId> =
    VecDeque::from ([ tree . root () . id () ]);
  while let Some (parent) = queue . pop_front () {
    visit::visit_parent (tree, parent, walk) ?;
    queue . extend ( tree . get (parent) . unwrap () . children ()
                     . map ( |child| child . id () )); }
  Ok (( )) }

/// The input holds at most one editable occurrence per gnid:
/// validation refuses a saved buffer with two
/// ('Multiple_Defining_Viewnodes'), and every other input is the
/// server's own output.
fn at_most_one_editable_occurrence_per_gnid (
  tree : &Tree<Viewnode>,
) -> bool {
  let mut seen : HashSet<&ID> = HashSet::new ();
  tree . root () . descendants ()
    . filter_map ( |node| node . value () . editable_gnid () )
    . all ( |gnid| seen . insert (gnid) ) }

/// Where each gnid's editable occurrence stands in 'tree'.
fn editable_occurrences_in (
  tree : &Tree<Viewnode>,
) -> HashMap<ID, NodeId> {
  tree . root () . descendants ()
    . filter_map ( |node| node . value () . editable_gnid ()
                   . map ( |gnid| ( gnid . clone (), node . id () )) )
    . collect () }

/// Whether the occurrence at 'treeid' carries an editable-view
/// request. During the walk the request stands for this refresh's
/// first extension of the occurrence, whoever asked for it: the user,
/// a request passing on to what it extends, or the drawing of an
/// occurrence that may be editable. It is consumed at the
/// occurrence's own visit ('consume_editableView_request') and
/// dropped when that visit ends.
pub(crate) fn carries_editableView_request (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> bool {
  matches! ( &tree . get (treeid) . unwrap () . value () . kind,
             ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
               if occurrence . view_requests . contains (&ViewRequest::Editable) ) }

/// Give the occurrence at 'treeid' an editable-view request.
pub(crate) fn add_editableView_request (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
) {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { occurrence . view_requests . insert (ViewRequest::Editable); }}

/// Two editable occurrences in the input may stand for one node when
/// a save made one's gnid an extra ID of the other's node (a
/// nodeMerge). The first in level order stays editable; the others
/// are write-protected, before any visit, so that none of them draws
/// content first.
fn keep_the_first_editable_occurrence_of_each_node (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut repeats : Vec<NodeId> = Vec::new ();
  let mut queue : VecDeque<NodeId> =
    VecDeque::from ([ tree . root () . id () ]);
  while let Some (treeid) = queue . pop_front () {
    let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
    if let Some (gnid) = node . value () . editable_gnid () {
      let pid : ID = match walk . truth . graph . gnid_presence_gated (
                             gnid, walk . truth . restriction ) {
        GnidPresence::Present => walk . truth . graph . pid_of (gnid)
          . unwrap_or_else ( || gnid . clone () ),
        _ => gnid . clone (), };
      if ! seen . insert (pid) { repeats . push (treeid); }}
    queue . extend ( node . children () . map ( |child| child . id () )); }
  for repeat in repeats {
    write_protect_occurrence (tree, repeat, walk); }}

/// An editable-view request in the text makes its occurrence the
/// node's editable one, and any other editable occurrence of the node
/// write-protected. Done before the walk, so that an occurrence
/// write-protected here never draws content first, though it stand
/// above the requester. (The input holds at most one such request per
/// gnid.) The request stays on the occurrence, for its own visit to
/// consume as any other ('consume_editableView_request'): its content
/// is drawn then, and the request passes on to what it extends.
fn fulfill_editableView_requests (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) {
  let requested : Vec<(NodeId, ID)> =
    tree . root () . descendants ()
    . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
        if occurrence . view_requests . contains (&ViewRequest::Editable) =>
        Some (( node . id (), occurrence . gnid . clone () )),
      _ => None, } )
    . collect ();
  for (treeid, gnid) in requested {
    let others : Vec<NodeId> =
      tree . root () . descendants ()
      . filter ( |node| node . id () != treeid
                        && node . value () . editable_gnid () == Some (&gnid) )
      . map ( |node| node . id () )
      . collect ();
    for other in others {
      write_protect_occurrence (tree, other, walk); }
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut tree . get_mut (treeid) . unwrap () . value () . kind
    { if occurrence . is_writeProtected () {
        occurrence . editability = Editability::Editable { // the re-kind gives it its body
          body : None, edit_request : None }; }}}}

/// Make the occurrence at 'treeid' write-protected, dropping (and
/// reporting) the node edit request its new state has no slot for,
/// and its editable-view request, and forgetting its gnid's editable
/// occurrence.
pub(crate) fn write_protect_occurrence (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let (gnid, dropped) : (Option<ID>, bool) =
    match &mut tree . get_mut (treeid) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
        if ! occurrence . is_writeProtected () => {
        let dropped : bool = occurrence . edit_request () . is_some ();
        occurrence . write_protect ();
        ( Some ( occurrence . gnid . clone () ), dropped ) },
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
        occurrence . write_protect (); // drops a request that would make it editable
        (None, false) },
      _ => (None, false), };
  if dropped {
    walk . buffer_corrections . record (
      tree, treeid, BufferCorrectionKind::EditRequestDropped ); }
  if let Some (gnid) = gnid {
    walk . release_editability (&gnid, treeid); }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/refresh/mod.rs"]
mod tests;
