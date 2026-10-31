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
//! changes the forest in place and reports nothing of what it changed;
//! it renders nothing, and its caller renders the forest once it is
//! done with it.

pub mod ancestry;
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
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Editability, ViewRequest, Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::ViewnodeIdRecord;
use crate::refresh::mode::RefreshMode;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;

/// The truth: the gated in-Rust graph, the worktree's graph as the
/// skgrepo restriction lets it be seen. In the diff view the mode
/// adds HEAD, through the graph straddle.
pub struct Truth<'a> {
  pub graph       : &'a InRustGraph,
  pub config      : &'a SkgConfig,
  pub restriction : &'a SkgrepoRestriction,
}

impl<'a> Truth<'a> {
  /// The truth of a runtime generation, under 'restriction'.
  pub fn of_runtime (
    runtime     : &'a RuntimeGeneration,
    restriction : &'a SkgrepoRestriction,
  ) -> Truth<'a> {
    Truth { graph : &runtime . graph, config : &runtime . config, restriction } }
}

/// Refresh the content view 'viewforest' against 'truth', drawing at
/// most the config's 'initial_node_limit' extensions of new editable
/// content: what every content view's refresh shares.
pub fn refresh_content_viewforest (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  truth              : &Truth,
) -> Result<(), Box<dyn Error>> {
  refresh_viewforest (
    viewforest, viewnode_id_record, truth, RefreshMode::ContentView,
    truth . config . initial_node_limit ) }

/// Refresh 'viewforest' against 'truth', in 'mode', drawing at most
/// 'node_budget' extensions of new editable content
/// ('extent.rs'). 'viewnode_id_record' assigns the vnids of the
/// viewnodes the refresh draws. Edit requests pass through, but for
/// one a headline's new kind or new write-protection has no slot for,
/// which is dropped; view requests are fulfilled and dropped.
pub fn refresh_viewforest (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  truth              : &Truth,
  mode               : RefreshMode,
  node_budget        : usize,
) -> Result<(), Box<dyn Error>> {
  let mut walk : Walk = Walk::new (truth, mode, node_budget);
  { let tree : &mut Tree<Viewnode> = viewforest . as_internal_tree_mut ();
    debug_assert! ( at_most_one_editable_occurrence_per_gnid (tree),
                    "the input holds two editable occurrences of one gnid" );
    fulfill_editableView_requests (tree, &mut walk);
    keep_the_first_editable_occurrence_of_each_node (tree, &mut walk);
    walk . editable_occurrence_of = editable_occurrences_in (tree, &walk);
    walk_in_level_order (tree, &mut walk) ?; }
  tail::finish (viewforest, viewnode_id_record, walk) }

/// What the walk carries from one parent to the next.
pub(crate) struct Walk<'a> {
  pub truth                    : &'a Truth<'a>,
  pub mode                     : RefreshMode<'a>,
  /// How many more editable occurrences may have their content drawn
  /// ('extent.rs').
  pub node_budget              : usize,
  /// Where each node's one editable occurrence stands, by the key
  /// 'editability_key' gives its gnid: the pid, whichever of the
  /// node's IDs the occurrence is written under. Which occurrences
  /// this refresh extends first the tree itself says: each carries an
  /// editable-view request through its own visit
  /// ('consume_editableView_request').
  pub editable_occurrence_of   : HashMap<ID, NodeId>,
  /// The compound folders this refresh added. Their members are drawn
  /// folded: in the clients' encoding a folder's collapse lives on its
  /// members, since 'folded' on a headline hides it in its parent.
  pub added_compound_folders   : HashSet<NodeId>,
}

impl<'a> Walk<'a> {
  fn new (
    truth       : &'a Truth<'a>,
    mode        : RefreshMode<'a>,
    node_budget : usize,
  ) -> Walk<'a> {
    Walk { truth, mode, node_budget,
           editable_occurrence_of : HashMap::new (),
           added_compound_folders : HashSet::new (), } }

  pub fn is_diff_view (
    &self,
  ) -> bool {
    matches! ( self . mode, RefreshMode::DiffView { .. } ) }

  /// The key of the editability of an occurrence written under
  /// 'gnid': the pid of the node 'gnid' names, else 'gnid' itself,
  /// since the ID rule points every occurrence at its node's pid. In
  /// the diff view, a gnid that named a node of HEAD's own before a
  /// nodeMerge made it an extra ID ('acquired_since_head') is its own
  /// key: the ID rule will make its occurrence that node's ghost, so
  /// it does not hold the acquirer's editability.
  pub fn editability_key (
    &self,
    gnid : &ID,
  ) -> ID {
    let pid : ID = self . truth . graph . pid_else_raw_id (gnid);
    if id_rule::acquired_since_head (gnid, &pid, self) { gnid . clone () }
    else { pid } }

  /// Let the occurrence of 'gnid' at 'treeid' hold its node's one
  /// editability, unless another occurrence already does. Whether it
  /// may.
  pub fn hold_editability (
    &mut self,
    gnid   : &ID,
    treeid : NodeId,
  ) -> bool {
    let key : ID = self . editability_key (gnid);
    *self . editable_occurrence_of . entry (key)
      . or_insert (treeid) == treeid }

  /// The occurrence of 'gnid' at 'treeid' holds its node's
  /// editability no more, if it held it.
  pub fn release_editability (
    &mut self,
    gnid   : &ID,
    treeid : NodeId,
  ) {
    let key : ID = self . editability_key (gnid);
    if self . editable_occurrence_of . get (&key) == Some (&treeid) {
      self . editable_occurrence_of . remove (&key); }}

  /// Whether some occurrence of the node 'gnid' names holds its
  /// editability.
  pub fn editability_is_held (
    &self,
    gnid : &ID,
  ) -> bool {
    self . editable_occurrence_of . contains_key (
      &self . editability_key (gnid) ) }
}

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

/// Where each node's editable occurrence stands in 'tree', by the
/// key of its editability ('Walk::editability_key'). Run after
/// 'keep_the_first_editable_occurrence_of_each_node', which leaves
/// one editable occurrence per key.
fn editable_occurrences_in (
  tree : &Tree<Viewnode>,
  walk : &Walk,
) -> HashMap<ID, NodeId> {
  tree . root () . descendants ()
    . filter_map ( |node| node . value () . editable_gnid ()
                   . map ( |gnid| ( walk . editability_key (gnid), node . id () )) )
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
/// content first. They are compared by the key of their editability
/// ('Walk::editability_key'), so that in the diff view an occurrence
/// the ID rule will make a ghost does not take the acquirer's.
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
      if ! seen . insert ( walk . editability_key (gnid) ) {
        repeats . push (treeid); }}
    queue . extend ( node . children () . map ( |child| child . id () )); }
  for repeat in repeats {
    write_protect_occurrence (tree, repeat, walk); }}

/// An editable-view request in the text makes its occurrence the
/// node's editable one, and any other editable occurrence of the node
/// write-protected, whichever of the node's IDs it is written under.
/// Done before the walk, so that an occurrence write-protected here
/// never draws content first, though it stand above the requester. (The input holds at most one such request per
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
    let key : ID = walk . editability_key (&gnid);
    let others : Vec<NodeId> =
      tree . root () . descendants ()
      . filter ( |node| node . id () != treeid
                        && node . value () . editable_gnid ()
                           . is_some_and ( |other| walk . editability_key (other) == key ))
      . map ( |node| node . id () )
      . collect ();
    for other in others {
      write_protect_occurrence (tree, other, walk); }
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut tree . get_mut (treeid) . unwrap () . value () . kind
    { if occurrence . is_writeProtected () {
        occurrence . editability = Editability::Editable { // the re-kind gives it its body
          body : None, edit_request : None }; }}}}

/// Make the occurrence at 'treeid' write-protected, dropping the
/// node edit request its new state has no slot for, and its
/// editable-view request, and forgetting its gnid's editable
/// occurrence.
pub(crate) fn write_protect_occurrence (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let gnid : Option<ID> =
    match &mut tree . get_mut (treeid) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
        if ! occurrence . is_writeProtected () => {
        occurrence . write_protect ();
        Some ( occurrence . gnid . clone () ) },
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
        occurrence . write_protect (); // drops a request that would make it editable
        None },
      _ => None, };
  if let Some (gnid) = gnid {
    walk . release_editability (&gnid, treeid); }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/refresh/mod.rs"]
mod tests;
