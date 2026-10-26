//! The refresh: the one operation that takes a buffer, parsed and
//! validated, and the truth, and returns the corrected buffer
//! ('TODO/simplify-refresh/04_1_plan_rewrite-refresh.org', "The model,
//! stateless"). Everything is decided from the buffer and the truth
//! as they are now.
//!
//! The walk is level order from the roots. A parent's own kind is
//! settled before its children are visited: at each parent, the
//! ladder ('ladder.rs') re-kinds its children by the ID rule
//! ('id_rule.rs'), merges siblings standing for one gnid, sets each
//! child's warrant against the parent's goal ('goal.rs'), marks the
//! claims, reinstates what the goal names and no child stands for
//! (as far as the extent allows, 'extent.rs'), reorders, substitutes
//! overriders, and fulfills the children's view requests. In the diff
//! view the goal also holds what HEAD alone warrants, and change
//! viewnodes are recomputed in place ('diff_view.rs'). After the
//! walk, the tail ('tail.rs') computes the derived facts, prunes the
//! childless tombstones, gives every viewnode a vnid and renders.
//! The refresh returns what it changed ('corrections.rs').

pub mod ancestry;
pub mod corrections;
mod diff_view;
mod extent;
mod goal;
mod id_rule;
mod ladder;
mod tail;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Editability, ViewRequest, Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::ViewnodeIdRecord;
use crate::update_buffer::completion_mode::CompletionMode;
use corrections::{Correction, CorrectionKind, CorrectionLog};

use ego_tree::{NodeId, Tree};
use std::collections::{HashSet, VecDeque};
use std::error::Error;

/// The truth: the gated in-Rust graph, the worktree's graph as the
/// skgrepo restriction lets it be seen. In the diff view the mode
/// adds HEAD, through the graph straddle.
pub struct Truth<'a> {
  pub graph       : &'a InRustGraph,
  pub config      : &'a SkgConfig,
  pub restriction : Option<&'a SkgrepoRestriction>,
}

/// What a refresh returns besides the corrected viewforest.
pub struct Refreshed {
  /// The buffer's text.
  pub text        : String,
  /// What the refresh changed, in walk order.
  pub corrections : Vec<Correction>,
}

/// Refresh 'viewforest' against 'truth', in 'mode', drawing at most
/// 'node_budget' extensions of new editable content
/// ('extent.rs'). In the diff view 'readiness_store' gives the
/// readiness facts; a content view passes None. 'viewnode_id_record'
/// assigns the vnids of the viewnodes the refresh draws. Edit requests
/// pass through untouched; view requests are fulfilled and dropped.
pub fn refresh_viewforest (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  truth              : &Truth,
  mode               : CompletionMode,
  node_budget        : usize,
  readiness_store    : Option<&ReadinessStore>,
) -> Result<Refreshed, Box<dyn Error>> {
  let mut walk : Walk = Walk::new (truth, mode, node_budget);
  { let tree : &mut Tree<Viewnode> = viewforest . as_internal_tree_mut ();
    debug_assert! ( at_most_one_editable_occurrence_per_gnid (tree),
                    "the input holds two editable occurrences of one gnid" );
    fulfill_editableView_requests (tree, &mut walk);
    walk . editable_gnids = editable_gnids_in (tree);
    walk_in_level_order (tree, &mut walk) ?; }
  let text : String = tail::finish (
    viewforest, viewnode_id_record, &walk, readiness_store ) ?;
  let corrections : Vec<Correction> =
    walk . corrections . into_corrections (viewforest . as_internal_tree_mut ());
  Ok ( Refreshed { text, corrections } ) }

/// What the walk carries from one parent to the next.
pub(crate) struct Walk<'a> {
  pub truth          : &'a Truth<'a>,
  pub mode           : CompletionMode<'a>,
  /// How many more editable occurrences may have their content drawn.
  pub node_budget    : usize,
  /// The gnids that have an editable occurrence: at most one each.
  pub editable_gnids : HashSet<ID>,
  /// The occurrences this refresh drew editable. At its visit, one
  /// that the spent budget allows no content becomes write-protected.
  pub drawn_editable : HashSet<NodeId>,
  /// The occurrences that carried an editable-view request: in the
  /// diff view only their extension may be charged to the budget.
  pub had_editableView_request : HashSet<NodeId>,
  pub corrections    : CorrectionLog,
}

impl<'a> Walk<'a> {
  fn new (
    truth       : &'a Truth<'a>,
    mode        : CompletionMode<'a>,
    node_budget : usize,
  ) -> Walk<'a> {
    Walk { truth, mode, node_budget,
           editable_gnids           : HashSet::new (),
           drawn_editable           : HashSet::new (),
           had_editableView_request : HashSet::new (),
           corrections              : CorrectionLog::default (), } }

  pub fn is_diff_view (
    &self,
  ) -> bool {
    matches! ( self . mode, CompletionMode::DiffView { .. } ) }
}

/// Visit every parent in level order, starting at the buffer root:
/// each visit runs the ladder over its children, which are then
/// visited after every parent already queued.
fn walk_in_level_order (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let mut queue : VecDeque<NodeId> =
    VecDeque::from ([ tree . root () . id () ]);
  while let Some (parent) = queue . pop_front () {
    ladder::visit_parent (tree, parent, walk) ?;
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
    . filter_map ( |node| node . value () . editable_skgid () )
    . all ( |gnid| seen . insert (gnid) ) }

fn editable_gnids_in (
  tree : &Tree<Viewnode>,
) -> HashSet<ID> {
  tree . root () . descendants ()
    . filter_map ( |node| node . value () . editable_skgid () . cloned () )
    . collect () }

/// An editable-view request makes its occurrence the node's editable
/// one, and any other editable occurrence of the node
/// write-protected. Done before the walk, so that an occurrence
/// write-protected here never draws content first. (The input holds
/// at most one such request per gnid.) Its content is drawn at its
/// own visit, as any editable occurrence's is.
fn fulfill_editableView_requests (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) {
  let requested : Vec<(NodeId, ID)> =
    tree . root () . descendants ()
    . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence))
        if occurrence . view_requests . contains (&ViewRequest::Editable) =>
        Some (( node . id (), occurrence . skgid . clone () )),
      _ => None, } )
    . collect ();
  for (treeid, gnid) in requested {
    let others : Vec<NodeId> =
      tree . root () . descendants ()
      . filter ( |node| node . id () != treeid
                        && node . value () . editable_skgid () == Some (&gnid) )
      . map ( |node| node . id () )
      . collect ();
    for other in others {
      write_protect_occurrence (tree, other, walk); }
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
      &mut tree . get_mut (treeid) . unwrap () . value () . kind
    { occurrence . view_requests . remove (&ViewRequest::Editable);
      if occurrence . is_writeProtected () {
        occurrence . editability = Editability::Editable {
          body : None, edit_request : None }; }}
    walk . had_editableView_request . insert (treeid); }}

/// Make the occurrence at 'treeid' write-protected, dropping (and
/// reporting) the node edit request its new state has no slot for,
/// and forgetting its gnid's editable occurrence.
pub(crate) fn write_protect_occurrence (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let (gnid, dropped) : (Option<ID>, bool) =
    match &mut tree . get_mut (treeid) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence))
        if ! occurrence . is_writeProtected () => {
        let dropped : bool = occurrence . edit_request () . is_some ();
        occurrence . write_protect ();
        ( Some ( occurrence . skgid . clone () ), dropped ) },
      _ => (None, false), };
  if dropped {
    walk . corrections . record (
      tree, treeid, CorrectionKind::EditRequestDropped ); }
  if let Some (gnid) = gnid {
    walk . editable_gnids . remove (&gnid);
    walk . drawn_editable . remove (&treeid); }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/refresh/mod.rs"]
mod tests;
