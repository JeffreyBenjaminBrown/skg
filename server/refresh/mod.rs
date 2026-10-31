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
mod editability;
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
use crate::types::viewnode::Viewnode;
use crate::types::views_state::ViewnodeIdRecord;
use crate::refresh::mode::RefreshMode;

use ego_tree::{NodeId, Tree};
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
/// ('editability.rs'). 'viewnode_id_record' assigns the vnids of the
/// viewnodes the refresh draws. Edit requests pass through, but for
/// one a headline's new kind or new write-protection has no slot for,
/// which is dropped; view requests are fulfilled and dropped, but for
/// a fork request, which stays for the save.
pub fn refresh_viewforest (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  truth              : &Truth,
  mode               : RefreshMode,
  node_budget        : usize,
) -> Result<(), Box<dyn Error>> {
  let mut walk : Walk = Walk::new (truth, mode, node_budget);
  { let tree : &mut Tree<Viewnode> = viewforest . as_internal_tree_mut ();
    editability::settle_before_the_walk (tree, &mut walk);
    walk_in_level_order (tree, &mut walk) ?; }
  tail::finish (viewforest, viewnode_id_record, walk) }

/// What the walk carries from one parent to the next.
pub(crate) struct Walk<'a> {
  pub truth                    : &'a Truth<'a>,
  pub mode                     : RefreshMode<'a>,
  /// How many more editable occurrences may have their content drawn
  /// ('editability.rs').
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
  /// The skgrepo restriction that keeps nothing out, of the truth's
  /// config: what override resolution asks under when it must not
  /// depend on the connection's ('carrier_on_owned_chain'). Built
  /// once per walk.
  pub unrestricted             : SkgrepoRestriction,
}

impl<'a> Walk<'a> {
  pub(crate) fn new (
    truth       : &'a Truth<'a>,
    mode        : RefreshMode<'a>,
    node_budget : usize,
  ) -> Walk<'a> {
    Walk { truth, mode, node_budget,
           editable_occurrence_of : HashMap::new (),
           added_compound_folders : HashSet::new (),
           unrestricted : SkgrepoRestriction::unrestricted (truth . config), } }

  pub fn is_diff_view (
    &self,
  ) -> bool {
    matches! ( self . mode, RefreshMode::DiffView { .. } ) }
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

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/refresh/mod.rs"]
mod tests;
