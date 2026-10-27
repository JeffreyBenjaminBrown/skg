//! The tail of a refresh, once, after the walk: compute the derived
//! facts (the stats of each occurrence: its props, rels and birth,
//! 'cycle', 'repoBoundary', 'omittedBody'; in the diff view the
//! '(ready ...)' change facts and the home skgrepo moves), prune every childless
//! tombstone, roots included (so a buffer may end with no roots), give
//! a vnid to every viewnode that lacks one, and render. The warrants
//! the refresh's rungs set are kept: the stats pass does not recompute them.

use crate::diff_view::pruned_change_view::set_diffView_facts_in_viewforest;
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, Walk};
use crate::refresh::visit::folder_goes_when_empty;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::tree::generic::{
  do_everywhere_in_tree_dfs_readonly, with_node_mut, write_at_ancestor_in_tree};
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::views_state::ViewnodeIdRecord;
use crate::refresh::mode::RefreshMode;
use crate::refresh::graphnodestats::set_graphnodestats_in_viewforest;
use crate::refresh::viewnodestats::set_viewnodestats_in_viewforest;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// See the module comment.
pub(crate) fn finish (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  walk               : &Walk,
) -> Result<String, Box<dyn Error>> {
  set_derived_facts (viewforest, walk) ?;
  prune_childless_tombstones (viewforest . as_internal_tree_mut ()) ?;
  viewnode_id_record . assign_viewnode_ids (viewforest . as_internal_tree_mut ());
  viewforest_to_string (viewforest, walk . truth . config) }

fn set_derived_facts (
  viewforest : &mut ViewForest,
  walk       : &Walk,
) -> Result<(), Box<dyn Error>> {
  let truth : &Truth = walk . truth;
  let (container_to_contents, content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>) =
    set_graphnodestats_in_viewforest (
      viewforest . as_internal_tree_mut (), truth . graph, truth . config,
      truth . restriction ) ?;
  set_viewnodestats_in_viewforest (
    viewforest . as_internal_tree_mut (), truth . graph,
    &container_to_contents, &content_to_containers,
    truth . config, truth . restriction );
  if let RefreshMode::DiffView { straddle, buffer_kind, readiness_store } = walk . mode {
    set_diffView_facts_in_viewforest (
      viewforest, straddle, buffer_kind, readiness_store, truth . graph ); }
  Ok (( )) }

/// Remove every childless tombstone, a root included, postorder, so
/// that a tombstone left childless by the removal of its last child
/// goes too, and so does a folder the removals leave childless that
/// goes when empty ('folder_goes_when_empty'), as it would have been
/// entombstoned had it been childless at its visit. Focus moves to the
/// parent of what is removed.
///
/// A tombstone that holds anything stays, even when all it holds is
/// tombstones: nested tombstones after a deletion are by design.
/// Collapsing them into one was considered and rejected, because it
/// could confuse the user about what stood where.
pub(crate) fn prune_childless_tombstones (
  tree : &mut Tree<Viewnode>,
) -> Result<(), Box<dyn Error>> {
  let mut prunable : Vec<NodeId> = Vec::new ();
  let root : NodeId = tree . root () . id ();
  do_everywhere_in_tree_dfs_readonly (
    tree, root,
    false, // postorder
    &mut |node| {
      if matches! ( node . value () . kind, ViewnodeKind::Tombstone )
         || folder_goes_when_empty ( node . value () ) {
        prunable . push (node . id ()); }
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  for treeid in prunable {
    if tree . get (treeid) . unwrap () . children () . next () . is_none () {
      detach_viewnode_transferring_focus (tree, treeid) ?; }}
  Ok (( )) }

/// Detach a viewnode, transferring focus to its parent first if the
/// detached subtree contained the focused node. The refresh's prune
/// uses it.
fn detach_viewnode_transferring_focus (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let has_focus : bool =
    subtree_satisfies ( tree, node, &|n : &Viewnode| n . foldAndFocus . focused ) ?;
  if has_focus {
    write_at_ancestor_in_tree (
      tree, node, 1,
      |vn : &mut Viewnode| { vn . foldAndFocus . focused = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  with_node_mut ( tree, node,
                  |mut n| { n . detach (); } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (()) }

/// Returns true if this node or any descendant satisfies the predicate.
fn subtree_satisfies<Node, Predicate> (
  tree      : &Tree<Node>,
  treeid     : NodeId,
  predicate : &Predicate,
) -> Result<bool, String>
where Predicate: Fn (&Node) -> bool {
  let node_ref : NodeRef<Node> =
    tree . get (treeid)
    . ok_or ("subtree_satisfies: node not found")?;
  if predicate( node_ref . value() ) {
    return Ok (true); }
  for child_ref in node_ref . children() {
    if subtree_satisfies( tree, child_ref . id(), predicate )? {
      return Ok (true); } }
  Ok (false) }
