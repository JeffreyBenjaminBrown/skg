//! The tail of a refresh, once, after the walk: compute the derived
//! facts (the stats of each occurrence: its props, rels and birth,
//! 'cycle', 'repoBoundary', 'omittedBody'; in the diff view the
//! readiness facts and the home skgrepo moves), prune every childless
//! tombstone, roots included (so a buffer may end with no roots), give
//! a vnid to every viewnode that lacks one, and render. The warrants
//! the ladder set are kept: the stats pass does not recompute them.

use crate::diff_view::pruned_change_view::set_diffView_facts_in_viewforest;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, Walk};
use crate::refresh::ladder::folder_goes_when_empty;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::tree::generic::do_everywhere_in_tree_dfs_readonly;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::views_state::ViewnodeIdRecord;
use crate::update_buffer::completion_mode::CompletionMode;
use crate::update_buffer::graphnodestats::{
  set_graphnodestats_in_viewforest, set_graphnodestats_in_viewforest_with_skgrepo_set};
use crate::update_buffer::util::detach_viewnode_transferring_focus;
use crate::update_buffer::viewnodestats::{
  WarrantsAndMemberClaims, set_viewnodestats_in_viewforest_with};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// See the module comment.
pub(crate) fn finish (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  walk               : &Walk,
  readiness_store    : Option<&ReadinessStore>,
) -> Result<String, Box<dyn Error>> {
  set_derived_facts (viewforest, walk, readiness_store) ?;
  prune_childless_tombstones (viewforest . as_internal_tree_mut ()) ?;
  viewnode_id_record . assign_viewnode_ids (viewforest . as_internal_tree_mut ());
  viewforest_to_string (viewforest, walk . truth . config) }

fn set_derived_facts (
  viewforest      : &mut ViewForest,
  walk            : &Walk,
  readiness_store : Option<&ReadinessStore>,
) -> Result<(), Box<dyn Error>> {
  let truth : &Truth = walk . truth;
  let (container_to_contents, content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>) =
    match truth . restriction {
      Some (restriction) => set_graphnodestats_in_viewforest_with_skgrepo_set (
        viewforest . as_internal_tree_mut (), truth . graph, truth . config, restriction ) ?,
      None => set_graphnodestats_in_viewforest (
        viewforest . as_internal_tree_mut (), truth . graph, truth . config ) ?, };
  set_viewnodestats_in_viewforest_with (
    viewforest . as_internal_tree_mut (), truth . graph,
    &container_to_contents, &content_to_containers,
    truth . config, truth . restriction,
    WarrantsAndMemberClaims::AlreadySet );
  if let CompletionMode::DiffView { straddle, buffer_kind } = walk . mode {
    let store : &ReadinessStore = readiness_store
      . ok_or ("refresh_viewforest: the diff view needs the readiness store") ?;
    set_diffView_facts_in_viewforest (
      viewforest, straddle, buffer_kind, store, truth . graph ); }
  Ok (( )) }

/// Remove every childless tombstone, a root included, postorder, so
/// that a tombstone left childless by the removal of its last child
/// goes too, and so does a folder the removals leave childless that
/// goes when empty ('folder_goes_when_empty'), as it would have been
/// entombstoned had it been childless at its visit. Focus moves to the
/// parent of what is removed.
fn prune_childless_tombstones (
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
