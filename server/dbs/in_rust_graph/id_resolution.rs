use std::collections::HashMap;
use ego_tree::{NodeRef, NodeMut, NodeId, Tree};

use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::maybe_placed_viewnode::MpVognode;
use crate::types::misc::ID;
use crate::dbs::in_rust_graph::InRustGraph;

pub fn replace_ids_with_pids(
  viewforest : &mut Tree<MpViewnode>,
  root_id    : NodeId,
  graph      : &InRustGraph,
) {
  let mut all_ids : Vec<ID> = Vec::new ();
  collect_ids_in_tree (viewforest . root (), &mut all_ids);
  let pid_map : HashMap<ID, Option<ID>> = all_ids . into_iter ()
    . map (|id| {
      let pid = graph . pid_of (&id);
      (id, pid) })
    . collect ();
  if let Some (root_mut) = viewforest . get_mut (root_id) {
    assign_pids_throughout_tree_from_map (root_mut, &pid_map); }
}

/// Collect IDs for bulk PID lookup
pub fn collect_ids_in_tree (
  node_ref : NodeRef < MpViewnode >,
  ids_to_lookup : & mut Vec < ID >
) {
  if let MpViewnodeKind::Vognode (
       MpVognode::Active (t))
    = &node_ref . value () . kind
    { if let Some (id) = &t . id
      { ids_to_lookup . push ( id . clone () ); }}
  for child in node_ref . children () { // recurse
    collect_ids_in_tree (
      child,
      ids_to_lookup ); }}

pub fn assign_pids_throughout_tree_from_map (
  mut node_ref : NodeMut < MpViewnode >,
  pid_map : & HashMap < ID, Option < ID > >
) {
  if let MpViewnodeKind::Vognode (
       MpVognode::Active (t))
    = &mut node_ref . value() . kind
    { let pid_opt : Option < ID > = t . id . as_ref ()
        . and_then ( |id| pid_map . get (id) )
        . and_then ( |opt| opt . clone () );
      if let Some (pid) = pid_opt {
        t . id = Some (pid); }}
  { // Recurse into children
    for child_treeid in {
      let treeid : NodeId = node_ref . id ();
      let child_treeids : Vec < NodeId > = {
        let tree : &Tree<MpViewnode> = node_ref . tree ();
        tree . get (treeid) . unwrap ()
          . children () . map ( | child | child . id () )
          . collect () };
      child_treeids } {
      if let Some (child_mut)
        = node_ref . tree () . get_mut (child_treeid)
      { assign_pids_throughout_tree_from_map (
        child_mut, pid_map ); }} }}
