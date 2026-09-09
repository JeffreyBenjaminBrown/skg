use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind, MpVognode};
use crate::types::misc::ID;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;

/// Replace resolvable active-node IDs within this subtree. Preserve
/// unknown IDs and anonymous/inactive nodes as authored.
pub fn replace_ids_with_pids (
  graph : &InRustGraph,
  viewforest : &mut Tree<MpViewnode>,
  root_id : NodeId,
) {
  let mut ids : Vec<ID> = Vec::new ();
  if let Some (root) = viewforest . get (root_id) {
    collect_ids_in_tree (root, &mut ids); }
  let resolved : HashMap<ID, Option<ID>> = pids_from_ids (graph, &ids);
  if let Some (root) = viewforest . get_mut (root_id) {
    assign_pids_throughout_tree_from_map (root, &resolved); } }

pub fn pids_from_ids (
  graph : &InRustGraph,
  node_ids : &[ID],
) -> HashMap<ID, Option<ID>> {
  node_ids . iter () . map ( |id| (id . clone (), graph . pid_of (id))) . collect () }

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
