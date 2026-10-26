use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::{
  Flag, Graphnode, flag_is_true};
use crate::types::viewnode::{PropertyMember, Viewnode, ViewnodeKind, viewnode_of};
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::util::{
  KeptChildOrder, complete_managed_children_in_viewforest, treat_certain_children};

use ego_tree::{NodeId, Tree};
use std::error::Error;

pub fn reconcile_flags_folder_children (
  tree         : &mut Tree<Viewnode>,
  folder_skgid : NodeId,
  graph        : &InRustGraph,
  config       : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let (pid, skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor (
      tree, folder_skgid, 0, "reconcile_flags_folder_children") ?;
  let node : Graphnode = graphnode_graphFirst_by_pid_and_skgrepo (
    graph, config, &pid, &skgrepo)
    . map_err (|_| "reconcile_flags_folder_children: parent not found") ?;
  let goals : Vec<Flag> = Flag::ALL . into_iter ()
    . filter (|flag| flag_is_true (&node . flags, *flag))
    . collect ();
  complete_managed_children_in_viewforest (
    tree, folder_skgid,
    KeptChildOrder::Unmoved,
    |viewnode| matches! (&viewnode . kind,
      ViewnodeKind::PropertyMember (PropertyMember::Flag { .. })),
    |viewnode| match &viewnode . kind {
      ViewnodeKind::PropertyMember (PropertyMember::Flag { flag, .. }) => Ok (*flag),
      _ => Err ("managed child is not a Flag" . to_string ()), },
    &goals,
    |flag| Ok (viewnode_of (ViewnodeKind::PropertyMember (PropertyMember::flag (
        *flag, String::new (), None ))))) ?;
  // The key-based reconciler retains an existing leaf. Restore its generated
  // titleless shape too, so title edits never linger after a refresh.
  treat_certain_children (
    tree, folder_skgid,
    |viewnode| matches! (&viewnode . kind,
      ViewnodeKind::PropertyMember (PropertyMember::Flag { .. })),
    |viewnode| if let ViewnodeKind::PropertyMember (PropertyMember::Flag {
      title, body, .. }) = &mut viewnode . kind
    { *title = String::new ();
      *body = None; }) ?;
  Ok (())
}
