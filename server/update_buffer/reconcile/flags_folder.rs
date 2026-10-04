use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_rustFirst_by_pid_and_repo;
use crate::types::misc::{ID, SkgConfig, RepoName};
use crate::types::nodes::complete::{
  Flag, Graphnode, flag_is_true};
use crate::types::viewnode::{Property, Viewnode, ViewnodeKind};
use crate::update_buffer::ancestry::pid_and_repo_from_required_ancestor;
use crate::update_buffer::util::{
  complete_relevant_children_in_viewnodetree, treat_certain_children};

use ego_tree::{NodeId, Tree};
use std::error::Error;

pub fn reconcile_flags_folder_children (
  tree      : &mut Tree<Viewnode>,
  folder_id : NodeId,
  graph     : &InRustGraph,
  config    : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let (pid, repo) : (ID, RepoName) =
    pid_and_repo_from_required_ancestor (
      tree, folder_id, 0, "reconcile_flags_folder_children") ?;
  let node : Graphnode = graphnode_rustFirst_by_pid_and_repo (
    graph, config, &pid, &repo)
    . map_err (|_| "reconcile_flags_folder_children: parent not found") ?;
  let goals : Vec<Flag> = Flag::ALL . into_iter ()
    . filter (|flag| flag_is_true (&node . misc, *flag))
    . collect ();
  complete_relevant_children_in_viewnodetree (
    tree, folder_id,
    |viewnode| matches! (&viewnode . kind,
      ViewnodeKind::Property (Property::Flag { .. })),
    |viewnode| match &viewnode . kind {
      ViewnodeKind::Property (Property::Flag { flag, .. }) => Ok (*flag),
      _ => Err ("relevant child is not a Flag" . to_string ()), },
    &goals,
    |flag| Ok (Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewnodeKind::Property (Property::Flag {
        flag : *flag,
        title    : String::new (),
        body     : None, }), })) ?;
  // The key-based reconciler retains an existing leaf. Restore its generated
  // titleless shape too, so title edits never linger after a refresh.
  treat_certain_children (
    tree, folder_id,
    |viewnode| matches! (&viewnode . kind,
      ViewnodeKind::Property (Property::Flag { .. })),
    |viewnode| if let ViewnodeKind::Property (Property::Flag {
      title, body, .. }) = &mut viewnode . kind
    { *title = String::new ();
      *body = None; }) ?;
  Ok (())
}
