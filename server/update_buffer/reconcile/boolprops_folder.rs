use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::{
  FileProperty, NodeComplete, file_property_is_true};
use crate::types::viewnode::{Qual, ViewNode, ViewNodeKind};
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::util::{
  complete_relevant_children_in_viewnodetree, treat_certain_children};

use ego_tree::{NodeId, Tree};
use std::error::Error;

pub fn reconcile_boolprops_folder_children (
  tree      : &mut Tree<ViewNode>,
  folder_id : NodeId,
  graph     : &InRustGraph,
  config    : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor (
      tree, folder_id, 0, "reconcile_boolprops_folder_children") ?;
  let node : NodeComplete = nodecomplete_rustFirst_by_pid_and_source (
    graph, config, &pid, &source)
    . map_err (|_| "reconcile_boolprops_folder_children: parent not found") ?;
  let goals : Vec<FileProperty> = FileProperty::ALL . into_iter ()
    . filter (|property| file_property_is_true (&node . misc, *property))
    . collect ();
  complete_relevant_children_in_viewnodetree (
    tree, folder_id,
    |viewnode| matches! (&viewnode . kind,
      ViewNodeKind::Qual (Qual::BoolProp { .. })),
    |viewnode| match &viewnode . kind {
      ViewNodeKind::Qual (Qual::BoolProp { property, .. }) => Ok (*property),
      _ => Err ("relevant child is not a Property" . to_string ()), },
    &goals,
    |property| Ok (ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewNodeKind::Qual (Qual::BoolProp {
        property : *property,
        title    : String::new (),
        body     : None, }), })) ?;
  // The key-based reconciler retains an existing leaf. Restore its generated
  // titleless shape too, so title edits never linger after a refresh.
  treat_certain_children (
    tree, folder_id,
    |viewnode| matches! (&viewnode . kind,
      ViewNodeKind::Qual (Qual::BoolProp { .. })),
    |viewnode| if let ViewNodeKind::Qual (Qual::BoolProp {
      title, body, .. }) = &mut viewnode . kind
    { *title = String::new ();
      *body = None; }) ?;
  Ok (())
}
