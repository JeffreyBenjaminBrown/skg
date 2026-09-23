use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::nodecomplete_from_graph;
use crate::to_org::util::{get_id_from_treenode, remove_completed_view_request};
use crate::types::misc::{ID, SkgConfig};
use crate::types::nodes::complete::{
  FileProperty, NodeComplete, file_property_is_true};
use crate::types::tree::viewnode_nodecomplete::{
  insert_scaffold_as_child, unique_scaffold_child_of_viewnode};
use crate::types::viewnode::{
  Qual, QualFolder, ViewNode, ViewNodeKind, ViewRequest};

use ego_tree::Tree;
use std::error::Error;

pub fn build_and_integrate_boolprops_then_drop_request (
  tree    : &mut Tree<ViewNode>,
  node_id : ego_tree::NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  errors  : &mut Vec<String>,
) -> Result<(), Box<dyn Error>> {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_boolprops (tree, node_id, graph, config);
  remove_completed_view_request (
    tree, node_id, ViewRequest::BoolProps,
    "Failed to integrate properties view", errors, result )
}

/// Add the read-only folder of true file properties.  The folder itself is
/// useful even when empty, so it is always created and preserved.
pub fn build_and_integrate_boolprops (
  tree     : &mut Tree<ViewNode>,
  node_id  : ego_tree::NodeId,
  graph    : &InRustGraph,
  _config  : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  if unique_scaffold_child_of_viewnode (
    tree, node_id,
    &ViewNodeKind::QualFolder (QualFolder::boolprops ())) ? . is_some ()
  { return Ok (()); }
  let pid : ID = get_id_from_treenode (tree, node_id) ?;
  let node : Option<NodeComplete> = nodecomplete_from_graph (graph, &pid);
  let folder : ego_tree::NodeId = insert_scaffold_as_child (
    tree, node_id,
    ViewNodeKind::QualFolder (QualFolder::boolprops ()), false ) ?;
  if let Some (node) = node {
    for property in FileProperty::ALL {
      if file_property_is_true (&node . misc, property) {
        insert_scaffold_as_child (
          tree, folder,
          ViewNodeKind::Qual (Qual::BoolProp {
            property,
            title : String::new (),
            body  : None, }),
          false ) ?; } } }
  Ok (())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
  use crate::org_to_text::viewforest_to_string;
  use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
  use crate::types::misc::{SourceName, SkgfileSource};
  use crate::types::nodes::complete::empty_node_complete;
  use crate::types::viewnode::mk_definitive_viewnode;
  use std::collections::HashMap;
  use std::path::PathBuf;
  use indoc::indoc;

  fn config () -> SkgConfig {
    let source = SourceName::from ("main");
    SkgConfig::fromSourcesAndTantivyFolder (HashMap::from ([
      (source . clone (), SkgfileSource {
        name: source, abbreviation: None, path: PathBuf::from ("main"),
        user_owns_it: true })]), "/tmp/none")
  }

  #[test]
  fn builder_emits_true_viewnodes_in_registry_order_and_keeps_empty_folder () {
    let source = SourceName::from ("main");
    let rich = NodeComplete {
      pid: ID::from ("rich"), title: "Rich" . to_string (),
      source: source . clone (),
      misc: vec![FileProperty::NoSearchMatching,
                 FileProperty::Had_ID_Before_Import],
      .. empty_node_complete () };
    let empty = NodeComplete {
      pid: ID::from ("empty"), title: "Empty" . to_string (),
      source: source . clone (), .. empty_node_complete () };
    let graph = InRustGraph::from_nodecompletes (&[rich, empty]);
    let mut tree = Tree::new (mk_definitive_viewnode (
      ID::from ("rich"), source . clone (), "Rich" . to_string (), None));
    let root = tree . root () . id ();
    build_and_integrate_boolprops (&mut tree, root, &graph, &config ())
      . unwrap ();
    let folder = tree . get (root) . unwrap () . children () . next () . unwrap ();
    let viewnodes : Vec<(FileProperty, String)> = folder . children ()
      . map (|child| match &child . value () . kind {
        ViewNodeKind::Qual (Qual::BoolProp { property, title, .. }) =>
          (*property, title . clone ()),
        other => panic! ("unexpected viewnode: {:?}", other), })
      . collect ();
    assert_eq! (viewnodes, vec![
      (FileProperty::Had_ID_Before_Import, String::new ()),
      (FileProperty::NoSearchMatching, String::new ())]);

    let mut empty_tree = Tree::new (mk_definitive_viewnode (
      ID::from ("empty"), source, "Empty" . to_string (), None));
    let empty_root = empty_tree . root () . id ();
    build_and_integrate_boolprops (
      &mut empty_tree, empty_root, &graph, &config ()) . unwrap ();
    let empty_folder = empty_tree . get (empty_root) . unwrap ()
      . children () . next () . unwrap ();
    assert! (matches! (&empty_folder . value () . kind,
      ViewNodeKind::QualFolder (QualFolder::BoolProps { .. })));
    assert_eq! (empty_folder . children () . count (), 0);
  }

  #[test]
  fn boolprops_surface_render_parse_round_trip_uses_all_canonical_atoms (
  ) {
    let org = indoc! {"
      * (skg (node (id owner) (source main))) Owner
      ** (skg propertiesFolder)
      *** (skg (property hadId))
      *** (skg (property wasOverloaded))
      *** (skg (property noSearchMatching))
    "};
    let (maybe, errors, warnings) =
      org_to_uninterpreted_viewforest (org) . unwrap ();
    assert! (errors . is_empty (), "{:?}", errors);
    assert! (warnings . is_empty (), "{:?}", warnings);
    let placed = maybePlaced_to_placed_viewforest (maybe) . unwrap ();
    assert_eq! (viewforest_to_string (&placed, &config ()) . unwrap (), org);
  }
}
