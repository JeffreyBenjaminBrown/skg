use super::{PreparedGraphUpdate, prepare_graph_update};
use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use crate::types::save::{DefineNode, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

fn config () -> SkgConfig {
  let source : SourceName = SourceName::from ("main");
  SkgConfig::dummyFromSources (HashMap::from ([
    (source . clone (), SkgfileSource {
      name         : source,
      abbreviation : None,
      path         : PathBuf::from ("unused"),
      user_owns_it : true,
    }),
  ]))
}

fn node (
  pid : &str,
) -> NodeComplete {
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node
}

#[test]
fn publication_uses_the_exact_prepared_candidate () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[node ("base")]));
  let graph : InRustGraphHandle = new_handle ((*base) . clone ());
  let actual_base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), actual_base, vec![DefineNode::Save (SaveNode (node ("new")))])
    . unwrap ();
  let expected : Arc<InRustGraph> = prepared . candidate () . clone ();
  let (_published, _definitions) : (Arc<InRustGraph>, Vec<DefineNode>) =
    prepared . publish (&graph) . unwrap ();
  let visible : Arc<InRustGraph> = graph . load_full ();
  assert! (Arc::ptr_eq (&visible, &expected));
}

#[test]
fn a_prepared_update_refuses_a_different_base () {
  let graph : InRustGraphHandle = new_handle (
    InRustGraph::from_nodecompletes (&[node ("base")]));
  let base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![DefineNode::Save (SaveNode (node ("new")))])
    . unwrap ();
  let replacement : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[node ("replacement")]));
  graph . store (replacement . clone ());
  assert! (prepared . publish (&graph) . is_err ());
  assert! (Arc::ptr_eq (&graph . load_full (), &replacement));
}

#[test]
fn preparation_owns_normalized_definitions () {
  let graph : InRustGraphHandle = new_handle (
    InRustGraph::from_nodecompletes (&[node ("base")]));
  let mut saved : NodeComplete = node ("new");
  saved . extra_ids = vec![
    ID::from ("E"), ID::from ("new"), ID::from ("E")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), graph . load_full (),
    vec![DefineNode::Save (SaveNode (saved))])
    . unwrap ();
  let DefineNode::Save (SaveNode (saved)) = &prepared . definitions () [0]
    else { panic! ("expected Save"); };
  assert_eq! (saved . extra_ids, vec![ID::from ("E")]);
}
