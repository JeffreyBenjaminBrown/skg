use skg::dbs::in_rust_graph::{InRustGraph, apply_definenodes_to_inRustGraph};
use skg::dbs::in_rust_graph::internal_index_validation::{
  format_internal_index_mismatches, validate_internal_indexes,
};
use skg::types::misc::{ID, MSV, SourceName, members_at_source};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::{DefineNode, DeleteNode, SaveNode};

fn node (pid : &str) -> NodeComplete {
  let mut node = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node }

fn full_fixture () -> InRustGraph {
  let source = SourceName::from ("main");
  let mut owner = node ("owner");
  owner . contains = members_at_source (
    &source, vec![ID::from ("target-alias"), ID::from ("dangling")]);
  owner . subscribes_to = MSV::Specified (members_at_source (
    &source, vec![ID::from ("target-alias")]));
  owner . hides_from_its_subscriptions = MSV::Specified (members_at_source (
    &source, vec![ID::from ("target-alias")]));
  owner . overrides_view_of = MSV::Specified (members_at_source (
    &source, vec![ID::from ("target-alias")]));
  owner . body = Some (
    "[[id:target-alias][target]] and [[id:text-dangling][missing]]" . to_string ());
  let mut target = node ("target");
  target . extra_ids = vec![ID::from ("target-alias")];
  InRustGraph::from_nodecompletes (&[owner, target])
}

#[test]
fn construction_recomputes_every_role_alias_and_dangling_key () {
  let graph = full_fixture ();
  assert_eq! (validate_internal_indexes (&graph), vec![]);
  assert! (graph . contained_by . contains_key (&ID::from ("target")));
  assert! (graph . contained_by . contains_key (&ID::from ("dangling")));
  assert! (graph . textlinks_in . contains_key (&ID::from ("target")));
  assert! (graph . textlinks_in . contains_key (&ID::from ("text-dangling")));
}

#[test]
fn exact_diagnostics_cover_all_six_corrupt_indexes_in_stable_order () {
  let mut graph = full_fixture ();
  graph . contained_by . remove (&ID::from ("target"));
  graph . subscribers_of . remove (&ID::from ("target"));
  graph . hiders_of . remove (&ID::from ("target"));
  graph . overriders_of . remove (&ID::from ("target"));
  graph . textlinks_in . remove (&ID::from ("target"));
  graph . extra_id_to_pid . insert (
    ID::from ("target-alias"), ID::from ("wrong"));
  let mismatches = validate_internal_indexes (&graph);
  assert_eq! (
    mismatches . iter () . map (|m| m . index) . collect::<Vec<_>> (),
    vec!["contained_by", "subscribers_of", "hiders_of", "overriders_of",
         "textlinks_in", "extra_id_to_pid"]);
  let formatted = format_internal_index_mismatches (&mismatches);
  assert! (formatted . contains (
    "index=extra_id_to_pid key=target-alias expected=[ID(\"target\")] actual=[ID(\"wrong\")]"));
}

#[test]
fn incremental_update_delete_and_extra_id_acquisition_remain_coherent () {
  let source = SourceName::from ("main");
  let mut owner = node ("owner");
  owner . contains = members_at_source (&source, vec![ID::from ("future-alias")]);
  let mut disposable = node ("disposable");
  disposable . contains = members_at_source (
    &source, vec![ID::from ("delete-target")]);
  let mut graph = InRustGraph::from_nodecompletes (&[owner . clone (), disposable]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);

  owner . subscribes_to = MSV::Specified (members_at_source (
    &source, vec![ID::from ("future-alias")]));
  apply_definenodes_to_inRustGraph (
    &mut graph, &[DefineNode::Save (SaveNode (owner))]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);

  let mut target = node ("target");
  target . extra_ids = vec![ID::from ("future-alias")];
  apply_definenodes_to_inRustGraph (
    &mut graph, &[DefineNode::Save (SaveNode (target))]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
  assert! (graph . contained_by . get (&ID::from ("target")) . unwrap ()
    . contains (&ID::from ("owner")));

  apply_definenodes_to_inRustGraph (&mut graph, &[DefineNode::Delete (DeleteNode {
    id : ID::from ("disposable"), source,
  })]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
  assert! (! graph . contained_by . contains_key (&ID::from ("delete-target")));
}
