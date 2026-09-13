use skg::dbs::in_rust_graph::{InRustGraph, apply_definenodes_to_inRustGraph};
use skg::dbs::in_rust_graph::internal_index_validation::{
  format_internal_index_mismatches, validate_internal_indexes,
};
use skg::types::misc::{ID, MSV, SourceName, members_at_source};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::{DefineNode, DeleteNode, SaveNode};

use proptest::prelude::*;

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

fn assert_same_graph (
  actual   : &InRustGraph,
  expected : &InRustGraph,
) {
  assert_eq! (actual . nodes, expected . nodes);
  assert_eq! (actual . contained_by, expected . contained_by);
  assert_eq! (actual . subscribers_of, expected . subscribers_of);
  assert_eq! (actual . hiders_of, expected . hiders_of);
  assert_eq! (actual . overriders_of, expected . overriders_of);
  assert_eq! (actual . textlinks_in, expected . textlinks_in);
  assert_eq! (actual . extra_id_to_pid, expected . extra_id_to_pid);
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

#[test]
fn alias_acquisition_rekeys_all_five_inverse_indexes () {
  let source : SourceName = SourceName::from ("main");
  let mut owner : NodeComplete = node ("owner");
  owner . contains = members_at_source (&source, vec![ID::from ("future")]);
  owner . subscribes_to = MSV::Specified (members_at_source (
    &source, vec![ID::from ("future")]));
  owner . hides_from_its_subscriptions = MSV::Specified (members_at_source (
    &source, vec![ID::from ("future")]));
  owner . overrides_view_of = MSV::Specified (members_at_source (
    &source, vec![ID::from ("future")]));
  owner . body = Some ("[[id:future][future]]" . to_string ());
  let mut graph : InRustGraph =
    InRustGraph::from_nodecompletes (&[owner . clone ()]);
  let mut target : NodeComplete = node ("target");
  target . extra_ids = vec![ID::from ("future")];
  apply_definenodes_to_inRustGraph (
    &mut graph, &[DefineNode::Save (SaveNode (target . clone ())) ]);

  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . textlinks_in,
  ] {
    assert_eq! (
      index . get (&ID::from ("target")),
      Some (&std::iter::once (ID::from ("owner")) . collect ()));
    assert! (! index . contains_key (&ID::from ("future"))); }
  let rebuilt : InRustGraph = InRustGraph::from_nodecompletes (&[owner, target]);
  assert_same_graph (&graph, &rebuilt);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
}

#[test]
fn merge_transfer_rekeys_primary_and_extra_spellings () {
  let source : SourceName = SourceName::from ("main");
  let mut owner : NodeComplete = node ("owner");
  owner . contains = members_at_source (
    &source, vec![ID::from ("acquiree"), ID::from ("old-extra")]);
  let mut acquiree : NodeComplete = node ("acquiree");
  acquiree . extra_ids = vec![ID::from ("old-extra")];
  let acquirer : NodeComplete = node ("acquirer");
  let mut graph : InRustGraph = InRustGraph::from_nodecompletes (&[
    owner . clone (), acquiree, acquirer,
  ]);
  let mut merged : NodeComplete = node ("acquirer");
  merged . extra_ids = vec![ID::from ("acquiree"), ID::from ("old-extra")];
  apply_definenodes_to_inRustGraph (&mut graph, &[
    DefineNode::Save (SaveNode (merged . clone ())),
    DefineNode::Delete (DeleteNode {
      id     : ID::from ("acquiree"),
      source : source,
    }),
  ]);

  assert_eq! (
    graph . contained_by . get (&ID::from ("acquirer")),
    Some (&std::iter::once (ID::from ("owner")) . collect ()));
  assert! (! graph . contained_by . contains_key (&ID::from ("acquiree")));
  let rebuilt : InRustGraph = InRustGraph::from_nodecompletes (&[owner, merged]);
  assert_same_graph (&graph, &rebuilt);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
}

#[test]
fn deletion_rekeys_surviving_raw_primary_extra_and_text_references () {
  let source : SourceName = SourceName::from ("main");
  let mut owner : NodeComplete = node ("foreign-owner");
  let raw_ids : Vec<ID> = vec![ID::from ("target"), ID::from ("extra")];
  owner . contains = members_at_source (&source, raw_ids . clone ());
  owner . subscribes_to = MSV::Specified (members_at_source (
    &source, raw_ids . clone ()));
  owner . hides_from_its_subscriptions = MSV::Specified (members_at_source (
    &source, raw_ids . clone ()));
  owner . overrides_view_of = MSV::Specified (members_at_source (
    &source, raw_ids . clone ()));
  owner . body = Some (
    "[[id:target][primary]] [[id:extra][extra]]" . to_string ());
  let mut target : NodeComplete = node ("target");
  target . extra_ids = vec![ID::from ("extra")];
  let mut graph : InRustGraph = InRustGraph::from_nodecompletes (&[
    owner . clone (), target,
  ]);
  apply_definenodes_to_inRustGraph (&mut graph, &[
    DefineNode::Delete (DeleteNode {
      id     : ID::from ("target"),
      source,
    }),
  ]);

  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . textlinks_in,
  ] {
    for raw in &raw_ids {
      assert! (index . get (raw) . unwrap () . contains (&ID::from ("foreign-owner"))); }}
  let rebuilt : InRustGraph = InRustGraph::from_nodecompletes (&[owner]);
  assert_same_graph (&graph, &rebuilt);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
}

#[test]
fn changed_owner_removes_and_adds_all_five_inverse_contributions () {
  let source : SourceName = SourceName::from ("main");
  let mut old : NodeComplete = node ("owner");
  old . contains = members_at_source (&source, vec![ID::from ("old")]);
  old . subscribes_to = MSV::Specified (old . contains . clone ());
  old . hides_from_its_subscriptions = MSV::Specified (old . contains . clone ());
  old . overrides_view_of = MSV::Specified (old . contains . clone ());
  old . body = Some ("[[id:old][old]]" . to_string ());
  let mut final_node : NodeComplete = node ("owner");
  final_node . contains = members_at_source (&source, vec![ID::from ("new")]);
  final_node . subscribes_to = MSV::Specified (final_node . contains . clone ());
  final_node . hides_from_its_subscriptions =
    MSV::Specified (final_node . contains . clone ());
  final_node . overrides_view_of = MSV::Specified (final_node . contains . clone ());
  final_node . body = Some ("[[id:new][new]]" . to_string ());
  let mut graph : InRustGraph = InRustGraph::from_nodecompletes (&[old]);
  apply_definenodes_to_inRustGraph (
    &mut graph, &[DefineNode::Save (SaveNode (final_node . clone ())) ]);

  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . textlinks_in,
  ] {
    assert! (! index . contains_key (&ID::from ("old")));
    assert! (index . get (&ID::from ("new")) . unwrap ()
      . contains (&ID::from ("owner"))); }
  let rebuilt : InRustGraph = InRustGraph::from_nodecompletes (&[final_node]);
  assert_same_graph (&graph, &rebuilt);
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (128))]

  #[test]
  fn batch_mutation_matches_a_full_rebuild (
    raw_indexes in proptest::collection::vec (0usize..6, 0..10),
    action in 0usize..3,
  ) {
    let source : SourceName = SourceName::from ("main");
    let universe : [&str; 6] = ["P", "E", "Q", "U", "X", "Y"];
    let raw_ids : Vec<ID> = raw_indexes . iter ()
      . map (|index| ID::from (universe [*index]))
      . collect ();
    let mut owner : NodeComplete = node ("owner");
    owner . contains = members_at_source (&source, raw_ids . clone ());
    owner . subscribes_to = MSV::Specified (owner . contains . clone ());
    owner . hides_from_its_subscriptions = MSV::Specified (owner . contains . clone ());
    owner . overrides_view_of = MSV::Specified (owner . contains . clone ());
    owner . body = Some (raw_indexes . iter ()
      . map (|index| format! ("[[id:{}][x]]", universe [*index]))
      . collect::<Vec<String>> () . join (" "));
    let mut target : NodeComplete = node ("P");
    target . extra_ids = vec![ID::from ("E")];
    let acquirer : NodeComplete = node ("Q");
    let mut graph : InRustGraph = InRustGraph::from_nodecompletes (&[
      owner . clone (), target . clone (), acquirer . clone (),
    ]);
    let (definitions, final_nodes) : (Vec<DefineNode>, Vec<NodeComplete>) =
      match action {
        0 => (vec![DefineNode::Delete (DeleteNode {
                id : ID::from ("P"), source : source . clone (),
              })],
              vec![owner, acquirer]),
        1 => {
          let mut merged : NodeComplete = acquirer;
          merged . extra_ids = vec![ID::from ("P"), ID::from ("E")];
          (vec![
             DefineNode::Save (SaveNode (merged . clone ())),
             DefineNode::Delete (DeleteNode {
               id : ID::from ("P"), source : source . clone (),
             }),
           ], vec![owner, merged])
        },
        _ => {
          let mut added : NodeComplete = node ("R");
          added . extra_ids = vec![ID::from ("U")];
          (vec![DefineNode::Save (SaveNode (added . clone ()))],
           vec![owner, target, acquirer, added])
        },
      };
    apply_definenodes_to_inRustGraph (&mut graph, &definitions);
    let rebuilt : InRustGraph = InRustGraph::from_nodecompletes (&final_nodes);
    assert_same_graph (&graph, &rebuilt);
    prop_assert_eq! (validate_internal_indexes (&graph), vec![]);
  }
}
