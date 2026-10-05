use skg::dbs::in_rust_graph::{InRustGraph, apply_nodeInstructions_to_inRustGraph};
use skg::dbs::in_rust_graph::internal_index_validation::{
  format_internal_index_mismatches, validate_internal_indexes,
};
use skg::types::misc::{ID, MSV, SkgrepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::save::{NodeInstruction, DeleteNode, SaveNode};

use proptest::prelude::*;

fn node (pid : &str) -> Graphnode {
  let mut node = empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node }

fn full_fixture () -> InRustGraph {
  let skgrepo = SkgrepoName::from ("main");
  let mut recorder = node ("recorder");
  recorder . contains = rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("target-alias"), ID::from ("dangling")]);
  recorder . subscribesTo = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("target-alias")]));
  recorder . hidesFromSubs = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("target-alias")]));
  recorder . overrides = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("target-alias")]));
  recorder . body = Some (
    "[[id:target-alias][target]] and [[id:text-dangling][missing]]" . to_string ());
  let mut target = node ("target");
  target . extra_ids = vec![ID::from ("target-alias")];
  InRustGraph::from_graphnodes (&[recorder, target])
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
  assert_eq! (actual . mentioners_of, expected . mentioners_of);
  assert_eq! (actual . extra_id_to_pid, expected . extra_id_to_pid);
}

#[test]
fn construction_recomputes_every_role_alias_and_dangling_key () {
  let graph = full_fixture ();
  assert_eq! (validate_internal_indexes (&graph), vec![]);
  assert! (graph . contained_by . contains_key (&ID::from ("target")));
  assert! (graph . contained_by . contains_key (&ID::from ("dangling")));
  assert! (graph . mentioners_of . contains_key (&ID::from ("target")));
  assert! (graph . mentioners_of . contains_key (&ID::from ("text-dangling")));
}

#[test]
fn exact_diagnostics_cover_all_six_corrupt_indexes_in_stable_order () {
  let mut graph = full_fixture ();
  graph . contained_by . remove (&ID::from ("target"));
  graph . subscribers_of . remove (&ID::from ("target"));
  graph . hiders_of . remove (&ID::from ("target"));
  graph . overriders_of . remove (&ID::from ("target"));
  graph . mentioners_of . remove (&ID::from ("target"));
  graph . extra_id_to_pid . insert (
    ID::from ("target-alias"), ID::from ("wrong"));
  let mismatches = validate_internal_indexes (&graph);
  assert_eq! (
    mismatches . iter () . map (|m| m . index) . collect::<Vec<_>> (),
    vec!["contained_by", "subscribers_of", "hiders_of", "overriders_of",
         "mentioners_of", "extra_id_to_pid"]);
  let formatted = format_internal_index_mismatches (&mismatches);
  assert! (formatted . contains (
    "index=extra_id_to_pid key=target-alias expected=[ID(\"target\")] actual=[ID(\"wrong\")]"));
}

#[test]
fn incremental_update_delete_and_extra_id_acquisition_remain_coherent () {
  let skgrepo = SkgrepoName::from ("main");
  let mut recorder = node ("recorder");
  recorder . contains = rel_partners_at_relRepo (&skgrepo, vec![ID::from ("future-alias")]);
  let mut disposable = node ("disposable");
  disposable . contains = rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("delete-target")]);
  let mut graph = InRustGraph::from_graphnodes (&[recorder . clone (), disposable]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);

  recorder . subscribesTo = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("future-alias")]));
  apply_nodeInstructions_to_inRustGraph (
    &mut graph, &[NodeInstruction::Save (SaveNode (recorder))]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);

  let mut target = node ("target");
  target . extra_ids = vec![ID::from ("future-alias")];
  apply_nodeInstructions_to_inRustGraph (
    &mut graph, &[NodeInstruction::Save (SaveNode (target))]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
  assert! (graph . contained_by . get (&ID::from ("target")) . unwrap ()
    . contains (&ID::from ("recorder")));

  apply_nodeInstructions_to_inRustGraph (&mut graph, &[NodeInstruction::Delete (DeleteNode {
    skgid : ID::from ("disposable"), home_skgrepo: skgrepo,
  })]);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
  assert! (! graph . contained_by . contains_key (&ID::from ("delete-target")));
}

#[test]
fn alias_acquisition_rekeys_all_five_inverse_indexes () {
  let skgrepo         : SkgrepoName = SkgrepoName::from ("main");
  let mut recorder : Graphnode = node ("recorder");
  recorder . contains = rel_partners_at_relRepo (&skgrepo, vec![ID::from ("future")]);
  recorder . subscribesTo = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("future")]));
  recorder . hidesFromSubs = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("future")]));
  recorder . overrides = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("future")]));
  recorder . body = Some ("[[id:future][future]]" . to_string ());
  let mut graph : InRustGraph =
    InRustGraph::from_graphnodes (&[recorder . clone ()]);
  let mut target : Graphnode = node ("target");
  target . extra_ids = vec![ID::from ("future")];
  apply_nodeInstructions_to_inRustGraph (
    &mut graph, &[NodeInstruction::Save (SaveNode (target . clone ())) ]);

  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . mentioners_of,
  ] {
    assert_eq! (
      index . get (&ID::from ("target")),
      Some (&std::iter::once (ID::from ("recorder")) . collect ()));
    assert! (! index . contains_key (&ID::from ("future"))); }
  let rebuilt : InRustGraph = InRustGraph::from_graphnodes (&[recorder, target]);
  assert_same_graph (&graph, &rebuilt);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
}

#[test]
fn merge_transfer_rekeys_primary_and_extra_spellings () {
  let skgrepo         : SkgrepoName = SkgrepoName::from ("main");
  let mut recorder : Graphnode = node ("recorder");
  recorder . contains = rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("acquiree"), ID::from ("old-extra")]);
  let mut acquiree : Graphnode = node ("acquiree");
  acquiree . extra_ids = vec![ID::from ("old-extra")];
  let acquirer : Graphnode = node ("acquirer");
  let mut graph : InRustGraph = InRustGraph::from_graphnodes (&[
    recorder . clone (), acquiree, acquirer,
  ]);
  let mut merged : Graphnode = node ("acquirer");
  merged . extra_ids = vec![ID::from ("acquiree"), ID::from ("old-extra")];
  apply_nodeInstructions_to_inRustGraph (&mut graph, &[
    NodeInstruction::Save (SaveNode (merged . clone ())),
    NodeInstruction::Delete (DeleteNode {
      skgid           : ID::from ("acquiree"),
      home_skgrepo : skgrepo,
    }),
  ]);

  assert_eq! (
    graph . contained_by . get (&ID::from ("acquirer")),
    Some (&std::iter::once (ID::from ("recorder")) . collect ()));
  assert! (! graph . contained_by . contains_key (&ID::from ("acquiree")));
  let rebuilt : InRustGraph = InRustGraph::from_graphnodes (&[recorder, merged]);
  assert_same_graph (&graph, &rebuilt);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
}

#[test]
fn deletion_rekeys_surviving_raw_primary_extra_and_text_references () {
  let skgrepo      : SkgrepoName = SkgrepoName::from ("main");
  let mut recorder : Graphnode = node ("foreign-recorder");
  let raw_skgids : Vec<ID> = vec![ID::from ("target"), ID::from ("extra")];
  recorder . contains = rel_partners_at_relRepo (&skgrepo, raw_skgids . clone ());
  recorder . subscribesTo = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, raw_skgids . clone ()));
  recorder . hidesFromSubs = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, raw_skgids . clone ()));
  recorder . overrides = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, raw_skgids . clone ()));
  recorder . body = Some (
    "[[id:target][primary]] [[id:extra][extra]]" . to_string ());
  let mut target : Graphnode = node ("target");
  target . extra_ids = vec![ID::from ("extra")];
  let mut graph : InRustGraph = InRustGraph::from_graphnodes (&[
    recorder . clone (), target,
  ]);
  apply_nodeInstructions_to_inRustGraph (&mut graph, &[
    NodeInstruction::Delete (DeleteNode {
      skgid     : ID::from ("target"),
      home_skgrepo: skgrepo,
    }),
  ]);

  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . mentioners_of,
  ] {
    for raw in &raw_skgids {
      assert! (index . get (raw) . unwrap () . contains (&ID::from ("foreign-recorder"))); }}
  let rebuilt : InRustGraph = InRustGraph::from_graphnodes (&[recorder]);
  assert_same_graph (&graph, &rebuilt);
  assert_eq! (validate_internal_indexes (&graph), vec![]);
}

#[test]
fn changed_recorder_removes_and_adds_all_five_inverse_contributions () {
  let skgrepo    : SkgrepoName = SkgrepoName::from ("main");
  let mut old : Graphnode = node ("recorder");
  old . contains = rel_partners_at_relRepo (&skgrepo, vec![ID::from ("old")]);
  old . subscribesTo = MSV::Specified (old . contains . clone ());
  old . hidesFromSubs = MSV::Specified (old . contains . clone ());
  old . overrides = MSV::Specified (old . contains . clone ());
  old . body = Some ("[[id:old][old]]" . to_string ());
  let mut final_node : Graphnode = node ("recorder");
  final_node . contains = rel_partners_at_relRepo (&skgrepo, vec![ID::from ("new")]);
  final_node . subscribesTo = MSV::Specified (final_node . contains . clone ());
  final_node . hidesFromSubs =
    MSV::Specified (final_node . contains . clone ());
  final_node . overrides = MSV::Specified (final_node . contains . clone ());
  final_node . body = Some ("[[id:new][new]]" . to_string ());
  let mut graph : InRustGraph = InRustGraph::from_graphnodes (&[old]);
  apply_nodeInstructions_to_inRustGraph (
    &mut graph, &[NodeInstruction::Save (SaveNode (final_node . clone ())) ]);

  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . mentioners_of,
  ] {
    assert! (! index . contains_key (&ID::from ("old")));
    assert! (index . get (&ID::from ("new")) . unwrap ()
      . contains (&ID::from ("recorder"))); }
  let rebuilt : InRustGraph = InRustGraph::from_graphnodes (&[final_node]);
  assert_same_graph (&graph, &rebuilt);
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (128))]

  #[test]
  fn batch_mutation_matches_a_full_rebuild (
    raw_indexes in proptest::collection::vec (0usize..6, 0..10),
    action in 0usize..3,
  ) {
    let skgrepo : SkgrepoName = SkgrepoName::from ("main");
    let universe : [&str; 6] = ["P", "E", "Q", "U", "X", "Y"];
    let raw_skgids : Vec<ID> = raw_indexes . iter ()
      . map (|index| ID::from (universe [*index]))
      . collect ();
    let mut recorder : Graphnode = node ("recorder");
    recorder . contains = rel_partners_at_relRepo (&skgrepo, raw_skgids . clone ());
    recorder . subscribesTo = MSV::Specified (recorder . contains . clone ());
    recorder . hidesFromSubs = MSV::Specified (recorder . contains . clone ());
    recorder . overrides = MSV::Specified (recorder . contains . clone ());
    recorder . body = Some (raw_indexes . iter ()
      . map (|index| format! ("[[id:{}][x]]", universe [*index]))
      . collect::<Vec<String>> () . join (" "));
    let mut target : Graphnode = node ("P");
    target . extra_ids = vec![ID::from ("E")];
    let acquirer : Graphnode = node ("Q");
    let mut graph : InRustGraph = InRustGraph::from_graphnodes (&[
      recorder . clone (), target . clone (), acquirer . clone (),
    ]);
    let (nodeInstructions, final_nodes) : (Vec<NodeInstruction>, Vec<Graphnode>) =
      match action {
        0 => (vec![NodeInstruction::Delete (DeleteNode {
                skgid : ID::from ("P"), home_skgrepo : skgrepo . clone (),
              })],
              vec![recorder, acquirer]),
        1 => {
          let mut merged : Graphnode = acquirer;
          merged . extra_ids = vec![ID::from ("P"), ID::from ("E")];
          (vec![
             NodeInstruction::Save (SaveNode (merged . clone ())),
             NodeInstruction::Delete (DeleteNode {
               skgid : ID::from ("P"), home_skgrepo : skgrepo . clone (),
             }),
           ], vec![recorder, merged])
        },
        _ => {
          let mut added : Graphnode = node ("R");
          added . extra_ids = vec![ID::from ("U")];
          (vec![NodeInstruction::Save (SaveNode (added . clone ()))],
           vec![recorder, target, acquirer, added])
        },
      };
    apply_nodeInstructions_to_inRustGraph (&mut graph, &nodeInstructions);
    let rebuilt : InRustGraph = InRustGraph::from_graphnodes (&final_nodes);
    assert_same_graph (&graph, &rebuilt);
    prop_assert_eq! (validate_internal_indexes (&graph), vec![]);
  }
}
